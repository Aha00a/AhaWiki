package logics

import logics.CrossInstanceBus._
import play.api.inject.ApplicationLifecycle
import play.api.libs.json.Json
import play.api.{Configuration, Environment, Logging, Mode}
import redis.clients.jedis.{DefaultJedisClientConfig, HostAndPort, Jedis, JedisPooled, JedisPubSub}

import java.util.UUID
import javax.inject._
import scala.concurrent.Future
import scala.util.Try
import scala.util.control.NonFatal

/**
 * `page.updated` across the two instances.
 *
 * [[PageCursorHub]] is process-local, so a save on one instance is not pushed to watchers on the
 * other (see the wiki page Dev WebSocket). This relays only `page.updated` -- low-frequency, and
 * the event Kanban realtime and the refresh toast depend on -- over Redis pub/sub on the cache
 * Redis both instances already share. Cursors stay instance-local: they are high-frequency and
 * cosmetic, so fanning every move out to every instance would cost far more than it is worth.
 *
 * Delivery is at-most-once and unordered, which is enough: the client ignores a `page.updated`
 * whose revision it has already seen. If Redis is unreachable the publish is logged and skipped,
 * degrading to the old instance-local behaviour; local watchers are served either way because
 * [[publishPageUpdated]] delivers locally before it publishes, and the sender skips its own
 * message when it comes back on the channel.
 */
@Singleton
class CrossInstanceBus @Inject()(
  configuration: Configuration,
  environment: Environment,
  lifecycle: ApplicationLifecycle,
) extends Logging {

  private val instanceId: String = UUID.randomUUID().toString
  private val host: String = configuration.getOptional[String]("play.cache.redis.host").filter(_.nonEmpty).getOrElse("localhost")
  private val port: Int = configuration.getOptional[Int]("play.cache.redis.port").getOrElse(6379)
  private val database: Int = configuration.getOptional[Int]("play.cache.redis.database").getOrElse(0)
  private val password: Option[String] = configuration.getOptional[String]("play.cache.redis.password").filter(_.nonEmpty)
  // pub/sub is not scoped to a Redis db, so the db goes in the channel name to keep two apps on the
  // same Redis but different dbs from hearing each other.
  private val channel: String = s"ahawiki:$database:page.updated"

  // Off under test -- there is no Redis there, and an eager singleton would otherwise spin trying to
  // reconnect. A config flag can also force it off in an environment that has Redis but should not relay.
  private val enabled: Boolean =
    environment.mode != Mode.Test &&
      configuration.getOptional[Boolean]("AhaWiki.realtime.crossInstance.enabled").getOrElse(true)

  @volatile private var running: Boolean = enabled

  private def clientConfig: DefaultJedisClientConfig =
    DefaultJedisClientConfig.builder().database(database).password(password.orNull).build()

  // The pool does not connect until first use, so constructing it never throws even if Redis is down.
  private lazy val publisher: JedisPooled = new JedisPooled(new HostAndPort(host, port), clientConfig)

  private val pubSub: JedisPubSub = new JedisPubSub {
    override def onMessage(ch: String, message: String): Unit = deliverForeign(message)
  }

  if (enabled) {
    startSubscriber()
    logger.info(s"CrossInstanceBus on: instance=$instanceId channel=$channel redis=$host:$port")
  } else {
    logger.info("CrossInstanceBus off (test mode or disabled) -- page.updated stays instance-local")
  }

  /** Deliver to local watchers now, then fan the event out to the other instance. */
  def publishPageUpdated(roomKey: String, saveSenderId: Option[String], payload: String): Unit = {
    PageCursorHub.broadcastPageUpdated(roomKey, saveSenderId, payload)
    if (enabled) {
      try publisher.publish(channel, encode(instanceId, roomKey, saveSenderId, payload))
      catch { case NonFatal(e) => logger.warn(s"CrossInstanceBus publish failed (delivered locally only): $e") }
    }
  }

  private def deliverForeign(message: String): Unit = decode(message) match {
    case Some(m) if m.origin != instanceId => PageCursorHub.broadcastPageUpdated(m.roomKey, m.saveSenderId, m.payload)
    case Some(_) => // our own publish, already delivered locally
    case None => logger.warn("CrossInstanceBus received an undecodable message")
  }

  private def startSubscriber(): Unit = {
    val thread = new Thread(() => {
      while (running) {
        var sub: Jedis = null
        try {
          sub = new Jedis(new HostAndPort(host, port), clientConfig)
          sub.subscribe(pubSub, channel) // blocks until unsubscribe() or a connection error
        } catch {
          case NonFatal(e) if running =>
            logger.warn(s"CrossInstanceBus subscriber dropped, retrying in 2s: $e")
            Try(Thread.sleep(2000))
          case NonFatal(_) => // stopping; fall through
        } finally {
          if (sub != null) Try(sub.close())
        }
      }
    }, "cross-instance-bus")
    thread.setDaemon(true)
    thread.start()
  }

  lifecycle.addStopHook { () =>
    running = false
    Try(pubSub.unsubscribe())
    Try(publisher.close())
    Future.successful(())
  }
}

object CrossInstanceBus {
  case class Msg(origin: String, roomKey: String, saveSenderId: Option[String], payload: String)

  /** Wire format on the channel: the room, the saver to exclude, and the exact client payload. */
  def encode(origin: String, roomKey: String, saveSenderId: Option[String], payload: String): String =
    Json.obj(
      "origin" -> origin,
      "roomKey" -> roomKey,
      "saveSenderId" -> saveSenderId,
      "payload" -> payload,
    ).toString()

  def decode(message: String): Option[Msg] =
    Try {
      val js = Json.parse(message)
      Msg(
        (js \ "origin").as[String],
        (js \ "roomKey").as[String],
        (js \ "saveSenderId").asOpt[String],
        (js \ "payload").as[String],
      )
    }.toOption
}
