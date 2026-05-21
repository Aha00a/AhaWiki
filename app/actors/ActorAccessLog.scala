package actors

import actors.ActorAccessLog._
import models.tables.{AccessLog, IpDeny, Site}
import play.api.Logging
import play.api.db.Database

import java.sql.Connection
import javax.inject.Inject

object ActorAccessLog {
  final case class Insert(
    site: Site,
    user: Option[Long],
    ipDeny: Option[Long],
    method: String,
    scheme: String,
    host: String,
    uri: String,
    remoteAddress: String,
    userAgent: String,
    status: Int,
    duration: Int,
  )

  final case class InsertAndDeny(insert: Insert, reason: String)
}

class ActorAccessLog @Inject()(database: Database) extends org.apache.pekko.actor.Actor with Logging {
  override def receive: PartialFunction[Any, Unit] = {
    case insert: Insert =>
      run("Insert", insert) {
        database.withConnection { implicit connection =>
          insertAccessLogIfSiteFound(insert)
        }
      }

    case InsertAndDeny(insert, reason) =>
      run("InsertAndDeny", insert) {
        database.withConnection { implicit connection =>
          val accessLogSeq = insertAccessLogIfSiteFound(insert)
          IpDeny.insert(insert.remoteAddress, accessLogSeq, reason)
        }
      }

    case unknown =>
      logger.error(s"Unknown AccessLog actor message: ${unknown.getClass.getName}")
  }

  private def insertAccessLogIfSiteFound(insert: Insert)(implicit connection: Connection): Option[Long] = {
    if (insert.site.isNotFound) {
      logger.warn(
        s"Skip AccessLog insert: site not found for host=${insert.host}, uri=${insert.uri}, remoteAddress=${insert.remoteAddress}"
      )
      None
    } else {
      AccessLog.insert(
        insert.site.seq,
        insert.user,
        insert.ipDeny,
        insert.method,
        insert.scheme,
        insert.host,
        insert.uri,
        insert.remoteAddress,
        insert.userAgent,
        insert.status,
        insert.duration,
      )
    }
  }

  private def run(label: String, insert: Insert)(body: => Unit): Unit = {
    try {
      body
    } catch {
      case t: Throwable =>
        logger.error(
          s"Async AccessLog write failed: $label ${insert.method} ${insert.status} ${insert.host}${insert.uri} ${insert.remoteAddress}",
          t,
        )
    }
  }
}
