package com.aha00a

import com.typesafe.config.ConfigFactory
import org.apache.pekko.actor.ActorSystem
import org.scalatest.freespec.AnyFreeSpec

import scala.concurrent.Await
import scala.concurrent.duration._

/** What `conf/base.conf` has to say for production to hold up -- the one file the production
  * configuration includes, so a setting that lives anywhere else does not reach it.
  */
class ConfigurationSpec extends AnyFreeSpec {
  private val config = ConfigFactory.load("base")

  // A request reads the cache by blocking on a future, on pekko's default dispatcher, with a
  // database connection in hand. If the cache finishes its futures on that same dispatcher, a
  // burst of requests leaves no thread to finish them (see the comment in base.conf). The specs
  // run without Redis, so what is checked is the wiring: the name, and that pekko can find it.
  "the Redis cache finishes its futures on a dispatcher of its own" in {
    val name = config.getString("play.cache.redis.dispatcher")
    assert(name != "pekko.actor.default-dispatcher")

    val system = ActorSystem("configuration-spec", config)
    try assert(system.dispatchers.hasDispatcher(name))
    finally Await.result(system.terminate(), 10.seconds)
  }
}
