package models

import akka.actor.ActorRef
import play.api.Configuration
import play.api.cache.CacheApi
import play.api.db.Database
import play.api.mvc.Request

case class WikiContext(name: String)(implicit
                                     val request: Request[Any],
                                     val cacheApi: CacheApi,
                                     val database: Database,
                                     val actorAhaWiki: ActorRef,
                                     val configuration: Configuration
)


