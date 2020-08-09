package logics

import akka.actor.ActorRef
import play.api.Configuration
import play.api.cache.SyncCacheApi
import play.api.db.Database

case class AhaWikiInjects()(
  implicit
  val syncCacheApi: SyncCacheApi,
  val database: Database,
  val actorAhaWiki: ActorRef,
  val configuration: Configuration
)
