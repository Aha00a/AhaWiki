package models

import org.apache.pekko.actor.ActorRef
import logics.AhaWikiCache
import logics.ApplicationConf
import play.api.db.Database

class Context()(
  implicit
  val database: Database,
  val actorAhaWiki: ActorRef,
  val applicationConf: ApplicationConf,
  val ahaWikiCache: AhaWikiCache,
) {
}
