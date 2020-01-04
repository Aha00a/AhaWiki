package models

import akka.actor.ActorRef
import logics.AhaWikiCache
import logics.wikis.RenderingMode
import logics.wikis.RenderingMode.RenderingMode
import play.api.Configuration
import play.api.cache.CacheApi
import play.api.db.Database
import play.api.mvc.Request

case class WikiContext(name: String, renderingMode: RenderingMode = RenderingMode.Normal)
                      (implicit
                       val request: Request[Any],
                       val cacheApi: CacheApi,
                       val database: Database,
                       val actorAhaWiki: ActorRef,
                       val configuration: Configuration
                      ) {
  def existPage(name: String): Boolean = AhaWikiCache.PageNameSet.get().contains(name) // TODO: this may causes performance issue.
}


