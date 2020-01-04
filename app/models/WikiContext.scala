package models

import akka.actor.ActorRef
import logics.AhaWikiCache
import logics.wikis.RenderingMode
import logics.wikis.RenderingMode.RenderingMode
import play.api.Configuration
import play.api.cache.CacheApi
import play.api.db.Database
import play.api.mvc.Request


object WikiContext {
  def apply(name: String, renderingMode: RenderingMode = RenderingMode.Normal)(implicit request: Request[Any], cacheApi: CacheApi, database: Database, actorAhaWiki: ActorRef, configuration: Configuration): WikiContext = new WikiContext(name, renderingMode)(request, cacheApi, database, actorAhaWiki, configuration)
}

class WikiContext(val name: String, val renderingMode: RenderingMode = RenderingMode.Normal)
                 (implicit
                  val request: Request[Any],
                  val cacheApi: CacheApi,
                  val database: Database,
                  val actorAhaWiki: ActorRef,
                  val configuration: Configuration
                 ) {
}


