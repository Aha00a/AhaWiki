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
  def apply(name: String)(
    implicit
    request: Request[Any],
    cacheApi: CacheApi,
    database: Database,
    actorAhaWiki: ActorRef,
    configuration: Configuration
  ): WikiContext = new WikiContext(Seq(name), RenderingMode.Normal)
  def preview(name: String)(
    implicit
    request: Request[Any],
    cacheApi: CacheApi,
    database: Database,
    actorAhaWiki: ActorRef,
    configuration: Configuration
  ): WikiContext = new WikiContext(Seq(name), RenderingMode.Preview)
}

class WikiContext(seqName: Seq[String], val renderingMode: RenderingMode)
                 (implicit
                  val request: Request[Any],
                  val cacheApi: CacheApi,
                  val database: Database,
                  val actorAhaWiki: ActorRef,
                  val configuration: Configuration
                 ) {
  def name: String = seqName.head
}


