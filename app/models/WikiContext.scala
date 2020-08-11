package models

import akka.actor.ActorRef
import logics.AhaWikiCache
import logics.AhaWikiInjects
import logics.IdProvider
import logics.wikis.{PageLogic, RenderingMode}
import logics.wikis.RenderingMode.RenderingMode
import play.api.Configuration
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.mvc.Request


object WikiContext {
  def apply(name: String)(
    implicit
    request: Request[Any],
    ahaWikiInjects: AhaWikiInjects
  ): WikiContext = {
    implicit val idProvider: IdProvider = IdProvider.createBy(request)
    new WikiContext(Seq(name), RenderingMode.Normal)
  }

  def preview(name: String)(
    implicit
    request: Request[Any],
    ahaWikiInjects: AhaWikiInjects
  ): WikiContext = {
    implicit val idProvider: IdProvider = IdProvider.createBy(request)
    new WikiContext(Seq(name), RenderingMode.Preview)
  }
}

class WikiContext(val seqName: Seq[String], val renderingMode: RenderingMode)
                 (implicit
                  val request: Request[Any],
                  val ahaWikiInjects: AhaWikiInjects,
                  val idProvider: IdProvider
                 ) {
  implicit val syncCacheApi: SyncCacheApi = ahaWikiInjects.syncCacheApi
  implicit val database: Database = ahaWikiInjects.database
  implicit val actorAhaWiki: ActorRef = ahaWikiInjects.actorAhaWiki
  implicit val configuration: Configuration = ahaWikiInjects.configuration

  import models.tables.PageWithoutContentWithSize

  def name: String = seqName.last
  def nameTop: String = seqName.head
  def nameBottom: String = seqName.last
  def push(name: String) = new WikiContext(name +: seqName, renderingMode)
  lazy val setPageNameAll: Set[String] = PageLogic.getListPage().map(_.name).toSet
  lazy val listPageByPermission: List[PageWithoutContentWithSize] = PageLogic.getListPageByPermission()
  lazy val seqPageNameByPermission: Seq[String] = listPageByPermission.map(_.name)
  lazy val setPageNameByPermission: Set[String] = seqPageNameByPermission.toSet
  def pageCanSee(name: String): Boolean = !setPageNameAll.contains(name) || setPageNameByPermission.contains(name)
}


