package models

import java.util.Locale

import akka.actor.ActorRef
import com.aha00a.play.Implicits._
import logics.SessionLogic
import logics.wikis.PageLogic
import logics.wikis.RenderingMode
import logics.wikis.RenderingMode.RenderingMode
import models.WikiContext.Provider
import models.tables.Site
import play.api.Configuration
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.mvc.Request

object WikiContext {
  trait Provider {
    import java.util.Locale
    def getId: Option[String]
    def locale: Locale
    def getQueryString(key: String): Option[String]
    val remoteAddress: String
    def flashGet(key: String): Option[String]
    def host: String
  }

  object Provider {
    def apply()(implicit request: Request[Any]): Provider = new Provider {
      override def getId: Option[String] = SessionLogic.getId(request)
      override def locale: Locale = request.locale
      override def getQueryString(key: String): Option[String] = request.getQueryString(key)
      override val remoteAddress: String = request.remoteAddressWithXRealIp
      override def flashGet(key: String): Option[String] = request.flash.get(key)
      override def host: String = request.host
    }

    def empty: Provider = new Provider {
      override def getId: Option[String] = None
      override def locale: Locale = Locale.KOREA
      override def getQueryString(key: String): Option[String] = None
      override val remoteAddress: String = "127.0.0.1"
      override def flashGet(key: String): Option[String] = None
      override def host: String = ""
    }
  }

  def apply(name: String)(
    implicit
    request: Request[Any],
    database: Database,
    actorAhaWiki: ActorRef,
    configuration: Configuration,
    site: Site
  ): WikiContext = {
    implicit val provider: Provider = Provider()
    new WikiContext(Seq(name), RenderingMode.Normal)
  }

  def preview(name: String)(
    implicit
    request: Request[Any],
    database: Database,
    actorAhaWiki: ActorRef,
    configuration: Configuration,
    site: Site
  ): WikiContext = {
    implicit val provider: Provider = Provider()
    new WikiContext(Seq(name), RenderingMode.Preview)
  }
}

class Context()(
  implicit
  val database: Database,
  val actorAhaWiki: ActorRef,
  val configuration: Configuration,
  val provider: Provider,
  val site: Site,
){
  def toWikiContext(seqName: Seq[String], renderingMode: RenderingMode) = new WikiContext(seqName, renderingMode)
}

class WikiContext(val seqName: Seq[String], val renderingMode: RenderingMode)(
  implicit
  database: Database,
  actorAhaWiki: ActorRef,
  configuration: Configuration,
  provider: Provider,
  site: Site,
) extends Context {
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


