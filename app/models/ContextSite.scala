package models

import akka.actor.ActorRef
import com.aha00a.play.Implicits.RichRequest
import logics.ApplicationConf
import logics.SessionLogic
import logics.wikis.PageLogic
import logics.wikis.RenderingMode.RenderingMode
import models.ContextSite.RequestWrapper
import models.tables.Page
import models.tables.PageWithoutContentWithSize
import models.tables.Site
import play.api.db.Database
import play.api.mvc.Request

import java.util.Locale

object ContextSite {
  trait RequestWrapper {

    import java.util.Locale

    def getId: Option[String]

    def locale: Locale

    def getQueryString(key: String): Option[String]

    val remoteAddress: String

    def flashGet(key: String): Option[String]

    def host: String
  }

  object RequestWrapper {
    def apply()(implicit request: Request[Any]): RequestWrapper = new RequestWrapper {
      override def getId: Option[String] = SessionLogic.getId(request)

      override def locale: Locale = request.locale

      override def getQueryString(key: String): Option[String] = request.getQueryString(key)

      override val remoteAddress: String = request.remoteAddressWithXRealIp

      override def flashGet(key: String): Option[String] = request.flash.get(key)

      override def host: String = request.host
    }

    def empty: RequestWrapper = new RequestWrapper {
      override def getId: Option[String] = None

      override def locale: Locale = Locale.KOREA

      override def getQueryString(key: String): Option[String] = None

      override val remoteAddress: String = "127.0.0.1"

      override def flashGet(key: String): Option[String] = None

      override def host: String = ""
    }
  }


  def apply()(
    implicit
    database: Database,
    actorAhaWiki: ActorRef,
    applicationConf: ApplicationConf,
    request: Request[Any],
    site: Site,
  ): ContextSite = {
    implicit val provider: RequestWrapper = RequestWrapper()
    new ContextSite()
  }

  def empty()(
    implicit
    database: Database,
    actorAhaWiki: ActorRef,
    applicationConf: ApplicationConf,
    site: Site,
  ): ContextSite = {
    implicit val provider: RequestWrapper = RequestWrapper.empty
    new ContextSite()
  }
}

class ContextSite()(
  implicit
  database: Database,
  actorAhaWiki: ActorRef,
  applicationConf: ApplicationConf,
  requestWrapper: RequestWrapper,
  val site: Site,
) extends Context {
  //noinspection ScalaWeakerAccess
  lazy val (
    setPageNameAll: Set[String],
    listPageByPermission: List[PageWithoutContentWithSize]
    ) = database.withConnection { implicit connection =>
    (
      Page.selectSeqPageName().toSet,
      PageLogic.getListPageByPermission()
    )
  }
  lazy val seqPageNameByPermission: Seq[String] = listPageByPermission.map(_.name)
  lazy val setPageNameByPermission: Set[String] = seqPageNameByPermission.toSet

  def pageCanSee(name: String): Boolean = !setPageNameAll.contains(name) || setPageNameByPermission.contains(name)

  def toWikiContext(seqName: Seq[String], renderingMode: RenderingMode) = new ContextWikiPage(seqName, renderingMode)
}
