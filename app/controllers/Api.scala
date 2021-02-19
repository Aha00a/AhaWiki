package controllers

import akka.actor.ActorRef
import akka.actor.ActorSystem
import javax.inject._
import logics.wikis.PageLogic
import play.api.Configuration
import play.api.Environment
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.libs.ws.WSClient
import play.api.mvc._

import scala.concurrent.ExecutionContext
import scala.util.Random


class Api @Inject()(
                     implicit val
                     controllerComponents: ControllerComponents,
                     actorSystem: ActorSystem,
                     database: Database,
                     @Named("db-actor") actorAhaWiki: ActorRef,
                     configuration: Configuration,
                     wsClient: WSClient,
                     executionContext: ExecutionContext
                   ) extends BaseController {
  import io.circe.generic.auto._
  import io.circe.syntax._
  import models.tables.Link

  def Ok(json: io.circe.Json): Result = Ok(json.toString()).as(JSON)

  def csrf: Action[AnyContent] = Action { implicit request =>
    import play.filters.csrf.CSRF
    val token: Option[CSRF.Token] = CSRF.getToken
    Ok(token.asJson)
  }

  def pageMap: Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      import models.tables.Link
      import models.tables.Site
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
      val listLink = Random.shuffle(Link.selectAllButNotEmpty()).take(10)
      Ok(listLink.asJson)
    }
  }

  def pageNames: Action[AnyContent] = Action { implicit request =>
    import models.ContextSite.RequestWrapper
    import models.tables.Site
    implicit val provider: RequestWrapper = RequestWrapper()
    database.withConnection { implicit connection =>
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
      Ok(PageLogic.getListPageByPermission().map(_.name).asJson)
    }
  }


  def links(name: String): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      import models.ContextWikiPage
      import models.tables.Site
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
      implicit val wikiContext: ContextWikiPage = ContextWikiPage(name)
      val seqLink: Seq[Link] = Link.select(name).filter(_.and(wikiContext.pageCanSee))
      Ok(seqLink.asJson)
    }
  }

  def statistics(): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      import java.util.Date

      import io.circe.Json
      import models.tables.Page
      import models.tables.PageWithoutContentWithSize
      import models.tables.Site
      implicit val site: Site = Site.selectWhereDomain(request.host).getOrElse(Site(-1, ""))
      val selectYmdCountOfFirstRevision: Seq[(String, Long)] = Page.selectYmdCountOfFirstRevision()
      val seqPage: Seq[PageWithoutContentWithSize] = Page.pageSelectPageList()
      val totalSize: Long = seqPage.map(_.size).sum

      val value1: Map[String, Seq[(String, Long)]] = Map(
        "arrayArrayYmdCountOfFirstRevision" -> selectYmdCountOfFirstRevision,
      )
      val value2: Map[String, Long] = Map(
        "totalSize" -> totalSize,
        "pageCount" -> seqPage.length.toLong,
        "count" -> Page.selectCount(),
      )
      val json1: Json = value1.asJson
      val json2: Json = value2.asJson

      Ok(json1.deepMerge(json2))
    }
  }
}

