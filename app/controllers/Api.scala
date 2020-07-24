package controllers

import javax.inject._
import logics.wikis.PageLogic
import play.api.cache.SyncCacheApi
import play.api.db.Database
import play.api.mvc._

import scala.util.Random


class Api @Inject()(
                     implicit val
                     controllerComponents: ControllerComponents,
                     syncCacheApi: SyncCacheApi,
                     database: Database
                   ) extends BaseController {
  import io.circe.generic.auto._
  import io.circe.syntax._
  import models.tables.Link

  def Ok(json: io.circe.Json): Result = Ok(json.toString()).as(JSON)

  def pageMap: Action[AnyContent] = Action {
    database.withConnection { implicit connection =>
      import models.tables.Link
      val listLink = Random.shuffle(Link.selectAllButNotEmpty()).take(10)
      Ok(listLink.asJson)
    }
  }

  def pageNames: Action[AnyContent] = Action { implicit request =>
    Ok(PageLogic.getListPageByPermission().map(_.name).asJson)
  }


  def links(name: String): Action[AnyContent] = Action { implicit request =>
    database.withConnection { implicit connection =>
      val seqLink: Seq[Link] = Link.select(name)
      Ok(seqLink.asJson)
    }
  }
}

