package controllers

import javax.inject._
import logics.wikis.PageLogic
import models.AhaWikiQuery
import play.api.cache.SyncCacheApi
import play.api.libs.json._
import play.api.mvc._

import scala.util.Random


class Api @Inject()(implicit val controllerComponents: ControllerComponents, syncCacheApi: SyncCacheApi, database:play.api.db.Database) extends BaseController {
  def pageMap: Action[AnyContent] = Action { database.withConnection { implicit connection =>
    import models.tables.Link
    val listLink = Random.shuffle(Link.selectAllButNotEmpty()).take(10)

    Ok(Json.toJson(Map(
      "links" -> listLink
        .map(l => Map("src" -> JsString(l.src), "dst" -> JsString(l.dst), "alias" -> JsString(l.alias))))
    ))
  }}

  def pageNames: Action[AnyContent] = Action { implicit request =>
    Ok(Json.toJson(PageLogic.getListPageByPermission().map(_.name)))
  }
}
