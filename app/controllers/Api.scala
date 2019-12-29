package controllers

import javax.inject._
import logics.AhaWikiCache
import models.{AhaWikiQuery, WikiContext}
import play.api.cache.CacheApi
import play.api.libs.json._
import play.api.mvc._

import scala.util.Random


class Api @Inject()(implicit cacheApi: CacheApi, database:play.api.db.Database) extends Controller {
  def pageMap: Action[AnyContent] = Action { database.withConnection { implicit connection =>
    val listLink = Random.shuffle(AhaWikiQuery().Link.linkSelect()).take(10)

    Ok(Json.toJson(Map(
      "links" -> listLink
        .map(l => Map("src" -> JsString(l.src), "dst" -> JsString(l.dst), "alias" -> JsString(l.alias))))
    ))
  }}

  def pageNames: Action[AnyContent] = Action { implicit request =>
    Ok(Json.toJson(AhaWikiCache.PageNameSet.get()))
  }
}
