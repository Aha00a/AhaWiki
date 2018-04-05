package controllers

import javax.inject._

import logics.Cache
import models.{Database, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database
import play.api.libs.json._
import play.api.mvc._

import scala.util.Random


class Api @Inject()(implicit cacheApi: CacheApi, database:play.api.db.Database) extends Controller {
  def pageMap = Action {
    val listLink = Random.shuffle(Database.linkSelect()).take(10)

    Ok(Json.toJson(Map(
      "links" -> listLink
        .map(l => Map("src" -> JsString(l.src), "dst" -> JsString(l.dst), "alias" -> JsString(l.alias))))
    ))
  }

  def pageNames = Action { implicit request =>
    implicit val wikiContext: WikiContext = WikiContext("")

    Ok(Json.toJson(Cache.PageNameSet.get()))
  }
}
