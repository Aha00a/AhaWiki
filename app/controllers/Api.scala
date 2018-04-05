package controllers

import javax.inject._

import logics.Cache
import models.{WikiContext, Database}
import play.api.cache.CacheApi
import play.api.libs.json._
import play.api.mvc._

import scala.util.Random


class Api @Inject()(implicit cacheApi: CacheApi) extends Controller {
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
