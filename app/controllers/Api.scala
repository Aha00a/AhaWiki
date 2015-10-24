package controllers

import javax.inject._

import logics.OnApplicationStart
import models.Database
import play.api.libs.json._
import play.api.mvc._

import scala.util.Random


class Api @Inject() (on:OnApplicationStart) extends Controller {
  def pageMap = Action {
    val listLink = Random.shuffle(Database.linkSelect()).take(10)

    Ok(Json.toJson(Map(
      "links" -> listLink
        .map(l => Map("src" -> JsString(l.src), "dst" -> JsString(l.dst), "alias" -> JsString(l.alias))))
    ))
  }
}
