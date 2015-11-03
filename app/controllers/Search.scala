package controllers

import javax.inject._

import logics.OnApplicationStart
import models.Database
import play.api.mvc._

class Search @Inject() (on:OnApplicationStart) extends Controller {
  def index(q:String) = Action { implicit request =>
    Database.Page

    Ok(q)
  }

}
