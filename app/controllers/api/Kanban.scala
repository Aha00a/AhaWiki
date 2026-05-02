package controllers.api

import javax.inject._
import play.api.mvc._

@Singleton
class Kanban @Inject()(val controllerComponents: ControllerComponents) extends BaseController {
  def put(pageName: String): Action[AnyContent] = Action { implicit request =>
    Ok(
      s"""{"status":"ok","message":"Kanban PUT endpoint scaffolded.","pageName":"$pageName"}"""
    ).as(JSON)
  }
}
