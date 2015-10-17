package models

import play.api.mvc.Request

case class WikiContext(name:String)(implicit val request:Request[Any])


