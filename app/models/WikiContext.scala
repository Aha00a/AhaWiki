package models

import play.api.cache.CacheApi
import play.api.db.Database
import play.api.mvc.Request

case class WikiContext(name:String)(
  implicit val request:Request[Any],
  val cacheApi: CacheApi,
  val db: Database
)


