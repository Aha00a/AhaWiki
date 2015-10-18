package logics

import models.DirectQuery.PageNameRevisionTimeAuthorRemoteAddressSizeComment
import models.{DirectQuery, WikiContext}
import play.api.Logger

import scala.concurrent.duration._

object Cache {
  object PageList {

    val key: String = "pageSelectPageList"

    def get()(implicit wikiContext: WikiContext): List[PageNameRevisionTimeAuthorRemoteAddressSizeComment] = {
      wikiContext.cacheApi.getOrElse[List[PageNameRevisionTimeAuthorRemoteAddressSizeComment]](key, 5.minutes) {
        Logger.info(s"Cache miss: $key")
        DirectQuery.pageSelectPageList()
      }
    }

    def invalidate()(implicit wikiContext: WikiContext): Unit = {
      wikiContext.cacheApi.remove(key)
    }
  }
}
