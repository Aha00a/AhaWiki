package logics

import models.{DirectQuery, WikiContext}
import play.api.Logger

import scala.concurrent.duration._

object Cache {
  object PageList {
    val key: String = getClass.getName

    def get()(implicit wikiContext: WikiContext) = {
      wikiContext.cacheApi.getOrElse(key, 60.minutes) {
        Logger.info(s"Cache miss: $key")
        DirectQuery.pageSelectPageList()
      }
    }

    def invalidate()(implicit wikiContext: WikiContext): Unit = {
      wikiContext.cacheApi.remove(key)
      wikiContext.cacheApi.remove(PageNameSet.key)
    }
  }
  object PageNameSet {
    val key: String = getClass.getName

    def get()(implicit wikiContext: WikiContext)= {
      wikiContext.cacheApi.getOrElse(key, 60.minutes) {
        PageList.get().map(_.name).toSet
      }
    }

    def invalidate()(implicit wikiContext: WikiContext): Unit = {
      wikiContext.cacheApi.remove(key)
    }
  }
}
