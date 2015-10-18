package logics

import logics.wikis.Interpreters
import models.{DirectQuery, MockDb, WikiContext}
import play.api.Logger

import scala.concurrent.duration._

object Cache {

  object PageList {
    val key: String = getClass.getName

    def get()(implicit wikiContext: WikiContext) = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      Logger.info(s"Cache miss: $key")
      DirectQuery.pageSelectPageList()
    }

    def invalidate()(implicit wikiContext: WikiContext): Unit = {
      wikiContext.cacheApi.remove(key)
      wikiContext.cacheApi.remove(PageNameSet.key)
    }
  }

  object PageNameSet {
    val key: String = getClass.getName

    def get()(implicit wikiContext: WikiContext) = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      PageList.get().map(_.name).toSet
    }

    def invalidate()(implicit wikiContext: WikiContext): Unit = {
      wikiContext.cacheApi.remove(key)
    }
  }

  object Header {
    val key: String = getClass.getName

    def get()(implicit wikiContext: WikiContext) = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      Interpreters.interpret(MockDb.selectPageLastRevision(".header").map(_.content).getOrElse(""))
    }

    def invalidate()(implicit wikiContext: WikiContext): Unit = {
      wikiContext.cacheApi.remove(key)
    }

  }

  object Footer {
    val key: String = getClass.getName

    def get()(implicit wikiContext: WikiContext) = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      Interpreters.interpret(MockDb.selectPageLastRevision(".footer").map(_.content).getOrElse(""))
    }

    def invalidate()(implicit wikiContext: WikiContext): Unit = {
      wikiContext.cacheApi.remove(key)
    }

  }

}
