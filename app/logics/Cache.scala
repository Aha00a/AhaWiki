package logics

import logics.wikis.Interpreters
import models.{DirectQuery, MockDb, WikiContext}
import play.api.Logger

import scala.concurrent.duration._

object Cache {
  trait CacheEntity {
    val key: String = getClass.getName
    def invalidate()(implicit wikiContext: WikiContext): Unit = {
      Logger.info(s"Invalidate Cache: $key")
      wikiContext.cacheApi.remove(key)
    }
  }

  object PageList extends CacheEntity {
    def get()(implicit wikiContext: WikiContext) = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      DirectQuery.pageSelectPageList()
    }

    override def invalidate()(implicit wikiContext: WikiContext): Unit = {
      super.invalidate()
      PageNameSet.invalidate()
    }
  }

  object PageNameSet extends CacheEntity {
    def get()(implicit wikiContext: WikiContext) = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      PageList.get().map(_.name).toSet
    }
  }

  object Header extends CacheEntity {
    def get()(implicit wikiContext: WikiContext) = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      Interpreters.interpret(MockDb.selectPageLastRevision(".header").map(_.content).getOrElse(""))
    }
  }

  object Footer extends CacheEntity {
    def get()(implicit wikiContext: WikiContext) = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      Interpreters.interpret(MockDb.selectPageLastRevision(".footer").map(_.content).getOrElse(""))
    }
  }

  object Config extends CacheEntity {
    def get()(implicit wikiContext: WikiContext) = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      MockDb.selectPageLastRevision(".config").map(_.content).getOrElse("")
    }
  }
}
