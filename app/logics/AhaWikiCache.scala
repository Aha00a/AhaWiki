package logics

import logics.wikis.Interpreters
import models.{AhaWikiDatabase, WikiContext}
import play.api.Logger
import play.api.cache.CacheApi
import play.api.db.Database

import scala.concurrent.duration._

object AhaWikiCache {
  trait CacheEntity {
    val key: String = getClass.getName
    def invalidate()(implicit wikiContext: WikiContext): Unit = {
      Logger.info(s"Invalidate Cache: $key")
      wikiContext.cacheApi.remove(key)
    }
  }

  object PageList extends CacheEntity {
    def get()(implicit cacheApi: CacheApi, db:Database): List[AhaWikiDatabase.PageNameRevisionTimeAuthorRemoteAddressSizeComment] = cacheApi.getOrElse(key, 60.minutes) {
      AhaWikiDatabase().pageSelectPageList()
    }

    override def invalidate()(implicit wikiContext: WikiContext): Unit = {
      super.invalidate()
      PageNameSet.invalidate()
    }
  }

  object PageNameSet extends CacheEntity {
    def get()(implicit cacheApi: CacheApi, db:Database): Set[String] = cacheApi.getOrElse(key, 60.minutes) {
      PageList.get().map(_.name).toSet
    }
  }

  object Header extends CacheEntity {
    def get()(implicit wikiContext: WikiContext): String = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      Interpreters.interpret(AhaWikiDatabase()(wikiContext.db).pageSelectLastRevision(".header").map(_.content).getOrElse(""))
    }
  }

  object Footer extends CacheEntity {
    def get()(implicit wikiContext: WikiContext): String = wikiContext.cacheApi.getOrElse(key, 60.minutes) {
      Interpreters.interpret(AhaWikiDatabase()(wikiContext.db).pageSelectLastRevision(".footer").map(_.content).getOrElse(""))
    }
  }

  object Config extends CacheEntity {
    def get()(implicit cacheApi: CacheApi, db:Database): String = cacheApi.getOrElse(key, 60.minutes) {
      AhaWikiDatabase().pageSelectLastRevision(".config").map(_.content).getOrElse("")
    }
  }
}
