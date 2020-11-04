package logics

import play.api.cache.SyncCacheApi
import play.api.db.Database

// TODO: remove

object AhaWikiConfig {
  def apply()(implicit syncCacheApi: SyncCacheApi, database:Database) = new AhaWikiConfig()
}

class AhaWikiConfig(implicit syncCacheApi: SyncCacheApi, database:Database) {
  object permission {
    object default {
      def read(): String = hocon().getOrElse(fqn, "all")
      def write(): String = hocon().getOrElse(fqn, "login")
    }
  }

  object google {
    object analytics {
      def trackingId(): String = hocon().getOrElse(fqn, "")
    }
  }

  def hocon(): Hocon = {
    new Hocon(AhaWikiCache.Config.get())
  }

  private def fqn: String = {
    val ste = Thread.currentThread.getStackTrace()(2)
    (ste.getClassName.replace(AhaWikiConfig.getClass.getName, "") + ste.getMethodName).replaceAll("\\$", ".")
  }
}
