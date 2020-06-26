package logics

import play.api.cache.SyncCacheApi
import play.api.db.Database

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

  object interpreter {
    object Vim {
      def debug(): Boolean = hocon().getOrElse(fqn, default = false)
      def colorscheme(): String = hocon().getOrElse(fqn, "elflord")
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
