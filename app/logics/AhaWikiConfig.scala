package logics

import play.api.cache.CacheApi
import play.api.db.Database

object AhaWikiConfig {
  def apply()(implicit cacheApi: CacheApi, database:Database) = new AhaWikiConfig()
}

class AhaWikiConfig(implicit cacheApi: CacheApi, database:Database) {
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
