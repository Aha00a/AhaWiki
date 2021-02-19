package logics

import models.tables.Site
import play.api.cache.SyncCacheApi
import play.api.db.Database

// TODO: fix to use models.tables.Config

object AhaWikiConfig {

  import models.tables.Site

  def apply()(implicit database:Database, site: Site) = new AhaWikiConfig()
}

class AhaWikiConfig(implicit database:Database, site: Site) {
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
