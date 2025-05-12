package logics

import models.ContextSite

// TODO: fix to use models.tables.Config

object AhaWikiConfig {

  def apply()(implicit contextSite: ContextSite) = new AhaWikiConfig()
}

class AhaWikiConfig(implicit contextSite: ContextSite) {
  def hocon(): Hocon = {
    new Hocon(contextSite.ahaWikiCache.Config.get())
  }

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

  private def fqn: String = {
    val ste = Thread.currentThread.getStackTrace()(2)
    (ste.getClassName.replace(AhaWikiConfig.getClass.getName, "") + ste.getMethodName).replaceAll("\\$", ".")
  }
}
