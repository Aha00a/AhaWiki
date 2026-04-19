package logics

import logics.wikis.macros.S3AttachmentUrlLogic
import models.ContextSite
import models.tables.Config
import models.tables.Site

// TODO: fix to use models.tables.Config

object AhaWikiConfig {

  def apply()(implicit contextSite: ContextSite) = new AhaWikiConfig()
}

class AhaWikiConfig(implicit contextSite: ContextSite) {
  private val defaultFaviconPath: String = "/public/favicon.png"
  private val faviconConfigKey: String = "site.favicon.objectKey"

  def hocon(): Hocon = {
    new Hocon(contextSite.ahaWikiCache.Config.get())
  }

  object site {
    def favicon(): String = {
      resolveFavicon(readFaviconConfig()).getOrElse(defaultFaviconPath)
    }
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

  private def readFaviconConfig(): String = {
    implicit val site: Site = contextSite.site
    contextSite.database.withConnection { implicit connection =>
      Config.select(faviconConfigKey).map(_.v.trim).getOrElse("")
    }
  }

  private def resolveFavicon(v: String): Option[String] = {
    if (v.isEmpty) {
      return Some(defaultFaviconPath)
    }

    if (v.startsWith("/")) {
      Some(v)
    } else if (v.startsWith("http://") || v.startsWith("https://")) {
      Some(v)
    } else {
      S3AttachmentUrlLogic.generatePresignedUrl(contextSite.applicationConf, v).toOption
    }
  }
}
