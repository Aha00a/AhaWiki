package logics

import logics.wikis.macros.S3AttachmentUrlLogic
import models.ContextSite
import models.tables.Config
import models.tables.Site

object AhaWikiConfig {

  def apply()(implicit contextSite: ContextSite) = new AhaWikiConfig()
}

class AhaWikiConfig(implicit contextSite: ContextSite) {
  private val defaultFaviconPath: String = "/public/favicon.png"
  private val faviconConfigKey: String = "site.favicon.objectKey"

  object site {
    def favicon(): String = {
      resolveFavicon(readFaviconConfig()).getOrElse(defaultFaviconPath)
    }
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
