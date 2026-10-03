package logics

import logics.wikis.macros.S3AttachmentUrlLogic
import models.ContextSite
import models.tables.Config
import models.tables.Site

/**
 * A site's own settings, kept in its Config rows.
 *
 * The favicon facts below were written out in three places until 2026-10-03 -- here, in
 * MacroAhaWikiSiteList and in ApiAdminSite -- each with its own copy of the key, the default path
 * and the resolution. They live here now, and the other two use them.
 */
object AhaWikiConfig {

  def apply()(implicit contextSite: ContextSite) = new AhaWikiConfig()

  /** The Config key a site's favicon is kept under: the S3 object key the admin upload writes, or
    * a path or URL written there by hand. */
  val FaviconConfigKey: String = "site.favicon.objectKey"

  /** What a site shows until it has a favicon of its own. */
  val DefaultFaviconPath: String = "/public/favicon.png"

  /** Where the favicon configured as `v` is served from: a path or a URL as it is, an S3 object key
    * as a presigned URL, nothing configured as the default. None when a key cannot be signed. */
  def resolveFavicon(v: String, applicationConf: ApplicationConf): Option[String] =
    if (v.isEmpty) Some(DefaultFaviconPath)
    else if (v.startsWith("/") || v.startsWith("http://") || v.startsWith("https://")) Some(v)
    else S3AttachmentUrlLogic.generatePresignedUrl(applicationConf, v).toOption
}

class AhaWikiConfig(implicit contextSite: ContextSite) {
  object site {
    def favicon(): String =
      AhaWikiConfig.resolveFavicon(readFaviconConfig(), contextSite.applicationConf).getOrElse(AhaWikiConfig.DefaultFaviconPath)
  }

  private def readFaviconConfig(): String = {
    implicit val site: Site = contextSite.site
    contextSite.withConnection { implicit connection =>
      Config.select(AhaWikiConfig.FaviconConfigKey).map(_.v.trim).getOrElse("")
    }
  }
}
