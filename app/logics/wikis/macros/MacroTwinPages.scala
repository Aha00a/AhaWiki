package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.AhaWikiCacheMemoryDomainSite
import logics.AhaWikiCacheMemoryPermission
import logics.PermissionLogic
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage
import models.tables.Permission
import models.tables.Site

object MacroTwinPages extends TraitMacro {
  private[macros] case class TwinPage(site: Site, pageName: String)

  override def isBlock: Boolean = true

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageName = argument.getOrElse(wikiContext.nameTop)
    val sites = AhaWikiCacheMemoryDomainSite
      .getSites()(wikiContext.database)
      .filter(_.seq != wikiContext.site.seq)

    wikiContext.database.withConnection { implicit connection =>
      val twinPages = collectTwinPages(pageName, sites, wikiContext.site)(
        pageExists = targetSite => {
          implicit val databaseSite: (play.api.db.Database, Site) = (wikiContext.database, targetSite)
          wikiContext.ahaWikiCache.PageMeta.SeqPageLatestSummary
            .get()
            .exists(_.name == pageName)
        },
        anonymousCanRead = targetSite => MacroTwinPages.anonymousCanRead(targetSite, pageName),
      )

      val markup = twinPages.map { twinPage =>
        val targetSite = twinPage.site
        s""" 1. [${url(targetSite, pageName)} ${targetSite.abbr}:$pageName]"""
      }.mkString("\n")

      markup.toOption.map(InterpreterWiki.toHtmlString).getOrElse("")
    }
  }

  private[macros] def collectTwinPages(
    pageName: String,
    sites: Seq[Site],
    currentSite: Site,
  )(
    pageExists: Site => Boolean,
    anonymousCanRead: Site => Boolean,
  ): Seq[TwinPage] = {
    sites.flatMap { targetSite =>
      if (targetSite.seq == currentSite.seq || !pageExists(targetSite) || !anonymousCanRead(targetSite)) {
        None
      } else {
        Some(TwinPage(targetSite, pageName))
      }
    }
  }

  private def anonymousCanRead(targetSite: Site, pageName: String)(implicit connection: java.sql.Connection): Boolean = {
    val permissionLogic = new PermissionLogic(AhaWikiCacheMemoryPermission.get()(connection, targetSite))
    permissionLogic.permitted(pageName, "", Permission.Action.Read.id)
  }

  private def url(site: Site, pageName: String): String =
    s"https://${site.mainDomain}/w/${java.net.URLEncoder.encode(pageName, "UTF-8").replace("+", "%20")}"
}
