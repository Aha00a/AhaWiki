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
  override def isBlock: Boolean = true

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageName = argument.getOrElse(wikiContext.nameTop)
    val sites = AhaWikiCacheMemoryDomainSite
      .getSites()(wikiContext.database)
      .filter(_.seq != wikiContext.site.seq)

    wikiContext.database.withConnection { implicit connection =>
      val markup = sites.flatMap { targetSite =>
        implicit val databaseSite: (play.api.db.Database, Site) = (wikiContext.database, targetSite)
        val hasSameNamePage = wikiContext.ahaWikiCache.PageMeta.SeqPageLatestSummary
          .get()
          .exists(_.name == pageName)
        if (!hasSameNamePage || !anonymousCanRead(targetSite, pageName)) {
          None
        } else {
          Some(s""" 1. [${url(targetSite, pageName)} ${targetSite.abbr}:$pageName]""")
        }
      }.mkString("\n")

      markup.toOption.map(InterpreterWiki.toHtmlString).getOrElse("")
    }
  }

  private def anonymousCanRead(targetSite: Site, pageName: String)(implicit connection: java.sql.Connection): Boolean = {
    val permissionLogic = new PermissionLogic(AhaWikiCacheMemoryPermission.get()(connection, targetSite))
    permissionLogic.permitted(pageName, "", Permission.Action.Read.id)
  }

  private def url(site: Site, pageName: String): String =
    s"https://${site.mainDomain}/w/${java.net.URLEncoder.encode(pageName, "UTF-8").replace("+", "%20")}"
}
