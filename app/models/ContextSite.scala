package models

import logics.AhaWikiCache
import logics.ApplicationConf
import logics.SiteThemeLogic
import logics.wikis.PageLogic
import logics.wikis.RenderingMode.RenderingMode
import models.tables.Config
import models.tables.Site
import play.api.db.Database
import play.api.mvc.Request

import java.time.LocalDate

object ContextSite {
  def apply()(
    implicit
    database: Database,
    wikiActors: WikiActors,
    applicationConf: ApplicationConf,
    ahaWikiCache: AhaWikiCache,
    request: Request[Any],
    site: Site,
  ): ContextSite = {
    implicit val provider: RequestWrapper = RequestWrapper()
    new ContextSite()
  }

  def empty()(
    implicit
    database: Database,
    wikiActors: WikiActors,
    applicationConf: ApplicationConf,
    ahaWikiCache: AhaWikiCache,
    site: Site,
  ): ContextSite = {
    implicit val provider: RequestWrapper = RequestWrapper.empty
    new ContextSite()
  }
}

/**
 * What holds for the whole site and the reader who asked, for the length of one request.
 *
 * Rendering an included page needs a context of its own — a different page name, one more entry
 * on the include stack — but none of the answers below change with it. `parent` is how the new
 * context says so: each value asks the context it came from before working the value out itself.
 *
 * `seqPageByPermission` is the one that matters. It fetches every page in the site from the
 * cache and runs a permission test on each, and a page holding several includes would otherwise
 * do that once per include. Consistency is the better reason: the including page and everything
 * it includes then decide what the reader may see from a single answer, not from several taken
 * moments apart.
 *
 * Delegation is written on each value rather than as a list somewhere else, so that a value
 * added here cannot be forgotten there.
 */
class ContextSite(parent: Option[ContextSite] = None)(
  implicit
  database: Database,
  wikiActors: WikiActors,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  val requestWrapper: RequestWrapper,
  val site: Site,
) extends Context {
  implicit val tupleDatabaseSite: (Database, Site) = (database, site)

  lazy val setPageName: Set[String] =
    parent.map(_.setPageName).getOrElse(ahaWikiCache.PageMeta.SeqPageName.get().toSet)

  lazy val seqPageByPermission: Seq[PageLatestSummary] =
    parent.map(_.seqPageByPermission).getOrElse(database.withConnection { implicit connection =>
      PageLogic.getListPageByPermission()(requestWrapper, connection, this, ahaWikiCache)
    })

  lazy val seqPageNameByPermission: Seq[String] =
    parent.map(_.seqPageNameByPermission).getOrElse(seqPageByPermission.map(_.name))

  lazy val setPageNameByPermission: Set[String] =
    parent.map(_.setPageNameByPermission).getOrElse(seqPageNameByPermission.toSet)

  lazy val defaultHue: Option[Int] =
    parent.map(_.defaultHue).getOrElse(database.withConnection { implicit connection =>
      Config.select(SiteThemeLogic.DefaultHueKey).flatMap(c => SiteThemeLogic.parseHue(c.v))
    })

  def pageCanSee(name: String): Boolean = !setPageName.contains(name) || setPageNameByPermission.contains(name)

  def toContextWikiPage(seqName: Seq[String], renderingMode: RenderingMode): ContextWikiPage =
    contextForPage(seqName, renderingMode, LocalDate.now())

  /** A context for another page of the same site, in the same request, inheriting the above. */
  def contextForPage(seqName: Seq[String], renderingMode: RenderingMode, localDateNow: LocalDate): ContextWikiPage =
    new ContextWikiPage(seqName, renderingMode, Some(this))(
      database, wikiActors, applicationConf, ahaWikiCache, requestWrapper, site, localDateNow,
    )
}
