package models

import logics.AhaWikiCache
import logics.ApplicationConf
import logics.wikis.RenderingMode
import logics.wikis.RenderingMode.RenderingMode
import models.tables.Site
import play.api.db.Database
import play.api.mvc.Request

import java.time.LocalDate

object ContextWikiPage {
  def apply(name: String)(
    implicit
    request: Request[Any],
    database: Database,
    wikiActors: WikiActors,
    applicationConf: ApplicationConf,
    ahaWikiCache: AhaWikiCache,
    site: Site
  ): ContextWikiPage = {
    implicit val provider: RequestWrapper = RequestWrapper()
    new ContextWikiPage(Seq(name), RenderingMode.Normal)
  }

  def preview(name: String)(
    implicit
    request: Request[Any],
    database: Database,
    wikiActors: WikiActors,
    applicationConf: ApplicationConf,
    ahaWikiCache: AhaWikiCache,
    site: Site
  ): ContextWikiPage = {
    implicit val provider: RequestWrapper = RequestWrapper()
    new ContextWikiPage(Seq(name), RenderingMode.Preview)
  }
}

/**
 * The page currently being rendered, and the pages it is being rendered inside of.
 *
 * `seqName` is an include stack, innermost first. A page the reader requested directly has a
 * stack of one; `Include` renders the included page under a [[push]]ed context, so a page
 * reached through two includes has a stack of three.
 *
 * A macro that needs to know it is inside an include asks [[isIncluded]] — `DayHeader` does,
 * to render a day page as a section of a month page rather than as a page of its own.
 */
class ContextWikiPage(val seqName: Seq[String], val renderingMode: RenderingMode)(
  implicit
  database: Database,
  wikiActors: WikiActors,
  applicationConf: ApplicationConf,
  ahaWikiCache: AhaWikiCache,
  requestWrapper: RequestWrapper,
  site: Site,
  val localDateNow: LocalDate = LocalDate.now(),
) extends ContextSite {

  /**
   * The page being rendered — inside an include, the included one.
   *
   * This is what nearly every caller wants: attachment keys, edit links, backlinks, schema.org
   * page URLs and self-link detection all belong to the page whose content is on screen, not to
   * whichever page happened to include it.
   *
   * It was `seqName.last` for five years, from the commit that introduced the stack until this
   * one. That was only ever safe because the stack never got past one entry, and the day it did
   * every one of those callers would have quietly addressed the wrong page.
   */
  def name: String = seqName.head

  /** The page the reader asked for — the bottom of the include stack. */
  def nameRoot: String = seqName.last

  /** Whether this page was reached through an include rather than requested directly. */
  def isIncluded: Boolean = seqName.size > 1

  /**
   * The context an included page renders under.
   *
   * Innermost first, so [[name]] follows the include down while [[nameRoot]] stays put.
   */
  def push(name: String) = new ContextWikiPage(name +: seqName, renderingMode)

  def at(localDateNow: LocalDate): ContextWikiPage = {
    new ContextWikiPage(seqName, renderingMode)(database, wikiActors, applicationConf, ahaWikiCache, requestWrapper, site, localDateNow)
  }
}
