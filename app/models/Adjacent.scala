package models

import models.tables.CalculatedLink
import models.tables.Site

import java.sql.Connection

object Adjacent {
  /** The page's links that the reader may see at both ends. */
  def getSeqLinkFiltered(name: String)(
    implicit
    contextSite: ContextSite,
    connection: Connection,
  ): Seq[CalculatedLink] = getSeqLink(name).filter(_.and(contextSite.pageCanSee))

  /**
   * Every link of the page -- its own links and its schema.org values -- whoever is asking.
   *
   * Apart from the reader's filter so that /api/links can cache this and filter per request. It
   * cached the filtered list until 2026-11-21, keyed by site and page only, so the first reader's
   * filter stood for ten minutes: an anonymous reader first, and the owner's graph was missing the
   * private pages the owner may see. Anything that hands this out must filter it.
   */
  def getSeqLink(name: String)(
    implicit
    contextSite: ContextSite,
    connection: Connection,
  ): Seq[CalculatedLink] = {
    import models.tables.CalculatedSchemaOrg
    implicit val site: Site = contextSite.site
    val seqLink: Seq[CalculatedLink] = CalculatedLink.select(name)
    // A page's colour is not something it is related to: drawn as a link, `#e60012` became a node.
    val seqSchemaOrg = CalculatedSchemaOrg.selectWherePageOrValue(name).filterNot(_.prop == logics.PageColor.Key)
    val seqLinkSchemaOrgPageOrValue: Seq[CalculatedLink] = seqSchemaOrg.map(s => CalculatedLink(s.page, s.value, ""))

//    val seqLinkSchemaOrgPageOrValue: Seq[CalculatedLink] =
//      seqSchemaOrg.map(s => CalculatedLink(s.page, s.toPageProp, "")) ++
//      seqSchemaOrg.map(s => CalculatedLink(s.toPageProp, s.value, ""))

//    val seqLinkSchemaOrgPageOrValue: Seq[CalculatedLink] =
//      seqSchemaOrg.map(s => CalculatedLink(s.page, s.toPageCls, "")) ++
//      seqSchemaOrg.map(s => CalculatedLink(s.toPageCls, s.toPageProp, "")) ++
//      seqSchemaOrg.map(s => CalculatedLink(s.toPageProp, s.value, ""))


    (seqLink ++ seqLinkSchemaOrgPageOrValue)
      .filterNot(_.isIdentical)
      .distinct
  }
}
