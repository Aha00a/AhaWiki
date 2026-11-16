package models

import models.tables.CalculatedLink
import models.tables.Site

import java.sql.Connection

object Adjacent {
  def getSeqLinkFiltered(name: String)(
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


    val seqLinkFiltered: Seq[CalculatedLink] = (seqLink ++ seqLinkSchemaOrgPageOrValue)
      .filterNot(_.isIdentical)
      .distinct
      .filter(_.and(contextSite.pageCanSee))
    seqLinkFiltered
  }
}
