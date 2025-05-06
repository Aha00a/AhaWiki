package models

import akka.actor.ActorRef
import models.tables.{Link, Site}
import play.api.Configuration
import play.api.db.Database
import play.api.mvc.Request

import java.sql.Connection
import scala.util.Random

object Adjacent {
  def getSeqLinkFiltered(name: String)(
    implicit
    contextSite: ContextSite,
    connection: Connection,
  ): Seq[Link] = {
    import models.tables.SchemaOrg
    implicit val site: Site = contextSite.site
    val seqLink: Seq[Link] = Link.select(name)
    val seqSchemaOrg = SchemaOrg.selectWherePageOrValue(name)
    val seqLinkSchemaOrgPageOrValue: Seq[Link] = seqSchemaOrg.map(s => Link(s.page, s.value, ""))

//    val seqLinkSchemaOrgPageOrValue: Seq[Link] =
//      seqSchemaOrg.map(s => Link(s.page, s.toPageProp, "")) ++
//      seqSchemaOrg.map(s => Link(s.toPageProp, s.value, ""))

//    val seqLinkSchemaOrgPageOrValue: Seq[Link] =
//      seqSchemaOrg.map(s => Link(s.page, s.toPageCls, "")) ++
//      seqSchemaOrg.map(s => Link(s.toPageCls, s.toPageProp, "")) ++
//      seqSchemaOrg.map(s => Link(s.toPageProp, s.value, ""))


    val seqLinkFiltered: Seq[Link] = (seqLink ++ seqLinkSchemaOrgPageOrValue)
      .filterNot(_.isIdentical)
      .distinct
      .filter(_.and(contextSite.pageCanSee))
    seqLinkFiltered
  }
}
