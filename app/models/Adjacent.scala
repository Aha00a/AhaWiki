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
    val seqLinkSchemaOrgPageOrValue: Seq[Link] = SchemaOrg.selectWherePageOrValue(name).map(s => Link(s.page, s.value, ""))
    val seqLinkFiltered: Seq[Link] = (seqLink ++ seqLinkSchemaOrgPageOrValue).filter(_.and(contextSite.pageCanSee))
    seqLinkFiltered
  }
}
