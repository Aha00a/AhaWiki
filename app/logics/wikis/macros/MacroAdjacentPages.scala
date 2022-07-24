package logics.wikis.macros

import com.aha00a.commons.Implicits._
import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage

import java.sql.Connection

object MacroAdjacentPages extends TraitMacro {
  import scala.util.matching.Regex

  val year: Regex = """\d{4}""".r
  val date: Regex = """\d{4}-\d{2}-\d{2}""".r

  override def toHtmlString(argument:String)(implicit wikiContext: ContextWikiPage): String = { wikiContext.database.withConnection { implicit connection =>
    getMarkupRelatedPages(argument.getOrElse(wikiContext.nameTop))
  }}

  def getMarkupRelatedPages(name: String)(implicit wikiContext: ContextWikiPage, connection: Connection): String = {
    import com.aha00a.commons.utils.SeqUtil
    import models.tables.Link
    import models.tables.SchemaOrg
    import models.tables.Site
    implicit val site: Site = wikiContext.site

    val seqLink: Seq[Link] = Link.select(name)
    val seqLinkSchemaOrgPageOrValue: Seq[Link] = SchemaOrg.selectWherePageOrValue(name).map(s => Link(s.page, s.value, ""))
    val seqLinkFiltered: Seq[Link] = (seqLink ++ seqLinkSchemaOrgPageOrValue).filter(l => l.and(wikiContext.pageCanSee))

    val seqName = seqLinkFiltered.flatMap(_.toSeqString).distinct

    val seqLinkExpandedByLink: Seq[Link] = Link.selectWhereSrcORDstIn(seqName)
    val seqLinkExpandedBySchema: Seq[Link] = SchemaOrg.selectWherePageOrValueIn(seqName).map(s => Link(s.page, s.value, ""))
    val seqLinkMerged = SeqUtil.mergeOneByOne(seqLinkExpandedByLink, seqLinkExpandedBySchema)
    val seqLinkMergedFiltered: Seq[Link] = seqLinkMerged.filter(l => l.and(wikiContext.pageCanSee))

    val result = seqLinkMergedFiltered
      .filter(l => (l.src, l.dst) match {
        case (year(), date()) => false
        case (date(), year()) => false
        case _ => true
      })
      .take(1000)
      .map(l => s"${l.src}->${l.dst}")
      .mkString("\n")

    views.html.Wiki.adjacentPages(Array(Array[String]()), true).toString()
  }
}
