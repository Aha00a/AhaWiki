package logics.wikis.macros

import java.sql.Connection

import com.aha00a.commons.Implicits._
import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroSeeAlso extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    InterpreterWiki.toHtmlString(getMarkupSeeAlso(argument.getOrElse(wikiContext.nameTop)))
  }}

  def getMarkupRelatedPages(name: String)(implicit wikiContext: WikiContext, connection: Connection): String = {
    import com.aha00a.commons.utils.SeqUtil
    import models.tables.Link
    import models.tables.SchemaOrg

    import scala.util.matching.Regex

    val year: Regex = """\d{4}""".r
    val date: Regex = """\d{4}-\d{2}-\d{2}""".r

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
      .take(300)
      .map(l => s"${l.src}->${l.dst}")
      .mkString("\n")

    result.toOption.map(r => {
      s"""[[[#!Graph enableWikiLink
         |$r
         |]]]
         |""".stripMargin
    }).getOrElse("")
  }


  def getMarkupSeeAlso(name: String)(implicit wikiContext: WikiContext, connection: Connection): String = {
    s"""
       |[[Html(<table class="seeAlso"><thead><tr><th>Page Suggestion</th><th>Related Pages</th></tr></thead><tbody><tr><td class="pageSuggestion">)]]
       |'''Backlinks'''
       |[[Backlinks]]
       |
       |'''Similar Pages'''
       |[[SimilarPages]]
       |
       |[[Html(</td><td class="relatedPages">)]]
       |
       |${getMarkupRelatedPages(name)}
       |
       |[[Html(</td></tr></tbody></table>)]]
       |""".stripMargin
  }

}
