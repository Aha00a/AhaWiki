package logics.wikis.macros

import java.sql.Connection

import com.aha00a.commons.Implicits._
import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroSeeAlso extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    InterpreterWiki.toHtmlString(getMarkupSeeAlso(argument.getOrElse(wikiContext.nameTop)))
  }}

  private def getMarkupSchema(name: String)(implicit wikiContext: WikiContext, connection: Connection) = {
    import models.tables.SchemaOrg
    val listSchemaOrg = SchemaOrg.selectWhereValue(name).filter(s => s.and(wikiContext.pageCanSee))
    val mapClsList = listSchemaOrg.groupBy(_.cls)
    mapClsList.keys.toSeq.sorted.map(k => {
      s""" * [schema:$k $k]
         |${mapClsList(k).map(t => s"""  * [schema:${t.prop} ${t.prop}] of ["${t.page}"]""").mkString("\n")}""".stripMargin
    }).mkString("\n")
  }

  def getMarkupRelatedPages(name: String)(implicit wikiContext: WikiContext, connection: Connection): String = {
    import models.tables.Link
    import models.tables.SchemaOrg

    import scala.util.matching.Regex

    val seqLink: Seq[Link] = Link.select(name)
    val seqSchemaOrg = SchemaOrg.selectWhereValue(name).filter(s => s.and(wikiContext.pageCanSee))
    val seqSchemaOrgLink = seqSchemaOrg.map(s => Link(s.page, s.value, ""))

    val year: Regex = """\d{4}""".r
    val date: Regex = """\d{4}-\d{2}-\d{2}""".r

    val seqLinkFiltered: Seq[Link] = (seqLink ++ seqSchemaOrgLink).filter(l => l.and(wikiContext.pageCanSee))
    val seqLinkFilteredExpanded: Seq[Link] = Link.expand(seqLinkFiltered)
    val seqLinkFilteredExpandedFiltered: Seq[Link] = seqLinkFilteredExpanded.filter(l => l.and(wikiContext.pageCanSee))
    val result = seqLinkFilteredExpandedFiltered
      .filter(l => (l.src, l.dst) match {
        case (year(), date()) => false
        case (date(), year()) => false
        case _ => true
      })
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
       |[[Html(<table class="seeAlso"><thead><tr><th>Page Suggestion</th><th>Related Pages</th></tr></thead><tbody><tr><td>)]]
       |'''[schema:Schema Schema]'''
       |${getMarkupSchema(name)}
       |
       |'''Similar Pages'''
       |[[SimilarPages]]
       |
       |'''Backlinks'''
       |[[Backlinks]]
       |[[Html(</td><td>)]]
       |${getMarkupRelatedPages(name)}
       |[[Html(</td></tr></tbody></table>)]]
       |""".stripMargin
  }

}
