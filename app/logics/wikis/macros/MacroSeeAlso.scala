package logics.wikis.macros

import java.sql.Connection

import com.aha00a.commons.Implicits._
import logics.wikis.interpreters.InterpreterWiki
import models.{AhaWikiQuery, Link, WikiContext}

object MacroSeeAlso extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    val ahaWikiQuery: AhaWikiQuery = AhaWikiQuery()
    InterpreterWiki.interpret(getMarkupSeeAlso(argument.getOrElse(wikiContext.nameTop), ahaWikiQuery))
  }}

  private def getMarkupSchema(name: String, ahaWikiQuery: AhaWikiQuery)(implicit wikiContext: WikiContext) = {
    val listSchemaOrg = ahaWikiQuery.SchemaOrg.selectWhereValue(name).filter(s => s.and(wikiContext.pageCanSee))
    val mapClsList = listSchemaOrg.groupBy(_.cls)
    mapClsList.keys.toSeq.sorted.map(k => {
      s""" * [schema:$k $k]
         |${mapClsList(k).map(t => s"""  * [schema:${t.prop} ${t.prop}] of ["${t.page}"]""").mkString("\n")}""".stripMargin
    }).mkString("\n")
  }

  def getMarkupRelatedPages(name: String, ahaWikiQuery: AhaWikiQuery)(implicit wikiContext: WikiContext, connection: Connection): String = {
    val ahaWikiQuery: AhaWikiQuery = AhaWikiQuery()
    val seqLink: Seq[Link] = ahaWikiQuery.Link.select(name)
    val seqSchemaOrg = ahaWikiQuery.SchemaOrg.selectWhereValue(name).filter(s => s.and(wikiContext.pageCanSee))
    val seqSchemaOrgLink = seqSchemaOrg.map(s => Link(s.page, s.value, ""))

    val seqLinkFiltered: Seq[Link] = (seqLink ++ seqSchemaOrgLink).filter(l => l.and(wikiContext.pageCanSee))
    val seqLinkFilteredExpanded: Seq[Link] = ahaWikiQuery.Link.expand(seqLinkFiltered)
    val seqLinkFilteredExpandedFiltered: Seq[Link] = seqLinkFilteredExpanded.filter(l => l.and(wikiContext.pageCanSee))
    val result = seqLinkFilteredExpandedFiltered
      .map(l => s"${l.src}->${l.dst}")
      .mkString("\n")

    result.toOption.map(r => {
      s"""[[[#!Graph enableWikiLink
         |$r
         |]]]
         |""".stripMargin
    }).getOrElse("")
  }


  def getMarkupSeeAlso(name: String, ahaWikiQuery: AhaWikiQuery)(implicit wikiContext: WikiContext, connection: Connection): String = {
    s"""
       |[[Html(<table class="seeAlso"><thead><tr><th>Page Suggestion</th><th>Related Pages</th></tr></thead><tbody><tr><td>)]]
       |'''[schema:Schema Schema]'''
       |${getMarkupSchema(name, ahaWikiQuery)}
       |
       |'''Similar Pages'''
       |[[SimilarPages]]
       |
       |'''Backlinks'''
       |[[Backlinks]]
       |[[Html(</td><td>)]]
       |${getMarkupRelatedPages(name, ahaWikiQuery)}
       |[[Html(</td></tr></tbody></table>)]]
       |""".stripMargin
  }

}
