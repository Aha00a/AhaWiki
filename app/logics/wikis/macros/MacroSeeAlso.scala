package logics.wikis.macros

import java.sql.Connection

import com.aha00a.commons.Implicits._
import logics.wikis.interpreters.InterpreterWiki
import models.{AhaWikiQuery, Link, WikiContext}

object MacroSeeAlso extends TraitMacro {
  override def apply(argument:String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    val ahaWikiQuery: AhaWikiQuery = AhaWikiQuery()
    InterpreterWiki(getMarkupSeeAlso(argument.getOrElse(wikiContext.nameTop), ahaWikiQuery))
  }}

  private def getMarkupSchema(name: String, ahaWikiQuery: AhaWikiQuery) = {
    val seqLinkSchema: List[Link] = ahaWikiQuery.Link.selectSchema(name)
    val mapClassSrcProperty: Map[String, List[(String, String, String)]] = seqLinkSchema.map(l => {
      val splitted = l.alias.split(":")
      splitted match {
        case Array(s0, s1) => (l.src, s1, "")
        case Array(s0, s1, s2) => (l.src, s1, s2)
      }
    }).filter(_._3.isNotNullOrEmpty).groupBy(_._2)
    mapClassSrcProperty.keys.toSeq.sorted.map(k => {
      s""" * [schema:$k $k]
         |${mapClassSrcProperty(k).map(t => s"""  * [schema:${t._3} ${t._3}] of ["${t._1}"]""").mkString("\n")}""".stripMargin
    }).mkString("\n")
  }

  def getMarkupRelatedPages(name: String, ahaWikiQuery: AhaWikiQuery)(implicit connection: Connection): String = {
    val ahaWikiQuery: AhaWikiQuery = AhaWikiQuery()
    val seqLink: Seq[Link] = ahaWikiQuery.Link.select(name)
    val seqLinkExpanded: Seq[Link] = ahaWikiQuery.Link.expand(seqLink)
    val result = seqLinkExpanded
      .map(l => s"${l.src}->${l.dst}")
      .mkString("\n")

    result.toOption.map(r => {
      s"""[[[#!Graph enableWikiLink
         |$r
         |]]]
         |""".stripMargin
    }).getOrElse("")
  }


  def getMarkupSeeAlso(name: String, ahaWikiQuery: AhaWikiQuery)(implicit connection: Connection): String = {
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
