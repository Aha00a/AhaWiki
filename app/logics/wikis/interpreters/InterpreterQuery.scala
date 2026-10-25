package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import logics.wikis.QueryEngine
import logics.wikis.QueryEngine.Row
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import logics.wikis.macros.MacroError
import models.ContextWikiPage
import models.PageContent
import models.tables.CalculatedLink
import models.tables.CalculatedSchemaOrg
import models.tables.Site

/**
 * A list of pages chosen by their `#!Schema` properties — a database view over the wiki.
 *
 *   [[[#!Query Person
 *   where   birthDate   md   today
 *   show    birthDate   birthPlace
 *   sort    birthDate
 *   ]]]
 *
 * The arguments name the classes; the body is `where` / `show` / `sort` / `group` / `limit` / `view`
 * lines, tab-separated like `#!Schema`'s. What the lines mean is `QueryEngine`'s; this reads the
 * rows the database has, asks the engine, and draws the answer.
 *
 * It reads when the page is drawn, so a page it lists appears as soon as its `#!Schema` is saved.
 */
object InterpreterQuery extends TraitInterpreter {

  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent = PageContent(content)
    val parsed = InterpreterSchema.parse(pageContent)
    QueryEngine.parse(parsed.schemaClasses, parsed.seqSeqField) match {
      case Left(error) => MacroError.toHtmlString(s"Query - ${error.escapeHtml()}")
      case Right(spec) => render(spec)
    }
  }

  private def render(spec: QueryEngine.Spec)(implicit wikiContext: ContextWikiPage): String = {
    val result = wikiContext.withConnection { implicit connection =>
      implicit val site: Site = wikiContext.site
      val inClass = if (spec.classes.isEmpty) None else Some(CalculatedSchemaOrg.selectPagesOfClasses(spec.classes))
      val facts = CalculatedSchemaOrg.selectFacts(spec.props, spec.classes)
        .groupBy(_._1)
        .map { case (page, rows) => page -> rows.groupBy(_._2).map { case (prop, vs) => prop -> vs.map(_._3).sorted } }
      QueryEngine.run(spec, facts, inClass, wikiContext.localDateNow)
    }

    // A page the reader may not see is not listed, and is not counted either.
    val visible = result.rows.filter(row => wikiContext.pageCanSee(row.page))
    if (visible.isEmpty) {
      """<div class="InterpreterQuery empty">No pages match.</div>"""
    } else {
      val body = spec.view match {
        case "list" => renderList(spec, visible)
        case _ => renderTable(spec, visible)
      }
      val more = if (result.total > visible.size) s" (showing ${visible.size})" else ""
      s"""<div class="InterpreterQuery">$body<div class="queryCount">${result.total} page(s)$more</div></div>"""
    }
  }

  private def link(name: String)(implicit wikiContext: ContextWikiPage): String =
    AhaMarkLink(name, noFollow = true).toHtmlString(wikiContext.setPageNameByPermission)

  private def cell(row: Row, prop: String)(implicit wikiContext: ContextWikiPage): String =
    QueryEngine.displayValues(row.values.getOrElse(prop, Seq.empty)).map(link).mkString(", ")

  private def renderTable(spec: QueryEngine.Spec, rows: Seq[Row])(implicit wikiContext: ContextWikiPage): String = {
    val head = ("Name" +: spec.show).map(h => s"<th>${h.escapeHtml()}</th>").mkString
    val columns = 1 + spec.show.size
    val sb = new StringBuilder
    sb.append(s"""<table class="wikiTableSimple tablesorter"><thead><tr>$head</tr></thead><tbody>""")
    var previousGroup: Option[Option[String]] = None
    rows.foreach { row =>
      spec.group.foreach { prop =>
        val group = row.first(prop)
        if (!previousGroup.contains(group)) {
          sb.append(s"""<tr class="queryGroup"><th colspan="$columns">${group.map(g => link(g)).getOrElse("—")}</th></tr>""")
          previousGroup = Some(group)
        }
      }
      sb.append("<tr><td><strong>").append(link(row.page)).append("</strong></td>")
      spec.show.foreach(prop => sb.append("<td>").append(cell(row, prop)).append("</td>"))
      sb.append("</tr>")
    }
    sb.append("</tbody></table>").result()
  }

  private def renderList(spec: QueryEngine.Spec, rows: Seq[Row])(implicit wikiContext: ContextWikiPage): String = {
    val sb = new StringBuilder
    var previousGroup: Option[Option[String]] = None
    var open = false
    rows.foreach { row =>
      spec.group.foreach { prop =>
        val group = row.first(prop)
        if (!previousGroup.contains(group)) {
          if (open) sb.append("</ul>")
          sb.append(s"""<h4 class="queryGroup">${group.map(g => link(g)).getOrElse("—")}</h4><ul>""")
          open = true
          previousGroup = Some(group)
        }
      }
      if (!open) { sb.append("<ul>"); open = true }
      val extra = spec.show.flatMap { prop =>
        val values = cell(row, prop)
        if (values.isEmpty) None else Some(s"""<span class="queryProp">${prop.escapeHtml()}</span> $values""")
      }
      sb.append("<li>").append(link(row.page))
      if (extra.nonEmpty) sb.append(" — ").append(extra.mkString(" · "))
      sb.append("</li>")
    }
    if (open) sb.append("</ul>")
    sb.result()
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = Seq()
}
