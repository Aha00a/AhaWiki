package logics

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.EnglishCaseConverter
import models.ContextWikiPage
import scalaz.LazyOption._
import scalaz._

import java.io.File
import java.sql.Connection
import scala.util.matching.Regex

object DefaultPageLogic {

  private val regexSchemaColon: Regex = """^schema:(.+)$""".r

  def isDefined(title: String): Boolean = {
    import com.aha00a.commons.utils.DateTimeUtil

    title match {
      case DateTimeUtil.regexIsoLocalDate(_, _, _) => true
      case DateTimeUtil.regexYearDashMonth(_, _) => true
      case DateTimeUtil.regexYear(_) => true
      case DateTimeUtil.regexDashDashMonthDashDay(_, _) => true
      case DateTimeUtil.regexDashDashMonth(_) => true
      case "schema:Schema" => true
      case regexSchemaColon(schema) => CalculatedSchemaOrg.mapAll.isDefinedAt(schema)
      case _ => new File("app/assets/Page", title).exists()
    }
  }

  def getOption(title: String)(implicit wikiContext: ContextWikiPage, connection: Connection): LazyOption[String] = {
    import com.aha00a.commons.utils.DateTimeUtil
    import models.tables.Site

    implicit val site: Site = wikiContext.site

    title match {
      case DateTimeUtil.regexIsoLocalDate(_, _, _) =>
        lazySome(s"[[DayHeader]]\n")

      case DateTimeUtil.regexYearDashMonth(y, m) =>
        lazySome(
          s"""= [$y]-$m
             |[[NavigationYearMonth]]
             |[[IncludeDays]]
             |""".stripMargin
        )

      case DateTimeUtil.regexYear(_) =>
        lazySome(
          s"""= $title
             |[[NavigationYear]]
             |[[Calendar]]
             |""".stripMargin
        )

      case DateTimeUtil.regexDashDashMonthDashDay(mm, dd) =>
        lazySome(
          s"""= $mm-$dd
             |[--$mm $mm]-[----$dd $dd]
             |[[[#!Html
             |${renderMonthDaysTable(mm)}
             |]]]
             |""".stripMargin
        )

      case DateTimeUtil.regexDashDashMonth(mm) =>
        lazySome(
          s"""= [[MonthName]]
             |[[[#!Html
             |${renderMonthDaysTable(mm)}
             |]]]
             |""".stripMargin
        )

      case "schema:Schema" =>
        lazySome(schemaIndexPageContent)

      case regexSchemaColon(schema) =>
        CalculatedSchemaOrg.mapAll.get(schema) match {
          case Some(schemaType) => lazySome(schemaPageContent(schema, schemaType))
          case None => lazyNone
        }

      case _ =>
        val file = new File("app/assets/Page", title)
        if(file.exists()) {
          lazySome(file.readAllString())
        } else {
          lazyNone
        }
    }
  }

  private def renderMonthDaysTable(mm: String)(implicit wikiContext: ContextWikiPage): String = {
    import com.aha00a.commons.utils.DateTimeUtil
    import logics.wikis.interpreters.ahaMark.AhaMarkLink
    import logics.wikis.macros.MacroMonthName

    val lastDay: Int = DateTimeUtil.getLastDay(mm.toInt)
    val table = <table class="month wikiTableSimple">
      <thead>
        <tr>
          <th colspan="31">
            {MacroMonthName.toHtmlString(s"--$mm")}
          </th>
        </tr>
      </thead>
      <tbody>
        <tr>
          {(1 to lastDay).grouped(5).map(days =>
          <tr>
            {days.map(Some(_)).padTo(5, None).map(day =>
            <td>
              {day.map(d => scala.xml.XML.loadString(AhaMarkLink(f"--$mm-$d%02d", f"$d%02d").toHtmlString())).getOrElse("")}
            </td>
          )}
          </tr>
        )}
        </tr>
      </tbody>
    </table>

    table.toString()
  }

  private def schemaIndexPageContent(implicit wikiContext: ContextWikiPage, connection: Connection): String = {
    implicit val site: models.tables.Site = wikiContext.site
    val listSchemaOrg = models.tables.CalculatedSchemaOrg.selectWhereProp("")
    val listSchemaOrgWithPermission = listSchemaOrg.filter(s => wikiContext.setPageNameByPermission.contains(s.page))
    val mapSchemaOrg = listSchemaOrgWithPermission.groupBy(_.cls)

    s"""= Schema
       |${listSchemaOrgWithPermission.size} page(s).
       |${CalculatedSchemaOrg.renderExistingPages(mapSchemaOrg.view.mapValues(s => s.map(_.page)).toMap)}
       |""".stripMargin
  }

  private def schemaPageContent(schema: String, schemaType: CalculatedSchemaOrg.SchemaType)(implicit wikiContext: ContextWikiPage, connection: Connection): String =
    if(schema(0).isUpper) schemaClassPageContent(schema, schemaType)
    else schemaPropertyPageContent(schema, schemaType)

  private def schemaClassPageContent(schema: String, schemaType: CalculatedSchemaOrg.SchemaType)(implicit wikiContext: ContextWikiPage, connection: Connection): String = {
    implicit val site: models.tables.Site = wikiContext.site
    val listSchemaOrg: List[models.tables.CalculatedSchemaOrg] = models.tables.CalculatedSchemaOrg.selectWhereCls(schema)
    val listSchemaOrgWithPermission = listSchemaOrg.filter(s => wikiContext.setPageNameByPermission.contains(s.page))

    s"""= ${EnglishCaseConverter.pascalCase2TitleCase(schemaType.id)}
       |[[[#!Markdown
       |${schemaType.comment.replaceAll("\\\\n", "\n")}
       |]]]
       |[https://schema.org/${schemaType.id}]
       |== Pages
       |<Columns count="3" gap="16" minWidth="220">
       |${listSchemaOrgWithPermission.map(s => s""" 1. ["${s.page}"]""").mkString("\n")}
       |</Columns>
       |""".stripMargin
  }

  private def schemaPropertyPageContent(schema: String, schemaType: CalculatedSchemaOrg.SchemaType)(implicit wikiContext: ContextWikiPage, connection: Connection): String = {
    implicit val site: models.tables.Site = wikiContext.site
    val listSchemaOrg: List[models.tables.CalculatedSchemaOrg] = models.tables.CalculatedSchemaOrg.selectWhereProp(schema)
    val listSchemaOrgWithPermission = listSchemaOrg.filter(s => wikiContext.setPageNameByPermission.contains(s.page))
    val groupedByClassAndValue = listSchemaOrgWithPermission.groupBy(_.cls).transform((_, v) => v.groupBy(_.value))

    s"""= ${EnglishCaseConverter.camelCase2TitleCase(schemaType.id)}
       |[[[#!Markdown
       |${schemaType.comment.replaceAll("\\\\n", "\n")}
       |]]]
       |[https://schema.org/${schemaType.id}]
       |${groupedByClassAndValue.toSeq.sortBy(_._1).map { case (cls, byValue) =>
      s"""== ["schema:$cls" ${EnglishCaseConverter.pascalCase2TitleCase(cls)}]
         |${byValue.toSeq.sortBy(_._1).map { case (value, pages) =>
        s"""=== ["$value" $value]
           |<Columns count="3" gap="16" minWidth="220">
           |${pages.map(s => s""" 1. ["${s.page}"]""").mkString("\n")}
           |</Columns>
           |""".stripMargin
      }.mkString("\n")}
         |""".stripMargin
    }.mkString("\n")}
       |""".stripMargin
  }
}
