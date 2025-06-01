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
    import logics.wikis.interpreters.ahaMark.AhaMarkLink
    import logics.wikis.macros.MacroMonthName
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
        // TODO: extract macro
        val lastDay: Int = DateTimeUtil.getLastDay(mm.toInt)

        val r = <table class="month simpleTable">
          <thead>
            <tr>
              <th colspan="31">
                {MacroMonthName.toHtmlString(s"--$mm")}
              </th>
            </tr>
          </thead>
          <tbody>
            <tr>
              {(1 to lastDay).grouped(5).map(t =>
              <tr>
                {t.map(Some(_)).padTo(5, None).map(d =>
                <td>
                  {d.map(d => scala.xml.XML.loadString(AhaMarkLink(f"--$mm-$d%02d", f"$d%02d").toHtmlString())).getOrElse("")}
                </td>
              )}
              </tr>
            )}
            </tr>
          </tbody>
        </table>
        lazySome(
          s"""= $mm-$dd
             |[--$mm $mm]-[----$dd $dd]
             |[[[#!Html
             |${r.toString()}
             |]]]
             |""".stripMargin
        )

      case DateTimeUtil.regexDashDashMonth(mm) =>
        // TODO: extract macro
        val lastDay: Int = DateTimeUtil.getLastDay(mm.toInt)

        val r = <table class="month simpleTable">
          <thead>
            <tr>
              <th colspan="31">
                {MacroMonthName.toHtmlString(s"--$mm")}
              </th>
            </tr>
          </thead>
          <tbody>
            <tr>
              {(1 to lastDay).grouped(5).map(t =>
              <tr>
                {t.map(Some(_)).padTo(5, None).map(d =>
                <td>
                  {d.map(d => scala.xml.XML.loadString(AhaMarkLink(f"--$mm-$d%02d", f"$d%02d").toHtmlString())).getOrElse("")}
                </td>
              )}
              </tr>
            )}
            </tr>
          </tbody>
        </table>
        lazySome(
          s"""= [[MonthName]]
             |[[[#!Html
             |${r.toString()}
             |]]]
             |""".stripMargin
        )

      //          case regexDashDashDashDashDay(mm) =>
      //            Ok(mm) // TODO

      case "schema:Schema" =>
        // TODO: extract macro
        val listSchemaOrg = models.tables.CalculatedSchemaOrg.selectWhereProp("")
        val listSchemaOrgWithPermission = listSchemaOrg.filter(s => wikiContext.setPageNameByPermission.contains(s.page))
        val mapSchemaOrg = listSchemaOrgWithPermission.groupBy(_.cls)

        lazySome(
          s"""= Schema
             |${listSchemaOrgWithPermission.size} page(s).
             |${CalculatedSchemaOrg.renderExistingPages(mapSchemaOrg.view.mapValues(s => s.map(_.page)).toMap)}
             |""".stripMargin
        )

      case regexSchemaColon(schema) =>
        // TODO: extract macro
        val optionSchemaType = CalculatedSchemaOrg.mapAll.get(schema)
        optionSchemaType match {
          case Some(schemaType) =>
            import models.tables.CalculatedSchemaOrg
            lazySome(
              if(schema(0).isUpper) {
                val listSchemaOrg: List[CalculatedSchemaOrg] = models.tables.CalculatedSchemaOrg.selectWhereCls(schema)
                s"""= ${EnglishCaseConverter.pascalCase2TitleCase(schemaType.id)}
                   |[[[#!Markdown
                   |${schemaType.comment.replaceAll("\\\\n", "\n")}
                   |]]]
                   |[https://schema.org/${schemaType.id}]
                   |== Pages
                   |[[Html(<div class="columnWidth350">)]]
                   |${listSchemaOrg.map(s => s""" 1. ["${s.page}"]""").mkString("\n")}
                   |[[Html(</div>)]]
                   |""".stripMargin
              } else {
                val listSchemaOrg: List[CalculatedSchemaOrg] = models.tables.CalculatedSchemaOrg.selectWhereProp(schema)
                val listSchemaOrgWithPermission = listSchemaOrg.filter(s => wikiContext.setPageNameByPermission.contains(s.page))
                s"""= ${EnglishCaseConverter.camelCase2TitleCase(schemaType.id)}
                   |[[[#!Markdown
                   |${schemaType.comment.replaceAll("\\\\n", "\n")}
                   |]]]
                   |[https://schema.org/${schemaType.id}]
                   |${listSchemaOrgWithPermission.groupBy(_.cls).transform((_, v) => v.groupBy(_.value)).toSeq.sortBy(_._1).map(t =>
                  s"""== ["schema:${t._1}" ${EnglishCaseConverter.pascalCase2TitleCase(t._1)}]
                     |${t._2.toSeq.sortBy(_._1).map(t2 =>
                    s"""=== ["${t2._1}" ${t2._1}]
                       |[[Html(<div class="columnWidth350">)]]
                       |${t2._2.map(s => s""" 1. ["${s.page}"]""").mkString("\n")}
                       |[[Html(</div>)]]
                       |""".stripMargin).mkString("\n")}
                     |""".stripMargin).mkString("\n")}
                   |""".stripMargin
              }
            )

          case _ =>
            lazyNone

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
}
