package logics

import java.io.File
import java.sql.Connection

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.EnglishCaseConverter
import models.ContextWikiPage
import scalaz.LazyOption._
import scalaz._

import scala.util.matching.Regex

object DefaultPageLogic {

  val regexSchemaColon: Regex = """^schema:(.+)$""".r

  def getOption(title: String)(implicit wikiContext: ContextWikiPage, connection: Connection): LazyOption[String] = {
    import com.aha00a.commons.utils.DateTimeUtil
    import logics.wikis.interpreters.ahaMark.AhaMarkLink
    import logics.wikis.macros.MacroMonthName
    import models.tables.Site

    implicit val site: Site = wikiContext.site

    title match {
      case DateTimeUtil.regexIsoLocalDate(y, m, d) =>
        lazySome(s"[[DayHeader]]\n")

      case DateTimeUtil.regexYearDashMonth(y, m) =>
        lazySome(
          s"""= [$y]-$m
             |[[NavigationYearMonth]]
             |[[IncludeDays]]
             |""".stripMargin
        )

      case DateTimeUtil.regexYear(y) =>
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
        val listSchemaOrg = models.tables.SchemaOrg.selectWhereProp("")
        val listSchemaOrgWithPermission = listSchemaOrg.filter(s => wikiContext.setPageNameByPermission.contains(s.page))
        val mapSchemaOrg = listSchemaOrgWithPermission.groupBy(_.cls)

        lazySome(
          s"""= Schema
             |${listSchemaOrgWithPermission.size} page(s).
             |${SchemaOrg.renderExistingPages(mapSchemaOrg.view.mapValues(s => s.map(_.page)).toMap)}
             |""".stripMargin
        )

      case regexSchemaColon(schema) =>
        // TODO: extract macro
        val optionSchemaType = SchemaOrg.mapAll.get(schema)
        optionSchemaType match {
          case Some(schemaType) =>
            import models.tables.SchemaOrg
            lazySome(
              if(schema(0).isUpper) {
                val listSchemaOrg: List[SchemaOrg] = models.tables.SchemaOrg.selectWhereCls(schema)
                s"""= ${EnglishCaseConverter.pascalCase2TitleCase(schemaType.id)}
                   |[[[#!Markdown
                   |${schemaType.comment.replaceAll("\\\\n", "\n")}
                   |]]]
                   |== Pages
                   |[[Html(<div class="columnWidth350">)]]
                   |${listSchemaOrg.map(s => s""" * ["${s.page}"]""").mkString("\n")}
                   |[[Html(</div>)]]
                   |""".stripMargin
              } else {
                val listSchemaOrg: List[SchemaOrg] = models.tables.SchemaOrg.selectWhereProp(schema)
                val listSchemaOrgWithPermission = listSchemaOrg.filter(s => wikiContext.setPageNameByPermission.contains(s.page))
                s"""= ${EnglishCaseConverter.camelCase2TitleCase(schemaType.id)}
                   |[[[#!Markdown
                   |${schemaType.comment.replaceAll("\\\\n", "\n")}
                   |]]]
                   |${listSchemaOrgWithPermission.groupBy(_.value).transform((k, v) => v.groupBy(_.cls)).toSeq.sortBy(_._1).map(t =>
                  s"""== ["${t._1}" ${t._1}]
                     |${t._2.toSeq.sortBy(_._1).map(t2 =>
                    s"""=== ["schema:${t2._1}" ${EnglishCaseConverter.pascalCase2TitleCase(t2._1)}]
                       |[[Html(<div class="columnWidth350">)]]
                       |${t2._2.map(s => s""" * ["${s.page}"]""").mkString("\n")}
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
