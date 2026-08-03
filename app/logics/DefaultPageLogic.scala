package logics

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.EnglishCaseConverter
import models.ContextWikiPage
import scalaz.LazyOption._
import scalaz._

import java.sql.Connection
import scala.util.matching.Regex

object DefaultPageLogic {

  private val regexSchemaColon: Regex = """^schema:(.+)$""".r

  // 기본 문서는 conf/Page/ 에 있고 클래스패스 루트의 Page/ 로 패키징된다.
  // 파일시스템 경로(app/assets/Page)로 읽으면 sbt stage/dist 배포본에서 동작하지 않는다.
  // conf/ 에 둔 이유는 app/assets/ 아래에 있으면 sbt-web 이 웹 에셋으로 패키징해
  // /assets/Page/FrontPage 로 원본이 그대로 노출되기 때문이다.
  private def defaultPageResource(title: String): String = s"Page/$title"

  private def defaultPageExists(title: String): Boolean =
    getClass.getClassLoader.getResource(defaultPageResource(title)) != null

  private def readDefaultPage(title: String): Option[String] =
    Option(getClass.getClassLoader.getResourceAsStream(defaultPageResource(title)))
      .map(is => com.aha00a.commons.utils.Using(scala.io.Source.fromInputStream(is)(scala.io.Codec.UTF8))(_.mkString))

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
      case _ => defaultPageExists(title)
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
        readDefaultPage(title) match {
          case Some(content) => lazySome(content)
          case None => lazyNone
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
