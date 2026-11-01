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
  private val regexSchemaFacet: Regex = """^schema:([^:]+):(.+)$""".r

  private def isSchemaClass(term: String): Boolean = term.nonEmpty && term(0).isUpper && CalculatedSchemaOrg.mapAll.isDefinedAt(term)
  private def isSchemaProperty(term: String): Boolean = term.nonEmpty && term(0).isLower && CalculatedSchemaOrg.mapAll.isDefinedAt(term)

  /**
   * What a schema name of more than one term narrows to. With the two single-term pages the names
   * make one system, each term a filter on CalculatedSchemaOrg:
   *
   * schema:TVSeries                    the class
   * schema:publisher                   the property, in every class
   * schema:TVSeries:publisher          the property in one class, by value
   * schema:publisher:JTBC              one value, in every class
   * schema:TVSeries:publisher:JTBC     one value in one class
   *
   * The case of the first term tells the third name from the fourth; the vocabulary spells every
   * class with a capital and every property without. A value may hold colons, so the rest of the
   * name after the property is the value.
   */
  case class SchemaFacet(cls: Option[String], prop: String, value: Option[String]) {
    def propertyName: String = "schema:" + (cls.toSeq :+ prop).mkString(":")
    def withValue(v: String): String = s"$propertyName:$v"
  }

  /**
   * Whether such a name exists is decided from the vocabulary alone, the way a date exists whether
   * or not a page was saved under it. `isDefined` runs for every link to a missing page and has no
   * connection, so it cannot ask which values are in use; that is why a value nothing uses still
   * gets a page, and why that page lists the values that are in use instead of being empty.
   */
  def parseSchemaFacet(title: String): Option[SchemaFacet] = title match {
    case regexSchemaFacet(first, rest) if isSchemaClass(first) =>
      val parts = rest.split(":", 2)
      Some(parts(0)).filter(isSchemaProperty).map(prop => SchemaFacet(Some(first), prop, parts.lift(1).filter(_.nonEmpty)))
    case regexSchemaFacet(first, rest) if isSchemaProperty(first) =>
      Some(SchemaFacet(None, first, Some(rest)))
    case _ => None
  }

  // 기본 문서는 conf/Page/ 에 있고 클래스패스 루트의 Page/ 로 패키징된다.
  // 파일시스템 경로(app/assets/Page)로 읽으면 sbt stage/dist 배포본에서 동작하지 않는다.
  // conf/ 에 둔 이유는 app/assets/ 아래에 있으면 sbt-web 이 웹 에셋으로 패키징해
  // /assets/Page/FrontPage 로 원본이 그대로 노출되기 때문이다.
  private def defaultPageResource(title: String): String = s"Page/$title"

  // Every link to a page that does not exist asks this, and asking the class loader means looking
  // through the jars: 4% of a render's time (2026-10-07). What is on the classpath does not change
  // while the process runs, so each answer is kept. The names come from page text, so the map is
  // emptied rather than allowed to grow without end.
  private val defaultPageExistsByTitle = new java.util.concurrent.ConcurrentHashMap[String, java.lang.Boolean]()
  private val defaultPageExistsMaxEntries = 10000

  private def defaultPageExists(title: String): Boolean = {
    if (defaultPageExistsByTitle.size() > defaultPageExistsMaxEntries) defaultPageExistsByTitle.clear()
    defaultPageExistsByTitle.computeIfAbsent(title, t => getClass.getClassLoader.getResource(defaultPageResource(t)) != null)
  }

  private def readDefaultPage(title: String): Option[String] =
    Option(getClass.getClassLoader.getResourceAsStream(defaultPageResource(title)))
      .map(is => com.aha00a.commons.utils.Using(scala.io.Source.fromInputStream(is)(scala.io.Codec.UTF8))(_.mkString))

  /** A file under conf/Page/, as opposed to a page made up from the shape of its name. */
  def isShippedPage(title: String): Boolean = defaultPageExists(title)

  def isDefined(title: String): Boolean = {
    import com.aha00a.commons.utils.DateTimeUtil

    title match {
      case DateTimeUtil.regexIsoLocalDate(_, _, _) => true
      case DateTimeUtil.regexYearDashMonth(_, _) => true
      case DateTimeUtil.regexYear(_) => true
      case DateTimeUtil.regexDashDashMonthDashDay(_, _) => true
      case DateTimeUtil.regexDashDashMonth(_) => true
      case "schema:Schema" => true
      case _ if parseSchemaFacet(title).isDefined => true
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
             |[[InlineDays]]
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
             |[--$mm|$mm]-[---$dd|$dd]
             |[[OnThisDay]]
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

      case _ if parseSchemaFacet(title).isDefined =>
        val facet = parseSchemaFacet(title).get
        lazySome(schemaFacetPageContent(facet, schemaRowsOf(facet)))

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

  /**
   * The line under a term's description saying where it is defined.
   *
   * schema.org's own terms link to their page there. Ours have no page there — that is the whole
   * reason they are in public/schema.org/custom.jsonld — so the same link would send the reader to
   * a 404, which is what these pages did from the moment defining the classes made them exist.
   *
   * The parent is named here rather than linked from the heading because this template draws no
   * hierarchy for any class, ours or schema.org's.
   */
  private def schemaSourceLine(schemaType: CalculatedSchemaOrg.SchemaType): String =
    if (CalculatedSchemaOrg.isCustom(schemaType.id))
      s"""''schema.org 에 없어서 이 위키가 정의한 class 입니다. 상위는 ["schema:${schemaType.subClassOf.headOption.getOrElse("Thing")}"] 입니다.''"""
    else
      s"[https://schema.org/${schemaType.id}]"

  private def schemaClassPageContent(schema: String, schemaType: CalculatedSchemaOrg.SchemaType)(implicit wikiContext: ContextWikiPage, connection: Connection): String = {
    implicit val site: models.tables.Site = wikiContext.site
    val listSchemaOrg: List[models.tables.CalculatedSchemaOrg] = models.tables.CalculatedSchemaOrg.selectWhereCls(schema)
    val listSchemaOrgWithPermission = listSchemaOrg.filter(s => wikiContext.setPageNameByPermission.contains(s.page))

    s"""= ${EnglishCaseConverter.pascalCase2TitleCase(schemaType.id)}
       |[[[#!Markdown
       |${schemaType.comment.replaceAll("\\\\n", "\n")}
       |]]]
       |${schemaSourceLine(schemaType)}
       |${schemaClassPropertiesSection(schema, models.tables.CalculatedSchemaOrg.selectPropertiesWhereCls(schema).filter(s => wikiContext.setPageNameByPermission.contains(s.page)))}
       |== Pages
       |${pageColumns(listSchemaOrgWithPermission.map(_.page))}
       |""".stripMargin
  }

  private def pageColumns(pages: Seq[String]): String =
    s"""<Columns count="3" gap="16" minWidth="220">
       |${pages.map(page => s""" 1. ["$page"]""").mkString("\n")}
       |</Columns>""".stripMargin

  /** One heading per value, with the pages holding it. `heading` is the level, `==` or `===`. */
  private def valueSections(heading: String, rows: Seq[models.tables.CalculatedSchemaOrg]): String =
    rows.groupBy(_.value).toSeq.sortBy(_._1).map { case (value, pages) =>
      s"""$heading ["$value" $value]
         |${pageColumns(pages.map(_.page))}
         |""".stripMargin
    }.mkString("\n")

  /**
   * The properties a class's pages use, each linking to that class's page of it. Labels the
   * vocabulary does not have are left out: they have no page to link to.
   */
  def schemaClassPropertiesSection(cls: String, rows: Seq[models.tables.CalculatedSchemaOrg]): String = {
    val lines = rows.filter(row => isSchemaProperty(row.prop)).groupBy(_.prop).toSeq.sortBy(_._1).map { case (prop, of) =>
      s""" * ["schema:$cls:$prop" ${EnglishCaseConverter.camelCase2TitleCase(prop)}] (${of.map(_.page).distinct.size})"""
    }
    if (lines.isEmpty) "" else ("== Properties" +: lines).mkString("\n")
  }

  private def schemaRowsOf(facet: SchemaFacet)(implicit wikiContext: ContextWikiPage, connection: Connection): Seq[models.tables.CalculatedSchemaOrg] = {
    implicit val site: models.tables.Site = wikiContext.site
    models.tables.CalculatedSchemaOrg.selectWhereProp(facet.prop)
      .filter(s => facet.cls.forall(_ == s.cls) && wikiContext.setPageNameByPermission.contains(s.page))
  }

  /**
   * The page of a [[SchemaFacet]]. `rows` are the property's rows in the facet's class, or in every
   * class, already filtered to what the reader may see, and not yet narrowed to the value.
   *
   * `facet.value` comes from the URL, so it reaches the markup only as literal text. A link to a
   * value is built from `rows`, which is to say from what a page declared.
   */
  def schemaFacetPageContent(facet: SchemaFacet, rows: Seq[models.tables.CalculatedSchemaOrg]): String = {
    val clsTitle = facet.cls.map(EnglishCaseConverter.pascalCase2TitleCase)
    val propTitle = EnglishCaseConverter.camelCase2TitleCase(facet.prop)
    val title = (clsTitle.toSeq :+ propTitle).mkString(": ")
    val clsLink = facet.cls.map(c => s"""["schema:$c" ${EnglishCaseConverter.pascalCase2TitleCase(c)}]""")
    val breadcrumb = (clsLink.toSeq :+ s"""["schema:${facet.prop}" $propTitle]""").mkString(" / ")
    val noPages = "No page has this property."

    facet.value match {
      case None =>
        s"""= $title
           |$breadcrumb
           |
           |${if (rows.isEmpty) noPages else valueSections("==", rows)}
           |""".stripMargin

      case Some(v) =>
        val matching = rows.filter(_.value == v)
        val valuesInUse = rows.map(_.value).distinct.sorted
        val pages =
          if (facet.cls.isDefined) pageColumns(matching.map(_.page).distinct.sorted)
          else matching.groupBy(_.cls).toSeq.sortBy(_._1).map { case (cls, of) =>
            s"""== ["schema:$cls:${facet.prop}:$v" ${EnglishCaseConverter.pascalCase2TitleCase(cls)}]
               |${pageColumns(of.map(_.page).distinct.sorted)}
               |""".stripMargin
          }.mkString("\n")
        val body =
          if (matching.nonEmpty)
            s"""["$v" $v] · ["${facet.propertyName}" All values]
               |
               |$pages""".stripMargin
          else if (valuesInUse.isEmpty)
            noPages
          else
            s"""No page has this value. Values in use:
               |${valuesInUse.map(u => s""" * ["${facet.withValue(u)}" $u]""").mkString("\n")}""".stripMargin

        s"""= $title: ${logics.wikis.WikiSnippet.asLiteralText(v)}
           |$breadcrumb
           |
           |$body
           |""".stripMargin
    }
  }

  private def schemaPropertyPageContent(schema: String, schemaType: CalculatedSchemaOrg.SchemaType)(implicit wikiContext: ContextWikiPage, connection: Connection): String = {
    implicit val site: models.tables.Site = wikiContext.site
    val listSchemaOrg: List[models.tables.CalculatedSchemaOrg] = models.tables.CalculatedSchemaOrg.selectWhereProp(schema)
    val listSchemaOrgWithPermission = listSchemaOrg.filter(s => wikiContext.setPageNameByPermission.contains(s.page))

    s"""= ${EnglishCaseConverter.camelCase2TitleCase(schemaType.id)}
       |[[[#!Markdown
       |${schemaType.comment.replaceAll("\\\\n", "\n")}
       |]]]
       |${schemaSourceLine(schemaType)}
       |${listSchemaOrgWithPermission.groupBy(_.cls).toSeq.sortBy(_._1).map { case (cls, rows) =>
      s"""== ["schema:$cls:$schema" ${EnglishCaseConverter.pascalCase2TitleCase(cls)}]
         |${valueSections("===", rows)}
         |""".stripMargin
    }.mkString("\n")}
       |""".stripMargin
  }
}
