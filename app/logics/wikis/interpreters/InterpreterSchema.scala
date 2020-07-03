package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.{DateTimeUtil, EnglishCaseConverter}
import logics.SchemaOrg
import logics.wikis.{PageNameLogic, RenderingMode}
import models.{AhaWikiQuery, PageContent, WikiContext}

object InterpreterSchema extends TraitInterpreter {

  import models.tables.Link

  case class ParseResult(schemaClass: String, seqSeqField: Seq[Seq[String]])

  def createPageContent(content: String): PageContent = {
    val pageContent: PageContent = PageContent(content)
    if (pageContent.interpreter.getOrElse("") != name)
      throw new Exception("pageContent.interpreter.getOrElse(\"\") != name")

    pageContent
  }

  def parse(pageContent: PageContent): ParseResult = {
    val schemaClass: String = pageContent.argument.headOption.getOrElse("")
    val contentLines: Seq[String] = pageContent.content.splitLinesSeq().filter(_.isNotNullOrEmpty)
    val seqSeqField: Seq[Seq[String]] = contentLines.map(_.splitTabsSeq().filter(_.isNotNullOrEmpty)).filter(_.nonEmpty)
    ParseResult(schemaClass, seqSeqField)
  }


  override def toHtmlString(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = createPageContent(content)
    val parseResult: ParseResult = parse(pageContent)

    val pageNameSet: Set[String] = wikiContext.setPageNameByPermission

    val seqPropertyUsed: Seq[String] = parseResult.seqSeqField.flatMap(_.headOption)
    val dl =
      <dl vocab="http://schema.org/" typeof={parseResult.schemaClass}>
        <h5>
          {
            SchemaOrg.getPathHierarchy(parseResult.schemaClass).map(seqClass => {
              scala.xml.XML.loadString(
                seqClass.map(c => SchemaOrg.mapClass.get(c)
                  .map(node => node.toLinkMarkup.toHtmlString(pageNameSet))
                  .getOrElse("")
                ).mkString("<div>", " / ", "</div>")
              )
            })
          }
        </h5>
        <div>
          {
            parseResult.seqSeqField.map { case key +: tail =>
              <div>
                <dt>
                  {
                    SchemaOrg.mapProperty.get(key).map(n => {
                      n.toXmlSpan()
                    }).getOrElse{
                      <span class="unknown" title="Unknown property">{EnglishCaseConverter.camelCase2TitleCase(key)}</span>
                    }
                  }
                </dt>
                {
                  tail.map {
                    case v if Seq("image", "logo").contains(key) =>
                      <dd property={key}><img src={v} alt={s"$v $key"}></img></dd>
                    case v if PageNameLogic.isExternal(v) =>
                      <dd property={key}><a href={v} target="_blank">{v}</a></dd>
                    case v =>
                      <dd property={key}><a href={v} class={if (pageNameSet.contains(v)) "" else "missing"}>{v}</a></dd>
                  }
                }
              </div>
            }
          }
        </div>
      </dl>
    wikiContext.renderingMode match {
      case RenderingMode.Normal =>
        val r = <div class="schema">{dl}</div>
        r.toString()
      case RenderingMode.Preview =>
        val recommendedProperties = if (parseResult.schemaClass.isNotNullOrEmpty){
          val listPropCount = wikiContext.database.withConnection { implicit connection =>
            import models.tables.SchemaOrg
            SchemaOrg.selectPropCountWhereCls(parseResult.schemaClass)
          }
          listPropCount.filterNot(pc => seqPropertyUsed.contains(pc.prop)).map(pc => s"${pc.prop}(${pc.cnt})").mkString(", ")
        } else {
          ""
        }
        val r =
          <div class="schema">
            {dl}
            <div class="preview info">
              <h6>Recommended Properties</h6>
              {recommendedProperties}
              <h6>Hierarchical Search</h6>
              {SchemaOrg.getHtmlTree(parseResult.schemaClass)}
              {
                if(SchemaOrg.mapClass.isDefinedAt(parseResult.schemaClass)) {
                  <div>{SchemaOrg.getHtmlProperties(parseResult.schemaClass, seqPropertyUsed)}</div>
                } else {

                }
              }
            </div>
          </div>
        r.toString()
    }
  }

  override def toSeqWord(content: String)(implicit wikiContext: WikiContext): Seq[String] = {
    val pageContent: PageContent = createPageContent(content)
    val parseResult: ParseResult = parse(pageContent)

    Seq("#!" + name, parseResult.schemaClass) ++ parseResult.seqSeqField.flatten.flatMap(_.split("""\s+"""))
  }

  override def toSeqLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = Seq()

  override def toSeqSchemaOrg(content: String)(implicit wikiContext: WikiContext): Seq[models.tables.SchemaOrg] = {
    import models.tables
    val pageContent: PageContent = createPageContent(content)
    val parseResult: ParseResult = parse(pageContent)

    val seqLinkProperty: Seq[tables.SchemaOrg] = parseResult.seqSeqField.flatMap {
      case key +: tail =>
        import models.tables
        tail.flatMap(DateTimeUtil.expand_ymd_to_ymd_ym_y_md_m_d).map(tables.SchemaOrg(wikiContext.name, parseResult.schemaClass, key, _))
    }
    tables.SchemaOrg(wikiContext.name, parseResult.schemaClass, "", "") +: seqLinkProperty
  }
}
