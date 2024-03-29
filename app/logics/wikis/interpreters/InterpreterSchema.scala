package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.DateTimeUtil
import com.aha00a.commons.utils.EnglishCaseConverter
import logics.wikis.PageNameLogic
import logics.wikis.RenderingMode
import models.ContextWikiPage
import models.PageContent

object InterpreterSchema extends TraitInterpreter {

  import models.tables.Link
  import models.tables.SchemaOrg

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


  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    import models.tables.Site
    implicit val site: Site = wikiContext.site
    val pageContent: PageContent = createPageContent(content)
    val parseResult: ParseResult = parse(pageContent)

    val pageNameSet: Set[String] = wikiContext.setPageNameByPermission

    val seqPropertyUsed: Seq[String] = parseResult.seqSeqField.flatMap(_.headOption)
//        <h5>
//          {
//            logics.SchemaOrg.getPathHierarchy(parseResult.schemaClass).map(seqClass => {
//              scala.xml.XML.loadString(
//                seqClass.map(c => logics.SchemaOrg.mapClass.get(c)
//                  .map(schemaType => schemaType.toAhaMarkLink.toHtmlString(pageNameSet))
//                  .getOrElse("")
//                ).mkString("<div>", " / ", "</div>")
//              )
//            })
//          }
//        </h5>

    val dl =
      <dl vocab="http://schema.org/" typeof={parseResult.schemaClass}>
        <h5>
          {scala.xml.XML.loadString(
            logics.SchemaOrg.getSchemaClass(parseResult.schemaClass).toAhaMarkLink.toHtmlString(pageNameSet)
          )}
        </h5>
        <div>
          {
            parseResult.seqSeqField.map { case key +: tail =>
              <div>
                <dt>
                  {
                  logics.SchemaOrg.mapProperty.get(key).map(n => {
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
                      <dd property={key}><a href={v.escapeHtml()} target="_blank" rel="noopener">{v}</a></dd>
                    case v =>
                      <dd property={key}><a
                        href={v.escapeHtml()}
                        class={if (pageNameSet.contains(v)) "" else "missing"}
                        rel={if (pageNameSet.contains(v)) "" else "nofollow"}
                      >{v}</a></dd>
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
              {logics.SchemaOrg.getHtmlTree(parseResult.schemaClass)}
              {
                if(logics.SchemaOrg.mapClass.isDefinedAt(parseResult.schemaClass)) {
                  <div>{logics.SchemaOrg.getHtmlProperties(parseResult.schemaClass, seqPropertyUsed)}</div>
                } else {

                }
              }
            </div>
          </div>
        r.toString()
    }
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = Seq()

  override def toSeqSchemaOrg(content: String)(implicit wikiContext: ContextWikiPage): Seq[SchemaOrg] = {
    val pageContent: PageContent = createPageContent(content)
    val parseResult: ParseResult = parse(pageContent)

    val seqLinkProperty: Seq[SchemaOrg] = parseResult.seqSeqField.flatMap {
      case key +: tail =>
        tail.flatMap(DateTimeUtil.expand_ymd_to_ymd_ym_y_md).map(SchemaOrg(wikiContext.name, parseResult.schemaClass, key, _))
    }
    SchemaOrg(wikiContext.name, parseResult.schemaClass, "", "") +: seqLinkProperty
  }
}
