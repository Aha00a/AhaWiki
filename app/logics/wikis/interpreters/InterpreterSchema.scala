package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.{DateTimeUtil, EnglishCaseConverter}
import logics.SchemaOrg
import logics.wikis.{PageNameLogic, RenderingMode}
import models.{AhaWikiQuery, Link, PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database
import play.api.mvc.Request

object InterpreterSchema extends TraitInterpreter {
  override def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    implicit val request: Request[Any] = wikiContext.request
    implicit val cacheApi: CacheApi = wikiContext.cacheApi
    implicit val database: Database = wikiContext.database

    val pageContent: PageContent = PageContent(content)
    val pageNameSet: Set[String] = wikiContext.setPageNameByPermission

    val schemaClass = pageContent.argument.headOption.getOrElse("")
    val contentLines = pageContent.content.splitLinesSeq().filter(_.isNotNullOrEmpty)
    val seqSeqField: Seq[Seq[String]] = contentLines.map(_.splitTabsSeq().filter(_.isNotNullOrEmpty))
    val seqPropertyUsed: Seq[String] = seqSeqField.flatMap(_.headOption)
    val dl =
      <dl vocab="http://schema.org/" typeof={schemaClass}>
        <h5>
          {
            SchemaOrg.getPathHierarchy(schemaClass).map(seqClass => {
              scala.xml.XML.loadString(
                seqClass.map(c => SchemaOrg.mapClass.get(c)
                  .map(node => node.toLinkMarkup.toHtmlString(pageNameSet))
                  .getOrElse("")
                ).mkString("<div>", " / ", "</div>")
              )
            })
          }
        </h5>
        {
          seqSeqField.map { case key +: tail =>
            <dt>
              {
                SchemaOrg.mapProperty.get(key).map(n => {
                  n.toXmlSpan()
                }).getOrElse{
                  <span class="unknown" title="Unknown property">{EnglishCaseConverter.camelCase2TitleCase(key)}</span>
                }
              }
            </dt>
            <dd property={key}>
            {
              tail.map {
                case v if Seq("image", "logo").contains(key) =>
                  <img src={v} alt={s"${v} ${key}"}></img><span> </span>
                case v if PageNameLogic.isExternal(v) =>
                  <a href={v} target="_blank">{v}</a><span> </span>
                case v =>
                  <a href={v} class={if (pageNameSet.contains(v)) "" else "missing"}>{v}</a><span> </span>
              }
            }
            </dd>
          }
        }
      </dl>
    wikiContext.renderingMode match {
      case RenderingMode.Normal =>
        val r = <div class="schema">{dl}</div>
        r.toString()
      case RenderingMode.Preview =>
        val recommendedProperties = if(schemaClass.isNotNullOrEmpty){
          val listPropCount = wikiContext.database.withConnection { implicit connection =>
            AhaWikiQuery().SchemaOrg.selectPropCountWhereCls(schemaClass)
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
              {SchemaOrg.getHtmlTree(schemaClass)}
              {
                if(SchemaOrg.mapClass.isDefinedAt(schemaClass)) {
                  <div>{SchemaOrg.getHtmlProperties(schemaClass, seqPropertyUsed)}</div>
                } else {

                }
              }
            </div>
          </div>
        r.toString()
    }
  }

  override def extractLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = {
    val pageContent = PageContent(content)
    if(pageContent.interpreter.getOrElse("") != name)
      throw new Exception("pageContent.interpreter.getOrElse(\"\") != name")

    val schemaClass: String = pageContent.argument.head
    val contentLines: Seq[String] = pageContent.content.splitLinesSeq()
    val linkSchema: Link = Link(wikiContext.name, SchemaOrg.withNameSpace(schemaClass), SchemaOrg.withNameSpace("Schema"))
    val seqSeqField: Seq[Seq[String]] = contentLines.map(_.splitTabsSeq().filter(_.isNotNullOrEmpty)).filter(_.nonEmpty)
    val seqLinkProperty: Seq[Link] = seqSeqField.flatMap { case key +: tail => tail.flatMap(DateTimeUtil.expand_ymd_to_ymd_ym_y_md_m_d).map(Link(wikiContext.name, _, SchemaOrg.withNameSpace(s"$schemaClass:$key"))) }
    linkSchema +: seqLinkProperty
  }

  override def extractSchema(content: String)(implicit wikiContext: WikiContext): Seq[models.SchemaOrg] = {
    val pageContent = PageContent(content)
    if(pageContent.interpreter.getOrElse("") != name)
      throw new Exception("pageContent.interpreter.getOrElse(\"\") != name")

    val schemaClass: String = pageContent.argument.head
    val contentLines: Seq[String] = pageContent.content.splitLinesSeq()
    val linkSchema: models.SchemaOrg = models.SchemaOrg(wikiContext.name, schemaClass, "", "")
    val seqSeqField: Seq[Seq[String]] = contentLines.map(_.splitTabsSeq().filter(_.isNotNullOrEmpty)).filter(_.nonEmpty)
    val seqLinkProperty: Seq[models.SchemaOrg] = seqSeqField.flatMap {
      case key +: tail =>
        tail.flatMap(DateTimeUtil.expand_ymd_to_ymd_ym_y_md_m_d).map(models.SchemaOrg(wikiContext.name, schemaClass, key, _))
    }
    linkSchema +: seqLinkProperty
  }
}
