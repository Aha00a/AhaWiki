package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.{DateTimeUtil, EnglishCaseConverter}
import logics.wikis.PageNameLogic
import logics.{AhaWikiCache, SchemaOrg}
import models.{Link, PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database

import scala.xml.{Elem, NodeSeq}

object InterpreterSchema extends TraitInterpreter {
  def apply(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    implicit val cacheApi: CacheApi = wikiContext.cacheApi
    implicit val database: Database = wikiContext.database
    val pageNameSet: Set[String] = AhaWikiCache.PageNameSet.get()

    val schemaClass = pageContent.argument.headOption.getOrElse("")
    val contentLines = pageContent.content.splitLinesSeq().filter(_.isNotNullOrEmpty)
    val seqSeqField: Seq[Seq[String]] = contentLines.map(_.splitTabsSeq().filter(_.isNotNullOrEmpty))
    val seqPropertyUsed: Seq[String] = seqSeqField.flatMap(_.headOption)
    val dl =
      <dl vocab="http://schema.org/" typeof={schemaClass}>
        <h5>{EnglishCaseConverter.pascalCase2TitleCase(schemaClass)}</h5>
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
              tail.map { v =>
                if(PageNameLogic.isExternal(v)) {
                  <a href={v} target="_blank">{v}</a><span> </span>
                } else {
                  <a href={v} class={if (pageNameSet.contains(v)) "" else "missing"}>{v}</a><span> </span>
                }
              }
            }
            </dd>
          }
        }
        <dt>Hierarchy</dt>
        {
          SchemaOrg.getPathHierarchy(schemaClass).map(v => {
            {
              scala.xml.XML.loadString(v.map(w =>(SchemaOrg.mapClass(w).toLinkMarkup().toRegexReplacement(pageNameSet))).mkString("<dd>", " / ", "</dd>"))
            }
          })
        }
      </dl>

    if(wikiContext.isPreview) {
      val r =
        <div class="schema">
          {dl}
          <div class="preview info">
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
    } else {
      val r = <div class="schema">
        {dl}
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
    val seqSeqField: Seq[Seq[String]] = contentLines.map(_.splitTabsSeq().filter(_.isNotNullOrEmpty))
    val seqLinkProperty: Seq[Link] = seqSeqField.flatMap { case key +: tail => tail.flatMap(DateTimeUtil.expand_ymd_to_ymd_y_ym_md).map(Link(wikiContext.name, _, SchemaOrg.withNameSpace(s"${schemaClass}:${key}"))) }
    linkSchema +: seqLinkProperty
  }
}
