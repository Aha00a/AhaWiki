package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.EnglishCaseConverter
import logics.wikis.PageNameLogic
import logics.{AhaWikiCache, Schema}
import models.{Link, PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database

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
        <h5>{schemaClass}</h5>
        {
          seqSeqField.map { case key +: tail =>
            <dt>{EnglishCaseConverter.camelCase2TitleCase(key)}</dt>
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
        <dd>
          {Schema.getHtmlTree(schemaClass)}
        </dd>
      </dl>

    if(wikiContext.isPreview) {
      val r =
        <div class="schema">
          {dl}
          <div class="preview">
            {
              if(Schema.mapClass.isDefinedAt(schemaClass)) {
                <div>
                  {
                    Schema.getHtmlProperties(schemaClass, seqPropertyUsed)
                  }
                </div>
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

    val schemaClass = pageContent.argument.head
    val contentLines = pageContent.content.splitLinesSeq()
    val seqSchema = Link(wikiContext.name, Schema.withNameSpace(schemaClass), Schema.withNameSpace("Schema"))
    val seqSeqField: Seq[Seq[String]] = contentLines.map(_.splitTabsSeq().filter(_.isNotNullOrEmpty))
    val seqLinkProperty: Seq[Link] = seqSeqField.flatMap { case key +: tail => tail.map(Link(wikiContext.name, _, Schema.withNameSpace(key))) }
    seqSchema +: seqLinkProperty
  }
}
