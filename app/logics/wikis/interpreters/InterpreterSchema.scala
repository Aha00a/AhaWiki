package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import logics.{AhaWikiCache, Schema}
import models.{Link, PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database

object InterpreterSchema {
  def apply(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    implicit val cacheApi: CacheApi = wikiContext.cacheApi
    implicit val database: Database = wikiContext.database
    val pageNameSet: Set[String] = AhaWikiCache.PageNameSet.get()

    val schemaClass = pageContent.argument.headOption.getOrElse("")
    val contentLines = pageContent.content.splitLinesSeq().filter(_.isNotNullOrEmpty)
    val seqSeqField: Seq[Seq[String]] = contentLines.map(_.splitTabsSeq().filter(_.isNotNullOrEmpty))
    val seqPropertiesUsed: Seq[String] = seqSeqField.flatMap(_.headOption)
    val dl =
      <dl vocab="http://schema.org/" typeof={schemaClass}>
        <h5>{schemaClass}</h5>
        {
          seqSeqField.map { case key +: tail =>
            <dt>{key}</dt> ++ tail.map { v =>
              <dd property={key}>
                <a href={v} class={if (pageNameSet.contains(v)) "" else "missing"}>{v}</a>
              </dd>
            }
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
                    Schema.getHtmlProperties(schemaClass, seqPropertiesUsed)
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
  def extractLink(pageContent: PageContent)(implicit wikiContext: WikiContext): Seq[Link] = {
    val schemaClass = pageContent.argument.head
    val contentLines = pageContent.content.splitLinesSeq()
    val fields: Seq[Seq[String]] = contentLines.map(_.splitTabsSeq())
    fields.flatMap(values => {
      val key = values.head
      values.tail.map(v => {
        Link(key, v, key)
      })
    })
  }
}
