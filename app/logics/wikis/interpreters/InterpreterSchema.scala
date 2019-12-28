package logics.wikis.interpreters

import com.aha00a.commons.Implicits._
import logics.{AhaWikiCache, Schema}
import models.{PageContent, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database

object InterpreterSchema {
  def apply(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    val schemaClass = pageContent.argument.head
    val contentLines = pageContent.content.splitLinesSeq()
    implicit val cacheApi: CacheApi = wikiContext.cacheApi
    implicit val database: Database = wikiContext.database
    val pageNameSet: Set[String] = AhaWikiCache.PageNameSet.get()
    val dl =
      <dl vocab="http://schema.org/" typeof={schemaClass}>
        <h5>{schemaClass}</h5>
        {
          contentLines.map(l => {
            val values = l.splitTabsSeq()
            val key = values.head
            <dt>{key}</dt> ++ values.tail.map(v => {
              <dd property={key}>{
                if(pageNameSet.contains(v)) {
                  <a href={v}>{v}</a>
                } else {
                  <a href={v} class="missing">{v}</a>
                }
              }</dd>
            })
          })
        }
        <dt>Hierarchy</dt>
        <dd>
          {Schema.getHtmlTree(schemaClass)}
        </dd>
      </dl>

    if(wikiContext.isPreview) {
      val properties: Seq[String] = contentLines.flatMap(_.splitTabsSeq().headOption)
      val r =
        <div class="schema">
          {dl}
          <div class="preview">
            {
              if(Schema.mapClass.isDefinedAt(schemaClass)) {
                <div>
                  {
                    Schema.getHtmlProperties(schemaClass, properties)
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
}
