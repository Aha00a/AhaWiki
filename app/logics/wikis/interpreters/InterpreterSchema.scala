package logics.wikis.interpreters

import logics.Schema
import logics.wikis.macros.MacroError
import models.{PageContent, WikiContext}
import com.aha00a.commons.Implicits._

object InterpreterSchema {
  def apply(pageContent: PageContent)(implicit wikiContext: WikiContext): String = {
    val schemaClass = pageContent.argument.head
    val contentLines = pageContent.content.splitLinesSeq()
    val dl =
      <dl vocab="http://schema.org/" typeof={schemaClass}>
        <h5>{schemaClass}</h5>
        {
          contentLines.map(l => {
            val values = l.splitTabsSeq()
            val key = values.head
            <dt>{key}</dt> ++ values.tail.map(v => {
              <dd property={key}>{v}</dd>
            })
          })
        }
      </dl>

    if(wikiContext.isPreview) {
      val properties: Seq[String] = contentLines.flatMap(_.splitTabsSeq().headOption)
      val r =
        <div class="schema">
          {dl}
          <div class="preview">
            <h5>Schema Hierarchy</h5>
            {Schema.getHtmlTree(schemaClass)}
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
