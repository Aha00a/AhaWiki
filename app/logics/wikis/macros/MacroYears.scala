package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroYears extends TraitMacro {
  override def toHtmlString(argument:String)(implicit wikiContext: WikiContext): String = { wikiContext.database.withConnection { implicit connection =>
    import com.aha00a.commons.Implicits._
    import models.tables.Link

    val seqDst: Seq[String] = Link.selectDistinctDstWhereDstIsYear()
    val markup = seqDst.map(n => s"[$n]").mkString(", ")
    InterpreterWiki.toHtmlString(markup)
  }}
}
