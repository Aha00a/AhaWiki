package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.WikiContext

object MacroBacklinks extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: WikiContext): String = {
    wikiContext.database.withConnection { implicit connection =>
      import com.aha00a.commons.Implicits._
      import models.tables.Link
      val listLink: List[Link] = Link.selectDst(wikiContext.name)
      val listLinkFiltered = listLink.filter(l => l.and(wikiContext.pageCanSee))
      val markup = listLinkFiltered
        .map(l => s""" * ["${l.src}" ${l.src}${l.alias.toOption.map(v => s"($v)").getOrElse("")}]""")
        .mkString("\n")
      markup
        .toOption
        .map(InterpreterWiki.toHtmlString)
        .map(s => s"""<div class="columnWidth350"><div>$s</div></div>""".stripMargin )
        .getOrElse("")
    }
  }
}
