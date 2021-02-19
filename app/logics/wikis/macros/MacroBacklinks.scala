package logics.wikis.macros

import logics.wikis.interpreters.InterpreterWiki
import models.ContextWikiPage

object MacroBacklinks extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    wikiContext.database.withConnection { implicit connection =>
      import com.aha00a.commons.Implicits._
      import models.tables.Link
      import models.tables.Site
      implicit val site: Site = wikiContext.site
      val listLink: List[Link] = Link.selectDst(wikiContext.name)
      val listLinkFiltered = listLink.filter(l => l.and(wikiContext.pageCanSee))
      val markup = listLinkFiltered
        .map(l => s""" 1. ["${l.src}" ${l.src}${l.alias.toOption.map(v => s"($v)").getOrElse("")}]""")
        .mkString("\n")
      markup
        .toOption
        .map(InterpreterWiki.toHtmlString)
        .map(s => s"""<div class="columnWidth350"><div>$s</div></div>""".stripMargin )
        .getOrElse("")
    }
  }
}
