package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.ContextWikiPage

/**
 * A button that lists the pages mentioning this page's name without linking to it.
 *
 * It draws only the button. Finding the pages scans the text of the whole wiki, as a search does,
 * and a page that did it on every view would make every view cost a search — so the click is what
 * asks (`GET /api/unlinkedMentions/<name>`, handled in js.js), and the page itself stays as cheap as
 * it was. Placed by hand anywhere, and by the generated Backlinks section on pages that have any.
 */
object MacroUnlinkedMentions extends TraitMacro {
  override def isBlock: Boolean = true

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val name = wikiContext.name
    s"""<div class="UnlinkedMentions" data-name="${name.escapeHtmlAttribute()}"><button type="button" class="unlinkedMentionsButton">Find unlinked mentions</button><div class="unlinkedMentionsResult"></div></div>"""
  }
}
