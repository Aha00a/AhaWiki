package logics.wikis.macros

import models.ContextWikiPage

object MacroCopyable extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    doToHtmlString(argument)
  }

  def doToHtmlString(argument: String): String = {
    <div class="MacroCopyable">
      <input class="auto resizeInputToContent" value={argument} readonly="readonly"/>
      <button type="button" aria-label="Copy" onclick={s"window.AhaWiki.Clipboard.copy('$argument')"}>
        <i class="far fa-copy" aria-hidden="true"></i>
      </button>
    </div>.toString()
  }
}
