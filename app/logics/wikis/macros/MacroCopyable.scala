package logics.wikis.macros

import models.ContextWikiPage

object MacroCopyable extends TraitMacro {
  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    doToHtmlString(argument)
  }

  def doToHtmlString(argument: String): String = {
    val escapedArgument = argument.replace("&", "&amp;").replace("\"", "&quot;")
    s"""<span class="MacroCopyable"><input class="auto resizeInputToContent" value="$escapedArgument" readonly="readonly"/><button type="button" aria-label="Copy" onclick="window.AhaWiki.Clipboard.copy(this.previousElementSibling.value);$$(this).next().fadeIn();setTimeout(() =&gt; $$(this).next().fadeOut(), 1000);"><i class="far fa-copy" aria-hidden="true"></i></button><span class="tooltip" onclick="$$(this).fadeOut();">Copied!!</span></span>"""
  }
}
