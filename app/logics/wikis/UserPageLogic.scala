package logics.wikis

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

object UserPageLogic {
  def pageName(nickname: String): String = s"User:$nickname"

  def wikiMarkup(nickname: String): String = s"""["${pageName(nickname)}" $nickname]"""

  def href(nickname: String): String =
    "/w/" + URLEncoder.encode(pageName(nickname), StandardCharsets.UTF_8.toString).replace("+", "%20")

  def toHtmlString(nickname: String)(implicit wikiContext: models.ContextWikiPage): String =
    logics.wikis.interpreters.InterpreterWiki.inlineToHtmlString(wikiMarkup(nickname))
}
