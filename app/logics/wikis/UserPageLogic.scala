package logics.wikis

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

object UserPageLogic {
  def pageName(nickname: String): String = s"User:$nickname"

  def wikiMarkup(nickname: String): String = s"""["${pageName(nickname)}" $nickname]"""

  def href(nickname: String): String =
    "/w/" + URLEncoder.encode(pageName(nickname), StandardCharsets.UTF_8.toString).replace("+", "%20")

  private def profileImageUrlByNickname(nickname: String)(implicit wikiContext: models.ContextWikiPage): Option[String] = {
    wikiContext.database.withConnection { implicit connection =>
      models.tables.User.selectByNickname(nickname).flatMap(_.profileImageUrl)
    }
  }

  def toHtmlString(nickname: String)(implicit wikiContext: models.ContextWikiPage): String =
    profileImageUrlByNickname(nickname)
      .map { imageUrl =>
        s"""<span class="userInlineProfile"><img src="$imageUrl" alt="$nickname" class="userInlineProfileImage"/><a href="${href(nickname)}">$nickname</a></span>"""
      }
      .getOrElse(logics.wikis.interpreters.InterpreterWiki.inlineToHtmlString(wikiMarkup(nickname)))
}
