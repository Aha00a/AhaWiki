package logics.wikis

import com.aha00a.commons.Implicits._

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

object UserPageLogic {
  def pageName(nickname: String): String = s"User:$nickname"

  def wikiMarkup(nickname: String): String = s"""["${pageName(nickname)}" $nickname]"""

  def href(nickname: String): String =
    "/w/" + URLEncoder.encode(pageName(nickname), StandardCharsets.UTF_8.toString).replace("+", "%20")

  private def profileImageUrlByNickname(nickname: String)(implicit wikiContext: models.ContextWikiPage): Option[String] = {
    implicit val userProfileImageCacheKey: (play.api.db.Database, models.tables.Site, String) = (wikiContext.database, wikiContext.site, nickname)
    wikiContext.ahaWikiCache.UserProfileImageUrl.get()
  }

  def toHtmlString(nickname: String)(implicit wikiContext: models.ContextWikiPage): String =
    profileImageUrlByNickname(nickname)
      .map { imageUrl =>
        s"""<span class="userInlineProfile"><img src="${imageUrl.escapeHtmlAttribute()}" alt="${nickname.escapeHtmlAttribute()}" class="userInlineProfileImage"/><a href="${href(nickname).escapeHtmlAttribute()}">${nickname.escapeHtml()}</a></span>"""
      }
      .getOrElse(logics.wikis.interpreters.InterpreterWiki.inlineToHtmlString(wikiMarkup(nickname)))
}
