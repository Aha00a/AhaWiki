package logics.wikis.interpreters.ahaMark

import com.aha00a.commons.Implicits._
import models.ContextWikiPage

case class AhaMarkLink(uri: String, alias: String = "", noFollow: Boolean = false)(implicit wikiContext: ContextWikiPage) extends AhaMark {

  import models.tables.CalculatedLink

  import scala.xml.Elem
  import scala.xml.XML

  lazy val uriNormalized: String = if (uri.startsWith("wiki:")) uri.substring(5) else uri
  lazy val aliasWithDefault: String = if (alias == null || alias.isEmpty) uriNormalized else alias

  private val iso3166Alpha2CodeSet: Set[String] = java.util.Locale.getISOCountries.toSet

  private def toCountryFlagEmoji(alpha2Code: String): Option[String] = {
    val code = alpha2Code.trim
    if (!code.matches("^[A-Z]{2}$") || !iso3166Alpha2CodeSet.contains(code)) {
      None
    } else {
      Some(code.map(ch => Character.toChars(0x1F1E6 + (ch - 'A')).mkString).mkString)
    }
  }

  def toHtmlString(set: Set[String] = Set[String]()): String = {
    if (wikiContext.name == uri) {
      s"""<b>$aliasWithDefault</b>"""
    } else {
      import com.aha00a.commons.utils.DateTimeUtil
      import logics.DefaultPageLogic
      import logics.wikis.PageNameLogic
      val external: Boolean = PageNameLogic.isExternal(uri)
      val isStartsWithHash = uriNormalized.startsWith("#")
      val isStartsWithQuestionMark = uriNormalized.startsWith("?")
      val isSchema = uriNormalized.startsWith("schema:")
      val schemaTypeClass = if (isSchema) {
        uriNormalized.stripPrefix("schema:").trim match {
          case "" => None
          case schemaType =>
            Some(
              "schema-" + schemaType
                .replaceAll("([a-z0-9])([A-Z])", "$1-$2")
                .replaceAll("[^A-Za-z0-9]+", "-")
                .toLowerCase
            )
        }
      } else {
        None
      }
      val href: String = if (external || isStartsWithHash || isStartsWithQuestionMark) uriNormalized else s"/w/$uriNormalized"
      val attrTarget: String = if (external) """ target="_blank" rel="noopener"""" else ""
      val isMissing = !(
        set.isEmpty ||
        external ||
        isStartsWithHash ||
        isStartsWithQuestionMark ||
        // uriNormalized.matches(DateTimeUtil.regexIsoLocalDate.pattern.pattern()) ||
        uriNormalized.matches(DateTimeUtil.regexYearDashMonth.pattern.pattern()) ||
        uriNormalized.matches(DateTimeUtil.regexDashDashDashDay.pattern.pattern()) ||
        uriNormalized.matches(DateTimeUtil.regexYear.pattern.pattern()) ||
        uriNormalized.matches(DateTimeUtil.regexDashDashMonthDashDay.pattern.pattern()) ||
        uriNormalized.matches(DateTimeUtil.regexDashDashMonth.pattern.pattern()) ||
        set.contains(uriNormalized.replaceAll("""[#?].+$""", "")) ||
        DefaultPageLogic.isDefined(uriNormalized)
      )
      val countryFlagEmoji = toCountryFlagEmoji(uriNormalized)
      val classList = Seq(
        if (isSchema) Some("schema") else None,
        if (isSchema) Some("schema-link") else None,
        schemaTypeClass,
        if (countryFlagEmoji.isDefined) Some("iso3166-alpha2-link") else None,
        if (isMissing) Some("missing") else None,
      ).flatten
      val attrClass = if (classList.nonEmpty) s""" class="${classList.mkString(" ")}"""" else ""
      val attrRelMissing = if (isMissing) """ rel="nofollow"""" else ""
      val attrRel = if(noFollow) """ rel="nofollow"""" else ""
      val isUserPage = !external && uriNormalized.startsWith("User:")
      val rawDisplayText = if (isUserPage && (alias == null || alias.isEmpty)) uriNormalized.stripPrefix("User:").trim else aliasWithDefault
      val displayText = countryFlagEmoji.map(flag => s"$flag ${rawDisplayText.escapeHtml()}").getOrElse(rawDisplayText.escapeHtml())
      val linkHtml = s"""<a href="${href.escapeHtmlAttribute()}"$attrTarget$attrClass$attrRelMissing$attrRel>${displayText}</a>"""

      if (isUserPage) {
        val nickname = uriNormalized.stripPrefix("User:").trim
        implicit val userProfileImageCacheKey: (play.api.db.Database, models.tables.Site, String) = (wikiContext.database, wikiContext.site, nickname)
        val profileImageUrl = wikiContext.ahaWikiCache.UserProfileImageUrl.get()

        profileImageUrl
          .map { imageUrl =>
            s"""<span class="userInlineProfile"><a href="${href.escapeHtmlAttribute()}"$attrTarget$attrClass$attrRelMissing$attrRel><img src="${imageUrl.escapeHtmlAttribute()}" alt="${nickname.escapeHtmlAttribute()}" class="userInlineProfileImage"/>${displayText}</a></span>"""
          }
          .getOrElse(linkHtml)
      } else {
        linkHtml
      }
    }
  }

  def toLink(src: String): CalculatedLink = CalculatedLink(src, uriNormalized, alias)

  override def toHtml: Elem = XML.loadString(toHtmlString())
}
