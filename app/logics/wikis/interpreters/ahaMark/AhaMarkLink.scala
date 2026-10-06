package logics.wikis.interpreters.ahaMark

import com.aha00a.commons.Implicits._
import models.ContextWikiPage

case class AhaMarkLink(uri: String, alias: String = "", noFollow: Boolean = false)(implicit wikiContext: ContextWikiPage) extends AhaMark {

  import models.tables.CalculatedLink

  import scala.xml.Elem
  import scala.xml.XML

  lazy val uriNormalized: String = if (uri.startsWith("wiki:")) uri.substring(5) else uri
  lazy val aliasWithDefault: String = if (alias == null || alias.isEmpty) uriNormalized else alias

  import AhaMarkLink._

  private def toCountryFlagEmoji(alpha2Code: String): Option[String] = {
    val code = alpha2Code.trim
    if (!regexAlpha2.matches(code) || !iso3166Alpha2CodeSet.contains(code)) {
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
      // The page set is asked first: most links name a page that exists. `Regex.matches` is the
      // whole-string match `String.matches` was, without compiling the pattern again per link.
      val isMissing = !(
        set.isEmpty ||
        external ||
        isStartsWithHash ||
        isStartsWithQuestionMark ||
        set.contains(regexAnchorOrQuery.replaceAllIn(uriNormalized, "")) ||
        // DateTimeUtil.regexIsoLocalDate.matches(uriNormalized) ||
        DateTimeUtil.regexYearDashMonth.matches(uriNormalized) ||
        DateTimeUtil.regexDashDashDashDay.matches(uriNormalized) ||
        DateTimeUtil.regexYear.matches(uriNormalized) ||
        DateTimeUtil.regexDashDashMonthDashDay.matches(uriNormalized) ||
        DateTimeUtil.regexDashDashMonth.matches(uriNormalized) ||
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
      val rawDisplayText =
        if (isUserPage && (alias == null || alias.isEmpty)) {
          uriNormalized.stripPrefix("User:").trim
        } else if (isSchema && (alias == null || alias.isEmpty)) {
          uriNormalized.stripPrefix("schema:").trim
        } else {
          aliasWithDefault
        }
      val displayText = countryFlagEmoji.map(flag => s"$flag ${rawDisplayText.escapeHtml()}").getOrElse(rawDisplayText.escapeHtml())
      val linkHtml = s"""<a href="${href.escapeHtmlAttribute()}"$attrTarget$attrClass$attrRelMissing$attrRel>${displayText}</a>"""

      if (isUserPage) {
        val nickname = uriNormalized.stripPrefix("User:").trim
        val profileImageUrl = logics.wikis.UserPageLogic.profileImageUrlByNickname(nickname)

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

object AhaMarkLink {
  // Here rather than in each link: building the country set once per link was 6% of a render's
  // time (2026-10-07).
  private val iso3166Alpha2CodeSet: Set[String] = java.util.Locale.getISOCountries.toSet
  private val regexAlpha2 = """[A-Z]{2}""".r
  private val regexAnchorOrQuery = """[#?].+$""".r
}
