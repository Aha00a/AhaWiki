package logics.wikis

import com.aha00a.commons.utils.UriUtil

import java.net.URLDecoder

/**
 * How a page name becomes a URL segment, and how it comes back.
 *
 * Both directions were written out wherever they were needed — the decode in four places,
 * the encode in eight — and the encode had drifted into two forms. Some call sites used
 * `UriUtil.encodeURIComponent`, which leaves `!'()~` literal, and others percent-encoded
 * them. The same page therefore had two spellings of its own URL depending on which code
 * produced the link: a sitemap entry, a Telegram notification and a rendered wiki link could
 * disagree.
 *
 * The `encodeURIComponent` form wins because the browser side already uses the JavaScript
 * function of that name. Server-made links now match client-made ones. Both forms resolve to
 * the same page, so the difference was cosmetic — but "cosmetic and inconsistent" is what a
 * canonical URL cannot be.
 */
object PageNameUrl {

  /** A page name as a path segment: `/w/${encode(name)}`. */
  def encode(pageName: String): String = UriUtil.encodeURIComponent(pageName)

  /**
   * The inverse, for a name arriving from a route.
   *
   * `+` is escaped to `%2B` first. Without that, `URLDecoder` reads a literal `+` in a page
   * name as the space it means in a query string, and the page is looked up under the wrong
   * name.
   */
  def decode(nameEncoded: String): String =
    URLDecoder.decode(nameEncoded.replace("+", "%2B"), "UTF-8")
}
