package logics

object SiteThemeLogic {
  val HeaderBackgroundColorKey: String = "site.theme.header.backgroundColor"
  val HeaderForegroundColorKey: String = "site.theme.header.foregroundColor"
  val FooterBackgroundColorKey: String = "site.theme.footer.backgroundColor"
  val FooterForegroundColorKey: String = "site.theme.footer.foregroundColor"

  private val CssHexColorRegex = "(?i)^#(?:[0-9a-f]{3}|[0-9a-f]{6}|[0-9a-f]{8})$".r

  def normalizeHexColor(v: String): Option[String] = {
    Option(v)
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap { trimmed =>
        CssHexColorRegex.findFirstIn(trimmed).map(_.toUpperCase)
      }
  }
}
