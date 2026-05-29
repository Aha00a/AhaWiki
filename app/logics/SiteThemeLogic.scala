package logics

object SiteThemeLogic {
  val HeaderBackgroundColorKey: String = "site.theme.header.backgroundColor"
  val HeaderForegroundColorKey: String = "site.theme.header.foregroundColor"
  val BodyBackgroundColorKey: String = "site.theme.body.backgroundColor"
  val BodyForegroundColorKey: String = "site.theme.body.foregroundColor"
  val FooterBackgroundColorKey: String = "site.theme.footer.backgroundColor"
  val FooterForegroundColorKey: String = "site.theme.footer.foregroundColor"
  val DefaultHueKey: String = "site.theme.defaultHue"

  private val CssHexColorRegex = "(?i)^#(?:[0-9a-f]{3}|[0-9a-f]{6}|[0-9a-f]{8})$".r

  def normalizeHexColor(v: String): Option[String] = {
    Option(v)
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap { trimmed =>
        CssHexColorRegex.findFirstIn(trimmed).map(_.toUpperCase)
      }
  }

  def parseHue(v: String): Option[Int] = {
    Option(v).map(_.trim).filter(_.nonEmpty).flatMap { s =>
      scala.util.Try(s.toInt).toOption.filter(h => h >= 0 && h <= 360)
    }
  }
}
