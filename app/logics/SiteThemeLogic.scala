package logics

object SiteThemeLogic {
  val DefaultHueKey: String = "site.theme.defaultHue"

  def parseHue(v: String): Option[Int] = {
    Option(v).map(_.trim).filter(_.nonEmpty).flatMap { s =>
      scala.util.Try(s.toInt).toOption.filter(h => h >= 0 && h <= 360)
    }
  }
}
