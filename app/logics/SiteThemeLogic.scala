package logics

object SiteThemeLogic {
  val DefaultHueKey: String = "site.theme.defaultHue"

  /**
   * The theme's hue when neither the site nor the reader chose one. An OKLCH hue, as every hue here
   * is since 2026-11-16: 263 is what the HSL default 220 it replaced is on OKLCH's circle, so the
   * default looks as it did. less.less writes it too, for the first paint before any script runs --
   * CSS cannot read this; test/theme.oklch-contrast.test.mjs checks the two agree.
   *
   * A Config row under DefaultHueKey written before then held an HSL hue. None existed in the
   * production database when the meaning changed, so none was converted.
   */
  val DefaultHue: Int = 263

  def parseHue(v: String): Option[Int] = {
    Option(v).map(_.trim).filter(_.nonEmpty).flatMap { s =>
      scala.util.Try(s.toInt).toOption.filter(h => h >= 0 && h <= 360)
    }
  }
}
