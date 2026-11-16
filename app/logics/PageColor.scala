package logics

/**
 * A page's own colour, written in its `#!Schema` block as `themeColor` (2026-11-16) -- what
 * Namuwiki does with a colour per topic. The infobox, the adjacent-pages graph and the link preview
 * use it.
 *
 * Not a schema.org property: schema.org has no colour for a Thing, and `color` is a Product's colour
 * ("red"), which on a product page would mean something else. So it is a key the wiki knows, like
 * `image`, kept out of the JSON-LD and out of the vocabulary.
 *
 * Only `#rgb` and `#rrggbb` are taken. Whatever passes is the only thing that reaches a style
 * attribute or a script, so a value cannot carry anything else into the page.
 */
object PageColor {
  val Key: String = "themeColor"

  private val short = """#([0-9a-fA-F])([0-9a-fA-F])([0-9a-fA-F])""".r
  private val long = """#([0-9a-fA-F]{6})""".r

  /** `#RGB` or `#rrggbb`, as lower-case `#rrggbb`; anything else is None. */
  def parse(raw: String): Option[String] = raw.trim match {
    case short(r, g, b) => Some(s"#$r$r$g$g$b$b".toLowerCase)
    case long(hex) => Some(s"#$hex".toLowerCase)
    case _ => None
  }

  /** The first valid `themeColor` among a block's fields (key +: values). */
  def fromFields(fields: Seq[Seq[String]]): Option[String] =
    fields.collect { case Key +: values => values }.flatten.flatMap(parse).headOption

  /**
   * The hue, 0 to 359, of a parsed colour. Large surfaces take only this and draw it the way the
   * site draws its own `--hue`, so text on them reads in either theme whatever colour was chosen.
   */
  def hue(color: String): Int = {
    val v = Integer.parseInt(color.stripPrefix("#"), 16)
    val (r, g, b) = (((v >> 16) & 0xff) / 255.0, ((v >> 8) & 0xff) / 255.0, (v & 0xff) / 255.0)
    val max = Seq(r, g, b).max
    val min = Seq(r, g, b).min
    val d = max - min
    if (d == 0) 0
    else {
      val h =
        if (max == r) ((g - b) / d) % 6
        else if (max == g) (b - r) / d + 2
        else (r - g) / d + 4
      ((math.round(h * 60).toInt % 360) + 360) % 360
    }
  }
}
