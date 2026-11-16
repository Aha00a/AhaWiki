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
 *
 * The colour is sent as written. Which shade of it is drawn depends on the theme the browser is
 * showing, so public/js/AhaWiki.PageColor.js decides that, against the background it lands on.
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
}
