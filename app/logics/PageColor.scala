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
 * showing, so public/js/AhaWiki.Color.js decides that, against the background it lands on.
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
   * A page without a colour of its own takes the colour of the page it is part of (2026-11-16):
   * written once on Marvel Cinematic Universe, it reaches the thirty films that say `isPartOf` it,
   * the way Namuwiki colours a topic's documents through the one template they all include.
   *
   * One step only. A parent's colour counts when the parent wrote it, never when the parent
   * inherited it, so where a colour came from is always the `isPartOf` line in the page's own
   * infobox, and two pages naming each other cannot loop.
   */
  val InheritedThrough: Seq[String] = Seq("isPartOf", "partOfSeries")

  /** The pages a block names through [[InheritedThrough]]. */
  def parentsFromFields(fields: Seq[Seq[String]]): Seq[String] =
    fields.collect { case key +: values if InheritedThrough.contains(key) => values }.flatten.map(_.trim).filter(_.nonEmpty)

  /**
   * The colour of the first of `names` that has one, by name. Both the parents a page inherits
   * from and the classes it declares are picked this way: the order they were written in is not
   * kept in CalculatedSchemaOrg, and the infobox and the API have to agree.
   */
  def firstByName(names: Seq[String], colour: String => Option[String]): Option[String] =
    names.distinct.sorted.flatMap(colour(_)).headOption

  /**
   * The last layer (2026-11-16): a page that neither wrote a colour nor inherits one takes the
   * colour its site's admin chose for its schema class, stored as a Config row per class. A
   * topic's colour says «this series», a class's says «a film»; the topic wins where both apply.
   *
   * The class exactly as declared. Movie's colour does not fall back to CreativeWork's: many
   * classes have several parents (Hospital sits under three), and the class at the top of the
   * infobox should be the whole answer to where a colour came from.
   */
  val ClassKeyPrefix: String = "site.schema.color."

  def classKey(schemaClass: String): String = ClassKeyPrefix + schemaClass

  /** Class -> colour, from Config rows (key, value); rows that are not a valid colour are left out. */
  def classColours(rows: Seq[(String, String)]): Map[String, String] =
    rows.collect { case (key, value) if key.startsWith(ClassKeyPrefix) => key.stripPrefix(ClassKeyPrefix) -> parse(value) }
      .collect { case (schemaClass, Some(colour)) => schemaClass -> colour }
      .toMap
}
