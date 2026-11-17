package logics

/**
 * A page's own colour, written in its `#!Schema` block as `themeColor` (2026-11-16) -- what
 * Namuwiki does with a colour per topic. The infobox, the adjacent-pages graph, the link preview
 * and a mobile browser's address bar (`<meta name="theme-color">`) use it.
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
   * A page's colour from the three layers, in order: its own, its parents' own, its classes'. The
   * one place the order is written; the infobox and CalculatedSchemaOrg.selectPageColorMap only
   * gather what it takes. `parentColour` is not called when the page has its own colour.
   */
  def resolve(own: Option[String], parents: Seq[String], parentColour: String => Option[String], classes: Seq[String]): Option[String] =
    own.orElse(firstByName(parents, parentColour)).orElse(firstByName(classes, ClassColours.get))

  /**
   * The last layer (2026-11-16): a page that neither wrote a colour nor inherits one takes its
   * schema class's colour. A topic's colour says «this series», a class's says «a film»; the topic
   * wins where both apply.
   *
   * Fixed here, the same on every site. A first version let each site's admin choose them on the
   * admin screen; the owner preferred one set that needs no choosing.
   *
   * Thirteen groups of related classes, their hues evenly spaced around OKLCH's circle from 25
   * degrees (25 + i * 360 / 13), all at lightness 65% and chroma 0.15 -- a lightness that stands at
   * about 3:1 on both themes' backgrounds, so AhaWiki.Color.js draws them nearly as written.
   * Neighbouring groups are related kinds (film beside television), so the colours that sit close
   * on the circle are the ones that may.
   *
   * The class exactly as declared. Movie's colour does not fall back to CreativeWork's: many
   * classes have several parents (Hospital sits under three), and the class at the top of the
   * infobox should be the whole answer to where a colour came from. The broadest classes (Thing,
   * CreativeWork, Intangible, Rating) have none: a colour shared by half the wiki tells nothing.
   */
  val ClassColours: Map[String, String] = Seq(
    "#dc655f" -> Seq("Movie", "MovieSeries"),
    "#d47125" -> Seq("TVSeries", "TVSeason", "TVEpisode", "Episode", "CreativeWorkSeason"),
    "#be8200" -> Seq("ComicSeries"),
    "#999500" -> Seq("Book", "BookSeries", "Periodical"),
    "#63a33e" -> Seq("Article", "TechArticle", "BlogPosting", "NewsArticle", "SocialMediaPosting", "HowTo", "Recipe"),
    "#00ab75" -> Seq("SoftwareApplication", "WebApplication", "SoftwareSourceCode", "WebSite", "ComputerLanguage"),
    "#00aaa5" -> Seq("Game", "VideoGame"),
    "#00a3cc" -> Seq("Person"),
    "#2d95e4" -> Seq("Organization", "Corporation", "NGO", "EducationalOrganization", "CollegeOrUniversity", "GovernmentOffice", "PoliticalParty", "PerformingGroup", "MusicGroup"),
    "#7485e9" -> Seq("Place", "City", "Country", "AdministrativeArea", "CivicStructure", "StadiumOrArena", "PerformingArtsTheater"),
    "#a176da" -> Seq("LocalBusiness", "FoodEstablishment", "Restaurant", "CafeOrCoffeeShop", "FinancialService"),
    "#c06abb" -> Seq("MedicalEntity", "MedicalCondition", "AnatomicalStructure", "Drug", "Substance", "Hospital", "MedicalOrganization"),
    "#d56390" -> Seq("Product"),
  ).flatMap { case (colour, classes) => classes.map(_ -> colour) }.toMap
}
