package logics.wikis

/** `@id`s more than one writer of a page's JSON-LD has to agree on. */
object JsonLdId {
  /** The site, as the Organization the page-level JSON-LD in view.scala.html declares. */
  def organization(host: String): String = s"https://$host/#organization"
}
