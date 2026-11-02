package logics.wikis

/** A page's image (`PageMeta.image`) as a full address. */
object PageImageUrl {
  private val SvgAddress = """(?i)\.svg(?:[?#].*)?$""".r

  /** `value` is a full URL, a protocol-relative one, a path, or a name relative to the site root. */
  def absolute(value: String, scheme: String, host: String): String =
    if (value.startsWith("http://") || value.startsWith("https://")) value
    else if (value.startsWith("//")) s"$scheme:$value"
    else if (value.startsWith("/")) s"$scheme://$host$value"
    else s"$scheme://$host/$value"

  /** What a page may name as its social preview. Not an `attachment:`: its address is presigned, so
    * it would expire and put a signature in the page's HTML. Not an SVG: most preview renderers skip
    * it, which is why the old `attachmentFallback.svg` was never seen. */
  def forSocialPreview(raw: String, host: String): Option[String] =
    Option(raw).map(_.trim)
      .filter(value => value.nonEmpty && !value.startsWith("attachment:"))
      .filter(value => SvgAddress.findFirstIn(value).isEmpty)
      .map(value => absolute(value, "https", host))
}
