package logics

/**
 * The few words around a search hit, as plain text, for a JSON answer.
 *
 * The search page has its own summary, which marks the hit up for HTML. A caller reading JSON — an
 * agent deciding which of twenty pages to open — wants the words themselves, not markup it has to
 * strip, and not the whole page.
 */
object SearchSnippet {
  /**
   * `width` characters of context on each side of the first hit of `q` in `content`, or the start
   * of the page when the hit is only in the page's name. Runs of whitespace collapse to one space,
   * and an ellipsis marks each side that was cut.
   */
  def of(content: String, q: String, width: Int = 80): String = {
    val flat = content.replaceAll("""\s+""", " ").trim
    val needle = q.trim
    val at = if (needle.isEmpty) -1 else flat.toLowerCase.indexOf(needle.toLowerCase)
    if (at < 0) {
      if (flat.length <= width * 2) flat else flat.take(width * 2).trim + "…"
    } else {
      val from = (at - width).max(0)
      val to = (at + needle.length + width).min(flat.length)
      (if (from > 0) "…" else "") + flat.substring(from, to).trim + (if (to < flat.length) "…" else "")
    }
  }
}
