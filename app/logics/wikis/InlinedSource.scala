package logics.wikis

/**
 * Where a line of an assembled document came from.
 *
 * A month page is not a page. `InlineDays` builds one by splicing that month's day pages
 * together and rendering the result, and until now the sections of that result claimed to be
 * editable at `/w/2020-01?revision=0&lineStart=101` — a page that does not exist, at a line
 * number into a document nobody can open. Recording where each assembled line came from lets
 * the edit control name the day page and the line within it instead.
 *
 * @param page      the page the line was taken from
 * @param revision  that page's revision at the time it was read, so the editor can detect that
 *                  it moved underneath
 * @param line      the line's position in that page, counting its directives
 * @param lineCount how long that page is, for a section that runs past the end of what was
 *                  spliced in
 */
case class InlinedSource(page: String, revision: Long, line: Int, lineCount: Int)

object InlinedSource {

  /** What a document that is only itself knows about its lines, which is nothing. */
  val nowhere: Int => Option[InlinedSource] = _ => None
}
