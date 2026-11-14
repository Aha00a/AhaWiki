package logics

import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage

/**
 * What the editor's Wiki Formatting Help shows: the markup people reach for most, each with what
 * it draws.
 *
 * The right-hand side is drawn by the renderer on every edit page rather than written down as
 * HTML here. A hand-written result would be a second statement of what the renderer does, and
 * the two would part the first time the renderer changed; drawn, the help cannot show something
 * the page would not.
 *
 * The markup itself is the one copy kept by hand. The longer cheat sheet is the AhaMark Basic
 * page on ahawiki.net, which the help links to first; it is a wiki page with its own readers, so
 * the two are not merged, and this list stays short enough to read without scrolling the editor
 * away.
 */
object EditHelp {

  /**
   * One row. `block` markup is inserted on a line of its own; inline markup goes where the
   * cursor is. `note` is for the one thing a reader would otherwise get wrong.
   */
  case class Example(label: String, markup: String, block: Boolean, note: String = "")

  case class Row(example: Example, html: String)

  val examples: Seq[Example] = Seq(
    Example("Bold, italic", "'''bold''' ''italic''", block = false),
    Example("Code", "`code`", block = false),
    Example("Link", "[FrontPage]", block = false, "A page that does not exist yet is created from its link."),
    Example("Link with a label", "[FrontPage|Home]", block = false),
    Example("Heading", "== Section", block = true, "More = for a lower level."),
    Example("List", " * item\n  * nested", block = true, "Starts with a space. Without it, it is plain text."),
    Example("Numbered list", " 1. first\n 1. second", block = true, "Starts with a space."),
    Example("Checkbox", " * [ ] to do\n * [[CB(x)]] done", block = true, "Done is [[CB(x)]], not [x]: [x] links to a page named x."),
    Example("Table", "[[[#!Table tsv 1\nName\tRole\nKim\tDesign\n]]]", block = true, "Cells are separated by tabs. Paste from a spreadsheet."),
    Example("Code block", "[[[#!Vim python\nprint(\"hello\")\n]]]", block = true),
    Example("Horizontal line", "----", block = true),
  )

  def rows(implicit wikiContext: ContextWikiPage): Seq[Row] =
    examples.map(example => Row(example, Interpreters.toHtmlString("#!wiki\n" + example.markup)))
}
