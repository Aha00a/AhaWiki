package logics.wikis.macros

import com.aha00a.commons.Implicits.RichString
import logics.wikis.interpreters.InterpreterTable
import models.ContextWikiPage

import java.util.Locale

/**
 * `[[Sparkline(3 5 2 8 6)]]`: a word-sized line of the values, drawn on the server as SVG.
 *
 * The values are separated by whitespace, not commas: the numbers this goes beside are written
 * `1,234`, and splitting on commas would turn three of them into six points and draw a wrong line
 * without a word. Each value is read as a table cell is (`InterpreterTable.parseNumber`), so
 * `1,234`, `-7%` and `(12)` mean here what they mean in the column next to it. Anything that is
 * not a number is an argument error rather than a gap: a line that silently skips a value tells
 * the reader something the author did not write.
 *
 * It is a macro rather than a `#!Table` option reading a span of columns (`spark=2-7`): a macro
 * also works in a sentence or a list, and no table on the owner's wikis had the monthly columns
 * such an option would read (2026-11-20). The option can come later on top of `svg`.
 */
object MacroSparkline extends TraitMacro {
  // The drawing's own units. CSS sizes it at 4em x 1em, the same 4:1, so it scales with the text.
  private val width = 80.0
  private val height = 20.0
  // Room for half the stroke and the end dot, so neither is clipped at the edge of the viewBox.
  private val inset = 2.5

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val words = argument.trim.split("""\s+""").toSeq.filter(_.nonEmpty)
    parse(words).map(values => svg(values, words.mkString(" "))).getOrElse(argumentError(argument))
  }

  /** The values, when there are at least two and every word is one. */
  private[macros] def parse(words: Seq[String]): Option[Seq[Double]] = {
    val values = words.flatMap(InterpreterTable.parseNumber)
    Some(values).filter(v => v.size >= 2 && v.size == words.size)
  }

  // Fixed-point with a dot whatever the JVM's locale: a decimal comma would be read as a
  // separator between coordinates.
  private def coordinate(value: Double): String = "%.2f".formatLocal(Locale.ROOT, value).replaceAll("""\.?0+$""", "")

  private[macros] def svg(values: Seq[Double], written: String): String = {
    val lo = values.min
    val hi = values.max
    val step = (width - 2 * inset) / (values.size - 1)
    // A flat series is a line across the middle, not a division by zero.
    def y(value: Double): Double =
      if (hi == lo) height / 2
      else inset + (hi - value) / (hi - lo) * (height - 2 * inset)
    val points = values.zipWithIndex.map { case (value, i) => (inset + i * step, y(value)) }
    val (lastX, lastY) = points.last
    val title = written.escapeHtml()
    s"""<svg class="MacroSparkline" viewBox="0 0 ${coordinate(width)} ${coordinate(height)}" role="img" aria-label="$title">""" +
      s"""<title>$title</title>""" +
      s"""<polyline points="${points.map { case (x, y) => s"${coordinate(x)},${coordinate(y)}" }.mkString(" ")}"/>""" +
      s"""<circle cx="${coordinate(lastX)}" cy="${coordinate(lastY)}" r="2"/>""" +
      "</svg>"
  }
}
