package logics

import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import play.api.libs.json.Json

/**
 * JSON written into an HTML page: inside a script element, or as a JavaScript literal.
 *
 * Json.stringify leaves `<` as it is, and the HTML parser ends a script element at the first
 * `</script` wherever it stands, quotes or no quotes. So until 2026-09-15 a page name or a schema
 * value holding one closed the element early and the rest ran as markup -- and a view of a page
 * that does not exist takes its name from the URL, so sending a link was enough. Here `<`, `>`
 * and `&` become unicode escapes, which mean the same to a JSON or JavaScript reader; so do the
 * two line separators, which older JavaScript took for the end of a line inside a string.
 */
object HtmlJson {
  def stringify(json: JsValue): String =
    Json.stringify(json)
      .replace("<", "\\u003c")
      .replace(">", "\\u003e")
      .replace("&", "\\u0026")
      .replace("\u2028", "\\u2028")
      .replace("\u2029", "\\u2029")

  /** A JavaScript string literal, quotes included. */
  def string(value: String): String = stringify(JsString(value))
}
