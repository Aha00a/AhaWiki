package logics.wikis.interpreters

import logics.Cache
import logics.wikis.{HeadingNumber, ExtractConvertApplyBackQuote, ExtractConvertApplyChunk, ExtractConvertApplyMacro}
import models.DirectQuery.Link
import models.WikiContext
import utils.RegexUtil

import scala.collection.mutable.ArrayBuffer




class InterpreterWiki {
  val extractConvertApplyChunk = new ExtractConvertApplyChunk()
  val extractConvertApplyMacro = new ExtractConvertApplyMacro()
  val extractConvertApplyBackQuote = new ExtractConvertApplyBackQuote()

  def interpret(wikiText:String)(implicit wikiContext:WikiContext):String = {
    val chunkExtracted = extractConvertApplyChunk.extract(wikiText)
    val chunkMacroExtracted = extractConvertApplyMacro.extract(chunkExtracted)
    val backQuoteExtracted = extractConvertApplyBackQuote.extract(chunkMacroExtracted)


    val arrayBuffer = ArrayBuffer[String]()
    val chunkExtractedSplit: Array[String] = backQuoteExtracted.split("""(\r\n|\n)""")

    val regexHr = """^-{4,}$""".r
    val regexHeading = """^(={1,6})\s+(.+?)(\s+\1(\s*#(.+))?)?""".r
    val regexList = """^(\s+)([*-]|(\d+|[a-zA-Z]+|[ivxIVX])\.)\s*(.+)""".r
    val regexListUnordered = """[*-]""".r
    val regexListDecimal = """\d+\.""".r
    val regexListLowerAlpha = """[a-z]+\.""".r
    val regexListUpperAlpha = """[A-Z]+\.""".r
    val regexListLowerRoman = """[ivx]+\.""".r
    val regexListUpperRoman = """[IVX]+\.""".r

    var oldIndent = 0
    def closeIndent() = {
      while(0 < oldIndent) {
        arrayBuffer += "</ul>"
        oldIndent -= 1
      }
    }

    val arrayBufferHeading = ArrayBuffer[String]()
    val headingNumber = new HeadingNumber()

    for(s <- chunkExtractedSplit) {
      s match {
        case "" =>
          closeIndent()
        case regexHr() =>
          closeIndent()
          arrayBuffer += """<hr/>"""

        case regexHeading(heading, title, _, _, id) =>
          closeIndent()

          val headingLength = heading.length
          val idNotEmpty = if(id == null) title.replaceAll("""[^\w가-힣]""", "") else id
          val listStyle = ",1.,A.,I.,a.,i.".split(",")
          val titleForToc = title
            .replaceAll("""(?<!\\)\[wiki:(\S+?)\]""", "$1")
            .replaceAll("""(?<!\\)\[wiki:(\S+?)\s(.+?)\]""", """$2""")
          arrayBufferHeading += s"${" " * (headingLength - 1)}${listStyle(headingLength - 1)} [#$idNotEmpty $titleForToc]"
          arrayBuffer += s"""<h$headingLength id="$idNotEmpty"><a href="#$idNotEmpty" class="headingNumber">${headingNumber.incrGet(headingLength - 1)}</a> ${formatInline(title)}</h$headingLength>"""

        case regexList(indentString, style, _, content) =>
          val indent = indentString.length


          if(oldIndent < indent) {
            val listType = style match {
              case regexListUnordered() => ""
              case regexListDecimal() => "decimal"
              case regexListLowerRoman() => "lower-roman"
              case regexListUpperRoman() => "upper-roman"
              case regexListLowerAlpha() => "lower-alpha"
              case regexListUpperAlpha() => "upper-alpha"
              case _ => ""
            }
            for(i <- 0 until indent - oldIndent) {
              arrayBuffer += "<ul style=\"list-style-type: " + listType + ";\">"
            }
          }
          if(oldIndent > indent) {
            for(i <- 0 until oldIndent - indent) {
              arrayBuffer += s"</ul>"
            }
          }

          if(indent == 0) {
            arrayBuffer += formatInline(content)
          } else {
            arrayBuffer += "<li>" + formatInline(content) + "</li>"
          }

          oldIndent = indent
        case _ =>
          closeIndent()
          arrayBuffer += s"<p>${formatInline(s)}</p>".toString
      }
    }

    closeIndent()

    if(arrayBufferHeading.length > 5)
      arrayBuffer.insert(0, """<div class="toc">""" + new InterpreterWiki().interpret(arrayBufferHeading.mkString("\n")) + """</div>""")

    extractConvertApplyChunk(extractConvertApplyMacro(extractConvertApplyBackQuote(arrayBuffer.mkString("\n"))))
  }

  var url = """"""


  val urlScheme = "[a-zA-Z][-a-zA-Z0-9+._]+"
  val regexSimple = ("""\[(""" + urlScheme + """://[^]\s]+)\s([^]]+)\]|\[(""" + urlScheme + """://[^]\s]+)\]|(""" + urlScheme + """://[^]\s]+)""").r
  def formatInline(line: String)(implicit wikiContext:WikiContext): String = {
    var s = line
    for((regex, replacement) <- List(
      ("""\<""".r, "&lt;"),
      ("""'''(.+?)'''""".r, "<b>$1</b>"),
      ("""''(.+?)''""".r, "<i>$1</i>"),
      ("""__(.+?)__""".r, "<u>$1</u>"),
      ("""~~(.+?)~~""".r, "<s>$1</s>"),
      ("""`(.+?)`""".r, "<code>$1</code>"),

      ("""""".r, "")
    )) {
      s = regex.replaceAllIn(s, replacement)
    }
    s = InterpreterWiki.replaceLink(s)
    s
  }
}

object InterpreterWiki {
  case class LinkMarkup(uri:String, alias:String) {
    def this(uri:String) = this(uri, "")

    def uriNormalized: String = if (uri.startsWith("wiki:")) uri.substring(5) else uri

    def aliasWithDefault = if(alias == null || alias.isEmpty) uriNormalized else alias

    def toRegexReplacement(set: Set[String] = Set[String]()) = {
      val external: Boolean = uri.contains("://")

      val href: String = uriNormalized
      val attrTarget: String = if (external) " target=\"_blank\"" else ""
      val display: String = aliasWithDefault
      val attrCss = if (external || uriNormalized.startsWith("#") || uriNormalized.startsWith("?") || set.contains(uriNormalized.replaceAll("""[#?].+$""", ""))) { "" } else { """ class="missing"""" }

      s"""<a href="${RegexUtil.escapeDollar(href)}"$attrTarget$attrCss>${RegexUtil.escapeDollar(display)}</a>"""
    }

    def toLink(src:String) = Link(src, uriNormalized, alias)

    def toDisplay = {
      if(alias == null || alias.isEmpty) {
        s"[${uri}]"
      } else {
        s"[${uri}] aliased by ${alias}"
      }
    }
  }

  val regexLink =
    """(?x)
                ((?<!\\)\\)?        ([a-zA-Z][-a-zA-Z0-9+._]+ :// \S+)          |
                ((?<!\\)\\)?        \[ ([^\]\s]+) \]                            |
                ((?<!\\)\\)?        \[ ([^\]\s]+) \s+ ([^\]]+) \]
    """.r

  def replaceLink(s:String)(implicit wikiContext:WikiContext):String = {
    val set: Set[String] = Cache.PageNameSet.get()
    regexLink.replaceAllIn(s, _ match {
      case regexLink(null, uri, null, null, null, null, null) => new LinkMarkup(uri).toRegexReplacement()
      case regexLink(null, null, null, uri, null, null, null) => new LinkMarkup(uri).toRegexReplacement(set)
      case regexLink(null, null, null, null, null, uri, alias) => new LinkMarkup(uri, alias).toRegexReplacement(set)

      case regexLink(escape, uri, null, null, null, null, null) => RegexUtil.escapeDollar(uri)
      case regexLink(null, null, escape, uri, null, null, null) => RegexUtil.escapeDollar(s"[$uri]")
      case regexLink(null, null, null, null, escape, uri, alias) => RegexUtil.escapeDollar(s"[$uri $alias]")

      case value => "wrong : " + value
    })
  }
  
  def extractLink(src:String, content:String):Iterator[Link] = {
    regexLink.findAllIn(content).map {
      case regexLink(null, uri, null, null, null, null, null) => new LinkMarkup(uri).toLink(src)
      case regexLink(null, null, null, uri, null, null, null) => new LinkMarkup(uri).toLink(src)
      case regexLink(null, null, null, null, null, uri, alias) => new LinkMarkup(uri, alias).toLink(src)
      case _ => null
    }.filter(_ != null).filterNot(_.dst.startsWith("[")) // TODO: Macro
  }
}
