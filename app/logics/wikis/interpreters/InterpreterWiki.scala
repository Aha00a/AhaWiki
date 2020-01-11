package logics.wikis.interpreters

import com.aha00a.commons.utils.{DateTimeUtil, RegexUtil, VariableHolder}
import logics.wikis._
import models.{Link, WikiContext}
import play.api.cache.CacheApi
import play.api.db.Database
import play.api.mvc.Request

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex




object InterpreterWiki extends TraitInterpreter {
  case class LinkMarkup(uri:String, alias:String = "")(implicit wikiContext:WikiContext) {
    lazy val uriNormalized: String = if (uri.startsWith("wiki:")) uri.substring(5) else uri
    lazy val aliasWithDefault: String = if(alias == null || alias.isEmpty) uriNormalized else alias

    def toHtmlString(set: Set[String] = Set[String]()): String = {
      if(wikiContext.name == uri){
        s"""<b>$aliasWithDefault</b>"""
      } else {
        val external: Boolean = PageNameLogic.isExternal(uri)
        val href: String = if(uriNormalized.startsWith("schema:")) s"./${uriNormalized}" else uriNormalized;
        val attrTarget: String = if (external) """ target="_blank"""" else ""
        val display: String = aliasWithDefault
        val attrCss = if(uriNormalized.startsWith("schema:")) {
          """ class="schema""""
        } else if (
            set.isEmpty ||
            external ||
            uriNormalized.startsWith("#") ||
            uriNormalized.startsWith("?") ||
            // uriNormalized.matches(DateTimeUtil.regexIsoLocalDate.pattern.pattern()) ||
            uriNormalized.matches(DateTimeUtil.regexYearDashMonth.pattern.pattern()) ||
            uriNormalized.matches(DateTimeUtil.regexDashDashDashDashDay.pattern.pattern()) ||
            uriNormalized.matches(DateTimeUtil.regexYear.pattern.pattern()) ||
            uriNormalized.matches(DateTimeUtil.regexDashDashMonthDashDay.pattern.pattern()) ||
            uriNormalized.matches(DateTimeUtil.regexDashDashMonth.pattern.pattern()) ||
            set.contains(uriNormalized.replaceAll("""[#?].+$""", ""))
        ) {
          ""
        } else {
          """ class="missing""""
        }
        s"""<a href="${RegexUtil.escapeDollar(href)}"$attrTarget$attrCss>${RegexUtil.escapeDollar(display)}</a>"""
      }
    }

    def toLink(src:String) = Link(src, uriNormalized, alias)
  }

  object State extends Enumeration {
    type State = Value
    val Normal, Hr, Heading, List = Value
  }

  def apply(wikiText:String)(implicit wikiContext:WikiContext):String = {
    val extractConvertApplyChunk = new ExtractConvertApplyChunk()
    val extractConvertApplyMacro = new ExtractConvertApplyMacro()
    val extractConvertApplyBackQuote = new ExtractConvertApplyBackQuote()

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




    val arrayBufferHeading = ArrayBuffer[String]()
    val headingNumber = new HeadingNumber()


    var oldIndent = 0
    //noinspection ScalaUnusedSymbol
    val variableHolder = new VariableHolder(State.Normal, (before:State.State, after:State.State) => {
      if(after != State.List) {
        while (0 < oldIndent) {
          arrayBuffer += "</ul>"
          oldIndent -= 1
        }
      }
    })

    for(s <- chunkExtractedSplit) {
      s match {
        case "" =>
          variableHolder := State.Normal
        case regexHr() =>
          variableHolder := State.Hr
          arrayBuffer += """<hr/>"""

        case regexHeading(heading, title, _, _, id) =>
          variableHolder := State.Heading

          val headingLength = heading.length
          val idNotEmpty = if(id == null) title.replaceAll("""[^\w가-힣]""", "") else id
          val listStyle = ",1.,A.,I.,a.,i.".split(",")
          val titleForToc = title
            .replaceAll("""(?<!\\)\[wiki:(\S+?)\]""", "$1")
            .replaceAll("""(?<!\\)\[wiki:(\S+?)\s(.+?)\]""", """$2""")
            .replaceAll("""(?<!\\)\[(\S+?)\]""", "$1")
          arrayBufferHeading += s"${" " * (headingLength - 1)}${listStyle(headingLength - 1)} [#$idNotEmpty $titleForToc]"
          arrayBuffer += s"""<h$headingLength id="$idNotEmpty"><a href="#$idNotEmpty" class="headingNumber">${headingNumber.incrGet(headingLength - 1)}</a> ${formatInline(title)}</h$headingLength>"""

        case regexList(indentString, style, _, content) =>
          variableHolder := State.List

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
            for(_ <- 0 until indent - oldIndent) {
              arrayBuffer += "<ul style=\"list-style-type: " + listType + ";\">"
            }
          }
          if(oldIndent > indent) {
            for(_ <- 0 until oldIndent - indent) {
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
          variableHolder := State.Normal
          if(Seq(extractConvertApplyChunk, extractConvertApplyMacro, extractConvertApplyBackQuote).forall(!_.contains(s))) {
            arrayBuffer += s"<p>${formatInline(s)}</p>"
          } else {
            arrayBuffer += formatInline(s)
          }
      }
    }

    variableHolder := State.Normal

    if(arrayBufferHeading.length > 5)
      arrayBuffer.insert(0, """<div class="toc">""" + InterpreterWiki(arrayBufferHeading.mkString("\n")) + """</div>""")

    extractConvertApplyChunk(extractConvertApplyMacro(extractConvertApplyBackQuote(arrayBuffer.mkString("\n"))))
  }


  override def extractLink(content:String)(implicit wikiContext: WikiContext):Seq[Link] = {
    val extractConvertApplyChunk = new ExtractConvertApplyChunk() // TODO: rename
    val extractConvertApplyMacro = new ExtractConvertApplyMacro()
    val extractConvertApplyBackQuote = new ExtractConvertApplyBackQuote()

    val chunkExtracted = extractConvertApplyChunk.extract(content)
    val chunkMacroExtracted = extractConvertApplyMacro.extract(chunkExtracted)
    val backQuoteExtracted = extractConvertApplyBackQuote.extract(chunkMacroExtracted)

    val seqLinkInterpreter: Seq[Link] = extractConvertApplyChunk.extractLink().toList
    val seqLinkMacro: Seq[Link] = extractConvertApplyMacro.extractLink().map(LinkMarkup(_).toLink(wikiContext.name)).toList
    val seqLinkWikiText: Seq[Link] = InterpreterWiki.extractLinkMarkup(backQuoteExtracted).map(_.toLink(wikiContext.name)).filterNot(_.dst.startsWith("[")).toList
    seqLinkInterpreter ++ seqLinkMacro ++ seqLinkWikiText
  }


  def formatInline(line: String)(implicit wikiContext:WikiContext): String = {
    var s = line
    for((regex, replacement) <- List(
      ("""<""".r, "&lt;"),
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


  val regexLink: Regex =
    """(?x)
          ((?<!\\)\\)?
          (?:
            ([a-zA-Z][-a-zA-Z0-9+._]+ :// \S+)          |
            \[" ([^\]"]+) "\]                           |
            \[ ([^\]\s]+) \]                            |
            \[" ([^\]"]+) " \s+ ([^\]]+) \]            |
            \[ ([^\]\s]+) \s+ ([^\]]+) \]
          )
    """.r

  def replaceLink(s:String)(implicit wikiContext:WikiContext):String = {
    implicit val request: Request[Any] = wikiContext.request
    implicit val cacheApi: CacheApi = wikiContext.cacheApi
    implicit val database: Database = wikiContext.database
    val set: Set[String] = wikiContext.setPageName

    regexLink.replaceAllIn(s, _ match {
      case regexLink(null, uri , null, null, null, null, null, null) => LinkMarkup(uri).toHtmlString()
      case regexLink(null, null, uri , null, null, null, null, null) => LinkMarkup(uri).toHtmlString(set)
      case regexLink(null, null, null, uri , null, null, null, null) => LinkMarkup(uri).toHtmlString(set)
      case regexLink(null, null, null, null, uri, alias, null, null) => LinkMarkup(uri, alias).toHtmlString(set)
      case regexLink(null, null, null, null, null, null, uri, alias) => LinkMarkup(uri, alias).toHtmlString(set)

      case regexLink(_   , uri , null, null, null, null, null, null) => RegexUtil.escapeDollar(uri)
      case regexLink(_   , null, uri , null, null, null, null, null) => RegexUtil.escapeDollar(s"""["$uri"]""")
      case regexLink(_   , null, null, uri , null, null, null, null) => RegexUtil.escapeDollar(s"[$uri]")
      case regexLink(_   , null, null, null, uri , alia, null, null) => RegexUtil.escapeDollar(s"""["$uri" $alia]""")
      case regexLink(_   , null, null, null, null, null, uri , alia) => RegexUtil.escapeDollar(s"""[$uri $alia]""")

      case value => "wrong : " + value
    })
  }

  def extractLinkMarkup(content:String)(implicit wikiContext:WikiContext):Iterator[LinkMarkup] = {
    regexLink.findAllIn(content).map {
      case regexLink(null, uri , null, null, null, null, null, null) => LinkMarkup(uri)
      case regexLink(null, null, uri , null, null, null, null, null) => LinkMarkup(uri)
      case regexLink(null, null, null, uri , null, null, null, null) => LinkMarkup(uri)
      case regexLink(null, null, null, null, uri, alias, null, null) => LinkMarkup(uri, alias)
      case regexLink(null, null, null, null, null, null, uri, alias) => LinkMarkup(uri, alias)
      case _ => null
    }.filter(_ != null)
  }
}


