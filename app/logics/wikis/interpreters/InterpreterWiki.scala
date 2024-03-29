package logics.wikis.interpreters

import com.aha00a.commons.utils.{DateTimeUtil, RegexUtil, VariableHolder}
import logics.wikis._
import models.{PageContent, ContextWikiPage}

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import logics.wikis.interpreters.ahaMark.AhaMarkLink




object InterpreterWiki extends TraitInterpreter {

  import models.tables.Link
  import models.tables.SchemaOrg


  object State extends Enumeration {
    type State = Value
    val Normal, Hr, Heading, List = Value
  }

  abstract class Handler[T](val pageContent: PageContent)(implicit wikiContext: ContextWikiPage) {
    val extractConvertInjectInterpreter = new ExtractConvertInjectInterpreter()
    val extractConvertInjectMacro = new ExtractConvertInjectMacro()
    val extractConvertInjectBackQuote = new ExtractConvertInjectBackQuote()

    val chunkExtracted: String = extractConvertInjectInterpreter.extract(pageContent.content)
    val chunkMacroExtracted: String = extractConvertInjectMacro.extract(chunkExtracted)
    val backQuoteExtracted: String = extractConvertInjectBackQuote.extract(chunkMacroExtracted)

    def process(): T
  }

  abstract class HandlerContentIterateBase[T](override val pageContent: PageContent)(implicit wikiContext:ContextWikiPage) extends Handler[T](pageContent) {
    val regexHr: Regex = """^-{4,}$""".r
    val regexHr2: Regex = """^={4,}$""".r
    val regexHeading: Regex = """^(={1,6})\s+(.+?)(\s+\1(\s*#(.+))?)?""".r
    val regexList: Regex = """^(\s+)([*-]|(\d+|[a-zA-Z]+|[ivxIVX]+|[가나다라마바사아자차카타파하]+|[ㄱㄴㄷㄹㅁㅂㅅㅇㅈㅊㅋㅌㅍㅎ]+)\.)\s*(.+)""".r
    val regexListUnordered: Regex = """[*-]""".r
    val regexListDecimal: Regex = """\d+\.""".r
    val regexListLowerAlpha: Regex = """[a-z]+\.""".r
    val regexListUpperAlpha: Regex = """[A-Z]+\.""".r
    val regexListLowerRoman: Regex = """[ivx]+\.""".r
    val regexListUpperRoman: Regex = """[IVX]+\.""".r
    val regexListUpperHangul: Regex = """[가나다라마바사아자차카타파하]+\.""".r
    val regexListUpperHangulConsonant: Regex = """[ㄱㄴㄷㄹㅁㅂㅅㅇㅈㅊㅋㅌㅍㅎ]+\.""".r

    override def process(): T = {
      for(s <- backQuoteExtracted.split("""(\r\n|\n)""")) {
        s match {
          case "" => emptyLine()
          case regexHr() => hr(s)
          case regexHr2() => hr2(s)
          case regexHeading(heading, title, _, _, id) => this.heading(heading, title, id)
          case regexList(indentString, style, _, content) => list(indentString, style, content);
          case _ => others(s)
        }
      }
      result()
    }

    def emptyLine(): Unit
    def hr(s: String): Unit
    def hr2(s: String): Unit
    def heading(heading: String, title: String, id: String): Unit
    def list(indentString: String, style: String, content: String): Unit
    def others(s: String): Unit
    def result(): T
  }

  class HandlerToHtmlString(override val pageContent: PageContent)(implicit wikiContext:ContextWikiPage) extends HandlerContentIterateBase[String](pageContent) {
    val arrayBuffer: ArrayBuffer[String] = ArrayBuffer[String]()
    val arrayBufferHeading: ArrayBuffer[String] = ArrayBuffer[String]()
    val headingNumber = new HeadingNumber()

    var oldIndent = 0
    val variableHolderState: VariableHolder[State.Value] = new VariableHolder(State.Normal, (_:State.State, after:State.State) => {
      if(after != State.List) {
        while (0 < oldIndent) {
          arrayBuffer += "</ul>"
          oldIndent -= 1
        }
      }
    })

    override def emptyLine(): Unit = {
      variableHolderState := State.Normal
    }

    override def hr(s: String): Unit = {
      variableHolderState := State.Hr
      arrayBuffer += s"""<hr class="hr${s.length}"/>"""
    }

    override def hr2(s: String): Unit = {
      variableHolderState := State.Hr
      arrayBuffer += s"""<hr class="hr${s.length} pageBreakAfterAlways"/>"""
    }

    override def heading(heading: String, title: String, id: String): Unit = {
      variableHolderState := State.Heading
      val headingLength = heading.length
      val listStyle = ",1.,A.,I.,a.,i.".split(",")
      val titleForToc = title
        .replaceAll("""(?<!\\)\[wiki:(\S+?)\]""", "$1")
        .replaceAll("""(?<!\\)\[wiki:(\S+?)\s(.+?)\]""", """$2""")
        .replaceAll("""(?<!\\)\["([^"]+?)"\]""", "$1")
        .replaceAll("""(?<!\\)\[("[^"]+?")\s(.+?)\]""", "$2")
        .replaceAll("""(?<!\\)\[(\S+?)\]""", "$1")
        .replaceAll("""(?<!\\)\[(\S+?)\s(.+?)\]""", "$2")
      val idNotEmpty = if(id == null) titleForToc.replaceAll("""\s+""", "-") else id

      arrayBufferHeading += s"${" " * (headingLength - 1)}${listStyle(headingLength - 1)} [#$idNotEmpty $titleForToc]"
      arrayBuffer += s"""</div><div class="$idNotEmpty"><h$headingLength id="$idNotEmpty"><a href="#$idNotEmpty" class="headingNumber">${headingNumber.incrGet(headingLength - 1)}</a> ${inlineToHtmlString(title)}</h$headingLength>"""
    }

    override def list(indentString: String, style: String, content: String): Unit = {
      variableHolderState := State.List
      val indent = indentString.length


      if(oldIndent < indent) {
        val listType = style match {
          case regexListUnordered() => "disc"
          case regexListDecimal() => "decimal"
          case regexListLowerRoman() => "lower-roman"
          case regexListUpperRoman() => "upper-roman"
          case regexListLowerAlpha() => "lower-alpha"
          case regexListUpperAlpha() => "upper-alpha"
          case regexListUpperHangul() => "hangul"
          case regexListUpperHangulConsonant() => "hangul-consonant"
          case _ => "disc"
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
        arrayBuffer += inlineToHtmlString(content)
      } else {
        arrayBuffer += "<li>" + inlineToHtmlString(content) + "</li>"
      }

      oldIndent = indent
    }

    override def others(s: String): Unit = {
      variableHolderState := State.Normal
      if(Seq(extractConvertInjectInterpreter, extractConvertInjectMacro, extractConvertInjectBackQuote).forall(!_.contains(s))) {
        arrayBuffer += s"<p>${inlineToHtmlString(s)}</p>"
      } else {
        arrayBuffer += inlineToHtmlString(s)
      }
    }

    override def result(): String = {
      variableHolderState := State.Normal
      if (arrayBufferHeading.length > 5 + 5)
        arrayBuffer.insert(0, """<div class="toc">""" + InterpreterWiki.toHtmlString(arrayBufferHeading.mkString("\n")) + """</div>""")

      val str = arrayBuffer.mkString("<div>", "\n", "</div>")
      extractConvertInjectInterpreter.inject(extractConvertInjectMacro.inject(extractConvertInjectBackQuote.inject(str)))
    }
  }

  class HandlerToSeqLink(override val pageContent: PageContent)(implicit wikiContext:ContextWikiPage) extends Handler[Seq[Link]](pageContent) {
    override def process(): Seq[Link] = {
      val seqLinkInterpreter: Seq[Link] = extractConvertInjectInterpreter.extractLink().toList
      val seqLinkMacro: Seq[Link] = extractConvertInjectMacro.extractLink().map(AhaMarkLink(_).toLink(wikiContext.name)).toList
      val seqLinkWikiText: Seq[Link] = InterpreterWiki.extractLinkMarkup(backQuoteExtracted).map(_.toLink(wikiContext.name)).filterNot(_.dst.startsWith("[")).toList
      seqLinkInterpreter ++ seqLinkMacro ++ seqLinkWikiText
    }
  }

  class HandlerToSeqSchemaOrg(override val pageContent: PageContent)(implicit wikiContext:ContextWikiPage) extends Handler[Seq[SchemaOrg]](pageContent) {
    override def process(): Seq[SchemaOrg] = {
      extractConvertInjectInterpreter.extractSchemaOrg()
    }
  }



  val regexLink: Regex =
    """(?x)
          ((?<!\\)\\)?
          (?:
            ([a-zA-Z][-a-zA-Z0-9+._]+ :// \S+)          |
            \[" ([^\]"]+) "\]                           |
            \[ ([^\]\s]+) \]                            |
            \[" ([^\]"]+) " \s+ ([^\]]+) \]             |
            \[ ([^\]\s]+) \s+ ([^\]]+) \]
          )
    """.r

  def replaceLink(s:String)(implicit wikiContext:ContextWikiPage):String = {
    val set: Set[String] = wikiContext.setPageNameByPermission

    regexLink.replaceAllIn(s, _ match {
      case regexLink(null, uri , null, null, null, null, null, null) => AhaMarkLink(uri).toHtmlString()
      case regexLink(null, null, uri , null, null, null, null, null) => AhaMarkLink(uri).toHtmlString(set)
      case regexLink(null, null, null, uri , null, null, null, null) => AhaMarkLink(uri).toHtmlString(set)
      case regexLink(null, null, null, null, uri, alias, null, null) => AhaMarkLink(uri, alias).toHtmlString(set)
      case regexLink(null, null, null, null, null, null, uri, alias) => AhaMarkLink(uri, alias).toHtmlString(set)

      case regexLink(_   , uri , null, null, null, null, null, null) => RegexUtil.escapeDollar(uri)
      case regexLink(_   , null, uri , null, null, null, null, null) => RegexUtil.escapeDollar(s"""["$uri"]""")
      case regexLink(_   , null, null, uri , null, null, null, null) => RegexUtil.escapeDollar(s"[$uri]")
      case regexLink(_   , null, null, null, uri , alia, null, null) => RegexUtil.escapeDollar(s"""["$uri" $alia]""")
      case regexLink(_   , null, null, null, null, null, uri , alia) => RegexUtil.escapeDollar(s"""[$uri $alia]""")

      case value => "wrong : " + value
    })
  }

  def extractLinkMarkup(content:String)(implicit wikiContext:ContextWikiPage):Iterator[AhaMarkLink] = {
    regexLink.findAllIn(content).map {
      case regexLink(null, uri , null, null, null, null, null, null) => AhaMarkLink(uri)
      case regexLink(null, null, uri , null, null, null, null, null) => AhaMarkLink(uri)
      case regexLink(null, null, null, uri , null, null, null, null) => AhaMarkLink(uri)
      case regexLink(null, null, null, null, uri, alias, null, null) => AhaMarkLink(uri, alias)
      case regexLink(null, null, null, null, null, null, uri, alias) => AhaMarkLink(uri, alias)
      case _ => null
    }.filter(_ != null)
  }

  def inlineToHtmlString(line: String)(implicit wikiContext:ContextWikiPage): String = {
    var s = line
    for((regex, replacement) <- List(
      ("""<""".r, "&lt;"),
      ("""'''(.+?)'''""".r, "<b>$1</b>"),
      ("""''(.+?)''""".r, "<i>$1</i>"),
      ("""__(.+?)__""".r, "<u>$1</u>"),
      ("""~~(.+?)~~""".r, "<s>$1</s>"),
    )) {
      s = regex.replaceAllIn(s, replacement)
    }
    s = InterpreterWiki.replaceLink(s)
    s
  }


  override def toHtmlString(content: String)(implicit wikiContext:ContextWikiPage):String = {
    val pageContent: PageContent = PageContent(content)
    val handler = new HandlerToHtmlString(pageContent)
    handler.process()
  }

  override def toSeqLink(content:String)(implicit wikiContext: ContextWikiPage):Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    pageContent.redirect match {
      case Some(v) => Seq(Link(wikiContext.nameTop, v, "redirect"))
      case None =>
        val pageContent: PageContent = PageContent(content)
        val handler = new HandlerToSeqLink(pageContent)
        handler.process()
    }
  }

  override def toSeqSchemaOrg(content: String)(implicit wikiContext: ContextWikiPage): Seq[SchemaOrg] = {
    val pageContent: PageContent = PageContent(content)
    pageContent.redirect match {
      case Some(_) => Seq()
      case None =>
        val pageContent: PageContent = PageContent(content)
        val handler = new HandlerToSeqSchemaOrg(pageContent)
        handler.process()
    }
  }
}

