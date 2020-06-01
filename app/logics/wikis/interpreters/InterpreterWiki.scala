package logics.wikis.interpreters

import com.aha00a.commons.utils.{DateTimeUtil, RegexUtil, VariableHolder}
import logics.wikis._
import models.{Link, PageContent, SchemaOrg, WikiContext}

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
        val href: String = if(uriNormalized.startsWith("schema:")) s"./$uriNormalized" else uriNormalized
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

  abstract class Handler[T](val pageContent: PageContent)(implicit wikiContext: WikiContext) {
    val extractConvertApplyInterpreter = new ExtractConvertApplyInterpreter()
    val extractConvertApplyMacro = new ExtractConvertApplyMacro()
    val extractConvertApplyBackQuote = new ExtractConvertApplyBackQuote()

    val chunkExtracted: String = extractConvertApplyInterpreter.extract(pageContent.content)
    val chunkMacroExtracted: String = extractConvertApplyMacro.extract(chunkExtracted)
    val backQuoteExtracted: String = extractConvertApplyBackQuote.extract(chunkMacroExtracted)

    def process(): T
  }

  abstract class HandlerContentIterateBase[T](override val pageContent: PageContent)(implicit wikiContext:WikiContext) extends Handler[T](pageContent) {
    val regexHr: Regex = """^-{4,}$""".r
    val regexHeading: Regex = """^(={1,6})\s+(.+?)(\s+\1(\s*#(.+))?)?""".r
    val regexList: Regex = """^(\s+)([*-]|(\d+|[a-zA-Z]+|[ivxIVX])\.)\s*(.+)""".r
    val regexListUnordered: Regex = """[*-]""".r
    val regexListDecimal: Regex = """\d+\.""".r
    val regexListLowerAlpha: Regex = """[a-z]+\.""".r
    val regexListUpperAlpha: Regex = """[A-Z]+\.""".r
    val regexListLowerRoman: Regex = """[ivx]+\.""".r
    val regexListUpperRoman: Regex = """[IVX]+\.""".r

    override def process(): T = {
      for(s <- backQuoteExtracted.split("""(\r\n|\n)""")) {
        s match {
          case "" => emptyLine()
          case regexHr() => hr()
          case regexHeading(heading, title, _, _, id) => this.heading(heading, title, id)
          case regexList(indentString, style, _, content) => list(indentString, style, content);
          case _ => others(s)
        }
      }
      result()
    }

    def emptyLine(): Unit
    def hr(): Unit
    def heading(heading: String, title: String, id: String): Unit
    def list(indentString: String, style: String, content: String): Unit
    def others(s: String): Unit
    def result(): T
  }

  class HandlerToHtmlString(override val pageContent: PageContent)(implicit wikiContext:WikiContext) extends HandlerContentIterateBase[String](pageContent) {
    val arrayBuffer: ArrayBuffer[String] = ArrayBuffer[String]()
    val arrayBufferHeading: ArrayBuffer[String] = ArrayBuffer[String]()
    val headingNumber = new HeadingNumber()

    var oldIndent = 0
    val variableHolderState: VariableHolder[State.Value] = new VariableHolder(State.Normal, (before:State.State, after:State.State) => {
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

    override def hr(): Unit = {
      variableHolderState := State.Hr
      arrayBuffer += """<hr/>"""
    }

    override def heading(heading: String, title: String, id: String): Unit = {
      variableHolderState := State.Heading
      val headingLength = heading.length
      val idNotEmpty = if(id == null) title.replaceAll("""[^\w가-힣]""", "") else id
      val listStyle = ",1.,A.,I.,a.,i.".split(",")
      val titleForToc = title
        .replaceAll("""(?<!\\)\[wiki:(\S+?)\]""", "$1")
        .replaceAll("""(?<!\\)\[wiki:(\S+?)\s(.+?)\]""", """$2""")
        .replaceAll("""(?<!\\)\[(\S+?)\]""", "$1")

      arrayBufferHeading += s"${" " * (headingLength - 1)}${listStyle(headingLength - 1)} [#$idNotEmpty $titleForToc]"
      arrayBuffer += s"""<h$headingLength id="$idNotEmpty"><a href="#$idNotEmpty" class="headingNumber">${headingNumber.incrGet(headingLength - 1)}</a> ${formatInline(title)}</h$headingLength>"""
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
        arrayBuffer += formatInline(content)
      } else {
        arrayBuffer += "<li>" + formatInline(content) + "</li>"
      }

      oldIndent = indent
    }

    override def others(s: String): Unit = {
      variableHolderState := State.Normal
      if(Seq(extractConvertApplyInterpreter, extractConvertApplyMacro, extractConvertApplyBackQuote).forall(!_.contains(s))) {
        arrayBuffer += s"<p>${formatInline(s)}</p>"
      } else {
        arrayBuffer += formatInline(s)
      }
    }

    override def result(): String = {
      variableHolderState := State.Normal
      if (arrayBufferHeading.length > 5)
        arrayBuffer.insert(0, """<div class="toc">""" + InterpreterWiki.toHtmlString(arrayBufferHeading.mkString("\n")) + """</div>""")

      extractConvertApplyInterpreter(extractConvertApplyMacro(extractConvertApplyBackQuote(arrayBuffer.mkString("\n"))))
    }
  }

  class HandlerToSeqWord(override val pageContent: PageContent)(implicit wikiContext:WikiContext) extends HandlerContentIterateBase[Seq[String]](pageContent) {
    val arrayWord: ArrayBuffer[String] = ArrayBuffer[String]()

    override def emptyLine(): Unit = {}

    override def hr(): Unit = {}

    override def heading(heading: String, title: String, id: String): Unit = {
      arrayWord += title
    }

    override def list(indentString: String, style: String, content: String): Unit = {
      arrayWord += content
    }

    override def others(s: String): Unit = {
      if(Seq(extractConvertApplyInterpreter, extractConvertApplyMacro, extractConvertApplyBackQuote).forall(!_.contains(s))) {
        arrayWord += s // TODO
      } else {
        arrayWord += s // TODO
      }
    }

    override def result(): Seq[String] = {
      arrayWord // TODO
    }
  }

  class HandlerToSeqLink(override val pageContent: PageContent)(implicit wikiContext:WikiContext) extends Handler[Seq[Link]](pageContent) {
    override def process(): Seq[Link] = {
      val seqLinkInterpreter: Seq[Link] = extractConvertApplyInterpreter.extractLink().toList
      val seqLinkMacro: Seq[Link] = extractConvertApplyMacro.extractLink().map(LinkMarkup(_).toLink(wikiContext.name)).toList
      val seqLinkWikiText: Seq[Link] = InterpreterWiki.extractLinkMarkup(backQuoteExtracted).map(_.toLink(wikiContext.name)).filterNot(_.dst.startsWith("[")).toList
      seqLinkInterpreter ++ seqLinkMacro ++ seqLinkWikiText
    }
  }

  class HandlerToSeqSchemaOrg(override val pageContent: PageContent)(implicit wikiContext:WikiContext) extends Handler[Seq[SchemaOrg]](pageContent) {
    override def process(): Seq[SchemaOrg] = {
      extractConvertApplyInterpreter.extractSchemaOrg()
    }
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
    val set: Set[String] = wikiContext.setPageNameByPermission

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


  override def toHtmlString(content: String)(implicit wikiContext:WikiContext):String = {
    val pageContent: PageContent = PageContent(content)
    val handler = new HandlerToHtmlString(pageContent)
    handler.process()
  }

  override def toSeqWord(content: String)(implicit wikiContext: WikiContext): Seq[String] = {
    val pageContent: PageContent = PageContent(content)
    val handler = new HandlerToSeqWord(pageContent)
    handler.process()
  }

  override def toSeqLink(content:String)(implicit wikiContext: WikiContext):Seq[Link] = {
    val pageContent: PageContent = PageContent(content)
    pageContent.redirect match {
      case Some(v) => Seq(Link(wikiContext.nameTop, v, "redirect"))
      case None =>
        val pageContent: PageContent = PageContent(content)
        val handler = new HandlerToSeqLink(pageContent)
        handler.process()
    }
  }

  override def toSeqSchemaOrg(content: String)(implicit wikiContext: WikiContext): Seq[SchemaOrg] = {
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

