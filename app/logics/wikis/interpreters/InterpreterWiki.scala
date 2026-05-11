package logics.wikis.interpreters

import com.aha00a.commons.utils.VariableHolder
import logics.wikis._
import logics.wikis.interpreters.ahaMark.AhaMarkLink
import models.ContextWikiPage
import models.PageContent

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex




object InterpreterWiki extends TraitInterpreter {

  import models.tables.CalculatedLink
  import models.tables.CalculatedSchemaOrg


  object State extends Enumeration {
    type State = Value
    val Normal, Hr, Heading, List = Value
  }

  abstract class Handler[T](val pageContent: PageContent) {
    val extractConvertInjectInterpreter = new ExtractConvertInjectInterpreter()
    val extractConvertInjectMacro = new ExtractConvertInjectMacro()
    val extractConvertInjectBackQuote = new ExtractConvertInjectBackQuote()

    val chunkExtracted: String = extractConvertInjectInterpreter.extract(pageContent.content)
    val chunkMacroExtracted: String = extractConvertInjectMacro.extract(chunkExtracted)
    val backQuoteExtracted: String = extractConvertInjectBackQuote.extract(chunkMacroExtracted)

    def process(): T
  }

  abstract class HandlerContentIterateBase[T](override val pageContent: PageContent) extends Handler[T](pageContent) {
    val regexHr: Regex = """^-{4,}$""".r
    val regexHr2: Regex = """^={4,}$""".r
    val regexHeading: Regex = """^(={1,6})(>)?\s+(.+?)(\s+\1(?:\s+([#.].+))?)?$""".r
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
      for((s, index) <- backQuoteExtracted.split("""(\r\n|\n)""", -1).zipWithIndex) {
        val lineNumber = extractConvertInjectInterpreter.originalLineNumber(index + 1)
        s match {
          case "" => emptyLine()
          case regexHr() => hr(s)
          case regexHr2() => hr2(s)
          case regexHeading(heading, collapseMarker, title, _, idAndClass) => this.heading(heading, title, idAndClass, lineNumber, collapseMarker != null)
          case regexList(indentString, style, _, content) => list(indentString, style, content, lineNumber);
          case _ => others(s, lineNumber)
        }
      }
      result()
    }

    def emptyLine(): Unit
    def hr(s: String): Unit
    def hr2(s: String): Unit
    def heading(heading: String, title: String, id: String, lineNumber: Int, isInitiallyCollapsed: Boolean): Unit
    def list(indentString: String, style: String, content: String, lineNumber: Int): Unit
    def others(s: String, lineNumber: Int): Unit
    def result(): T
  }

  class HandlerToHtmlString(
                             override val pageContent: PageContent,
                             suppressToc: Boolean = false
                           )(implicit wikiContext:ContextWikiPage) extends HandlerContentIterateBase[String](pageContent) {
    val arrayBuffer: ArrayBuffer[String] = ArrayBuffer[String]()
    val arrayBufferHeading: ArrayBuffer[String] = ArrayBuffer[String]()
    val headingNumber = new HeadingNumber()
    private val revision = InterpreterWiki.getRevisionForPartialEdit
    private val headingLineRangeByLineStart: Map[Int, Int] = InterpreterWiki.buildHeadingLineRangeByLineStart(pageContent.content, regexHeading)

    var oldIndent = 0
    private val regexColumnsStart: Regex = """^\s*<Columns(?:\s+([^>]*))?>\s*$""".r
    private val regexColumnsEnd: Regex = """^\s*</Columns>\s*$""".r
    private val regexDivStart: Regex = """^\s*<div(?:\s+([^>]*))?>\s*$""".r
    private val regexDivEnd: Regex = """^\s*</div>\s*$""".r
    private val regexSpanStart: Regex = """^\s*<span(?:\s+([^>]*))?>\s*$""".r
    private val regexSpanEnd: Regex = """^\s*</span>\s*$""".r
    private def previewLineStartAttr(lineNumber: Int): String =
      if (wikiContext.renderingMode == RenderingMode.Preview) s""" data-line-start="$lineNumber"""" else ""

    val variableHolderState: VariableHolder[State.Value] = new VariableHolder(State.Normal, (_:State.State, after:State.State) => {
      if(after != State.List) {
        while (0 < oldIndent) {
          arrayBuffer += "</ul>"
          oldIndent -= 1
        }
      }
    })

    override def process(): String = {
      val lines = backQuoteExtracted.split("""(\r\n|\n)""", -1).toVector
      var index = 0
      while (index < lines.length) {
        val s = lines(index)
        val lineNumber = extractConvertInjectInterpreter.originalLineNumber(index + 1)
        s match {
          case _ =>
            s match {
              case regexColumnsStart(attrsRaw) =>
                variableHolderState := State.Normal
                arrayBuffer += renderColumnsOpen(attrsRaw)
              case regexColumnsEnd() =>
                variableHolderState := State.Normal
                arrayBuffer += "</div>"
              case regexDivStart(attrsRaw) =>
                variableHolderState := State.Normal
                arrayBuffer += s"<div${renderAllowedAttributes(attrsRaw)}>"
              case regexDivEnd() =>
                variableHolderState := State.Normal
                arrayBuffer += "</div>"
              case regexSpanStart(attrsRaw) =>
                variableHolderState := State.Normal
                arrayBuffer += s"<span${renderAllowedAttributes(attrsRaw)}>"
              case regexSpanEnd() =>
                variableHolderState := State.Normal
                arrayBuffer += "</span>"
              case "" => emptyLine()
              case regexHr() => hr(s)
              case regexHr2() => hr2(s)
              case regexHeading(heading, collapseMarker, title, _, idAndClass) => this.heading(heading, title, idAndClass, lineNumber, collapseMarker != null)
              case regexList(indentString, style, _, content) => list(indentString, style, content, lineNumber)
              case _ => others(s, lineNumber)
            }
            index += 1
        }
      }
      result()
    }


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

    override def heading(heading: String, title: String, id: String, lineNumber: Int, isInitiallyCollapsed: Boolean): Unit = {
      variableHolderState := State.Heading
      val headingLength = heading.length
      val listStyle = ",1.,A.,I.,a.,i.".split(",")
      val titleForToc = title
        .replaceAll("""(?<!\\)\[wiki:(\S+?)]""", "$1")
        .replaceAll("""(?<!\\)\[wiki:(\S+?)\s(.+?)]""", """$2""")
        .replaceAll("""(?<!\\)\["([^"]+?)"]""", "$1")
        .replaceAll("""(?<!\\)\[("[^"]+?")\s(.+?)]""", "$2")
        .replaceAll("""(?<!\\)\[(\S+?)]""", "$1")
        .replaceAll("""(?<!\\)\[(\S+?)\s(.+?)]""", "$2")
      val headingAttributes = Option(id).map(_.trim).filter(_.nonEmpty).map { raw =>
        """([#.])([^#.\s]+)""".r.findAllMatchIn(raw).toSeq
      }.getOrElse(Seq.empty)
      val headingId = headingAttributes.collectFirst { case m if m.group(1) == "#" => m.group(2) }
      val headingClasses = headingAttributes.collect { case m if m.group(1) == "." => m.group(2) }
      val idNotEmpty = headingId.getOrElse(titleForToc.replaceAll("""\s+""", "-"))
      val wrapperClass = Seq(idNotEmpty, if (isInitiallyCollapsed) "sectionCollapsed" else "").filter(_.nonEmpty).mkString(" ")
      val isGeneratedHeading = headingClasses.contains("generated")
      val normalizedHeadingClasses = if (isGeneratedHeading) headingClasses :+ "generated" else headingClasses
      val headingClassAttribute = normalizedHeadingClasses.distinct.mkString(" ")
      val lineEndExclusive = headingLineRangeByLineStart.getOrElse(lineNumber, lineNumber) + 1
      val editUrl = InterpreterWiki.getEditUrlForPartialEdit(revision, lineNumber, lineEndExclusive)
      val editTitle = s"Edit section (r$revision, L$lineNumber-L${lineEndExclusive - 1})"
      val editDataAttrs = if (isGeneratedHeading) {
        ""
      } else {
        s""" data-edit-link="$editUrl" data-line-start="$lineNumber" data-line-end="$lineEndExclusive" data-edit-title="$editTitle""""
      }

      arrayBufferHeading += s"${" " * (headingLength - 1)}${listStyle(headingLength - 1)} [#$idNotEmpty $titleForToc]"
      arrayBuffer += s"""</div><div class="$wrapperClass"><div class="InterpreterRenderMetaWrapper" style="position: relative;"$editDataAttrs><div class="InterpreterRenderContent"><h$headingLength id="$idNotEmpty" class="$headingClassAttribute"><a href="#$idNotEmpty" class="headingNumber">${headingNumber.incrGet(headingLength - 1)}</a> ${inlineToHtmlString(title)}</h$headingLength></div></div>"""
    }

    override def list(indentString: String, style: String, content: String, lineNumber: Int): Unit = {
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
        arrayBuffer += s"<li${previewLineStartAttr(lineNumber)}>" + inlineToHtmlString(content) + "</li>"
      }

      oldIndent = indent
    }

    override def others(s: String, lineNumber: Int): Unit = {
      variableHolderState := State.Normal

      if (extractConvertInjectInterpreter.contains(s) || extractConvertInjectBackQuote.contains(s)) {
        arrayBuffer += inlineToHtmlString(s)
      } else if (extractConvertInjectMacro.contains(s)) {
        if (extractConvertInjectMacro.isBlockToken(s)) {
          arrayBuffer += inlineToHtmlString(s)
        } else {
          arrayBuffer += s"<p${previewLineStartAttr(lineNumber)}>${inlineToHtmlString(s)}</p>"
        }
      } else {
        arrayBuffer += s"<p${previewLineStartAttr(lineNumber)}>${inlineToHtmlString(s)}</p>"
      }
    }

    override def result(): String = {
      variableHolderState := State.Normal
      if (!suppressToc && arrayBufferHeading.length > 6)
        arrayBuffer.insert(0,
          """<div class="toc" data-default-collapsed-mobile="true"><button type="button" class="tocToggle" aria-expanded="true"><i class="fas fa-chevron-down fa-fw"></i></button><div class="tocBody">""" +
            InterpreterWiki.toHtmlString(arrayBufferHeading.mkString("\n")) +
            """</div></div>""")

      val str = arrayBuffer.mkString("<div>", "\n", "</div>")
      extractConvertInjectInterpreter.inject(
        extractConvertInjectMacro.inject(
          extractConvertInjectBackQuote.inject(str)
        )
      )
    }

    private def renderColumnsOpen(attrsRaw: String): String = {
      val raw = Option(attrsRaw).getOrElse("")
      val count = extractIntAttr(raw, "count").filter(_ > 0).getOrElse(3)
      val gap = extractIntAttr(raw, "gap").filter(_ >= 0).getOrElse(16)
      val minWidth = extractIntAttr(raw, "minWidth").filter(_ > 0).getOrElse(220)
      s"""<div class="WikiColumns" style="column-count: $count; column-gap: ${gap}px; --column-min-width: ${minWidth}px;">"""
    }

    private def renderAllowedAttributes(attrsRaw: String): String = {
      val raw = Option(attrsRaw).getOrElse("")
      val allowedKeys = Set("id", "class", "style")
      val attrPattern = """([a-zA-Z_:][-a-zA-Z0-9_:.]*)\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s"'>]+))""".r
      val sanitized = attrPattern.findAllMatchIn(raw).flatMap { m =>
        val key = Option(m.group(1)).map(_.toLowerCase).getOrElse("")
        if (!allowedKeys.contains(key)) None
        else {
          val value = Option(m.group(2)).orElse(Option(m.group(3))).orElse(Option(m.group(4))).getOrElse("")
          Some(s"""$key="${escapeHtmlAttr(value)}"""")
        }
      }.toSeq.mkString(" ")
      if (sanitized.nonEmpty) s" $sanitized" else ""
    }

    private def extractIntAttr(rawAttrs: String, key: String): Option[Int] = {
      (s"""\\b$key\\s*=\\s*"([0-9]+)"""".r.findFirstMatchIn(rawAttrs).map(_.group(1))
        .orElse(s"""\\b$key\\s*=\\s*'([0-9]+)'""".r.findFirstMatchIn(rawAttrs).map(_.group(1)))
        .orElse(s"""\\b$key\\s*=\\s*([0-9]+)""".r.findFirstMatchIn(rawAttrs).map(_.group(1))))
        .flatMap(v => scala.util.Try(v.toInt).toOption)
    }

    private def escapeHtmlAttr(v: String): String =
      v
        .replace("&", "&amp;")
        .replace("\"", "&quot;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
  }

  private def buildHeadingLineRangeByLineStart(content: String, regexHeading: Regex): Map[Int, Int] = {
    val lines = content.split("""\r\n|\n""", -1).toVector
    val headings = lines.zipWithIndex.flatMap { case (line, index) =>
      line match {
        case regexHeading(heading, _, _, _, _) => Some((index + 1, heading.length))
        case _ => None
      }
    }

    headings.map { case (lineStart, level) =>
      val lineEnd = headings
        .dropWhile(_._1 <= lineStart)
        .collectFirst { case (nextLineStart, nextLevel) if nextLevel <= level => nextLineStart - 1 }
        .getOrElse(lines.length)
      lineStart -> lineEnd
    }.toMap
  }

  private def getRevisionForPartialEdit(implicit wikiContext: ContextWikiPage): Long = {
    wikiContext.requestWrapper
      .getQueryString("revision")
      .flatMap(v => scala.util.Try(v.toLong).toOption)
      .filter(_ > 0)
      .getOrElse {
        val (database, site) = wikiContext.tupleDatabaseSite
        database.withConnection { implicit connection =>
          implicit val implicitSite: models.tables.Site = site
          models.tables.Page.selectLastRevision(wikiContext.name).map(_.revision).getOrElse(0L)
        }
      }
  }

  private def getEditUrlForPartialEdit(revision: Long, lineStart: Int, lineEnd: Int)(implicit wikiContext: ContextWikiPage): String = {
    val nameEncoded = java.net.URLEncoder.encode(wikiContext.name, "UTF-8").replace("+", "%20")
    s"/w/$nameEncoded?action=edit&revision=$revision&lineStart=$lineStart&lineEnd=$lineEnd"
  }

  class HandlerToSeqLink(override val pageContent: PageContent)(implicit wikiContext:ContextWikiPage) extends Handler[Seq[CalculatedLink]](pageContent) {
    override def process(): Seq[CalculatedLink] = {
      val seqLinkInterpreter: Seq[CalculatedLink] = extractConvertInjectInterpreter.extractLink().toList
      val seqLinkMacro: Seq[CalculatedLink] = extractConvertInjectMacro.extractLink().map(AhaMarkLink(_).toLink(wikiContext.name)).toList
      val seqLinkWikiText: Seq[CalculatedLink] = InterpreterWiki.extractLinkMarkup(backQuoteExtracted).map(_.toLink(wikiContext.name)).filterNot(_.dst.startsWith("[")).toList
      seqLinkInterpreter ++ seqLinkMacro ++ seqLinkWikiText
    }
  }

  class HandlerToSeqSchemaOrg(override val pageContent: PageContent)(implicit wikiContext:ContextWikiPage) extends Handler[Seq[CalculatedSchemaOrg]](pageContent) {
    override def process(): Seq[CalculatedSchemaOrg] = {
      extractConvertInjectInterpreter.extractSchemaOrg()
    }
  }


  val regexLink: Regex =
    """(?x)
          ((?<!\\)\\)?                                  # 1: Optional escape
          (?:
            ([a-zA-Z][-a-zA-Z0-9+._]+ :// \S+)    |     # 2: URL with scheme (http://...)
            \[" ([^]"]+) "]                       |     # 3: ["Page"]
            \[ ([^]\s]+) ]                        |     # 4: [FrontPage]
            \[" ([^]"]+) " \s+ ([^]]+) ]          |     # 5+6: ["Page" Alias]
            \[ ([^]\s]+) \s+ ([^]]+) ]                  # 7+8: [Page Alias]
          )
    """.r

  def replaceLink(s:String)(implicit wikiContext:ContextWikiPage):String = {
    val set: Set[String] = wikiContext.setPageNameByPermission

    regexLink.replaceAllIn(s, _ match {
      case regexLink(null, uri , null, null, null, null, null, null) => Regex.quoteReplacement(AhaMarkLink(uri).toHtmlString())
      case regexLink(null, null, uri , null, null, null, null, null) => Regex.quoteReplacement(AhaMarkLink(uri).toHtmlString(set))
      case regexLink(null, null, null, uri , null, null, null, null) => Regex.quoteReplacement(AhaMarkLink(uri).toHtmlString(set))
      case regexLink(null, null, null, null, uri, alias, null, null) => Regex.quoteReplacement(AhaMarkLink(uri, alias).toHtmlString(set))
      case regexLink(null, null, null, null, null, null, uri, alias) => Regex.quoteReplacement(AhaMarkLink(uri, alias).toHtmlString(set))

      case regexLink(_   , uri , null, null, null, null, null, null) => Regex.quoteReplacement(uri)
      case regexLink(_   , null, uri , null, null, null, null, null) => Regex.quoteReplacement(s"""["$uri"]""")
      case regexLink(_   , null, null, uri , null, null, null, null) => Regex.quoteReplacement(s"[$uri]")
      case regexLink(_   , null, null, null, uri , alia, null, null) => Regex.quoteReplacement(s"""["$uri" $alia]""")
      case regexLink(_   , null, null, null, null, null, uri , alia) => Regex.quoteReplacement(s"""[$uri $alia]""")

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
    }.filter(_ != null).filterNot(_.uri.startsWith("#"))
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

  override def toSeqLink(content:String)(implicit wikiContext: ContextWikiPage):Seq[CalculatedLink] = {
    val pageContent: PageContent = PageContent(content)
    pageContent.redirect match {
      case Some(v) => Seq(CalculatedLink(wikiContext.nameTop, v, "redirect"))
      case None =>
        val pageContent: PageContent = PageContent(content)
        val handler = new HandlerToSeqLink(pageContent)
        handler.process()
    }
  }

  override def toSeqSchemaOrg(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedSchemaOrg] = {
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
