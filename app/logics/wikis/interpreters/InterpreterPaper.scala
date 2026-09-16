package logics.wikis.interpreters

import models.{PageContent, ContextWikiPage}

object InterpreterPaper extends TraitInterpreter {

  import models.tables.CalculatedLink

  //noinspection ZeroIndexToHead
  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)

    // The split, variable seeding and per-chunk render are shared with InterpreterSlide; Paper only
    // wraps each chunk into an A4 page with a 6-position header/footer.
    val (eciv, chunks) = PaperSlideCore.render(content)

    // 인수 우선, 없으면 변수에서 fallback
    val cssClass = pageContent.argument.lift(0).filter(_.nonEmpty)
      .getOrElse(eciv.variables.getOrElse("class", ""))
    val docId = pageContent.argument.lift(1).filter(_.nonEmpty)
      .getOrElse(eciv.variables.getOrElse("docId", ""))

    // header/footer 6-position templates — {{pageNo}} 등이 페이지별로 치환됨
    // topRight 기본값 = docId (하위호환), bottomRight 기본값 = {{pageNo}} (페이지번호 유지)
    val topLeftRaw      = eciv.variables.getOrElse("topLeft",      "")
    val topCenterRaw    = eciv.variables.getOrElse("topCenter",    "")
    val topRightRaw     = eciv.variables.getOrElse("topRight",     docId)
    val bottomLeftRaw   = eciv.variables.getOrElse("bottomLeft",   "")
    val bottomCenterRaw = eciv.variables.getOrElse("bottomCenter", "")
    val bottomRightRaw  = eciv.variables.getOrElse("bottomRight",  "{{pageNo}} / {{pageTotal}}")

    val pages = chunks.map { c =>
      // {{pageNo}} in the header/footer templates is resolved here, so set it to this chunk's number
      // (the core left it at the last chunk's). {{pageTotal}} is constant and already set.
      eciv.variables("pageNo") = c.pageNo.toString
      val pageExtraClass = if (c.isLandscape) " landscape" else if (c.isPortrait) " portrait" else ""
      s"""<div class="page$pageExtraClass">
         |  <!-- ${c.index} -->
         |  <div class="pageHeader">
         |    <div class="topLeft">${eciv.applyVariables(topLeftRaw)}</div>
         |    <div class="topCenter">${eciv.applyVariables(topCenterRaw)}</div>
         |    <div class="topRight">${eciv.applyVariables(topRightRaw)}</div>
         |  </div>
         |  <div class="pageFooter">
         |    <div class="bottomLeft">${eciv.applyVariables(bottomLeftRaw)}</div>
         |    <div class="bottomCenter">${eciv.applyVariables(bottomCenterRaw)}</div>
         |    <div class="bottomRight">${eciv.applyVariables(bottomRightRaw)}</div>
         |  </div>
         |  <div class="pageContent">
         |    <div>
         |      ${c.html}
         |    </div>
         |  </div>
         |  <!-- ${c.index} -->
         |</div>""".stripMargin
    }

    s"""<div class="paperContent $cssClass">""" +
      pages.mkString("\n") +
      """</div>"""
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] =
    PaperSlideCore.toSeqLink(content)
}
