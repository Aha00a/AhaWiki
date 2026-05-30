package logics.wikis.interpreters

import logics.wikis.ExtractConvertInjectVariable
import models.{PageContent, ContextWikiPage}

object InterpreterPaper extends TraitInterpreter {

  import models.tables.CalculatedLink

  //noinspection ZeroIndexToHead
  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)

    // Step 1: #!var 디렉티브 변수 시드 + [[[#!Variable]]] 블록 추출·파싱
    //         (split 전에 처리해야 모든 페이지에서 {{key}} 치환이 동작함)
    val eciv = new ExtractConvertInjectVariable()
    eciv.variables ++= pageContent.variables          // #!var key=value
    val bodyExtracted = eciv.extract(pageContent.content)  // [[[#!Variable]]] 블록도 추가
    val bodyResolved  = eciv.applyVariables(bodyExtracted)

    // Step 2: 인수 우선, 없으면 변수에서 fallback
    val cssClass = Option(pageContent.argument(0)).filter(_.nonEmpty)
      .getOrElse(eciv.variables.getOrElse("class", ""))
    val docId = Option(pageContent.argument(1)).filter(_.nonEmpty)
      .getOrElse(eciv.variables.getOrElse("docId", ""))

    // Step 3: ---- 기준으로 split하되, 각 청크 앞에 \n × offset을 붙여 원본 줄 번호 보존
    //   lineOffset[N+1] = lineOffset[N] + chunks[N].count('\n')
    //   (chunk의 선행 \n이 separator 줄을 포함하므로 +1 불필요)
    val chunks = bodyExtracted.split("""(?m)^-{4,}$""", -1)
    // lineOffset starts at the number of directive lines (#! lines stripped by PageContent),
    // so that padded content line numbers map to raw-document line numbers.
    var lineOffset = pageContent.directives.length
    val pages = chunks.zipWithIndex.map { case (chunk, index) =>
      val offset = lineOffset
      lineOffset += chunk.count(_ == '\n')
      val paddedResolved = "\n" * offset + eciv.applyVariables(chunk)
      val rendered = InterpreterWiki.toHtmlString(paddedResolved)
      s"""<div class="page">
         |  <!-- $index -->
         |  <div class="pageHeader">
         |    <div class="documentId">$docId</div>
         |  </div>
         |  <div class="pageFooter">
         |    <div class="pageNo">${index + 1}</div>
         |  </div>
         |  <div class="pageContent">
         |    <div>
         |      $rendered
         |    </div>
         |  </div>
         |  <!-- $index -->
         |</div>""".stripMargin
    }

    s"""<div class="paperContent $cssClass">""" +
      pages.mkString("\n") +
      """</div>"""
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = {
    val pageContent: PageContent = PageContent(content)
    val eciv = new ExtractConvertInjectVariable()
    eciv.variables ++= pageContent.variables
    val bodyExtracted = eciv.extract(pageContent.content)
    val bodyResolved  = eciv.applyVariables(bodyExtracted)
    InterpreterWiki.toSeqLink(bodyResolved)
  }
}
