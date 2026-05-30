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

    s"""<div class="paperContent $cssClass">""" +
      bodyResolved.split("""(?m)^-{4,}$""").map(InterpreterWiki.toHtmlString)
        .zipWithIndex
        .map { case (s, index) =>
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
             |      $s
             |    </div>
             |  </div>
             |  <!-- $index -->
             |</div>""".stripMargin
        }.mkString("\n") +
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
