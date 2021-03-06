package logics.wikis.interpreters

import logics.wikis.RenderingMode
import models.{PageContent, ContextWikiPage}

import scala.collection.mutable

object InterpreterGraph extends TraitInterpreter {

  import models.tables.Link

  private def parse(wikiContext: ContextWikiPage, pageContent: PageContent): Array[Array[String]] = {
    val lines = pageContent.content.trim.split("""(\r\n|\n)+""")
    val linesCut = wikiContext.renderingMode match {
      case RenderingMode.Normal => lines
      case RenderingMode.Preview => lines.take(100)
    }
    val array: Array[Array[String]] = linesCut.flatMap(_.split("->").sliding(2).map(_.toArray))
    array
  }
  override def toHtmlString(content: String)(implicit wikiContext: ContextWikiPage): String = {
    val pageContent: PageContent = PageContent(content)
    val array = parse(wikiContext, pageContent)
    views.html.Wiki.graph(array, enableWikiLink = pageContent.shebang.contains("enableWikiLink")).toString()
  }

  override def toSeqLink(content: String)(implicit wikiContext: ContextWikiPage): Seq[Link] = Seq()
}
