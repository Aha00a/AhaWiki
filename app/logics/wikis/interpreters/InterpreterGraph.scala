package logics.wikis.interpreters

import logics.wikis.RenderingMode
import models.{Link, PageContent, WikiContext}

import scala.collection.mutable

object InterpreterGraph extends TraitInterpreter {
  private def parse(wikiContext: WikiContext, pageContent: PageContent): Array[Array[String]] = {
    val lines = pageContent.content.trim.split("""(\r\n|\n)+""")
    val linesCut = wikiContext.renderingMode match {
      case RenderingMode.Normal => lines
      case RenderingMode.Preview => lines.take(100)
    }
    val array: Array[Array[String]] = linesCut.flatMap(_.split("->").sliding(2).map(_.toArray))
    array
  }
  override def interpret(content: String)(implicit wikiContext: WikiContext): String = {
    val pageContent: PageContent = PageContent(content)
    val array = parse(wikiContext, pageContent)
    views.html.Wiki.graph(array, enableWikiLink = pageContent.shebang.contains("enableWikiLink")).toString()
  }


  override def extractWord(content: String)(implicit wikiContext: WikiContext): Seq[String] = {
    val pageContent: PageContent = PageContent(content)
    val array = parse(wikiContext, pageContent)
    array.flatten.toSeq
  }

  override def extractLink(content: String)(implicit wikiContext: WikiContext): Seq[Link] = Seq()
}
