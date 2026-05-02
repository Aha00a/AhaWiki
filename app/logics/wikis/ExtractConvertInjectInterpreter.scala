package logics.wikis

import com.aha00a.commons.utils.ShebangUtil
import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage
import models.tables.Page

class ExtractConvertInjectInterpreter() extends ExtractConvertInject {
  private case class InterpreterChunk(lineStart: Int, lineEnd: Int)
  private val chunkMap = scala.collection.mutable.Map.empty[String, InterpreterChunk]

  import models.tables.CalculatedLink
  import models.tables.CalculatedSchemaOrg

  override def extract(s: String): String = {
    if (s == null || !s.contains("[[[") || !s.contains("]]]")) {
      s
    } else {
      val open = "[[["
      val close = "]]]"
      val builder = new StringBuilder
      var cursor = 0
      var openIndex = s.indexOf(open, cursor)

      while (openIndex >= 0) {
        val closeIndex = s.indexOf(close, openIndex + open.length)
        if (closeIndex < 0) {
          builder.append(s.substring(cursor))
          return builder.toString()
        }

        val uniqueKey = getUniqueKey
        val body = s.substring(openIndex + open.length, closeIndex)

        arrayBuffer += uniqueKey -> body
        chunkMap += uniqueKey -> InterpreterChunk(
          lineStart = lineNumber(s, openIndex),
          lineEnd = lineNumber(s, closeIndex + close.length - 1)
        )

        builder.append(s.substring(cursor, openIndex))
        builder.append(uniqueKey)
        builder.append("\n" * body.count(_ == '\n'))
        cursor = closeIndex + close.length
        openIndex = s.indexOf(open, cursor)
      }

      builder.append(s.substring(cursor))
      builder.toString()
    }
  }

  override def convert(s: String)(implicit wikiContext: ContextWikiPage): String = Interpreters.toHtmlString(ShebangUtil.addWhenNotExist(s, "text"))

  override def inject(s: String)(implicit wikiContext: ContextWikiPage): String = {
    var result = s
    val revision = getRevision
    for ((key, value) <- arrayBuffer) {
      val converted = Interpreters.toHtmlString(ShebangUtil.addWhenNotExist(value, "text"))
      val withMeta = chunkMap.get(key) match {
        case Some(chunk) =>
          val editUrl = getEditUrl(revision, chunk.lineStart, chunk.lineEnd)
          val editTitle = s"Edit (r$revision, L${chunk.lineStart}-L${chunk.lineEnd})"
          s"""<div class="InterpreterRenderMetaWrapper" style="position: relative;"
             |  data-edit-link="$editUrl"
             |  data-line-start="${chunk.lineStart}"
             |  data-line-end="${chunk.lineEnd}"
             |  data-edit-title="$editTitle">
             |  <div class="InterpreterRenderContent">$converted</div>
             |</div>""".stripMargin
        case None =>
          converted
      }
      result = result.replace(key, withMeta)
    }
    result
  }

  def extractLink()(implicit wikiContext: ContextWikiPage): Seq[CalculatedLink] = {
    arrayBuffer.map(_._2).flatMap(c => Interpreters.toSeqLink(c)).toSeq
  }

  def extractSchemaOrg()(implicit wikiContext: ContextWikiPage): Seq[CalculatedSchemaOrg] = {
    arrayBuffer.map(_._2).flatMap(c => Interpreters.toSeqSchemaOrg(c)).toSeq
  }

  private def lineNumber(s: String, charIndexInclusive: Int): Int = {
    if (charIndexInclusive <= 0) {
      1
    } else {
      s.substring(0, charIndexInclusive + 1).count(_ == '\n') + 1
    }
  }

  private def getRevision(implicit wikiContext: ContextWikiPage): Long = {
    wikiContext.requestWrapper
      .getQueryString("revision")
      .flatMap(v => scala.util.Try(v.toLong).toOption)
      .filter(_ > 0)
      .getOrElse {
        val (database, site) = wikiContext.tupleDatabaseSite
        database.withConnection { implicit connection =>
          implicit val implicitSite: models.tables.Site = site
          Page.selectLastRevision(wikiContext.name).map(_.revision).getOrElse(0L)
        }
      }
  }

  private def getEditUrl(revision: Long, lineStart: Int, lineEnd: Int)(implicit wikiContext: ContextWikiPage): String = {
    val nameEncoded = java.net.URLEncoder.encode(wikiContext.name, "UTF-8").replace("+", "%20")
    s"/w/$nameEncoded?action=edit&revision=$revision&lineStart=$lineStart&lineEnd=$lineEnd"
  }
}
