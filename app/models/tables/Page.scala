package models.tables

import java.util.Date

import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.RangeUtil
import models.WithDateTime
import models.tables

import scala.util.matching.Regex

case class Page                        (name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String, comment: String, permRead: String, content: String) extends WithDateTime
case class PageWithoutContent          (name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String, comment: String, permRead: String) extends WithDateTime
case class PageWithoutContentWithSize  (name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String, comment: String, permRead: String, size: Long) extends WithDateTime


case class SearchResult(name:String, content:String, dateTime: Date) {

  def summarise(q: String): SearchResultSummary = {
    val lines = content.split("""(\r\n|\n)+""").toSeq
    tables.SearchResultSummary(
      name,
      lines
        .zipWithIndex
        .filter(s => s"(?i)${Regex.quote(q)}".r.findFirstIn(s._1).isDefined)
        .map(_._2)
        .flatMap(RangeUtil.around(_, 3))
        .distinct
        .filter(lines.isDefinedAt)
        .splitBy((a, b) => a + 1 != b)
        .map(_.map(i => (i + 1, lines(i)))).toSeq,
      dateTime
    )
  }
}

case class SearchResultSummary(name: String, summary:Seq[Seq[(Int, String)]], dateTime: Date)

