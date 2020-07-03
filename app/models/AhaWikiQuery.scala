package models

import java.sql.Connection
import java.util.Date

import anorm.SqlParser._
import anorm._
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.RangeUtil
import logics.wikis.PageNameLogic

import scala.collection.immutable
import scala.language.postfixOps
import scala.util.matching.Regex

case class Page                        (name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String, comment: String, permRead: String, content: String) extends WithDateTime
case class PageWithoutContent          (name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String, comment: String, permRead: String) extends WithDateTime
case class PageWithoutContentWithSize  (name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String, comment: String, permRead: String, size: Long) extends WithDateTime





case class SearchResultSummary(name: String, summary:Seq[Seq[(Int, String)]], dateTime: Date)

case class SearchResult(name:String, content:String, dateTime: Date) {
  def summarise(q: String): SearchResultSummary = {
    val lines = content.split("""(\r\n|\n)+""").toSeq
    SearchResultSummary(
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






object AhaWikiQuery {
  def apply()(implicit connection: Connection): AhaWikiQuery = new AhaWikiQuery()
}

class AhaWikiQuery()(implicit connection: Connection) {

  import models.tables.HighScoredTerm

  object Page {

    import models.tables.Link
    import models.tables.SchemaOrg

    private val rowParser = str("name") ~ long("revision") ~ date("dateTime") ~ str("author") ~ str("remoteAddress") ~ str("comment") ~ str("permRead") ~ str("content")

    def selectCount(): Long = {
      SQL("SELECT COUNT(*) cnt FROM Page").as(long("cnt") single)
    }

    def select(name: String, revision: Int): Option[Page] = {
      if (revision == 0) {
        selectLastRevision(name)
      } else {
        selectSpecificRevision(name, revision)
      }
    }

    def selectLastRevision(name: String): Option[Page] = {
      SQL("SELECT name, revision, dateTime, author, remoteAddress, comment, IFNULL(permRead, '') permRead, content FROM Page WHERE name = {name} ORDER BY revision DESC LIMIT 1")
        .on('name -> name)
        .as(rowParser singleOpt).map(flatten)
        .map(models.Page.tupled)
    }

    def selectFirstRevision(name: String): Option[Page] = {
      SQL("SELECT name, revision, dateTime, author, remoteAddress, comment, IFNULL(permRead, '') permRead, content FROM Page WHERE name = {name} ORDER BY revision ASC LIMIT 1")
        .on('name -> name)
        .as(rowParser singleOpt).map(flatten)
        .map(models.Page.tupled)
    }

    def selectSpecificRevision(name: String, revision: Int): Option[Page] = {
      SQL("SELECT name, revision, dateTime, author, remoteAddress, comment, IFNULL(permRead, '') permRead, content FROM Page WHERE name = {name} AND revision = {revision} ORDER BY revision ASC LIMIT 1")
        .on('name -> name, 'revision -> revision)
        .as(rowParser singleOpt).map(flatten)
        .map(models.Page.tupled)
    }

    def selectHistory(name: String): List[PageWithoutContent] = {
      SQL"SELECT name, revision, dateTime, author, remoteAddress, comment, IFNULL(permRead, '') permRead FROM Page WHERE name = $name ORDER BY revision DESC"
        .as(str("name") ~ long("revision") ~ date("dateTime") ~ str("author") ~ str("remoteAddress") ~ str("comment") ~ str("permRead") *).map(flatten)
        .map(PageWithoutContent.tupled)
    }

    def selectHistoryStream[T](name: String, t:T, f:(T, models.Page) => T): T = {
      SQL"SELECT name, revision, dateTime, author, remoteAddress, content, comment, IFNULL(permRead, '') permRead FROM Page WHERE name = $name ORDER BY revision ASC"
        .as(rowParser *).map(flatten)
        .foldLeft(t)((a, v) => f(a, models.Page.tupled(v)))
    }

    def insert(p: Page): Option[Long] = {
      SQL"""
           INSERT INTO Page
           (name, revision, dateTime, author, remoteAddress, comment, permRead, content) values
           (${p.name}, ${p.revision}, ${p.dateTime}, ${p.author}, ${p.remoteAddress}, ${p.comment}, ${p.permRead}, ${p.content})
        """.executeInsert()
    }

    def deleteLinkCosignSimilarityTermFrequency(name: String)(implicit connection:Connection): Int = {
      import models.tables.CosineSimilarity
      import models.tables.TermFrequency
      val linkCount = Link.delete(name)
      val cosineSimilarityCount = CosineSimilarity.delete(name)
      val termFrequencyCount = TermFrequency.delete(name)
      linkCount + cosineSimilarityCount + termFrequencyCount
    }

    def deleteWithRelatedData(name:String): Int = {
      deleteLinkCosignSimilarityTermFrequency(name)
      SQL"DELETE FROM Page WHERE name = $name".executeUpdate()
    }

    def deleteSpecificRevisionWithRelatedData(name:String, revision:Long): Int = {
      deleteLinkCosignSimilarityTermFrequency(name)
      SQL"DELETE FROM Page WHERE name = $name AND revision = $revision".executeUpdate()
    }

    def rename(name: String, newName: String): Int = {
      deleteLinkCosignSimilarityTermFrequency(name)
      SQL"UPDATE Page SET name = $newName WHERE name = $name".executeUpdate()
    }

    def updateSimilarPage(name: String, wordCount: Map[String, Int])(implicit connection:Connection): Int = {
      import models.tables.CosineSimilarity
      import models.tables.TermFrequency
      TermFrequency.delete(name)
      TermFrequency.insert(name, wordCount)
      CosineSimilarity.recalc(name)
    }

    def updateLink(name: String, seqLink: Seq[Link])(implicit connection:Connection): Array[Int] = {
      Link.delete(name)
      Link.insert(seqLink)
    }

    def updateSchemaOrg(name:String, seqSchemaOrg: Seq[SchemaOrg])(implicit connection:Connection): Array[Int] = {
      SchemaOrg.delete(name)
      SchemaOrg.insert(seqSchemaOrg)
    }

  }


  // TODO: remove IFNULL(permRead) and fix schema
  def pageSelectPageList(): List[PageWithoutContentWithSize] = {
    SQL( """SELECT w.name, w.revision, w.dateTime, w.author, w.remoteAddress, w.comment, IFNULL(w.permRead, '') permRead, LENGTH(content) size
           |    FROM Page w
           |    INNER JOIN (
           |        SELECT
           |            name, MAX(revision) revision
           |            FROM Page
           |            GROUP BY name
           |            ORDER BY MAX(dateTime) DESC
           |    ) NV ON w.name = NV.name AND w.revision = NV.revision
           |    ORDER BY name
           |""".stripMargin)
      .as(str("name") ~ long("revision") ~ date("dateTime") ~ str("author") ~ str("remoteAddress") ~ str("comment") ~ str("permRead") ~ long("size") *).map(flatten)
      .map(PageWithoutContentWithSize.tupled)
  }

  def pageSearch(q:String): immutable.Seq[SearchResult] = {
    SQL("""
SELECT w.name, w.revision, w.dateTime, w.author, w.remoteAddress, w.comment, IFNULL(w.permRead, '') permRead, w.content
     FROM Page w
     INNER JOIN (
         SELECT
             name, MAX(revision) revision
             FROM Page
             GROUP BY name
     ) NV ON w.name = NV.name AND w.revision = NV.revision
     WHERE
         w.name LIKE CONCAT('%', {q}, '%') COLLATE utf8mb4_general_ci OR
         w.content LIKE CONCAT('%', {q}, '%') COLLATE utf8mb4_general_ci
     ORDER BY w.name""")
      .on('q -> q)
      .as(str("name") ~ str("content") ~ date("dateTime") *).map(flatten).map(SearchResult.tupled)
  }


  def pageSelectNameWhereNoCosineSimilarity(): Option[String] = {
    SQL( """SELECT
           |    name
           |    FROM (
           |        SELECT DISTINCT(name) name FROM Page
           |    ) w
           |    WHERE name NOT IN (
           |        SELECT DISTINCT(name1) FROM CosineSimilarity
           |    )
           |    ORDER BY RAND()
           |    LIMIT 1
           | """.stripMargin)
      .as(str("name") singleOpt)
  }

  def pageSelectNameWhereNoLinkSrc(): Seq[String] = {
    SQL( """SELECT
           |    name
           |    FROM (
           |        SELECT DISTINCT(name) name FROM Page
           |    ) w
           |    WHERE name NOT IN (
           |        SELECT DISTINCT(src) FROM Link
           |    )
           |    ORDER BY RAND()
           |    LIMIT 100
           | """.stripMargin)
      .as(str("name") *)
  }

}