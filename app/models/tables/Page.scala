package models.tables

import java.sql.Connection
import java.util.Date

import anorm._
import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.RangeUtil
import models.WithDateTime
import models.tables

import scala.collection.immutable
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


object Page {
  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  private val rowParser = str("name") ~ long("revision") ~ date("dateTime") ~ str("author") ~ str("remoteAddress") ~ str("comment") ~ str("permRead") ~ str("content")

  def selectCount()(implicit connection: Connection, site: Site): Long = {
    SQL"SELECT COUNT(*) cnt FROM Page WHERE site = ${site.seq}".as(long("cnt") single)
  }

  def select(name: String, revision: Int)(implicit connection: Connection, site: Site): Option[Page] = {
    if (revision == 0) {
      selectLastRevision(name)
    } else {
      selectSpecificRevision(name, revision)
    }
  }

  def selectLastRevision(name: String)(implicit connection: Connection, site: Site): Option[Page] = {
    SQL"SELECT name, revision, dateTime, author, remoteAddress, comment, IFNULL(permRead, '') permRead, content FROM Page WHERE site = ${site.seq} AND name = $name ORDER BY revision DESC LIMIT 1"
      .as(rowParser singleOpt).map(flatten)
      .map(Page.tupled)
  }

  def selectFirstRevision(name: String)(implicit connection: Connection, site: Site): Option[Page] = {
    SQL"SELECT name, revision, dateTime, author, remoteAddress, comment, IFNULL(permRead, '') permRead, content FROM Page WHERE site = ${site.seq} AND name = $name ORDER BY revision ASC LIMIT 1"
      .as(rowParser singleOpt).map(flatten)
      .map(Page.tupled)
  }

  def selectSpecificRevision(name: String, revision: Int)(implicit connection: Connection, site: Site): Option[Page] = {
    SQL"SELECT name, revision, dateTime, author, remoteAddress, comment, IFNULL(permRead, '') permRead, content FROM Page WHERE site = ${site.seq} AND name = $name AND revision = $revision ORDER BY revision ASC LIMIT 1"
      .as(rowParser singleOpt).map(flatten)
      .map(Page.tupled)
  }

  def selectHistory(name: String)(implicit connection: Connection, site: Site): List[PageWithoutContent] = {
    SQL"SELECT name, revision, dateTime, author, remoteAddress, comment, IFNULL(permRead, '') permRead FROM Page WHERE site = ${site.seq} AND name = $name ORDER BY revision DESC"
      .as(str("name") ~ long("revision") ~ date("dateTime") ~ str("author") ~ str("remoteAddress") ~ str("comment") ~ str("permRead") *).map(flatten)
      .map(PageWithoutContent.tupled)
  }

  def selectHistoryStream[T](name: String, t:T, f:(T, Page) => T)(implicit connection: Connection, site: Site): T = {
    SQL"SELECT name, revision, dateTime, author, remoteAddress, content, comment, IFNULL(permRead, '') permRead FROM Page WHERE site = ${site.seq} AND name = $name ORDER BY revision ASC"
      .as(rowParser *).map(flatten)
      .foldLeft(t)((a, v) => f(a, Page.tupled(v)))
  }

  def insert(p: Page)(implicit connection: Connection, site: Site): Option[Long] = {
    SQL"""
           INSERT INTO Page
           (site, name, revision, dateTime, author, remoteAddress, comment, permRead, content) values
           (${site.seq}, ${p.name}, ${p.revision}, ${p.dateTime}, ${p.author}, ${p.remoteAddress}, ${p.comment}, ${p.permRead}, ${p.content})
        """.executeInsert()
  }

  def deleteLinkCosignSimilarityTermFrequency(name: String)(implicit connection: Connection, site: Site): Int = {
    val linkCount = Link.delete(name)
    val cosineSimilarityCount = CosineSimilarity.delete(name)
    val termFrequencyCount = TermFrequency.delete(name)
    val schemaOrgCount = SchemaOrg.delete(name)
    linkCount + cosineSimilarityCount + termFrequencyCount + schemaOrgCount
  }

  def deleteWithRelatedData(name:String)(implicit connection: Connection, site: Site): Int = {
    deleteLinkCosignSimilarityTermFrequency(name)
    SQL"DELETE FROM Page WHERE site = ${site.seq} AND name = $name".executeUpdate()
  }

  def deleteSpecificRevisionWithRelatedData(name:String, revision:Long)(implicit connection: Connection, site: Site): Int = {
    deleteLinkCosignSimilarityTermFrequency(name)
    SQL"DELETE FROM Page WHERE site = ${site.seq} AND name = $name AND revision = $revision".executeUpdate()
  }

  def rename(name: String, newName: String)(implicit connection: Connection, site: Site): Int = {
    deleteLinkCosignSimilarityTermFrequency(name)
    SQL"UPDATE Page SET name = $newName WHERE site = ${site.seq} AND name = $name".executeUpdate()
  }

  def updateSimilarPage(name: String, wordCount: Map[String, Int])(implicit connection: Connection, site: Site): Int = {
    TermFrequency.delete(name)
    TermFrequency.insert(name, wordCount)
    CosineSimilarity.recalc(name)
  }

  def updateLink(name: String, seqLink: Seq[Link])(implicit connection: Connection, site: Site): Array[Int] = {
    Link.delete(name)
    Link.insert(seqLink)
  }

  def updateSchemaOrg(name:String, seqSchemaOrg: Seq[SchemaOrg])(implicit connection: Connection, site: Site): Array[Int] = {
    SchemaOrg.delete(name)
    SchemaOrg.insert(seqSchemaOrg)
  }

  // TODO: remove IFNULL(permRead) and fix schema
  def pageSelectPageList()(implicit connection: Connection, site: Site): List[PageWithoutContentWithSize] = {
    SQL"""
        SELECT w.name, w.revision, w.dateTime, w.author, w.remoteAddress, w.comment, IFNULL(w.permRead, '') permRead, LENGTH(content) size
            FROM Page w
            INNER JOIN (
                SELECT
                    site, name, MAX(revision) revision
                    FROM Page
                    WHERE site = ${site.seq}
                    GROUP BY site, name
                    ORDER BY MAX(dateTime) DESC
            ) NV ON w.site = NV.site AND w.name = NV.name AND w.revision = NV.revision
            ORDER BY name
        """
      .as(str("name") ~ long("revision") ~ date("dateTime") ~ str("author") ~ str("remoteAddress") ~ str("comment") ~ str("permRead") ~ long("size") *).map(flatten)
      .map(PageWithoutContentWithSize.tupled)
  }

  def selectYmdCountOfFirstRevision()(implicit connection: Connection, site: Site): Seq[(String, Long)] = {
    SQL"""SELECT
               DATE_FORMAT(dateTime, '%Y-%m-%d') ymd, COUNT(*) cnt
               FROM Page w
               WHERE site = ${site.seq} AND revision = 1
               GROUP BY DATE_FORMAT(dateTime, '%Y-%m-%d')
               ORDER BY DATE_FORMAT(dateTime, '%Y-%m-%d')
           """
      .as(str("ymd") ~ long("cnt") *).map(flatten)
  }

  def pageSearch(q:String)(implicit connection: Connection, site: Site): immutable.Seq[SearchResult] = {
    SQL"""
SELECT w.name, w.revision, w.dateTime, w.author, w.remoteAddress, w.comment, IFNULL(w.permRead, '') permRead, w.content
     FROM Page w
     INNER JOIN (
         SELECT
             site, name, MAX(revision) revision
             FROM Page
             WHERE site = ${site.seq}
             GROUP BY site, name
     ) NV ON w.site = NV.site AND w.name = NV.name AND w.revision = NV.revision
     WHERE
         w.name LIKE CONCAT('%', $q, '%') COLLATE utf8mb4_general_ci OR
         w.content LIKE CONCAT('%', $q, '%') COLLATE utf8mb4_general_ci
     ORDER BY w.name"""
      .as(str("name") ~ str("content") ~ date("dateTime") *).map(flatten).map(SearchResult.tupled)
  }


  def pageSelectNameWhereNoCosineSimilarity()(implicit connection: Connection, site: Site): Option[String] = {
    SQL"""SELECT
              name
              FROM (
                  SELECT DISTINCT(name) name FROM Page WHERE site = ${site.seq}
              ) w
              WHERE name NOT IN (
                  SELECT DISTINCT(name1) FROM CosineSimilarity WHERE site = ${site.seq}
              )
              ORDER BY RAND()
              LIMIT 1
           """
      .as(str("name") singleOpt)
  }

  def pageSelectNameWhereNoLinkSrc()(implicit connection: Connection, site: Site): Seq[String] = {
    SQL"""SELECT
             name
             FROM (
                 SELECT DISTINCT(name) name FROM Page WHERE site = ${site.seq}
             ) w
             WHERE name NOT IN (
                 SELECT DISTINCT(src) FROM Link WHERE site = ${site.seq}
             )
             ORDER BY RAND()
             LIMIT 1000
          """
      .as(str("name") *)
  }
}
