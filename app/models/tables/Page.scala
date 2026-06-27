package models.tables

import anorm.SqlParser._
import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm.SqlParser.bool
import anorm._
import com.aha00a.commons.Implicits._
import com.aha00a.commons.utils.RangeUtil
import com.aha00a.play.AnormSqlParser._
import models.WithDateTime
import models.tables

import java.sql.Connection
import java.sql.Savepoint
import java.sql.SQLIntegrityConstraintViolationException
import java.time.LocalDateTime
import scala.collection.immutable
import scala.util.matching.Regex

case class Page                        (name: String, revision: Long, dateTime: LocalDateTime, nickname: Option[String], user: Option[Long], remoteAddress: String, comment: String, isMinorEdit: Boolean, content: String, viaApi: Boolean = false, userApiKey: Option[Long] = None) extends WithDateTime
case class PageWithoutContent          (name: String, revision: Long, dateTime: LocalDateTime, nickname: Option[String], user: Option[Long], remoteAddress: String, comment: String, isMinorEdit: Boolean, viaApi: Boolean = false, userApiKey: Option[Long] = None) extends WithDateTime

case class SearchResult(name: String, dateTime: LocalDateTime, content: String) {

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

case class SearchResultSummary(name: String, summary:Seq[Seq[(Int, String)]], dateTime: LocalDateTime)


object Page {
  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  private val rowParserPage = str("name") ~ long("revision") ~ localDateTime("dateTime") ~ get[Option[String]]("nickname") ~ optionLong("user") ~str("remoteAddress") ~ str("comment") ~ bool("isMinorEdit") ~ str("content") ~ bool("viaApi") ~ optionLong("userApiKey")
  private val rowParserPageWithoutContent = str("name") ~ long("revision") ~ localDateTime("dateTime") ~ optionStr("nickname") ~ optionLong("user") ~str("remoteAddress") ~ str("comment") ~ bool("isMinorEdit") ~ bool("viaApi") ~ optionLong("userApiKey")
  private val rowParserSearchResult = str("name") ~ localDateTime("dateTime") ~ str("content")

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
    //language=sql
    SQL"""
SELECT P.name, P.revision, dateTime, U.nickname AS nickname, P.`user` AS `user`, remoteAddress, comment, isMinorEdit, content, viaApi, P.userApiKey
    FROM Page P
    LEFT JOIN User U ON U.seq = P.user
    WHERE P.site = ${site.seq} AND P.name = $name
    ORDER BY revision DESC
    LIMIT 1
    """
      .as(rowParserPage singleOpt).map(flatten)
      .map(Page.tupled)
  }

  def selectLastRevision(seqName: Seq[String])(implicit connection: Connection, site: Site): Seq[Page] = {
    //language=sql
    SQL"""
SELECT P.name, P.revision, dateTime, U.nickname AS nickname, P.`user` AS `user`, remoteAddress, comment, isMinorEdit, content, viaApi, P.userApiKey
    FROM Page P
    LEFT JOIN User U ON U.seq = P.user
    WHERE P.site = ${site.seq} AND P.name IN ($seqName)
      AND P.revision = (
        SELECT MAX(P2.revision)
        FROM Page P2
        WHERE P2.site = P.site AND P2.name = P.name
      )
    ORDER BY P.name DESC
    """
      .as(rowParserPage *).map(flatten)
      .map(Page.tupled)
  }

  def selectFirstRevision(name: String)(implicit connection: Connection, site: Site): Option[Page] = {
    //language=sql
    SQL"""
SELECT name, revision, dateTime, U.nickname nickname, P.`user` AS `user`, remoteAddress, comment, isMinorEdit, content, viaApi, P.userApiKey
    FROM Page P
    LEFT JOIN User U ON U.seq = P.user
    WHERE site = ${site.seq} AND name = $name
    ORDER BY revision ASC
    LIMIT 1
    """
      .as(rowParserPage singleOpt).map(flatten)
      .map(Page.tupled)
  }

  def selectSpecificRevision(name: String, revision: Int)(implicit connection: Connection, site: Site): Option[Page] = {
    //language=sql
    SQL"""
SELECT name, revision, dateTime, U.nickname nickname, P.`user` AS `user`, remoteAddress, comment, isMinorEdit, content, viaApi, P.userApiKey
    FROM Page P
    LEFT JOIN User U ON U.seq = P.user
    WHERE site = ${site.seq} AND name = $name AND revision = $revision
    """
      .as(rowParserPage singleOpt).map(flatten)
      .map(Page.tupled)
  }

  def selectHistory(name: String)(implicit connection: Connection, site: Site): List[PageWithoutContent] = {
    //language=sql
    SQL"""
SELECT name, revision, dateTime, U.nickname nickname, P.`user` AS `user`, remoteAddress, comment, isMinorEdit, viaApi, P.userApiKey
    FROM Page P
    LEFT JOIN User U ON U.seq = P.user
    WHERE site = ${site.seq} AND name = $name
    ORDER BY revision DESC
    """
      .as(rowParserPageWithoutContent *).map(flatten)
      .map(PageWithoutContent.tupled)
  }

  def selectHistoryStream[T](name: String, t:T, f:(T, Page) => T)(implicit connection: Connection, site: Site): T = {
    //language=sql
    SQL"""
SELECT name, revision, dateTime, U.nickname nickname, P.`user` AS `user`, remoteAddress, comment, isMinorEdit, content, viaApi, P.userApiKey
    FROM Page P
    LEFT JOIN User U ON U.seq = P.user
    WHERE site = ${site.seq} AND name = $name
    ORDER BY revision ASC
    """
      .as(rowParserPage *).map(flatten)
      .foldLeft(t)((a, v) => f(a, Page.tupled(v)))
  }

  def insert(p: Page)(implicit connection: Connection, site: Site): Option[Long] = {
    //language=sql
    SQL"""
INSERT INTO Page
    (site, name, revision, dateTime, `user`, remoteAddress, comment, isMinorEdit, viaApi, userApiKey, content) values
    (${site.seq}, ${p.name}, ${p.revision}, ${p.dateTime}, ${p.user.map(_.toString).getOrElse(null)}, ${p.remoteAddress}, ${p.comment}, ${p.isMinorEdit}, ${p.viaApi}, ${p.userApiKey.map(_.toString).getOrElse(null)}, ${p.content})
    """.executeInsert()
  }

  def deleteLinkCosignSimilarityTermFrequency(name: String)(implicit connection: Connection, site: Site): Int = {
    val linkCount = CalculatedLink.delete(name)
    val cosineSimilarityCount = CalculatedCosineSimilarity.delete(name)
    val termFrequencyCount = CalculatedTermFrequency.delete(name)
    val schemaOrgCount = CalculatedSchemaOrg.delete(name)
    val pageMetaCount = PageMeta.delete(name)
    linkCount + cosineSimilarityCount + termFrequencyCount + schemaOrgCount + pageMetaCount
  }

  private def withLocalTransaction[T](f: => T)(implicit connection: Connection): T = {
    if (connection.getAutoCommit) {
      connection.setAutoCommit(false)
      try {
        val result = f
        connection.commit()
        result
      } catch {
        case e: Throwable =>
          connection.rollback()
          throw e
      } finally {
        connection.setAutoCommit(true)
      }
    } else {
      val savepoint: Savepoint = connection.setSavepoint()
      try {
        f
      } catch {
        case e: Throwable =>
          connection.rollback(savepoint)
          throw e
      }
    }
  }

  def deleteWithRelatedData(name:String)(implicit connection: Connection, site: Site): Int = {
    withLocalTransaction {
      deleteLinkCosignSimilarityTermFrequency(name)
      SQL"DELETE FROM Page WHERE site = ${site.seq} AND name = $name".executeUpdate()
    }
  }

  def deleteSpecificRevisionWithRelatedData(name:String, revision:Long)(implicit connection: Connection, site: Site): Int = {
    withLocalTransaction {
      deleteLinkCosignSimilarityTermFrequency(name)
      try {
        SQL"DELETE FROM Page WHERE site = ${site.seq} AND name = $name AND revision = $revision".executeUpdate()
      } catch {
        case _: SQLIntegrityConstraintViolationException =>
          // async recalculation can re-insert calculated rows between delete and page removal
          deleteLinkCosignSimilarityTermFrequency(name)
          SQL"DELETE FROM Page WHERE site = ${site.seq} AND name = $name AND revision = $revision".executeUpdate()
      }
    }
  }

  def rename(name: String, newName: String)(implicit connection: Connection, site: Site): Int = {
    withLocalTransaction {
      deleteLinkCosignSimilarityTermFrequency(name)
      SQL"UPDATE Page SET name = $newName WHERE site = ${site.seq} AND name = $name".executeUpdate()
    }
  }

  def selectYmdCountOfFirstRevision()(implicit connection: Connection, site: Site): Seq[(String, Long)] = {
    //language=sql
    SQL"""
SELECT
    DATE_FORMAT(dateTime, '%Y-%m-%d') ymd, COUNT(*) cnt
    FROM Page w
    WHERE site = ${site.seq} AND revision = 1
    GROUP BY DATE_FORMAT(dateTime, '%Y-%m-%d')
    ORDER BY DATE_FORMAT(dateTime, '%Y-%m-%d')
    """
      .as(str("ymd") ~ long("cnt") *).map(flatten)
  }

  def pageSearch(q:String)(implicit connection: Connection, site: Site): immutable.Seq[SearchResult] = {
    //language=sql
    SQL"""
SELECT P.name, P.dateTime, P.content
     FROM Page P
     INNER JOIN (
        SELECT site, name, MAX(revision) AS revision
        FROM Page
        WHERE site = ${site.seq}
        GROUP BY site, name
     ) SPNR ON P.site = SPNR.site AND P.name = SPNR.name AND P.revision = SPNR.revision
     WHERE
         P.site = ${site.seq} AND (
             P.name LIKE CONCAT('%', $q, '%') COLLATE utf8mb4_general_ci OR
             P.content LIKE CONCAT('%', $q, '%') COLLATE utf8mb4_general_ci
         )
     ORDER BY P.name"""
      .as(rowParserSearchResult *).map(flatten).map(SearchResult.tupled)
  }
}
