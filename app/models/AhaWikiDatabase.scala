package models

import java.time.{LocalDate, LocalDateTime}

import anorm.SqlParser._
import anorm._
import com.aha00a.commons.Implicits.{LocalDateTimeFormatter, _}
import com.aha00a.commons.utils.{DateTimeFormatterHolder, LocalDateTimeUtil}
import models.AhaWikiDatabase._
import play.api.db.Database

import scala.collection.immutable
import scala.language.postfixOps
import scala.util.matching.Regex

object AhaWikiDatabase {
  def apply()(implicit db:Database): AhaWikiDatabase = new AhaWikiDatabase()

  trait WithTime {
    val time:Long

    lazy val localDateTime: LocalDateTime = LocalDateTimeUtil.fromEpochNano(time)
    lazy val localDate: LocalDate = localDateTime.toLocalDate
    lazy val year: Int = localDate.getYear
    lazy val yearDashMonth: String = localDate.format(DateTimeFormatterHolder.yearDashMonth)

    lazy val isoLocalDateTime: String = localDateTime.toIsoLocalDateTimeString
  }

  // TODO: Option comment?
  case class Page(name: String, revision: Long, time: Long, author: String, remoteAddress: String, content: String, comment: Option[String]) extends WithTime

  // TODO: Option comment?
  case class PageRevisionTimeAuthorRemoteAddressComment(revision: Long, time: Long, author: String, remoteAddress: String, comment: Option[String]) extends WithTime

  case class PageNameRevisionTimeAuthorRemoteAddressSizeComment(name:String, revision: Long, time: Long, author: String, remoteAddress: String, size:Long, comment: Option[String]) extends WithTime

  case class PageNameRevisionTime(name: String, revision: Int, time: Long) extends WithTime

  case class TermFrequency(name:String, term:String, frequency:Int) {
    def this(name:String, kv:(String, Int)) = this(name, kv._1, kv._2)
  }


  case class Link(src:String, dst:String, alias:String) {
    def or(a: String => Boolean):Boolean = a(src) || a(dst)
  }

  case class CosineSimilarity(name1: String, name2: String, similarity: Double)

  case class HighScoredTerm(name:String, term:String, frequency1:Float, frequency2:Float)

  case class SearchResultSummary(name: String, summary:Seq[Seq[(Int, String)]])

  case class SearchResult(name:String, content:String, time:Long) {
    def summarise(q: String): SearchResultSummary = {
      def around(i:Int, distance: Int = 3) = (i - distance) to (i + distance)
      val lines = content.split("""(\r\n|\n)+""").toSeq
      SearchResultSummary(
        name,
        lines
          .zipWithIndex
          .filter(s => s"(?i)${Regex.quote(q)}".r.findFirstIn(s._1).isDefined)
          .map(_._2)
          .flatMap(around(_))
          .distinct
          .filter(lines.isDefinedAt)
          .splitBy((a, b) => a + 1 != b)
          .map(_.map(i => (i + 1, lines(i)))).toSeq
        )
    }
  }
}

class AhaWikiDatabase()(implicit db:Database) {

  object PageTable {
    def selectCount(): Long = db.withConnection { implicit connection =>
      SQL("SELECT COUNT(*) cnt FROM Page").as(long("cnt") single)
    }
  }

  def pageSelectCount(): Long = PageTable.selectCount()

  def pageSelect(name: String, revision: Int): Option[AhaWikiDatabase.Page] = {
    if (revision == 0) {
      pageSelectLastRevision(name)
    } else {
      pageSelectSpecificRevision(name, revision)
    }
  }

  def pageSelectLastRevision(name: String): Option[Page] = db.withConnection { implicit connection =>
    SQL("SELECT name, revision, time, author, remoteAddress, content, comment FROM Page WHERE name = {name} ORDER BY revision DESC LIMIT 1")
      .on('name -> name)
      .as(str("name") ~ long("revision") ~ long("time") ~ str("author") ~ str("remoteAddress") ~ str("content") ~ str("comment").? singleOpt).map(flatten)
      .map(Page.tupled)
  }

  def pageSelectFirstRevision(name: String): Option[Page] = db.withConnection { implicit connection =>
    SQL("SELECT name, revision, time, author, remoteAddress, content, comment FROM Page WHERE name = {name} ORDER BY revision ASC LIMIT 1")
      .on('name -> name)
      .as(str("name") ~ long("revision") ~ long("time") ~ str("author") ~ str("remoteAddress") ~ str("content") ~ str("comment").? singleOpt).map(flatten)
      .map(Page.tupled)
  }

  def pageSelectSpecificRevision(name: String, revision: Int): Option[Page] = db.withConnection { implicit connection =>
    SQL("SELECT name, revision, time, author, remoteAddress, content, comment FROM Page WHERE name = {name} AND revision = {revision} ORDER BY revision ASC LIMIT 1")
      .on('name -> name, 'revision -> revision)
      .as(str("name") ~ long("revision") ~ long("time") ~ str("author") ~ str("remoteAddress") ~ str("content") ~ str("comment").? singleOpt).map(flatten)
      .map(Page.tupled)
  }

  def pageSelectNameGroupByNameOrderByName: List[String] = db.withConnection { implicit connection =>
    //noinspection LanguageFeature
    SQL"SELECT name FROM Page GROUP BY name ORDER BY name".as(str("name") *)
  }

  def pageSelectHistory(name: String): List[PageRevisionTimeAuthorRemoteAddressComment] = db.withConnection { implicit connection =>
    SQL("SELECT revision, time, author, remoteAddress, comment FROM Page WHERE name = {name} ORDER BY revision DESC")
      .on('name -> name)
      .as(long("revision") ~ long("time") ~ str("author") ~ str("remoteAddress") ~ str("comment").? *).map(flatten)
      .map(PageRevisionTimeAuthorRemoteAddressComment.tupled)
  }

  def pageSelectPageList(): List[PageNameRevisionTimeAuthorRemoteAddressSizeComment] = db.withConnection { implicit connection =>
    SQL( """SELECT w.name, w.revision, w.time, w.author, w.remoteAddress, LENGTH(content) size, w.comment
           |    FROM Page w
           |    INNER JOIN (
           |        SELECT
           |            name, MAX(revision) revision
           |            FROM Page
           |            GROUP BY name
           |            ORDER BY MAX(time) DESC
           |    ) NV ON w.name = NV.name AND w.revision = NV.revision
           |    ORDER BY name
           |""".stripMargin)
      .as(str("name") ~ long("revision") ~ long("time") ~ str("author") ~ str("remoteAddress") ~ long("size") ~ str("comment").? *).map(flatten)
      .map(PageNameRevisionTimeAuthorRemoteAddressSizeComment.tupled)
  }

  def pageInsert(name: String, revision: Long, time: Long, author: String, remoteAddress: String, content: String, comment: String): Option[Long] = db.withConnection { implicit connection =>
    SQL"INSERT INTO Page (name, revision, time, author, remoteAddress, content, comment) values ($name, $revision, $time, $author, $remoteAddress, $content, $comment)".executeInsert()
  }


  def pageSearch(q:String): immutable.Seq[SearchResult] = db.withConnection { implicit connection =>
    SQL("""
SELECT w.name, w.revision, w.time, w.author, w.remoteAddress, w.content, w.comment
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
      .as(str("name") ~ str("content") ~ long("time") *).map(flatten).map(SearchResult.tupled)
  }


  def termFrequencyDelete(name: String): Int = db.withConnection { implicit connection =>
    SQL"DELETE FROM TermFrequency WHERE name = $name".executeUpdate()
  }

  def termFrequencyInsert(name: String, term: String, frequency: Long): Option[Long] = db.withConnection { implicit connection =>
    SQL"INSERT INTO TermFrequency (name, term, frequency) values ($name, $term, $frequency)".executeInsert()
  }

  def termFrequencyInsert(name: String, map:Map[String, Int]): Array[Int] = termFrequencyInsert(map.map(kv => new TermFrequency(name, kv)).toSeq)

  def termFrequencyInsert(seqTermFrequency: Seq[TermFrequency]): Array[Int] = db.withConnection { implicit connection =>
    if(seqTermFrequency.isEmpty) {
      Array()
    } else {
      val values = seqTermFrequency.map(s => Seq[NamedParameter]('name -> s.name, 'term -> s.term, 'frequency -> s.frequency))
      BatchSql(
        "INSERT INTO TermFrequency (name, term, frequency) values ({name}, {term}, {frequency})",
        values.head,
        values.tail: _*
      ).execute()
    }
  }

  def cosineSimilarityUpdate(name: String): Int = db.withConnection { implicit connection =>
    SQL"""REPLACE INTO CosineSimilarity (name1, name2, similarity)
          SELECT
             TF3.name,
             $name,
             IFNULL(
                 ( SELECT SUM(TF1.frequency * TF2.frequency) product FROM TermFrequency TF1 INNER JOIN TermFrequency TF2 ON TF1.term = TF2.term WHERE TF1.name = TF3.name AND TF2.name = $name)
                 /
                 (
                     (SELECT SQRT(SUM(frequency * frequency)) FROM TermFrequency WHERE name = TF3.name) * (SELECT SQRT(SUM(frequency * frequency)) FROM TermFrequency WHERE name = $name)
                 ),
                 0
             ) similarity
             FROM (SELECT DISTINCT name FROM TermFrequency) TF3""".executeUpdate()
    SQL"""REPLACE INTO CosineSimilarity (name1, name2, similarity)
       SELECT name2, name1, similarity FROM CosineSimilarity WHERE name2 = $name""".executeUpdate()
  }

  def cosineSimilaritySelect(name: String): List[CosineSimilarity] = db.withConnection { implicit connection =>
    SQL"SELECT name1, name2, similarity FROM CosineSimilarity WHERE similarity > 0 AND name1 = $name AND name1 != name2 ORDER BY similarity DESC LIMIT 10"
      .as(str("name1") ~ str("name2") ~ double("similarity") *).map(flatten)
      .map(CosineSimilarity.tupled)
  }

  def pageSelectNameWhereNoCosineSimilarity(): Option[String] = db.withConnection { implicit connection =>
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

  def selectHighScoredTerm(name:String, similarPageNames:Seq[String]): immutable.Seq[HighScoredTerm] = db.withConnection { implicit connection =>
    if(similarPageNames.isEmpty) {
      immutable.Seq()
    } else {
      SQL("""SELECT
            |    tf2.name, tf2.term, tf1.frequency frequency1, tf2.frequency frequency2
            |    FROM TermFrequency tf1
            |    INNER JOIN TermFrequency tf2 ON tf1.term = tf2.term
            |    WHERE
            |        tf1.name = {name} AND tf2.name IN ({pageNames})
            |    ORDER BY frequency1 + frequency2 DESC""".stripMargin)
        .on('name -> name, 'pageNames -> similarPageNames)
        .as(str("name") ~ str("term") ~ float("frequency1") ~ float("frequency2") *).map(flatten)
        .map(HighScoredTerm.tupled)
    }
  }
  
  def pageDeleteWithRelatedData(name:String): Int = db.withConnection { implicit connection => // TODO: transaction, FK
    SQL"DELETE FROM Link WHERE src = $name".executeUpdate()
    SQL"DELETE FROM CosineSimilarity WHERE name1 = $name OR name2 = $name".executeUpdate()
    SQL"DELETE FROM TermFrequency WHERE name = $name".executeUpdate()
    SQL"DELETE FROM Page WHERE name = $name".executeUpdate()
  }

  def pageDeleteRevisionWithRelatedData(name:String, revision:Long): Int = db.withConnection { implicit connection =>
    SQL"DELETE FROM Link WHERE src = $name".executeUpdate()
    SQL"DELETE FROM CosineSimilarity WHERE name1 = $name OR name2 = $name".executeUpdate()
    SQL"DELETE FROM TermFrequency WHERE name = $name".executeUpdate()
    SQL"DELETE FROM Page WHERE name = $name AND revision = $revision".executeUpdate()
  }

  def linkDelete(name: String): Int = db.withConnection { implicit connection =>
    SQL"DELETE FROM Link WHERE src = $name".executeUpdate()
  }

  def pageRename(name: String, newName: String): Int = db.withConnection { implicit connection =>
    SQL"DELETE FROM Link WHERE src = $name".executeUpdate()
    SQL"DELETE FROM CosineSimilarity WHERE name1 = $name OR name2 = $name".executeUpdate()
    SQL"DELETE FROM TermFrequency WHERE name = $name".executeUpdate()
    SQL"UPDATE Page SET name = $newName WHERE name = $name".executeUpdate()
  }

  def linkInsert(seq: Seq[Link]): Array[Int] = db.withConnection { implicit connection =>
    if(seq.isEmpty) {
      Array[Int]()
    } else {
      val values = seq.map(s => Seq[NamedParameter]('src -> s.src, 'dst -> s.dst, 'alias -> s.alias))
      BatchSql(
        "REPLACE INTO Link (src, dst, alias) values ({src}, {dst}, {alias})",
        values.head,
        values.tail: _*
      ).execute()
    }
  }


  def linkSelect(name: String): List[Link] = db.withConnection { implicit connection =>
    SQL"SELECT src, dst, alias FROM Link WHERE src = $name OR dst = $name"
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(Link.tupled)
      .filterNot(_.or(_.startsWith(".")))
      .filterNot(_.or(_.contains("://")))
  }

  def linkSelect(): List[Link] = db.withConnection { implicit connection =>
    SQL"SELECT src, dst, alias FROM Link"
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(Link.tupled)
      .filterNot(_.or(_.startsWith(".")))
      .filterNot(_.or(_.contains("://")))
  }
}