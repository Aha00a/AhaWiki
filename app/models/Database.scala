package models

import java.time.Instant

import anorm.SqlParser._
import anorm._
import implicits.Implicits.LocalDateTimeFormatter
import play.api.Play.current
import play.api.db.DB
import utils.{DateTimeFormatterHolder, LocalDateTimeUtil}

import scala.language.postfixOps

object Database {

  trait WithTime {
    val time:Long

    lazy val localDateTime = LocalDateTimeUtil.fromEpochNano(time)
    lazy val localDate = localDateTime.toLocalDate
    lazy val localYearMonth = localDate.format(DateTimeFormatterHolder.yearDashMonth)

    lazy val isoDateTime = localDateTime.toIsoDateTimeString
  }

  case class Page(name: String, revision: Long, time: Long, author: String, remoteAddress: String, content: String, comment: Option[String]) extends WithTime {
    def this(name: String, content: String) = this(name, 0L, Instant.now().toEpochMilli * 1000, "aha00a", "0.0.0.0", content, Some("")) // TODO: remove
  }

  case class PageRevisionTimeAuthorRemoteAddressComment(revision: Long, time: Long, author: String, remoteAddress: String, comment: Option[String]) extends WithTime
  case class PageNameRevisionTimeAuthorRemoteAddressSizeComment(name:String, revision: Long, time: Long, author: String, remoteAddress: String, size:Long, comment: Option[String]) extends WithTime
  case class PageNameRevisionTime(name: String, revision: Int, time: Long) extends WithTime


  case class TermFrequency(name:String, term:String, frequency:Int) {
    def this(name:String, kv:Tuple2[String, Int]) = this(name, kv._1, kv._2)
  }


  case class Link(src:String, dst:String, alias:String) {
    def or(a:(String => Boolean)):Boolean = a(src) || a(dst)
  }


  case class CosineSimilarity(name1: String, name2: String, similarity: Double)

  object PageTable {
    def selectCount(): Long = DB.withConnection { implicit connection =>
      SQL("SELECT COUNT(*) cnt FROM Page").as(long("cnt") single)
    }
  }

  def pageSelectCount() = PageTable.selectCount()

  def pageSelectLastRevision(name: String): Option[Page] = DB.withConnection { implicit connection =>
    SQL("SELECT name, revision, time, author, remoteAddress, content, comment FROM Page WHERE name = {name} ORDER BY revision DESC LIMIT 1")
      .on('name -> name)
      .as(str("name") ~ long("revision") ~ long("time") ~ str("author") ~ str("remoteAddress") ~ str("content") ~ str("comment").? singleOpt).map(flatten)
      .map(Page.tupled)
  }

  def pageSelectFirstRevision(name: String) = DB.withConnection { implicit connection =>
    SQL("SELECT name, revision, time, author, remoteAddress, content, comment FROM Page WHERE name = {name} ORDER BY revision ASC LIMIT 1")
      .on('name -> name)
      .as(str("name") ~ long("revision") ~ long("time") ~ str("author") ~ str("remoteAddress") ~ str("content") ~ str("comment").? singleOpt).map(flatten)
      .map(Page.tupled)
  }

  def pageSelectSpecificRevision(name: String, revision: Int) = DB.withConnection { implicit connection =>
    SQL("SELECT name, revision, time, author, remoteAddress, content, comment FROM Page WHERE name = {name} AND revision = {revision} ORDER BY revision ASC LIMIT 1")
      .on('name -> name, 'revision -> revision)
      .as(str("name") ~ long("revision") ~ long("time") ~ str("author") ~ str("remoteAddress") ~ str("content") ~ str("comment").? singleOpt).map(flatten)
      .map(Page.tupled)
  }

  def pageSelectRecentChanges(): List[PageNameRevisionTime] = DB.withConnection { implicit connection =>
    SQL( """SELECT name, MAX(revision) revision, MAX(time) time
           |    FROM Page
           |    GROUP BY name
           |    ORDER BY MAX(time) DESC
         """.stripMargin)
      .as(str("name") ~ int("revision") ~ long("time") *)
      .map(flatten).map(PageNameRevisionTime.tupled)
  }

  def pageSelectNameGroupByNameOrderByName: List[String] = DB.withConnection { implicit connection =>
    //noinspection LanguageFeature
    SQL"SELECT name FROM Page GROUP BY name ORDER BY name".as(str("name") *)
  }

  def pageSelectHistory(name: String) = DB.withConnection { implicit connection =>
    SQL("SELECT revision, time, author, remoteAddress, comment FROM Page WHERE name = {name} ORDER BY revision DESC")
      .on('name -> name)
      .as(long("revision") ~ long("time") ~ str("author") ~ str("remoteAddress") ~ str("comment").? *).map(flatten)
      .map(PageRevisionTimeAuthorRemoteAddressComment.tupled)
  }

  def pageSelectPageList() = DB.withConnection { implicit connection =>
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

  def pageInsert(name: String, revision: Long, time: Long, author: String, remoteAddress: String, content: String, comment: String) = DB.withConnection { implicit connection =>
    SQL"INSERT INTO Page (name, revision, time, author, remoteAddress, content, comment) values ($name, $revision, $time, $author, $remoteAddress, $content, $comment)".executeInsert()
  }

  def termFrequencyDelete(name: String): Int = DB.withConnection { implicit connection =>
    SQL"DELETE FROM TermFrequency WHERE name = $name".executeUpdate()
  }

  def termFrequencyInsert(name: String, term: String, frequency: Long) = DB.withConnection { implicit connection =>
    SQL"INSERT INTO TermFrequency (name, term, frequency) values ($name, $term, $frequency)".executeInsert()
  }

  def termFrequencyInsert(name: String, map:Map[String, Int]): Array[Int] = termFrequencyInsert(map.map(kv => new TermFrequency(name, kv)).toSeq)

  def termFrequencyInsert(seqTermFrequency: Seq[TermFrequency]): Array[Int] = DB.withConnection { implicit connection =>
    if(seqTermFrequency.isEmpty) {
      Array[Int]()
    } else {
      val insertQuery = SQL("INSERT INTO TermFrequency (name, term, frequency) values ({name}, {term}, {frequency})")
      (insertQuery.asBatch /: seqTermFrequency)((sql, elem) => sql.addBatchParams(elem.name, elem.term, elem.frequency)).execute()
    }
  }

  def cosineSimilarityUpdate(name: String) = DB.withConnection { implicit connection =>
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

  def cosineSimilaritySelect(name: String) = DB.withConnection { implicit connection =>
    SQL"SELECT name1, name2, similarity FROM CosineSimilarity WHERE similarity > 0 AND name1 = $name AND name1 != name2 ORDER BY similarity DESC LIMIT 10"
      .as(str("name1") ~ str("name2") ~ double("similarity") *).map(flatten)
      .map(CosineSimilarity.tupled)
  }

  def pageSelectNameWhereNoCosineSimilarity(): Option[String] = DB.withConnection { implicit connection =>
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


  def pageSelectNameRandom():String = DB.withConnection { implicit connection =>
    connection.getMetaData.getDatabaseProductName match {
      case "H2" => // TODO
        SQL( """SELECT
               |    DISTINCT(name)
               |    FROM Page
               |    WHERE content <> ''
               |    ORDER BY RAND()
               |    LIMIT 1
               | """.stripMargin)
          .as(str("name") single)
      case _ =>
        SQL( """SELECT
               |    DISTINCT(name)
               |    FROM Page
               |    WHERE content <> ''
               |    ORDER BY RAND()
               |    LIMIT 1
               | """.stripMargin)
          .as(str("name") single)
    }
  }

  def pageDelete(name:String): Int = DB.withConnection { implicit connection =>
    SQL"DELETE FROM Page WHERE name = $name".executeUpdate()
  }

  def pageDeleteWithRelatedData(name:String): Int = DB.withConnection { implicit connection => // TODO: transaction, FK
    SQL"DELETE FROM Link WHERE src = $name".executeUpdate()
    SQL"DELETE FROM CosineSimilarity WHERE name1 = $name OR name2 = $name".executeUpdate()
    SQL"DELETE FROM TermFrequency WHERE name = $name".executeUpdate()
    SQL"DELETE FROM Page WHERE name = $name".executeUpdate()
  }

  def linkDelete(name: String): Int = DB.withConnection { implicit connection =>
    SQL"DELETE FROM Link WHERE src = $name".executeUpdate()
  }

  def linkInsert(seq: Seq[Link]): Array[Int] = DB.withConnection { implicit connection =>
    if(seq.isEmpty) {
      Array[Int]()
    } else {
      val insertQuery = SQL("REPLACE INTO Link (src, dst, alias) values ({src}, {dst}, {alias})")
      (insertQuery.asBatch /: seq)((sql, elem) => sql.addBatchParams(elem.src, elem.dst, elem.alias)).execute()
    }
  }


  def linkSelect(name: String) = DB.withConnection { implicit connection =>
    SQL"SELECT src, dst, alias FROM Link WHERE src = $name OR dst = $name"
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(Link.tupled)
      .filterNot(_.or(_.startsWith(".")))
      .filterNot(_.or(_.contains("://")))
  }

  def linkSelect(): List[Link] = DB.withConnection { implicit connection =>
    SQL"SELECT src, dst, alias FROM Link"
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(Link.tupled)
      .filterNot(_.or(_.startsWith(".")))
      .filterNot(_.or(_.contains("://")))
  }
}