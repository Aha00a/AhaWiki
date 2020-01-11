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

case class Page                        (name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String, content: String, comment: String            ) extends WithDateTime
case class PageWithoutContent          (name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String,                  comment: String            ) extends WithDateTime
case class PageWithoutContentWithSize  (name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String,                  comment: String, size: Long) extends WithDateTime


case class TermFrequency(name:String, term:String, frequency:Int) {
  def this(name:String, kv:(String, Int)) = this(name, kv._1, kv._2)
}

case class Link(src:String, dst:String, alias:String) {
  lazy val isDstExternal: Boolean = PageNameLogic.isExternal(dst)

  def or(a: String => Boolean):Boolean = a(src) || a(dst)
}

case class CosineSimilarity(name1: String, name2: String, similarity: Double)

case class HighScoredTerm(name:String, term:String, frequency1:Float, frequency2:Float)

case class SearchResultSummary(name: String, summary:Seq[Seq[(Int, String)]])

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
        .map(_.map(i => (i + 1, lines(i)))).toSeq
    )
  }
}

case class GeocodeCache(address: String, lat: Double, lng: Double, created: Date) {
  lazy val latLng: LatLng = LatLng(lat, lng)
}


object AhaWikiQuery {
  def apply()(implicit connection: Connection): AhaWikiQuery = new AhaWikiQuery()
}

class AhaWikiQuery()(implicit connection: Connection) {

  object Page {
    private val rowParser = str("name") ~ long("revision") ~ date("dateTime") ~ str("author") ~ str("remoteAddress") ~ str("content") ~ str("comment")

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
      SQL("SELECT name, revision, dateTime, author, remoteAddress, content, comment FROM Page WHERE name = {name} ORDER BY revision DESC LIMIT 1")
        .on('name -> name)
        .as(rowParser singleOpt).map(flatten)
        .map(models.Page.tupled)
    }

    def selectFirstRevision(name: String): Option[Page] = {
      SQL("SELECT name, revision, dateTime, author, remoteAddress, content, comment FROM Page WHERE name = {name} ORDER BY revision ASC LIMIT 1")
        .on('name -> name)
        .as(rowParser singleOpt).map(flatten)
        .map(models.Page.tupled)
    }

    def selectSpecificRevision(name: String, revision: Int): Option[Page] = {
      SQL("SELECT name, revision, dateTime, author, remoteAddress, content, comment FROM Page WHERE name = {name} AND revision = {revision} ORDER BY revision ASC LIMIT 1")
        .on('name -> name, 'revision -> revision)
        .as(rowParser singleOpt).map(flatten)
        .map(models.Page.tupled)
    }

    def selectHistory(name: String): List[PageWithoutContent] = {
      SQL"SELECT name, revision, dateTime, author, remoteAddress, comment FROM Page WHERE name = $name ORDER BY revision DESC"
        .as(str("name") ~ long("revision") ~ date("dateTime") ~ str("author") ~ str("remoteAddress") ~ str("comment") *).map(flatten)
        .map(PageWithoutContent.tupled)
    }

    def selectHistoryStream[T](name: String, t:T, f:(T, models.Page) => T): T = {
      SQL"SELECT name, revision, dateTime, author, remoteAddress, content, comment FROM Page WHERE name = $name ORDER BY revision ASC"
        .as(rowParser *).map(flatten)
        .foldLeft(t)((a, v) => f(a, models.Page.tupled(v)))
    }

    def selectPermNullAndContentStartsWithSheBangRead(): Option[Page] = {
      SQL"""
           SELECT name, revision, dateTime, author, remoteAddress, content, comment
              FROM Page
              WHERE
                  permRead IS NULL AND
                  content LIKE ${"#!read %"}
              ORDER BY RAND()
              LIMIT 1
      """
        .as(rowParser singleOpt).map(flatten)
        .map(models.Page.tupled)

    }

    def updatePerm(page: Page): Int = {
      val pageContent: PageContent = PageContent(page.content)
      SQL"""
           UPDATE Page SET
              permRead = ${pageContent.read}
              WHERE name = ${page.name} AND revision = ${page.revision}
      """.executeUpdate()
    }


    def deleteLinkCosignSimilarityTermFrequency(name: String)(implicit connection:Connection): Int = {
      val linkCount = Link.delete(name)
      val cosineSimilarityCount = CosineSimilarity.delete(name)
      val termFrequencyCount = TermFrequency.delete(name)
      linkCount + cosineSimilarityCount + termFrequencyCount
    }

    def deleteWithRelatedData(name:String): Int = { // TODO: transaction, FK
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
      TermFrequency.delete(name)
      TermFrequency.insert(name, wordCount)
      CosineSimilarity.recalc(name)
    }
    def updateLink(name: String, seqLink: Seq[Link])(implicit connection:Connection): Array[Int] = {
      Link.delete(name)
      Link.insert(seqLink)
    }
  }

  object GeocodeCache {
    private val rowParser = str("address") ~ double("lat") ~ double("lng") ~ date("created")
    def select(address: String): Option[GeocodeCache] = {
      SQL"SELECT address, lat, lng, created FROM GeocodeCache WHERE address = $address"
        .as(rowParser singleOpt).map(flatten)
        .map(models.GeocodeCache.tupled)
    }

    def replace(address: String, latLng: LatLng): Int = {
      SQL"""REPLACE INTO GeocodeCache (address, lat, lng) VALUES ($address, ${latLng.lat}, ${latLng.lng})""".executeUpdate()
    }
  }

  object Link {
    def selectCountWhereAlias(alias: String): Long = {
      SQL"SELECT COUNT(*) cnt FROM Link WHERE alias = $alias"
        .as(long("cnt") single)
    }


    def selectBacklink(name: String): List[Link] = {
      SQL"SELECT src, dst, alias FROM Link WHERE dst = $name"
        .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
        .map(models.Link.tupled)
    }

    def selectBacklinkOfDatePage(name: String): List[Link] = {
      SQL"""SELECT src, dst, alias FROM Link WHERE dst = $name AND src REGEXP '[0-9]{4}-(0[0-9]|1[12])-([012][0-9]|3[01])'"""
        .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
        .map(models.Link.tupled)
    }

    def select(name: String): List[Link] = {
      SQL"""SELECT src, dst, alias
           FROM Link
           WHERE
            src != '' AND dst != '' AND (src = $name OR dst = $name)
        """
        .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
        .map(models.Link.tupled)
    }

    def selectDst(dst: String): List[Link] = {
      SQL"SELECT src, dst, alias FROM Link WHERE dst = $dst"
        .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
        .map(models.Link.tupled)
    }

    def selectSchema(name: String): List[Link] = {
      SQL"SELECT src, dst, alias FROM Link WHERE dst = $name AND alias LIKE ${"schema:%"}"
        .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
        .map(models.Link.tupled)
    }

    def selectSchemaClass(src: Seq[String]): List[Link] = {
      SQL"SELECT src, dst, alias FROM Link WHERE src IN ($src) AND alias LIKE 'schema:Schema'"
        .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
        .map(models.Link.tupled)
    }



    def selectAllButNotEmpty(): List[Link] = {
      SQL"SELECT src, dst, alias FROM Link WHERE src != '' AND dst != ''"
        .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
        .map(models.Link.tupled)
    }


    def expand(seq: Seq[Link]): Seq[Link] = {
      val backward: Seq[Link] = seq.flatMap(l => select(l.src))
      val forward: Seq[Link] = seq.flatMap(l => select(l.dst))
      val seqExpanded: Seq[Link] = seq ++ backward ++ forward
      seqExpanded.distinct
    }


    def insert(seq: Seq[Link]): Array[Int] = {
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

    def delete(name: String)(implicit connection:Connection): Int = {
      SQL"DELETE FROM Link WHERE src = $name".executeUpdate()
    }
  }

  object TermFrequency {
    def insert(name: String, map:Map[String, Int]): Array[Int] = insert(map.map(kv => new TermFrequency(name, kv)).toSeq)

    def insert(seqTermFrequency: Seq[TermFrequency]): Array[Int] = {
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


    def delete(name: String)(implicit connection:Connection): Int = {
      SQL"DELETE FROM TermFrequency WHERE name = $name".executeUpdate()
    }
  }

  object CosineSimilarity {
    def recalc(name: String): Int = {
      SQL"""DELETE FROM CosineSimilarity WHERE name1 = $name OR name2 = $name""".executeUpdate()

      SQL"""
REPLACE INTO CosineSimilarity (name1, name2, similarity)
SELECT *
    FROM (
        SELECT
            TF3.name name1,
            $name name2,
            IFNULL(
                ( SELECT SUM(TF1.frequency * TF2.frequency) product FROM TermFrequency TF1 INNER JOIN TermFrequency TF2 ON TF1.term = TF2.term WHERE TF1.name = TF3.name AND TF2.name = $name)
                /
                (
                    (SELECT SQRT(SUM(frequency * frequency)) FROM TermFrequency WHERE name = TF3.name)
                        *
                    (SELECT SQRT(SUM(frequency * frequency)) FROM TermFrequency WHERE name = $name)
                ),
                0
            ) similarity
            FROM (SELECT DISTINCT name FROM TermFrequency) TF3
    ) CS1
    WHERE similarity > 0
      """.executeUpdate()

      SQL"""
REPLACE INTO CosineSimilarity (name1, name2, similarity)
SELECT name2, name1, similarity FROM CosineSimilarity WHERE name2 = $name
      """.executeUpdate()
    }

    def select(name: String): List[CosineSimilarity] = {
      SQL"SELECT name1, name2, similarity FROM CosineSimilarity WHERE similarity > 0 AND name1 = $name AND name1 != name2 ORDER BY similarity DESC LIMIT 10"
        .as(str("name1") ~ str("name2") ~ double("similarity") *).map(flatten)
        .map(models.CosineSimilarity.tupled)
    }

    def delete(name: String)(implicit connection:Connection): Int = {
      SQL"""DELETE FROM CosineSimilarity WHERE name1 = $name OR name2 = $name""".executeUpdate()
    }
  }

  def pageSelectNameGroupByNameOrderByName: List[String] = {
    //noinspection LanguageFeature
    SQL"SELECT name FROM Page GROUP BY name ORDER BY name".as(str("name") *)
  }

  def pageSelectPageList(): List[PageWithoutContentWithSize] = {
    SQL( """SELECT w.name, w.revision, w.dateTime, w.author, w.remoteAddress, w.comment, LENGTH(content) size
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
      .as(str("name") ~ long("revision") ~ date("dateTime") ~ str("author") ~ str("remoteAddress") ~ str("comment") ~ long("size") *).map(flatten)
      .map(PageWithoutContentWithSize.tupled)
  }

  def pageInsert(name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String, content: String, comment: String): Option[Long] = {
    SQL"INSERT INTO Page (name, revision, dateTime, author, remoteAddress, content, comment) values ($name, $revision, $dateTime, $author, $remoteAddress, $content, $comment)".executeInsert()
  }


  def pageSearch(q:String): immutable.Seq[SearchResult] = {
    SQL("""
SELECT w.name, w.revision, w.dateTime, w.author, w.remoteAddress, w.content, w.comment
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

  def selectHighScoredTerm(name:String, similarPageNames:Seq[String]): immutable.Seq[HighScoredTerm] = {
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
}