package models.tables

import java.sql.Connection

import anorm.BatchSql
import anorm.NamedParameter
import anorm._

case class CalculatedTermFrequency(name:String, term:Long, frequency:Int)

object CalculatedTermFrequency {
  //noinspection TypeAnnotation
  def tupled = (apply _).tupled



  def insert(name: String, term: Long, frequency: Int)(implicit connection: Connection, site: Site): Int = {
    SQL"INSERT INTO CalculatedTermFrequency (site, name, term, frequency) VALUES (${site.seq}, $name, $term, $frequency)".executeUpdate()
  }

  def insert(name: String, term: String, frequency: Int)(implicit connection: Connection, site: Site): Option[Int] = {
    CalculatedTerm.ensureSeq(term).map(seqTerm => insert(name, seqTerm, frequency))
  }

  def insert(name: String, termFrequencies: Seq[(Long, Int)])(implicit connection: Connection, site: Site): Array[Int] = {
    if (termFrequencies.isEmpty) {
      Array.empty[Int]
    } else {
      val values = termFrequencies.map { case (term, frequency) =>
        Seq[NamedParameter](
          "site" -> site.seq,
          "name" -> name,
          "term" -> term,
          "frequency" -> frequency,
        )
      }
      // REPLACE, not INSERT: two instances recalculate the same page concurrently (each runs
      // PageLogic.calculate on its own traffic), and this is a delete-then-insert with no lock
      // between them. The loser's batch used to die on a duplicate PRIMARY (site, name, term).
      // A derived row's latest calculation is authoritative, so overwriting is correct —
      // CalculatedLink and CalculatedSchemaOrg already do the same for the same reason.
      BatchSql(
        "REPLACE INTO CalculatedTermFrequency (site, name, term, frequency) VALUES ({site}, {name}, {term}, {frequency})",
        values.head,
        values.tail: _*
      ).execute()
    }
  }

  /**
   * How many calculated pages, over every site, hold each of these terms. Terms nobody holds are
   * absent. One query on the index that leads with `term`, which is the same index the
   * similarity join walks -- this asks for the width of what that join would walk.
   */
  def selectDocumentFrequency(terms: Seq[Long])(implicit connection: Connection): Map[Long, Int] = {
    val distinctTerms = terms.distinct
    // `IN ()` is a syntax error, not an empty match -- see GeocodeCache.select.
    if (distinctTerms.isEmpty) {
      Map.empty
    } else {
      SQL"""
        SELECT term, COUNT(*) AS documentFrequency
        FROM CalculatedTermFrequency
        WHERE term IN ($distinctTerms)
        GROUP BY term
      """.as((SqlParser.long("term") ~ SqlParser.int("documentFrequency")).map { case term ~ df => term -> df }.*).toMap
    }
  }

  /** How many pages, over every site, have a term vector -- one norm row each. */
  def countCalculatedPages()(implicit connection: Connection): Long =
    SQL"SELECT COUNT(*) AS cnt FROM CalculatedTermFrequencyNorm".as(SqlParser.long("cnt").single)

  def replaceNorm(name: String)(implicit connection: Connection, site: Site): Int = {
    SQL"""
      REPLACE INTO CalculatedTermFrequencyNorm (site, name, norm)
      SELECT site, name, SQRT(SUM(frequency * frequency)) AS norm
      FROM CalculatedTermFrequency
      WHERE site = ${site.seq} AND name = $name
      GROUP BY site, name
    """.executeUpdate()
  }

  /** The page's frequencies and norm, swapped for `termFrequencies` in one transaction. A page view
    * recalculates the page one time in ten, so readers of SimilarPages are always nearby; with the
    * delete and the insert committed separately they read the page with some or all of its terms
    * missing. */
  def replace(name: String, termFrequencies: Seq[(Long, Int)])(implicit connection: Connection, site: Site): Unit =
    LocalTransaction.retryingDeadlock {
      delete(name)
      insert(name, termFrequencies)
      replaceNorm(name)
    }

  def delete(name: String)(implicit connection:Connection, site: Site): Int = {
    val count = SQL"DELETE FROM CalculatedTermFrequency WHERE site = ${site.seq} AND name = $name".executeUpdate()
    SQL"DELETE FROM CalculatedTermFrequencyNorm WHERE site = ${site.seq} AND name = $name".executeUpdate()
    count
  }
}
