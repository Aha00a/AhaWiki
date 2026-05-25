package models.tables

import anorm.SqlParser.double
import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm._
import models.tables

import java.sql.Connection

case class CalculatedCosineSimilarity(site1: Long, name1: String, site2: Long, name2: String, similarity: Double) {
  def and(a: String => Boolean):Boolean = a(name1) && a(name2)
  def or(a: String => Boolean):Boolean = a(name1) || a(name2)
  def isSameSite: Boolean = site1 == site2
}

object CalculatedCosineSimilarity {

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  private val MinimumSimilarity = 0.3

  def recalc(name: String)(implicit connection: Connection, site: Site): Int = {
    delete(name)
    SQL"""
INSERT INTO CalculatedCosineSimilarity (site1, name1, site2, name2, similarity)
SELECT
    CAST(${site.seq} AS SIGNED) AS site1,
    CAST($name AS CHAR(255)) AS name1,
    CandidateDot.site AS site2,
    CandidateDot.name AS name2,
    CandidateDot.dotProduct / (SourceNorm.norm * CandidateNorm.norm) AS similarity
FROM (
    SELECT
        TF1.site,
        TF1.name,
        SUM(TF1.frequency * TF2.frequency) AS dotProduct
    FROM CalculatedTermFrequency TF2
    INNER JOIN CalculatedTermFrequency TF1
        ON TF1.term = TF2.term
    WHERE
        TF2.site = ${site.seq}
        AND TF2.name = $name
        AND NOT (TF1.site = ${site.seq} AND TF1.name = $name)
    GROUP BY TF1.site, TF1.name
) CandidateDot
INNER JOIN CalculatedTermFrequencyNorm CandidateNorm
    ON CandidateNorm.site = CandidateDot.site
    AND CandidateNorm.name = CandidateDot.name
CROSS JOIN (
    SELECT norm
    FROM CalculatedTermFrequencyNorm
    WHERE site = ${site.seq} AND name = $name
) SourceNorm
WHERE
    SourceNorm.norm > 0
    AND CandidateNorm.norm > 0
    AND CandidateDot.dotProduct / (SourceNorm.norm * CandidateNorm.norm) > $MinimumSimilarity
ON DUPLICATE KEY UPDATE
    similarity = VALUES(similarity)
    """.executeUpdate()

    SQL"""
INSERT INTO CalculatedCosineSimilarity (site1, name1, site2, name2, similarity)
SELECT site2, name2, site1, name1, similarity FROM CalculatedCosineSimilarity
    WHERE
        site1 = ${site.seq} AND name1 = $name
ON DUPLICATE KEY UPDATE
    similarity = VALUES(similarity)
      """.executeUpdate()
  }

  def selectSameSite(name: String)(implicit connection: Connection, site: Site): List[CalculatedCosineSimilarity] = {
    SQL"""
        SELECT site1, name1, site2, name2, similarity
            FROM CalculatedCosineSimilarity
            WHERE
                similarity > 0 AND
                site1 = ${site.seq} AND
                site2 = ${site.seq} AND
                name1 = $name AND
                name1 != name2
            ORDER BY similarity DESC
      """
      .as(long("site1") ~ str("name1") ~ long("site2") ~ str("name2") ~ double("similarity") *).map(flatten)
      .map(tables.CalculatedCosineSimilarity.tupled)
  }

  def selectCrossSite(name: String)(implicit connection: Connection, site: Site): List[CalculatedCosineSimilarity] = {
    SQL"""
        SELECT site1, name1, site2, name2, similarity
            FROM CalculatedCosineSimilarity
            WHERE
                similarity > 0 AND
                site1 = ${site.seq} AND
                site2 != ${site.seq} AND
                name1 = $name
            ORDER BY similarity DESC
      """
      .as(long("site1") ~ str("name1") ~ long("site2") ~ str("name2") ~ double("similarity") *).map(flatten)
      .map(tables.CalculatedCosineSimilarity.tupled)
  }

  def select(name: String)(implicit connection: Connection, site: Site): List[CalculatedCosineSimilarity] = {
    selectSameSite(name)
  }

  def delete(name: String)(implicit connection:Connection, site: Site): Int = {
    val sourceCount = SQL"""
      DELETE FROM CalculatedCosineSimilarity
      WHERE site1 = ${site.seq} AND name1 = $name
    """.executeUpdate()
    val targetCount = SQL"""
      DELETE FROM CalculatedCosineSimilarity
      WHERE site2 = ${site.seq} AND name2 = $name
    """.executeUpdate()
    sourceCount + targetCount
  }
}
