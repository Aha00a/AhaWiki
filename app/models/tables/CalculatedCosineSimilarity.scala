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

  def recalc(name: String)(implicit connection: Connection, site: Site): Int = {
    SQL"""
      DELETE FROM CalculatedCosineSimilarity
      WHERE (site1 = ${site.seq} AND name1 = $name)
         OR (site2 = ${site.seq} AND name2 = $name)
    """.executeUpdate()
    SQL"""
REPLACE INTO CalculatedCosineSimilarity (site1, name1, site2, name2, similarity)
SELECT *
    FROM (
        SELECT
            TF3.site AS site1,
            TF3.name AS name1,
            CAST(${site.seq} AS SIGNED) AS site2,
            CAST($name AS CHAR(255)) AS name2,
            IFNULL(
                (
                    SELECT
                        SUM(TF1.frequency * TF2.frequency) product
                        FROM CalculatedTermFrequency TF1
                        INNER JOIN CalculatedTermFrequency TF2 ON TF1.term = TF2.term
                        WHERE
                            TF1.site = TF3.site AND TF1.name = TF3.name AND
                            TF2.site = ${site.seq} AND TF2.name = $name
                )
                /
                (
                    (
                        SELECT
                            SQRT(SUM(frequency * frequency))
                            FROM CalculatedTermFrequency
                            WHERE
                                site = TF3.site AND name = TF3.name
                    )
                    *
                    (
                        SELECT
                            SQRT(SUM(frequency * frequency))
                            FROM CalculatedTermFrequency
                            WHERE
                                site = ${site.seq} AND name = $name
                    )
                ),
                0
            ) similarity
            FROM (
                SELECT
                    DISTINCT TF.site, TF.name
                    FROM CalculatedTermFrequency TF
                    INNER JOIN PageMeta PM
                        ON PM.site = TF.site
                        AND PM.name = TF.name
            ) TF3
    ) CS1
    WHERE similarity > 0.3 AND NOT (site1 = site2 AND name1 = name2)
    """.executeUpdate()

    SQL"""
REPLACE INTO CalculatedCosineSimilarity (site1, name1, site2, name2, similarity)
SELECT site2, name2, site1, name1, similarity FROM CalculatedCosineSimilarity
    WHERE
        site2 = ${site.seq} AND name2 = $name
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
    SQL"""
      DELETE FROM CalculatedCosineSimilarity
      WHERE (site1 = ${site.seq} AND name1 = $name)
         OR (site2 = ${site.seq} AND name2 = $name)
    """.executeUpdate()
  }
}

