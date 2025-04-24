package models.tables

import anorm.SqlParser.double
import anorm.SqlParser.flatten
import anorm.SqlParser.str
import anorm._
import models.tables

import java.sql.Connection
import scala.util.Random

case class CosineSimilarity(name1: String, name2: String, similarity: Double) {
  def and(a: String => Boolean):Boolean = a(name1) && a(name2)
  def or(a: String => Boolean):Boolean = a(name1) || a(name2)
}

object CosineSimilarity {

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def recalc(name: String)(implicit connection: Connection, site: Site): Int = {
    SQL"""DELETE FROM CosineSimilarity WHERE site = ${site.seq} AND (name1 = $name OR name2 = $name)""".executeUpdate()
    if(Random.nextBoolean()) {
      SQL"""
REPLACE INTO CosineSimilarity (site, name1, name2, similarity)
SELECT *
    FROM (
        SELECT
            ${site.seq},
            TF3.name name1,
            $name name2,
            IFNULL(
                (
                    SELECT
                        SUM(TF1.frequency * TF2.frequency) product
                        FROM TermFrequency TF1
                        INNER JOIN TermFrequency TF2 ON TF1.term = TF2.term
                        WHERE
                            TF1.site = ${site.seq} AND TF1.name = TF3.name AND
                            TF2.site = ${site.seq} AND TF2.name = $name
                )
                /
                (
                    (
                        SELECT
                            SQRT(SUM(frequency * frequency))
                            FROM TermFrequency
                            WHERE
                                site = ${site.seq} AND name = TF3.name
                    )
                    *
                    (
                        SELECT
                            SQRT(SUM(frequency * frequency))
                            FROM TermFrequency
                            WHERE
                                site = ${site.seq} AND name = $name
                    )
                ),
                0
            ) similarity
            FROM (
                SELECT
                    DISTINCT name
                    FROM TermFrequency
                    WHERE site = ${site.seq}
            ) TF3
    ) CS1
    WHERE similarity > 0.3 AND name1 != name2
      """.executeUpdate()
    } else {
      SQL"""
REPLACE INTO CosineSimilarity (site, name1, name2, similarity)
WITH
Norm AS (
    SELECT
        name,
        SQRT(SUM(frequency * frequency)) AS norm
    FROM TermFrequency
    WHERE site = ${site.seq}
    GROUP BY name
),
NormTarget AS (
    SELECT
        SQRT(SUM(frequency * frequency)) AS norm
    FROM TermFrequency
    WHERE site = ${site.seq} AND name = $name
),
DotProduct AS (
    SELECT
        TF1.name AS name1,
        SUM(TF1.frequency * TF2.frequency) AS dot
    FROM TermFrequency TF1
    JOIN TermFrequency TF2
        ON TF1.term = TF2.term
    WHERE
        TF1.site = ${site.seq} AND
        TF2.site = ${site.seq} AND
        TF2.name = $name
    GROUP BY TF1.name
)
SELECT
    ${site.seq} AS site,
    DP.name1,
    $name AS name2,
    IFNULL(DP.dot / (N.norm * NT.norm), 0) AS similarity
FROM DotProduct DP
JOIN Norm N ON DP.name1 = N.name
CROSS JOIN NormTarget NT
WHERE
    DP.name1 != $name
    AND IFNULL(DP.dot / (N.norm * NT.norm), 0) > 0.3
      """.executeUpdate()
    }

    SQL"""
REPLACE INTO CosineSimilarity (site, name1, name2, similarity)
SELECT site, name2, name1, similarity FROM CosineSimilarity
    WHERE
        site = ${site.seq} AND
        name2 = $name
      """.executeUpdate()
  }

  def select(name: String)(implicit connection: Connection, site: Site): List[CosineSimilarity] = {
    SQL"""
        SELECT name1, name2, similarity
            FROM CosineSimilarity
            WHERE
                similarity > 0 AND
                site = ${site.seq} AND name1 = $name AND name1 != name2
            ORDER BY similarity DESC
      """
      .as(str("name1") ~ str("name2") ~ double("similarity") *).map(flatten)
      .map(tables.CosineSimilarity.tupled)
  }

  def delete(name: String)(implicit connection:Connection, site: Site): Int = {
    SQL"""DELETE FROM CosineSimilarity WHERE site = ${site.seq} AND name1 = $name OR name2 = $name""".executeUpdate()
  }
}

