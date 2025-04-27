package models.tables

import anorm.SqlParser.double
import anorm.SqlParser.flatten
import anorm.SqlParser.str
import anorm._
import models.tables

import java.sql.Connection
import scala.util.Random

case class CalculatedCosineSimilarity(name1: String, name2: String, similarity: Double) {
  def and(a: String => Boolean):Boolean = a(name1) && a(name2)
  def or(a: String => Boolean):Boolean = a(name1) || a(name2)
}

object CalculatedCosineSimilarity {

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def recalc(name: String)(implicit connection: Connection, site: Site): Int = {
    SQL"""DELETE FROM CalculatedCosineSimilarity WHERE site = ${site.seq} AND (name1 = $name OR name2 = $name)""".executeUpdate()
    SQL"""
REPLACE INTO CalculatedCosineSimilarity (site, name1, name2, similarity)
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
                        FROM CalculatedTermFrequency TF1
                        INNER JOIN CalculatedTermFrequency TF2 ON TF1.term = TF2.term
                        WHERE
                            TF1.site = ${site.seq} AND TF1.name = TF3.name AND
                            TF2.site = ${site.seq} AND TF2.name = $name
                )
                /
                (
                    (
                        SELECT
                            SQRT(SUM(frequency * frequency))
                            FROM CalculatedTermFrequency
                            WHERE
                                site = ${site.seq} AND name = TF3.name
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
                    DISTINCT name
                    FROM CalculatedTermFrequency
                    WHERE site = ${site.seq}
            ) TF3
    ) CS1
    WHERE similarity > 0.3 AND name1 != name2
    """.executeUpdate()

    SQL"""
REPLACE INTO CalculatedCosineSimilarity (site, name1, name2, similarity)
SELECT site, name2, name1, similarity FROM CalculatedCosineSimilarity
    WHERE
        site = ${site.seq} AND
        name2 = $name
      """.executeUpdate()
  }

  def select(name: String)(implicit connection: Connection, site: Site): List[CalculatedCosineSimilarity] = {
    SQL"""
        SELECT name1, name2, similarity
            FROM CalculatedCosineSimilarity
            WHERE
                similarity > 0 AND
                site = ${site.seq} AND name1 = $name AND name1 != name2
            ORDER BY similarity DESC
      """
      .as(str("name1") ~ str("name2") ~ double("similarity") *).map(flatten)
      .map(tables.CalculatedCosineSimilarity.tupled)
  }

  def delete(name: String)(implicit connection:Connection, site: Site): Int = {
    SQL"""DELETE FROM CalculatedCosineSimilarity WHERE site = ${site.seq} AND name1 = $name OR name2 = $name""".executeUpdate()
  }
}

