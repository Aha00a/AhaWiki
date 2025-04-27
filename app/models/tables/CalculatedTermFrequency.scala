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

  def delete(name: String)(implicit connection:Connection, site: Site): Int = {
    SQL"DELETE FROM CalculatedTermFrequency WHERE site = ${site.seq} AND name = $name".executeUpdate()
  }
}
