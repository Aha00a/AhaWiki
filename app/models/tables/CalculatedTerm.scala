package models.tables

import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime

import java.sql.Connection
import java.time.LocalDateTime
import scala.annotation.tailrec

case class CalculatedTerm(seq: Long, dateInserted: LocalDateTime, term:String)

object CalculatedTerm {
  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def insert(term: String)(implicit connection: Connection): Option[Long] = {
    SQL"INSERT INTO CalculatedTerm (term) VALUES ($term)".executeInsert()
  }


  def select(term: String)(implicit connection: Connection): Option[CalculatedTerm] = {
    SQL"""SELECT seq, dateInserted, term FROM CalculatedTerm WHERE term = $term"""
      .as(long("seq") ~ localDateTime("dateInserted") ~ str("term") singleOpt).map(flatten)
      .map(tupled)
  }

  def select(seq: Long)(implicit connection: Connection): Option[CalculatedTerm] = {
    SQL"""SELECT seq, dateInserted, term FROM CalculatedTerm WHERE seq = $seq"""
      .as(long("seq") ~ localDateTime("dateInserted") ~ str("term") singleOpt).map(flatten)
      .map(tupled)
  }

  def delete(term: String)(implicit connection:Connection): Int = {
    SQL"DELETE FROM CalculatedTerm WHERE term = $term".executeUpdate()
  }

  @tailrec
  def ensureSeq(term: String) (implicit connection: Connection, site: Site): Option[Long] = {
    select(term) match {
      case Some(v) => Some(v.seq)
      case None =>
        insert(term)
        ensureSeq(term)
    }
  }
}
