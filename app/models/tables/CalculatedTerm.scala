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

  // The no-op ON DUPLICATE KEY UPDATE absorbs the ensureSeq race: both instances select, both
  // see the term missing, both insert, and the loser used to throw on the UNIQUE key `term`.
  // Callers re-select after inserting, so keeping the winner's row is all that is needed.
  // Not REPLACE — REPLACE deletes the existing row, which would hand the same term a new seq
  // and pull it out from under CalculatedTermFrequency rows pointing at the old one.
  def insert(term: String)(implicit connection: Connection): Option[Long] = {
    SQL"INSERT INTO CalculatedTerm (term) VALUES ($term) ON DUPLICATE KEY UPDATE seq = seq".executeInsert()
  }


  def select(term: String)(implicit connection: Connection): Option[CalculatedTerm] = {
    SQL"""SELECT seq, dateInserted, term FROM CalculatedTerm WHERE term = $term"""
      .as(long("seq") ~ localDateTime("dateInserted") ~ str("term") singleOpt).map(flatten)
      .map(tupled)
  }

  def select(terms: Seq[String])(implicit connection: Connection): Seq[CalculatedTerm] = {
    val distinctTerms = terms.distinct
    if (distinctTerms.isEmpty) {
      Seq.empty
    } else {
      SQL"""SELECT seq, dateInserted, term FROM CalculatedTerm WHERE term IN ($distinctTerms)"""
        .as(long("seq") ~ localDateTime("dateInserted") ~ str("term") *).map(flatten)
        .map(tupled)
    }
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

  def ensureSeqByTerm(terms: Seq[String])(implicit connection: Connection): Map[String, Long] = {
    val distinctTerms = terms.distinct
    if (distinctTerms.isEmpty) {
      Map.empty
    } else {
      val existing = select(distinctTerms).map(term => term.term -> term.seq).toMap
      distinctTerms.filterNot(existing.contains).foreach(insert)
      select(distinctTerms).map(term => term.term -> term.seq).toMap
    }
  }
}
