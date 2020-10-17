package models.tables

import java.sql.Connection

import anorm.BatchSql
import anorm.NamedParameter
import anorm._
import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str
import models.tables

case class Link(src:String, dst:String, alias:String) {

  import logics.wikis.PageNameLogic

  lazy val isDstExternal: Boolean = PageNameLogic.isExternal(dst)

  def and(a: String => Boolean):Boolean = a(src) && a(dst)
  def or(a: String => Boolean):Boolean = a(src) || a(dst)
}

object Link {

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def selectCountWhereAlias(alias: String)(implicit connection: Connection): Long = {
    SQL"SELECT COUNT(*) cnt FROM Link WHERE alias = $alias"
      .as(long("cnt") single)
  }

  def selectBacklinkOfDatePage(name: String)(implicit connection: Connection): List[Link] = {
    SQL"""SELECT src, dst, alias FROM Link WHERE dst = $name AND src REGEXP '[0-9]{4}-(0[1-9]|1[012])-([012][0-9]|3[01])'"""
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(tables.Link.tupled)
  }

  def select(name: String)(implicit connection: Connection): List[Link] = {
    SQL"""SELECT src, dst, alias
           FROM Link
           WHERE
            src != '' AND dst != '' AND (src = $name OR dst = $name)
        """
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(tables.Link.tupled)
  }

  def selectDst(dst: String)(implicit connection: Connection): List[Link] = {
    SQL"SELECT src, dst, alias FROM Link WHERE dst = $dst"
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(tables.Link.tupled)
  }

  def selectAllButNotEmpty()(implicit connection: Connection): List[Link] = {
    SQL"SELECT src, dst, alias FROM Link WHERE src != '' AND dst != ''"
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(tables.Link.tupled)
  }

  def selectDistinctDstWhereDstIsYear()(implicit connection: Connection): List[String] = {
    SQL"""SELECT DISTINCT(dst) dst FROM Link WHERE dst REGEXP '^[0-9]{4}$$' ORDER BY dst DESC"""
      .as(str("dst") *)
  }


  def expand(seq: Seq[Link])(implicit connection: Connection): Seq[Link] = {
    val backward: Seq[Link] = seq.flatMap(l => select(l.src))
    val forward: Seq[Link] = seq.flatMap(l => select(l.dst))
    val seqExpanded: Seq[Link] = seq ++ backward ++ forward
    seqExpanded.distinct
  }


  def insert(seq: Seq[Link])(implicit connection: Connection): Array[Int] = {
    if(seq.isEmpty) {
      Array[Int]()
    } else {
      val values = seq.map(s => Seq[NamedParameter](Symbol("src") -> s.src, Symbol("dst") -> s.dst, Symbol("alias") -> s.alias))
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
