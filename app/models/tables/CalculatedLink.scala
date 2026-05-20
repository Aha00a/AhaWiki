package models.tables

import anorm.BatchSql
import anorm.NamedParameter
import anorm.SqlParser.flatten
import anorm.SqlParser.long
import anorm.SqlParser.str
import anorm._
import models.tables

import java.sql.Connection

case class CalculatedLink(src:String, dst:String, alias:String) {

  import logics.wikis.PageNameLogic

  lazy val isIdentical: Boolean = src == dst
  lazy val isDstExternal: Boolean = PageNameLogic.isExternal(dst)

  def and(a: String => Boolean):Boolean = a(src) && a(dst)
  def or(a: String => Boolean):Boolean = a(src) || a(dst)
}

object CalculatedLink {

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  case class DstMaxMinCount(dst: String, max: String, min: String, count: Long)
  def selectDstMaxMinCountWhereSrcIsDatePage(seqName: Seq[String])(implicit connection: Connection, site: Site): Seq[DstMaxMinCount] = {
    SQL"""
        SELECT dst, MAX(src) max, MIN(src) min, COUNT(*) count
            FROM CalculatedLink
            WHERE
                site = ${site.seq} AND
                dst IN ($seqName) AND
                src REGEXP '[0-9]{4}-(0[1-9]|1[012])-([012][0-9]|3[01])'
            GROUP BY dst
            ORDER BY dst
      """
      .as(str("dst") ~ str("max") ~ str("min") ~ long("count") *).map(flatten)
      .map(DstMaxMinCount.tupled)
  }

  def select(name: String)(implicit connection: Connection, site: Site): List[CalculatedLink] = {
    SQL"""
        SELECT src, dst, alias
            FROM CalculatedLink
            WHERE
                site = ${site.seq} AND
                src != '' AND
                dst != '' AND
                (src = $name OR dst = $name)
      """
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(tables.CalculatedLink.tupled)
  }

  def selectDst(dst: String)(implicit connection: Connection, site: Site): List[CalculatedLink] = {
    SQL"SELECT src, dst, alias FROM CalculatedLink WHERE site = ${site.seq} AND dst = $dst"
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(tables.CalculatedLink.tupled)
  }

  def selectDstLimit1(dst: String)(implicit connection: Connection, site: Site): Option[CalculatedLink] = {
    SQL"SELECT src, dst, alias FROM CalculatedLink WHERE site = ${site.seq} AND dst = $dst LIMIT 1"
      .as(str("src") ~ str("dst") ~ str("alias") singleOpt).map(flatten)
      .map(tables.CalculatedLink.tupled)
  }

  def selectAllButNotEmpty()(implicit connection: Connection, site: Site): List[CalculatedLink] = {
    SQL"SELECT src, dst, alias FROM CalculatedLink WHERE site = ${site.seq} AND src != '' AND dst != ''"
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(tables.CalculatedLink.tupled)
  }

  def selectDistinctDstWhereDstIsYear()(implicit connection: Connection, site: Site): List[String] = {
    SQL"""SELECT DISTINCT(dst) dst FROM CalculatedLink WHERE site = ${site.seq} AND dst REGEXP '^[0-9]{4}$$' ORDER BY dst DESC"""
      .as(str("dst") *)
  }

  def insert(seq: Seq[CalculatedLink])(implicit connection: Connection, site: Site): Array[Int] = {
    if(seq.isEmpty) {
      Array[Int]()
    } else {
      val values = seq.map(s => Seq[NamedParameter](
        "site" -> site.seq,
        "src" -> s.src,
        "dst" -> s.dst,
        "alias" -> s.alias
      ))
      BatchSql(
        "REPLACE INTO CalculatedLink (site, src, dst, alias) values ({site}, {src}, {dst}, {alias})",
        values.head,
        values.tail: _*
      ).execute()
    }
  }

  def delete(name: String)(implicit connection:Connection, site: Site): Int = {
    SQL"DELETE FROM CalculatedLink WHERE site = ${site.seq} AND src = $name".executeUpdate()
  }
}
