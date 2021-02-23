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
  def toSeqString: Seq[String] = Seq(src, dst)
}

object Link {

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def selectCountWhereAlias(alias: String)(implicit connection: Connection, site: Site): Long = {
    SQL"""SELECT COUNT(*) cnt FROM Link WHERE site = ${site.seq} AND alias = $alias"""
      .as(long("cnt") single)
  }

  def selectBacklinkOfDatePage(seqName: Seq[String])(implicit connection: Connection, site: Site): List[Link] = {
    SQL"""
        SELECT src, dst, alias
            FROM Link
            WHERE
                site = ${site.seq} AND
                dst IN ($seqName) AND
                src REGEXP '[0-9]{4}-(0[1-9]|1[012])-([012][0-9]|3[01])'
            ORDER BY dst, src DESC
      """
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(tables.Link.tupled)
  }

  def select(name: String)(implicit connection: Connection, site: Site): List[Link] = {
    SQL"""
        SELECT src, dst, alias
            FROM Link
            WHERE
                site = ${site.seq} AND
                src != '' AND
                dst != '' AND
                (src = $name OR dst = $name)
      """
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(tables.Link.tupled)
  }

  def selectDst(dst: String)(implicit connection: Connection, site: Site): List[Link] = {
    SQL"SELECT src, dst, alias FROM Link WHERE site = ${site.seq} AND dst = $dst"
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(tables.Link.tupled)
  }

  def selectDstLimit1(dst: String)(implicit connection: Connection, site: Site): Option[Link] = {
    SQL"SELECT src, dst, alias FROM Link WHERE site = ${site.seq} AND dst = $dst LIMIT 1"
      .as(str("src") ~ str("dst") ~ str("alias") singleOpt).map(flatten)
      .map(tables.Link.tupled)
  }

  def selectAllButNotEmpty()(implicit connection: Connection, site: Site): List[Link] = {
    SQL"SELECT src, dst, alias FROM Link WHERE site = ${site.seq} AND src != '' AND dst != ''"
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(tables.Link.tupled)
  }

  def selectDistinctDstWhereDstIsYear()(implicit connection: Connection, site: Site): List[String] = {
    SQL"""SELECT DISTINCT(dst) dst FROM Link WHERE site = ${site.seq} AND dst REGEXP '^[0-9]{4}$$' ORDER BY dst DESC"""
      .as(str("dst") *)
  }
  
  def selectWhereSrcORDstIn(seq: Seq[String])(implicit connection: Connection, site: Site): Seq[Link] = {
    if(seq.isEmpty) {
      return Seq()
    }

    SQL"""
        SELECT src, dst, alias
            FROM Link
            WHERE
                site = ${site.seq} AND
                src != '' AND dst != ''
                AND (src IN ($seq) OR dst IN ($seq))
      """
      .as(str("src") ~ str("dst") ~ str("alias") *).map(flatten)
      .map(tables.Link.tupled)
  }


  def insert(seq: Seq[Link])(implicit connection: Connection, site: Site): Array[Int] = {
    if(seq.isEmpty) {
      Array[Int]()
    } else {
      val values = seq.map(s => Seq[NamedParameter](
        Symbol("site") -> site.seq,
        Symbol("src") -> s.src,
        Symbol("dst") -> s.dst,
        Symbol("alias") -> s.alias
      ))
      BatchSql(
        "REPLACE INTO Link (site, src, dst, alias) values ({site}, {src}, {dst}, {alias})",
        values.head,
        values.tail: _*
      ).execute()
    }
  }

  def delete(name: String)(implicit connection:Connection, site: Site): Int = {
    SQL"DELETE FROM Link WHERE site = ${site.seq} AND src = $name".executeUpdate()
  }
}
