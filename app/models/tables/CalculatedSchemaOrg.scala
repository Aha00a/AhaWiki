package models.tables

import java.sql.Connection
import anorm.BatchSql
import anorm.NamedParameter
import anorm._
import anorm.SqlParser.flatten
import anorm.SqlParser.int
import anorm.SqlParser.str
import com.aha00a.commons.Implicits.RichString
import models.tables

case class CalculatedSchemaOrg(page: String, cls: String, prop: String, value: String) {
  def and(a: String => Boolean): Boolean = a(page) && a(value)
  def or(a: String => Boolean): Boolean = a(page) || a(value)

  private val delimiter = ":"
  def toPageCls: String = Seq(page, cls).filterNot(_.isNullOrEmpty).mkString(delimiter)
  def toPageClsProp: String = Seq(page, cls, prop).filterNot(_.isNullOrEmpty).mkString(delimiter)
  def toPageClsPropValue: String = Seq(page, cls, prop, value).filterNot(_.isNullOrEmpty).mkString(delimiter)
  def toPageProp: String = Seq(page, prop).filterNot(_.isNullOrEmpty).mkString(delimiter)
}

object CalculatedSchemaOrg {

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def insert(seq: Seq[CalculatedSchemaOrg])(implicit connection: Connection, site: Site): Array[Int] = {
    if(seq.isEmpty) {
      Array[Int]()
    } else {
      val values = seq.map(s => Seq[NamedParameter](
        "site" -> site.seq,
        "page" -> s.page,
        "cls" -> s.cls,
        "prop" -> s.prop,
        "value" -> s.value
      ))
      BatchSql(
        "REPLACE INTO CalculatedSchemaOrg (site, page, cls, prop, value) values ({site}, {page}, {cls}, {prop}, {value})",
        values.head,
        values.tail: _*
      ).execute()
    }
  }

  case class PropCnt(prop: String, cnt: Int)
  case class ClsCnt(cls: String, cnt: Int)

  def selectPropCountWhereCls(cls: String)(implicit connection: Connection, site: Site): List[PropCnt] = {
    SQL"""
        SELECT prop, COUNT(*) cnt
            FROM CalculatedSchemaOrg
            WHERE
                site = ${site.seq} AND
                cls = $cls AND
                prop <> ''
            GROUP BY prop
            ORDER BY cnt DESC
      """
      .as(str("prop") ~ int("cnt") *).map(flatten)
      .map(PropCnt.tupled)
  }

  def selectClsCount()(implicit connection: Connection, site: Site): List[ClsCnt] = {
    SQL"""
        SELECT cls, COUNT(*) cnt
            FROM CalculatedSchemaOrg
            WHERE
                site = ${site.seq} AND
                cls <> ''
            GROUP BY cls
            ORDER BY cnt DESC, cls ASC
      """
      .as(str("cls") ~ int("cnt") *).map(flatten)
      .map(ClsCnt.tupled)
  }

  def selectWherePage(page: String)(implicit connection: Connection, site: Site): List[CalculatedSchemaOrg] = {
    SQL"SELECT page, cls, prop, value FROM CalculatedSchemaOrg WHERE site = ${site.seq} AND page = $page"
      .as(str("page") ~ str("cls") ~ str("prop") ~ str("value") *).map(flatten)
      .map(tables.CalculatedSchemaOrg.tupled)
  }

  def selectWhereCls(cls: String)(implicit connection: Connection, site: Site): List[CalculatedSchemaOrg] = {
    SQL"SELECT page, cls, prop, value FROM CalculatedSchemaOrg WHERE site = ${site.seq} AND cls = $cls AND prop = ''"
      .as(str("page") ~ str("cls") ~ str("prop") ~ str("value") *).map(flatten)
      .map(tables.CalculatedSchemaOrg.tupled)
  }

  def selectWhereProp(prop: String)(implicit connection: Connection, site: Site): List[CalculatedSchemaOrg] = {
    SQL"SELECT page, cls, prop, value FROM CalculatedSchemaOrg WHERE site = ${site.seq} AND prop = $prop"
      .as(str("page") ~ str("cls") ~ str("prop") ~ str("value") *).map(flatten)
      .map(tables.CalculatedSchemaOrg.tupled)
  }

  def selectWhereValue(value: String)(implicit connection: Connection, site: Site): List[CalculatedSchemaOrg] = {
    SQL"SELECT page, cls, prop, value FROM CalculatedSchemaOrg WHERE site = ${site.seq} AND prop <> '' AND value = $value"
      .as(str("page") ~ str("cls") ~ str("prop") ~ str("value") *).map(flatten)
      .map(tables.CalculatedSchemaOrg.tupled)
  }

  def selectWherePageOrValue(pageOrValue: String)(implicit connection: Connection, site: Site): List[CalculatedSchemaOrg] = {
    SQL"""
        SELECT page, cls, prop, value
            FROM CalculatedSchemaOrg
            WHERE
                site = ${site.seq} AND
                value != '' AND
                (page = $pageOrValue OR value = $pageOrValue)
      """
      .as(str("page") ~ str("cls") ~ str("prop") ~ str("value") *).map(flatten)
      .map(tables.CalculatedSchemaOrg.tupled)
  }

  def selectWherePageOrValueIn(seq: Seq[String])(implicit connection: Connection, site: Site): Seq[CalculatedSchemaOrg] = {
    if(seq.isEmpty) {
      return Seq()
    }

    SQL"""
        SELECT page, cls, prop, value
            FROM CalculatedSchemaOrg
            WHERE
                site = ${site.seq} AND
                (page IN ($seq) OR value IN ($seq))
      """
      .as(str("page") ~ str("cls") ~ str("prop") ~ str("value") *).map(flatten)
      .map(tables.CalculatedSchemaOrg.tupled)
  }

  def delete(name: String)(implicit connection:Connection, site: Site): Int = {
    SQL"DELETE FROM CalculatedSchemaOrg WHERE site = ${site.seq} AND page = $name".executeUpdate()
  }
}
