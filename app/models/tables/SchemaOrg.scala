package models.tables

import java.sql.Connection

import anorm.BatchSql
import anorm.NamedParameter
import anorm._
import anorm.SqlParser.flatten
import anorm.SqlParser.int
import anorm.SqlParser.str
import models.tables

case class SchemaOrg(page: String, cls: String, prop: String, value: String) {
  def and(a: String => Boolean):Boolean = a(page) && a(value)
  def or(a: String => Boolean):Boolean = a(page) || a(value)
}

object SchemaOrg {

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def insert(seq: Seq[SchemaOrg])(implicit connection: Connection): Array[Int] = {
    if(seq.isEmpty) {
      Array[Int]()
    } else {
      val values = seq.map(s => Seq[NamedParameter](Symbol("page") -> s.page, Symbol("cls") -> s.cls, Symbol("prop") -> s.prop, Symbol("value") -> s.value))
      BatchSql(
        "REPLACE INTO SchemaOrg (page, cls, prop, value) values ({page}, {cls}, {prop}, {value})",
        values.head,
        values.tail: _*
      ).execute()
    }
  }

  case class PropCnt(prop: String, cnt: Int)

  def selectPropCountWhereCls(cls: String)(implicit connection: Connection): List[PropCnt] = {
    SQL"""SELECT prop, COUNT(*) cnt FROM SchemaOrg WHERE cls = $cls AND prop <> '' GROUP BY prop ORDER BY cnt DESC"""
      .as(str("prop") ~ int("cnt") *).map(flatten)
      .map(PropCnt.tupled)
  }

  def selectWherePage(page: String)(implicit connection: Connection): List[SchemaOrg] = {
    SQL"SELECT page, cls, prop, value FROM SchemaOrg WHERE page = $page"
      .as(str("page") ~ str("cls") ~ str("prop") ~ str("value") *).map(flatten)
      .map(tables.SchemaOrg.tupled)
  }

  def selectWhereCls(cls: String)(implicit connection: Connection): List[SchemaOrg] = {
    SQL"SELECT page, cls, prop, value FROM SchemaOrg WHERE cls = $cls AND prop = ''"
      .as(str("page") ~ str("cls") ~ str("prop") ~ str("value") *).map(flatten)
      .map(tables.SchemaOrg.tupled)
  }

  def selectWhereProp(prop: String)(implicit connection: Connection): List[SchemaOrg] = {
    SQL"SELECT page, cls, prop, value FROM SchemaOrg WHERE prop = $prop"
      .as(str("page") ~ str("cls") ~ str("prop") ~ str("value") *).map(flatten)
      .map(tables.SchemaOrg.tupled)
  }

  def selectWhereValue(value: String)(implicit connection: Connection): List[SchemaOrg] = {
    SQL"SELECT page, cls, prop, value FROM SchemaOrg WHERE value = $value"
      .as(str("page") ~ str("cls") ~ str("prop") ~ str("value") *).map(flatten)
      .map(tables.SchemaOrg.tupled)
  }

  def selectWherePageOrValue(pageOrValue: String)(implicit connection: Connection): List[SchemaOrg] = {
    SQL"SELECT page, cls, prop, value FROM SchemaOrg WHERE value != '' AND page = $pageOrValue OR value = $pageOrValue"
      .as(str("page") ~ str("cls") ~ str("prop") ~ str("value") *).map(flatten)
      .map(tables.SchemaOrg.tupled)
  }

  def selectWherePageOrValueIn(seq: Seq[String])(implicit connection: Connection): List[SchemaOrg] = {
    SQL"SELECT page, cls, prop, value FROM SchemaOrg WHERE page IN ($seq) OR value IN ($seq)"
      .as(str("page") ~ str("cls") ~ str("prop") ~ str("value") *).map(flatten)
      .map(tables.SchemaOrg.tupled)
  }

  def delete(name: String)(implicit connection:Connection): Int = {
    SQL"DELETE FROM SchemaOrg WHERE page = $name".executeUpdate()
  }
}
