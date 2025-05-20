package com.aha00a.play

import anorm.SqlParser.get
import anorm._

import java.time.LocalDateTime

object AnormSqlParser {
  def localDateTime(columnName: String)(implicit c: Column[LocalDateTime]): RowParser[LocalDateTime] = get[LocalDateTime](columnName)(c)
  def optionStr(columnName: String)(implicit c: Column[Option[String]]): RowParser[Option[String]] = get[Option[String]](columnName)(c)
  def optionInt(columnName: String)(implicit c: Column[Option[Int]]): RowParser[Option[Int]] = get[Option[Int]](columnName)(c)
}
