package com.aha00a.play

import anorm.SqlParser.get
import anorm._

import java.time.LocalDateTime
import java.sql.Timestamp
import scala.util.Try

object AnormSqlParser {
  implicit val localDateTimeColumn: Column[LocalDateTime] = Column.nonNull {
    (value, meta) =>
      value match {
        case v: LocalDateTime => Right(v)
        case v: Timestamp => Right(v.toLocalDateTime)
        case v: String =>
          Try(LocalDateTime.parse(v))
            .toOption
            .toRight(TypeDoesNotMatch(s"Cannot convert $v: ${value.getClass.getName} to Java8 LocalDateTime for column ${meta.column}"))
        case _ =>
          Left(TypeDoesNotMatch(s"Cannot convert $value: ${value.getClass.getName} to Java8 LocalDateTime for column ${meta.column}"))
      }
  }

  def localDateTime(columnName: String): RowParser[LocalDateTime] =
    get[LocalDateTime](columnName)(localDateTimeColumn)
  def optionStr(columnName: String)(implicit c: Column[Option[String]]): RowParser[Option[String]] = get[Option[String]](columnName)(c)
  def optionInt(columnName: String)(implicit c: Column[Option[Int]]): RowParser[Option[Int]] = get[Option[Int]](columnName)(c)
  def optionLong(columnName: String)(implicit c: Column[Option[Long]]): RowParser[Option[Long]] = get[Option[Long]](columnName)(c)
}
