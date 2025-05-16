package com.aha00a.play

import anorm.SqlParser.get
import anorm._

import java.time.LocalDateTime

object AnormSqlParser {
  def localDateTime(columnName: String)(implicit c: Column[LocalDateTime]): RowParser[LocalDateTime] = get[LocalDateTime](columnName)(c)
}
