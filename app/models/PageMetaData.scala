package models

import models.tables.Page

import java.time.LocalDateTime

class PageMetaData(val revision: Long, val dateTime: LocalDateTime, val nickname: String, val comment: String) extends WithDateTime {
  def this(page: Page) = this(page.revision, page.dateTime, page.nickname.getOrElse(""), page.comment)
}
