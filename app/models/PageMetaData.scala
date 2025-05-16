package models

import models.tables.Page

import java.time.LocalDateTime

class PageMetaData(val revision: Long, val dateTime: LocalDateTime, val author: String, val comment: String) extends WithDateTime {
  def this(page: Page) = this(page.revision, page.dateTime, page.author, page.comment)
}

