package models

import java.util.Date
import models.tables.Page

class PageMetaData(val revision: Long, val dateTime:Date, val author:String, val comment:String) extends WithDateTime {
  def this(page:Page) = this(page.revision, page.dateTime, page.author, page.comment)
}

