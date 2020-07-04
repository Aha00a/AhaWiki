package models.tables

import java.util.Date

import models.WithDateTime

case class Page                        (name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String, comment: String, permRead: String, content: String) extends WithDateTime
case class PageWithoutContent          (name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String, comment: String, permRead: String) extends WithDateTime
case class PageWithoutContentWithSize  (name: String, revision: Long, dateTime: Date, author: String, remoteAddress: String, comment: String, permRead: String, size: Long) extends WithDateTime
