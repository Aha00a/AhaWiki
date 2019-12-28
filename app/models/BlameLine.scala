package models

import java.util.Date

case class BlameLine(revision: Long, dateTime:Date, author:String, comment:String, line:String) extends WithDateTime
