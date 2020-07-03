package models.tables

import java.sql.Connection

import anorm.BatchSql
import anorm.NamedParameter
import anorm._

case class TermFrequency(name:String, term:String, frequency:Int) {
  def this(name:String, kv:(String, Int)) = this(name, kv._1, kv._2)
}

object TermFrequency {
  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def insert(name: String, map:Map[String, Int])(implicit connection: Connection): Array[Int] = insert(map.map(kv => new TermFrequency(name, kv)).toSeq)

  def insert(seqTermFrequency: Seq[TermFrequency])(implicit connection: Connection): Array[Int] = {
    if(seqTermFrequency.isEmpty) {
      Array()
    } else {
      val values = seqTermFrequency.map(s => Seq[NamedParameter]('name -> s.name, 'term -> s.term, 'frequency -> s.frequency))
      BatchSql(
        "INSERT INTO TermFrequency (name, term, frequency) values ({name}, {term}, {frequency})",
        values.head,
        values.tail: _*
      ).execute()
    }
  }


  def delete(name: String)(implicit connection:Connection): Int = {
    SQL"DELETE FROM TermFrequency WHERE name = $name".executeUpdate()
  }
}
