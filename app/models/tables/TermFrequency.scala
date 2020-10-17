package models.tables

import java.sql.Connection

import anorm.BatchSql
import anorm.NamedParameter
import anorm._

case class TermFrequency(name:String, term:String, frequency:Int) {
  def this(name:String, kv:(String, Int)) = this(name, kv._1, kv._2)
}

case class HighScoredTerm(name:String, term:String, frequency1:Int, frequency2:Int)

object TermFrequency {
  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def insert(name: String, map:Map[String, Int])(implicit connection: Connection): Array[Int] = insert(map.map(kv => new TermFrequency(name, kv)).toSeq)

  def insert(seqTermFrequency: Seq[TermFrequency])(implicit connection: Connection): Array[Int] = {
    if(seqTermFrequency.isEmpty) {
      Array()
    } else {
      val values = seqTermFrequency.map(s => Seq[NamedParameter](Symbol("name") -> s.name, Symbol("term") -> s.term, Symbol("frequency") -> s.frequency))
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

  def selectHighScoredTerm(name:String, similarPageNames:Seq[String])(implicit connection: Connection): Seq[HighScoredTerm] = {
    import anorm.SqlParser.flatten
    import anorm.SqlParser.int
    import anorm.SqlParser.str

    import scala.collection.immutable
    if(similarPageNames.isEmpty) {
      immutable.Seq()
    } else {
      SQL("""SELECT
            |    tf2.name, tf2.term, tf1.frequency frequency1, tf2.frequency frequency2
            |    FROM TermFrequency tf1
            |    INNER JOIN TermFrequency tf2 ON tf1.term = tf2.term
            |    WHERE
            |        tf1.name = {name} AND tf2.name IN ({pageNames})
            |    ORDER BY frequency1 + frequency2 DESC""".stripMargin)
        .on(Symbol("name") -> name, Symbol("pageNames") -> similarPageNames)
        .as(str("name") ~ str("term") ~ int("frequency1") ~ int("frequency2") *).map(flatten)
        .map(HighScoredTerm.tupled)
    }
  }

}
