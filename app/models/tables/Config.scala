package models.tables

import java.util.Date

import anorm.SqlParser.date
import anorm.SqlParser.flatten
import anorm.SqlParser.str
import anorm._
import play.api.Logging
import com.aha00a.commons.Implicits._

case class Config(k: String, v: String, created: Date, updated: Date)

object Config extends Logging{

  import java.sql.Connection

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def select(k: String)(implicit connection: Connection, site: Site): Option[Config] = {
    SQL"""
        SELECT k, v, created, updated
            FROM Config
            WHERE site = ${site.seq} AND k = $k"""
      .as(str("k") ~ str("v") ~ date("created") ~ date("updated") singleOpt).map(flatten)
      .map(tupled)
  }

  def getOrElse(k: String, default: String)(implicit connection: Connection, site: Site): String = select(k).map(_.v).getOrElse(default)
  def getOrElse(k: String, default: Boolean)(implicit connection: Connection, site: Site): Boolean = getOrElse(k, default.toString).toBoolGenerously

  object Query {
    private def fqn: String = {
      val ste = Thread.currentThread.getStackTrace()(2)
      (ste.getClassName.replace(getClass.getName, "") + ste.getMethodName).replaceAll("\\$", ".")
    }

    object InterpreterVim {
      def colorScheme()(implicit connection: Connection, site: Site): String = Config.getOrElse(fqn, "ron")
      def debug()(implicit connection: Connection, site: Site): Boolean = Config.getOrElse(fqn, default = false)
    }
  }
}

