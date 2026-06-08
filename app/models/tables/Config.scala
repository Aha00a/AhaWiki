package models.tables

import anorm.SqlParser.flatten
import anorm.SqlParser.str
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime
import play.api.Logging
import com.aha00a.commons.Implicits._
import java.time.LocalDateTime

case class Config(k: String, v: String, created: LocalDateTime, updated: LocalDateTime)

object Config extends Logging{

  import java.sql.Connection

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  def select(k: String)(implicit connection: Connection, site: Site): Option[Config] = {
      SQL"""
        SELECT k, v, created, updated
            FROM Config
            WHERE site = ${site.seq} AND k = $k"""
      .as(str("k") ~ str("v") ~ localDateTime("created") ~ localDateTime("updated") singleOpt).map(flatten)
      .map(tupled)
  }

  def getOrElse(k: String, default: String)(implicit connection: Connection, site: Site): String = select(k).map(_.v).getOrElse(default)
  def getOrElse(k: String, default: Boolean)(implicit connection: Connection, site: Site): Boolean = getOrElse(k, default.toString).toBoolGenerously
  def upsert(k: String, v: String)(implicit connection: Connection, site: Site): Int = {
    SQL"""
      INSERT INTO Config (site, k, v)
      VALUES (${site.seq}, $k, $v)
      ON DUPLICATE KEY UPDATE v = $v, updated = NOW()
    """.executeUpdate()
  }
  def delete(k: String)(implicit connection: Connection, site: Site): Int = {
    SQL"DELETE FROM Config WHERE site = ${site.seq} AND k = $k".executeUpdate()
  }

  object Query {
    private def fqn: String = {
      val ste = Thread.currentThread.getStackTrace()(2)
      (ste.getClassName.replace(getClass.getName, "") + ste.getMethodName).replaceAll("\\$", ".")
    }

    // no use of this. But I want to keep this for the future use.
    object Google {
      object AdSense {
        def adClient()(implicit connection: Connection, site: Site): String = Config.getOrElse(fqn, "")
        def adsTxtContent()(implicit connection: Connection, site: Site): String = Config.getOrElse(fqn, "")
      }
    }

    object Telegram {
      private val key = "Telegram.chatId"
      def chatId()(implicit connection: Connection, site: Site): Option[String] =
        Config.select(key).map(_.v.trim).filter(_.nonEmpty)
      def saveChatId(v: String)(implicit connection: Connection, site: Site): Unit =
        if (v.trim.nonEmpty) Config.upsert(key, v.trim)
        else                 Config.delete(key)
    }
  }
}
