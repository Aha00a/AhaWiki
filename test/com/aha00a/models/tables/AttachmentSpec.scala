package com.aha00a.models.tables

import com.aha00a.tests.TestSchema
import anorm.SQL
import models.tables.Attachment
import org.scalatest.freespec.AnyFreeSpec

import java.sql.Connection
import java.sql.DriverManager

/** `selectObjectKeysHeldByOtherPages` is what keeps a page delete from taking another page's S3
  * objects. A page delete lists S3 by the sanitized page prefix, and sanitizing maps `A B` and
  * `A_B` onto the one prefix `A_B/`, so deleting `A B` lists `A_B`'s objects; the caller drops
  * any key this query says another page still holds. */
class AttachmentSpec extends AnyFreeSpec {
  private def withConnection[T](f: Connection => T): T = {
    Class.forName("org.h2.Driver")
    val dbName = s"attachment_${java.util.UUID.randomUUID().toString.replace("-", "")}"
    val connection = DriverManager.getConnection(s"jdbc:h2:mem:$dbName;MODE=MySQL;DB_CLOSE_DELAY=-1")
    try {
      TestSchema.createAll()(connection)
      Seq(
        "INSERT INTO Site (seq, name, abbr) VALUES (1, 'SiteA', 'SiteA')",
        "INSERT INTO Site (seq, name, abbr) VALUES (2, 'SiteB', 'SiteB')",
      ).foreach(sql => SQL(sql).execute()(connection))
      f(connection)
    } finally {
      connection.close()
    }
  }

  private def insert(site: Long, pageName: String, objectKey: String, deleted: Boolean = false)(implicit c: Connection): Unit = {
    val dateDeleted = if (deleted) "NOW()" else "NULL"
    SQL(
      s"""INSERT INTO Attachment
         |  (site, pageName, originalFilename, storedFilename, bucket, objectKey, contentType, fileSize, status, dateDeleted)
         |VALUES
         |  ($site, '$pageName', 'x.png', 'x.png', 'b', '$objectKey', 'image/png', 1, 'Uploaded', $dateDeleted)""".stripMargin
    ).execute()
  }

  "selectObjectKeysHeldByOtherPages" - {
    val key = "Attachment/1/A_B/x.png"

    "returns a key another page on the site still holds" in withConnection { implicit c =>
      insert(1, "A_B", key)
      assert(Attachment.selectObjectKeysHeldByOtherPages(1, "A B", Seq(key)) === Seq(key))
    }

    "does not return a key the same page holds" in withConnection { implicit c =>
      insert(1, "A_B", key)
      assert(Attachment.selectObjectKeysHeldByOtherPages(1, "A_B", Seq(key)).isEmpty)
    }

    "does not return a key whose only holder is soft-deleted" in withConnection { implicit c =>
      insert(1, "A_B", key, deleted = true)
      assert(Attachment.selectObjectKeysHeldByOtherPages(1, "A B", Seq(key)).isEmpty)
    }

    "is scoped to the site" in withConnection { implicit c =>
      insert(2, "A_B", key)
      assert(Attachment.selectObjectKeysHeldByOtherPages(1, "A B", Seq(key)).isEmpty)
    }

    "returns empty for no keys, and for a key nobody holds" in withConnection { implicit c =>
      insert(1, "A_B", key)
      assert(Attachment.selectObjectKeysHeldByOtherPages(1, "A B", Seq.empty).isEmpty)
      assert(Attachment.selectObjectKeysHeldByOtherPages(1, "A B", Seq("Attachment/1/Other/y.png")).isEmpty)
    }
  }
}
