package com.aha00a.tests

import org.scalatest.freespec.AnyFreeSpec

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.jdk.CollectionConverters._

/**
 * Every table in the database is `utf8mb4_bin`, but the database's own default is
 * `utf8mb4_unicode_ci`, so a `CREATE TABLE` that does not say which one it wants gets the
 * other one. Evolution 68 did exactly that and nothing noticed until the table was compared
 * with production; evolution 69 repairs it. From 69 on, a `CREATE TABLE` in an evolution has
 * to carry a table-level `COLLATE`.
 */
object EvolutionConvention {
  val FirstGovernedEvolution: Int = 69

  private val createTableName = """(?is)CREATE\s+TABLE\s+(?:IF\s+NOT\s+EXISTS\s+)?`?(\w+)`?""".r

  /** Tables the SQL creates without a table-level collation, in order of appearance. */
  def tablesCreatedWithoutCollation(sql: String): Seq[String] = {
    val withoutComments = sql.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")
    withoutComments.split(";").toSeq.map(_.trim).filter(_.nonEmpty).flatMap { statement =>
      createTableName.findFirstMatchIn(statement).map(_.group(1)).filter { _ =>
        // Column definitions sit inside the parentheses; what follows the last one is the
        // table options, which is the only place a table-level COLLATE can be.
        val tableOptions = statement.substring(statement.lastIndexOf(')') + 1)
        !tableOptions.toUpperCase.contains("COLLATE")
      }
    }
  }

  /**
   * Comment lines that contain a semicolon.
   *
   * Play splits an evolution into statements on every `;` before the database sees any of
   * it, and it does not know what a comment is. A semicolon inside a comment therefore ends
   * a statement there, and whatever follows on the next lines runs as SQL of its own —
   * which is prose, and fails. The first attempt at evolution 69 did this in production.
   */
  def commentLinesWithSemicolon(sql: String): Seq[String] =
    sql.linesIterator.map(_.trim).filter(line => (line.startsWith("#") || line.startsWith("--")) && line.contains(";")).toSeq

  def evolutionFiles(directory: Path): Seq[(Int, Path)] =
    Files.list(directory).iterator().asScala.toSeq.flatMap { path =>
      """(\d+)\.sql""".r.findFirstMatchIn(path.getFileName.toString).map(m => (m.group(1).toInt, path))
    }.sortBy(_._1)
}

class EvolutionConventionSpec extends AnyFreeSpec {
  private val evolutionsDirectory = Paths.get("conf", "evolutions", "default")

  "tablesCreatedWithoutCollation" - {
    "flags a table that only says COLLATE on a column, which is what evolution 68 did" in {
      val evolution68 = Files.readString(evolutionsDirectory.resolve("68.sql"))
      assert(EvolutionConvention.tablesCreatedWithoutCollation(evolution68) === Seq("UserNicknameChangeRequest"))
    }

    "accepts a table-level COLLATE and ignores statements that create nothing" in {
      val sql =
        """# --- !Ups
          |CREATE TABLE Fine (
          |    seq BIGINT NOT NULL,
          |    name VARCHAR(255) COLLATE utf8mb4_general_ci NOT NULL,
          |    CONSTRAINT Fine_pk PRIMARY KEY (seq)
          |) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_bin;
          |CREATE INDEX Fine_name_index ON Fine (name);
          |ALTER TABLE Fine ADD COLUMN other INT NULL;
          |""".stripMargin
      assert(EvolutionConvention.tablesCreatedWithoutCollation(sql) === Seq.empty)
    }
  }

  "commentLinesWithSemicolon" - {
    "flags the comment that broke the first evolution 69, and nothing else in that script" in {
      val brokenScript =
        """# --- !Ups
          |
          |# The convert moves the columns without a collation of their own (status, rejectReason) onto
          |# bin; requestedNickname is then put back to the case-insensitive collation it shares with
          |# User.nickname, which the convert would otherwise have overwritten.
          |ALTER TABLE UserNicknameChangeRequest CONVERT TO CHARACTER SET utf8mb4 COLLATE utf8mb4_bin;
          |""".stripMargin
      assert(EvolutionConvention.commentLinesWithSemicolon(brokenScript) ===
        Seq("# bin; requestedNickname is then put back to the case-insensitive collation it shares with"))
    }
  }

  "every evolution" - {
    "keeps semicolons out of its comments" in {
      val offenders = EvolutionConvention.evolutionFiles(evolutionsDirectory).flatMap { case (id, path) =>
        EvolutionConvention.commentLinesWithSemicolon(Files.readString(path)).map(line => s"$id.sql: $line")
      }
      assert(offenders === Seq.empty,
        "\nPlay splits the script on every semicolon and does not know what a comment is, so the " +
        "text after this one runs as SQL. Rephrase the comment.")
    }
  }

  s"every evolution from ${EvolutionConvention.FirstGovernedEvolution} on" - {
    "gives each table it creates a table-level COLLATE" in {
      val offenders = EvolutionConvention.evolutionFiles(evolutionsDirectory)
        .filter(_._1 >= EvolutionConvention.FirstGovernedEvolution)
        .flatMap { case (id, path) =>
          EvolutionConvention.tablesCreatedWithoutCollation(Files.readString(path)).map(table => s"$id.sql: $table")
        }
      assert(offenders === Seq.empty,
        "\nA CREATE TABLE without a table-level COLLATE takes the database default, which is not " +
        "what the other tables use. Add `COLLATE=utf8mb4_bin` to the table options.")
    }
  }
}
