package com.aha00a.tests

import anorm.SQL

import java.nio.file.Files
import java.nio.file.Paths
import java.sql.Connection
import scala.util.matching.Regex

/**
 * Builds the specs' H2 schema from `schema/schema.sql`, the committed dump of the real
 * database.
 *
 * It used to be a hand-written copy, one `CREATE TABLE` per table maintained here — and
 * before that, one per spec. Both copies drifted. The last round: nine integer widths
 * disagreed with production, two TEXT columns were declared VARCHAR(255), three ENUM columns
 * were declared VARCHAR so a spec could store a `targetType` the real column rejects, and a
 * `UserSite` table was declared that evolution 55 had dropped.
 *
 * Building from the evolutions was tried first and does not work: 17 of the 67 files use
 * MySQL grammar H2 rejects, all of it in `ALTER` — `AFTER`, `FIRST`, dropping and adding a
 * key in one statement — plus `TRUNCATE` and `DATE_ADD`. A dump contains none of that. It is
 * `CREATE TABLE` and nothing else, which is why this works where that did not.
 *
 * Refresh the dump with `schemaDump.sh` when the schema changes. The specs follow
 * automatically, and a column they depend on going away shows up as a failing spec instead
 * of as a copy quietly going stale.
 *
 * Three adjustments happen while loading, each for a MySQL/H2 difference rather than a
 * difference of opinion about the schema: foreign keys are added afterwards, `tinyint(1)` is
 * read back as BOOLEAN, and keys onto non-unique columns are dropped. `docs/Testing.md`
 * explains each and what the third one costs.
 */
object TestSchema {
  private val createTableName: Regex = """CREATE TABLE `(\w+)`""".r

  /**
   * Foreign keys are lifted out of the `CREATE TABLE` and added afterwards.
   *
   * Two reasons. A dump is ordered alphabetically, so a table's foreign key routinely points
   * at one that does not exist yet; and `AccessLog` and `IpDeny` reference each other, which
   * no ordering can satisfy.
   */
  private val foreignKey: Regex =
    """(?m)^\s*CONSTRAINT `(\w+)` FOREIGN KEY \((.+?)\) REFERENCES `(\w+)` \((.+?)\)(.*?),?$""".r

  private lazy val (createStatements, alterStatements): (Seq[String], Seq[String]) = {
    val raw = new String(Files.readAllBytes(Paths.get("schema/schema.sql")), "UTF-8")
    val cleaned = raw
      .replaceAll("(?s)/\\*!.*?\\*/;?", "") // mysqldump 의 /*!40101 ... */ 조건부 지시문
      .replaceAll("(?m)^--.*$", "")
    val statements = cleaned.split(";").map(_.trim).filter(_.nonEmpty).toSeq

    // 대상 테이블별로 유니크한 컬럼 조합(PK, UNIQUE KEY)을 모아 둔다.
    def columnList(s: String): Seq[String] = s.split(",").map(_.trim.replace("`", "")).toSeq
    val uniqueKeys: Map[String, Set[Seq[String]]] = statements.flatMap { statement =>
      createTableName.findFirstMatchIn(statement).map { table =>
        val pk = """PRIMARY KEY \((.+?)\)""".r.findFirstMatchIn(statement).map(m => columnList(m.group(1)))
        val uks = """UNIQUE KEY `\w+` \((.+?)\)""".r.findAllMatchIn(statement).map(m => columnList(m.group(1)))
        table.group(1) -> (pk.toSet ++ uks.toSet)
      }
    }.toMap

    val alters = statements.flatMap { statement =>
      createTableName.findFirstMatchIn(statement).toSeq.flatMap { table =>
        foreignKey.findAllMatchIn(statement).flatMap { fk =>
          val referenced = columnList(fk.group(4))
          // MySQL 은 대상 컬럼이 어떤 인덱스의 접두사이기만 하면 FK 를 허용한다. H2 는
          // 대상이 유니크해야 하고, 아니면 유니크 인덱스를 만들어 버린다. Page(site, name)
          // 을 가리키는 FK 가 그런 경우인데, 그대로 두면 H2 가 Page 에 (site, name) 유니크를
          // 걸어서 같은 페이지의 두 번째 리비전을 넣을 수 없게 된다. 그런 FK 는 건너뛴다.
          if (uniqueKeys.getOrElse(fk.group(3), Set.empty).contains(referenced)) {
            Some(s"ALTER TABLE `${table.group(1)}` ADD CONSTRAINT `${fk.group(1)}` " +
              s"FOREIGN KEY (${fk.group(2)}) REFERENCES `${fk.group(3)}` (${fk.group(4)})")
          } else {
            None
          }
        }
      }
    }
    val creates = statements.map { statement =>
      foreignKey.replaceAllIn(statement, "")
        .replaceAll("""(?m),\s*\n\s*\)""", "\n)")
        // MySQL 에는 BOOLEAN 이 없어서 tinyint(1) 로 덤프된다. 되돌리지 않으면 H2 가
        // 그 컬럼을 Integer 로 돌려주고, Boolean 을 기대하는 파서가 깨진다.
        .replaceAll("""(?i)\btinyint\(1\)""", "BOOLEAN")
    }
    (creates, alters)
  }

  /**
   * Creates every table the real database has.
   *
   * All of them rather than a named subset: the foreign keys tie them together, and a spec
   * naming only what it believes it needs is how such a list goes stale. An unused table in
   * an in-memory database costs nothing.
   */
  def createAll()(implicit connection: Connection): Unit = {
    createStatements.foreach(statement => SQL(statement).execute())
    alterStatements.foreach(statement => SQL(statement).execute())
  }
}
