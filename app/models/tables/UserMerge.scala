package models.tables

import anorm._

import java.sql.Connection
import java.sql.ResultSet

object UserMerge {
  private case class UserReference(table: String, column: String)

  private def resultSetSeq[A](resultSet: ResultSet)(f: ResultSet => A): Seq[A] = {
    try {
      val builder = Seq.newBuilder[A]
      while (resultSet.next()) {
        builder += f(resultSet)
      }
      builder.result()
    } finally {
      resultSet.close()
    }
  }

  private def identifierMatches(actual: String, expected: String): Boolean =
    actual != null && actual.equalsIgnoreCase(expected)

  private def catalogCandidates(implicit connection: Connection): Seq[String] =
    Seq(Option(connection.getCatalog).orNull, null).distinct

  private def identifierCandidates(identifier: String): Seq[String] =
    Seq(identifier, identifier.toUpperCase, identifier.toLowerCase).distinct

  private def hasColumn(table: String, column: String)(implicit connection: Connection): Boolean = {
    val metadata = connection.getMetaData
    catalogCandidates.exists { catalog =>
      identifierCandidates(table).exists { tablePattern =>
        identifierCandidates(column).exists { columnPattern =>
          resultSetSeq(metadata.getColumns(catalog, null, tablePattern, columnPattern)) { resultSet =>
            identifierMatches(resultSet.getString("TABLE_NAME"), table) &&
              identifierMatches(resultSet.getString("COLUMN_NAME"), column)
          }.contains(true)
        }
      }
    }
  }

  private def quoteIdentifier(identifier: String): String =
    "`" + identifier.replace("`", "``") + "`"

  private def selectUserReferencesFromMetadata()(implicit connection: Connection): Seq[UserReference] = {
    val metadata = connection.getMetaData

    catalogCandidates.flatMap { catalog =>
      identifierCandidates("User").flatMap { tablePattern =>
        resultSetSeq(metadata.getExportedKeys(catalog, null, tablePattern)) { resultSet =>
          val pkTable = resultSet.getString("PKTABLE_NAME")
          val pkColumn = resultSet.getString("PKCOLUMN_NAME")
          val fkTable = resultSet.getString("FKTABLE_NAME")
          val fkColumn = resultSet.getString("FKCOLUMN_NAME")

          if (identifierMatches(pkTable, "User") && identifierMatches(pkColumn, "seq"))
            Some(UserReference(fkTable, fkColumn))
          else
            None
        }.flatten
      }
    }.distinct
  }

  private def selectUserReferencesFromInformationSchema()(implicit connection: Connection): Seq[UserReference] = {
    SQL"""
      SELECT TABLE_NAME, COLUMN_NAME
      FROM INFORMATION_SCHEMA.KEY_COLUMN_USAGE
      WHERE TABLE_SCHEMA = DATABASE()
        AND REFERENCED_TABLE_NAME = 'User'
        AND REFERENCED_COLUMN_NAME = 'seq'
    """.as((anorm.SqlParser.str("TABLE_NAME") ~ anorm.SqlParser.str("COLUMN_NAME")).map {
      case table ~ column => UserReference(table, column)
    }.*)
  }

  private def selectUserReferences()(implicit connection: Connection): Seq[UserReference] = {
    val metadataReferences = selectUserReferencesFromMetadata()
    val databaseProductName = Option(connection.getMetaData.getDatabaseProductName).getOrElse("")
    val isMySqlFamily = databaseProductName.contains("MySQL") || databaseProductName.contains("MariaDB")

    if (metadataReferences.nonEmpty || !isMySqlFamily)
      metadataReferences
    else
      selectUserReferencesFromInformationSchema()
  }

  private def updateUserReference(table: String, column: String, canonicalUser: Long, duplicateUser: Long)
                                 (implicit connection: Connection): Int = {
    SQL(s"UPDATE ${quoteIdentifier(table)} SET ${quoteIdentifier(column)} = {canonicalUser} WHERE ${quoteIdentifier(column)} = {duplicateUser}")
      .on(
        "canonicalUser" -> canonicalUser,
        "duplicateUser" -> duplicateUser,
      )
      .executeUpdate()
  }

  private def mergeUserSiteIfPresent(canonicalUser: Long, duplicateUser: Long)(implicit connection: Connection): Unit = {
    if (hasColumn("UserSite", "user") && hasColumn("UserSite", "site")) {
      val canonicalSites = SQL"SELECT site FROM UserSite WHERE `user` = $canonicalUser"
        .as(anorm.SqlParser.long("site").*)
        .toSet
      val duplicateSites = SQL"SELECT site FROM UserSite WHERE `user` = $duplicateUser"
        .as(anorm.SqlParser.long("site").*)

      duplicateSites.foreach { site =>
        if (canonicalSites.contains(site)) {
          SQL"DELETE FROM UserSite WHERE `user` = $duplicateUser AND site = $site".executeUpdate()
        } else {
          SQL"UPDATE UserSite SET `user` = $canonicalUser WHERE `user` = $duplicateUser AND site = $site".executeUpdate()
        }
      }
    }
  }

  def mergeInto(canonicalUser: Long, duplicateUser: Long)(implicit connection: Connection): Unit = {
    if (canonicalUser == duplicateUser) return

    LocalTransaction {
      SQL"UPDATE UserEmail SET isPrimary = false WHERE `user` = $duplicateUser".executeUpdate()

      mergeUserSiteIfPresent(canonicalUser, duplicateUser)
      selectUserReferences()
        .filterNot(_.table == "UserSite")
        .foreach(reference => updateUserReference(reference.table, reference.column, canonicalUser, duplicateUser))

      if (!UserEmail.hasPrimary(canonicalUser)) {
        UserEmail.selectByUser(canonicalUser).headOption.foreach { userEmail =>
          UserEmail.setPrimary(canonicalUser, userEmail.email)
        }
      }

      SQL(s"DELETE FROM ${quoteIdentifier("User")} WHERE ${quoteIdentifier("seq")} = {duplicateUser}")
        .on("duplicateUser" -> duplicateUser)
        .executeUpdate()
    }
  }
}
