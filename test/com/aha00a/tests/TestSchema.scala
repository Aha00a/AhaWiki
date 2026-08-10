package com.aha00a.tests

import anorm.SQL

import java.sql.Connection

/**
 * The H2 tables the specs run against, one definition per table.
 *
 * Evolutions are MySQL-flavoured and do not apply to H2, so the specs build their schema by
 * hand. They used to build it each on their own: `User` existed in seven specs under four
 * different definitions, `Site` in six under three. A spec could then pass against a table
 * shape no other spec — and no production database — agreed with.
 *
 * Types follow `schema/schema.sql`, the committed dump of the real database, down to the
 * ENUM value lists. They did not: integer widths disagreed on nine columns, `Page.comment`
 * and `remoteAddress` were VARCHAR(255) where production has TEXT, and three ENUM columns
 * were VARCHAR — so a spec could store a `targetType` the real column would have rejected.
 *
 * A table being here at all is a claim that production has it. `UserSite` was declared here
 * and dropped by evolution 55, so a spec was exercising merge behaviour for a table nothing
 * has; the dump is what showed it.
 *
 * This is still a mirror rather than the schema. Building it from the evolutions instead was
 * tried and does not work: 17 of the 67 files use MySQL grammar H2 rejects. See
 * `docs/Testing.md`. `schema/schema.sql` is the closest thing to an oracle, and comparing
 * against it is manual today.
 */
object TestSchema {
  private val ddl: Map[String, String] = Map(
    "Site" ->
      """
        CREATE TABLE IF NOT EXISTS Site (
          seq INT AUTO_INCREMENT PRIMARY KEY,
          created DATETIME DEFAULT NOW() NOT NULL,
          updated DATETIME DEFAULT NOW() NOT NULL,
          name VARCHAR(200) NOT NULL,
          abbr VARCHAR(200) NOT NULL DEFAULT '',
          mainDomain VARCHAR(255) NOT NULL DEFAULT '',
          publicListedOrder DECIMAL(10, 2) NULL
        )
      """,
    "User" ->
      """
        CREATE TABLE IF NOT EXISTS `User` (
          seq INT AUTO_INCREMENT PRIMARY KEY,
          created DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,
          updated DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,
          nickname VARCHAR(255) NOT NULL,
          profileImageUrl VARCHAR(512) NULL
        )
      """,
    "SiteDomain" ->
      """
        CREATE TABLE IF NOT EXISTS SiteDomain (
          created DATETIME DEFAULT NOW() NOT NULL,
          site INT NOT NULL,
          domain VARCHAR(255) NOT NULL,
          PRIMARY KEY (site, domain),
          CONSTRAINT SiteDomain_Site_seq_fk FOREIGN KEY (site) REFERENCES Site (seq)
        )
      """,
    "SiteAdmin" ->
      """
        CREATE TABLE IF NOT EXISTS SiteAdmin (
          site INT NOT NULL,
          `user` INT NOT NULL,
          dateInserted DATETIME DEFAULT NOW() NOT NULL,
          PRIMARY KEY (site, `user`),
          CONSTRAINT SiteAdmin_Site_seq_fk FOREIGN KEY (site) REFERENCES Site (seq),
          CONSTRAINT SiteAdmin_User_seq_fk FOREIGN KEY (`user`) REFERENCES `User` (seq)
        )
      """,
    "UserEmail" ->
      """
        CREATE TABLE IF NOT EXISTS UserEmail (
          `user` INT NOT NULL,
          email VARCHAR(255) NOT NULL,
          isPrimary BOOLEAN NOT NULL DEFAULT FALSE,
          created DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL,
          PRIMARY KEY (`user`, email),
          UNIQUE (email),
          FOREIGN KEY (`user`) REFERENCES `User` (seq)
        )
      """,
    "AccessLog" ->
      """
        CREATE TABLE IF NOT EXISTS AccessLog (
          seq INT AUTO_INCREMENT PRIMARY KEY,
          `user` INT NULL,
          FOREIGN KEY (`user`) REFERENCES `User` (seq)
        )
      """,
    "UserApiKey" ->
      """
        CREATE TABLE IF NOT EXISTS UserApiKey (
          seq BIGINT NOT NULL AUTO_INCREMENT,
          `user` INT NOT NULL,
          keyHash VARCHAR(64) NOT NULL,
          keyPrefix VARCHAR(32) NOT NULL,
          name VARCHAR(255) NOT NULL,
          dateInserted DATETIME NOT NULL DEFAULT NOW(),
          dateLastUsed DATETIME NULL,
          dateRevoked DATETIME NULL,
          PRIMARY KEY (seq),
          UNIQUE (keyHash)
        )
      """,
    "Permission" ->
      """
        CREATE TABLE IF NOT EXISTS Permission (
          site INT NOT NULL,
          target VARCHAR(255) NOT NULL,
          targetType ENUM('All','Exact','StartsWith','EndsWith','RegularExpression') NOT NULL,
          actor VARCHAR(255) NOT NULL,
          actorType ENUM('All','Login','Exact','Domain') NOT NULL DEFAULT 'Exact',
          action INT NOT NULL,
          dateUpdated DATETIME DEFAULT NOW() NOT NULL,
          PRIMARY KEY (site, target, targetType, actor, actorType)
        )
      """,
    "IpDeny" ->
      """
        CREATE TABLE IF NOT EXISTS IpDeny (
          seq INT NOT NULL AUTO_INCREMENT,
          accessLog INT NULL,
          dateInserted DATETIME DEFAULT NOW() NOT NULL,
          ip VARCHAR(46) NOT NULL,
          reason VARCHAR(255) NOT NULL DEFAULT '',
          PRIMARY KEY (seq)
        )
      """,
    "Page" ->
      """
        CREATE TABLE IF NOT EXISTS Page (
          site INT NOT NULL,
          name VARCHAR(255) NOT NULL,
          revision INT NOT NULL DEFAULT 0,
          dateTime DATETIME DEFAULT NOW() NOT NULL,
          user INT NULL,
          remoteAddress TEXT,
          comment TEXT NOT NULL,
          isMinorEdit BOOLEAN NOT NULL DEFAULT FALSE,
          viaApi BOOLEAN NOT NULL DEFAULT FALSE,
          userApiKey BIGINT NULL,
          content CLOB NOT NULL,
          PRIMARY KEY (site, name, revision)
        )
      """,
    "PageMeta" ->
      """
        CREATE TABLE IF NOT EXISTS PageMeta (
          site INT NOT NULL,
          name VARCHAR(255) NOT NULL,
          dateInserted DATETIME NOT NULL DEFAULT NOW(),
          dateUpdated DATETIME NULL,
          revision INT NOT NULL,
          image VARCHAR(512) NULL,
          description VARCHAR(512) NULL,
          size BIGINT NOT NULL DEFAULT 0,
          PRIMARY KEY (site, name)
        )
      """,
    "Attachment" ->
      """
        CREATE TABLE IF NOT EXISTS Attachment (
          seq BIGINT NOT NULL AUTO_INCREMENT,
          site INT NOT NULL,
          pageName VARCHAR(255) NOT NULL,
          user INT NULL,
          uploaderEmail VARCHAR(255) NULL,
          originalFilename VARCHAR(255) NOT NULL,
          storedFilename VARCHAR(255) NOT NULL,
          bucket VARCHAR(255) NOT NULL,
          objectKey VARCHAR(512) NOT NULL,
          contentType VARCHAR(255) NOT NULL,
          fileSize BIGINT NOT NULL,
          status ENUM('Initiated','Uploaded','Verified','Deleted','Failed') NOT NULL,
          etag VARCHAR(255) NULL,
          dateInserted DATETIME DEFAULT NOW() NOT NULL,
          dateUpdated DATETIME NULL,
          dateUploaded DATETIME NULL,
          dateDeleted DATETIME NULL,
          PRIMARY KEY (seq)
        )
      """,
    "CalculatedLink" ->
      """
        CREATE TABLE IF NOT EXISTS CalculatedLink (
          site INT NOT NULL,
          src VARCHAR(255) NOT NULL,
          dst VARCHAR(255) NOT NULL,
          alias VARCHAR(255) NOT NULL DEFAULT ''
        )
      """,
    "CalculatedSchemaOrg" ->
      """
        CREATE TABLE IF NOT EXISTS CalculatedSchemaOrg (
          site INT NOT NULL,
          page VARCHAR(255) NOT NULL,
          cls VARCHAR(255) NOT NULL DEFAULT '',
          prop VARCHAR(255) NOT NULL DEFAULT '',
          `value` VARCHAR(255) NOT NULL DEFAULT ''
        )
      """,
    "CalculatedTerm" ->
      """
        CREATE TABLE IF NOT EXISTS CalculatedTerm (
          seq BIGINT AUTO_INCREMENT PRIMARY KEY,
          term VARCHAR(255) NOT NULL UNIQUE
        )
      """,
    "CalculatedTermFrequency" ->
      """
        CREATE TABLE IF NOT EXISTS CalculatedTermFrequency (
          site INT NOT NULL,
          name VARCHAR(255) NOT NULL,
          term BIGINT NOT NULL,
          frequency INT NOT NULL,
          PRIMARY KEY (site, name, term)
        )
      """,
    "CalculatedTermFrequencyNorm" ->
      """
        CREATE TABLE IF NOT EXISTS CalculatedTermFrequencyNorm (
          site INT NOT NULL,
          name VARCHAR(255) NOT NULL,
          norm DOUBLE NOT NULL,
          PRIMARY KEY (site, name),
          FOREIGN KEY (site, name) REFERENCES PageMeta (site, name)
        )
      """,
    "CalculatedCosineSimilarity" ->
      """
        CREATE TABLE IF NOT EXISTS CalculatedCosineSimilarity (
          site1 INT NOT NULL,
          name1 VARCHAR(255) NOT NULL,
          site2 INT NOT NULL,
          name2 VARCHAR(255) NOT NULL,
          similarity DOUBLE NOT NULL,
          PRIMARY KEY (site1, name1, site2, name2),
          FOREIGN KEY (site1, name1) REFERENCES PageMeta (site, name),
          FOREIGN KEY (site2, name2) REFERENCES PageMeta (site, name)
        )
      """,
  )

  /** Referenced tables first, so a foreign key never precedes what it points at. */
  private val creationOrder: Seq[String] = Seq(
    "Site",
    "User",
    "SiteDomain",
    "SiteAdmin",
    "UserEmail",
    "AccessLog",
    "UserApiKey",
    "Permission",
    "IpDeny",
    "Page",
    "PageMeta",
    "Attachment",
    "CalculatedLink",
    "CalculatedSchemaOrg",
    "CalculatedTerm",
    "CalculatedTermFrequency",
    "CalculatedTermFrequencyNorm",
    "CalculatedCosineSimilarity",
  )

  def create(tables: String*)(implicit connection: Connection): Unit = {
    val requested = tables.toSet
    val unknown = requested -- ddl.keySet
    require(unknown.isEmpty, s"TestSchema has no definition for: ${unknown.toSeq.sorted.mkString(", ")}")
    creationOrder.filter(requested).foreach(table => SQL(ddl(table)).execute())
  }
}
