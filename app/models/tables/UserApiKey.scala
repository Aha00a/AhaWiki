package models.tables

import anorm.SqlParser._
import anorm._
import com.aha00a.play.AnormSqlParser.localDateTime

import java.security.MessageDigest
import java.security.SecureRandom
import java.sql.Connection
import java.time.LocalDateTime
import java.util.Base64

case class UserApiKey(
  seq: Long,
  user: Long,
  keyHash: String,
  keyPrefix: String,
  name: String,
  dateInserted: LocalDateTime,
  dateLastUsed: Option[LocalDateTime],
  dateRevoked: Option[LocalDateTime],
)

object UserApiKey {
  case class Created(row: UserApiKey, rawKey: String)

  private val KeyPrefix = "ahawiki_"
  private val random = new SecureRandom()

  //noinspection TypeAnnotation
  def tupled = (apply _).tupled

  private val rowParser =
    long("seq") ~ long("user") ~ str("keyHash") ~ str("keyPrefix") ~ str("name") ~
      localDateTime("dateInserted") ~ localDateTime("dateLastUsed").? ~ localDateTime("dateRevoked").?

  def generateRawKey(): String = {
    val bytes = new Array[Byte](32)
    random.nextBytes(bytes)
    KeyPrefix + Base64.getUrlEncoder.withoutPadding().encodeToString(bytes)
  }

  def hash(rawKey: String): String = {
    val digest = MessageDigest.getInstance("SHA-256").digest(rawKey.getBytes("UTF-8"))
    digest.map(byte => "%02x".format(byte & 0xff)).mkString
  }

  def displayPrefix(rawKey: String): String =
    rawKey.take(16)

  def selectByHash(keyHash: String)(implicit connection: Connection): Option[UserApiKey] = {
    SQL"""
      SELECT seq, `user`, keyHash, keyPrefix, name, dateInserted, dateLastUsed, dateRevoked
      FROM UserApiKey
      WHERE keyHash = $keyHash
        AND dateRevoked IS NULL
    """.as(rowParser.singleOpt).map(flatten).map(UserApiKey.tupled)
  }

  def selectByUser(user: Long)(implicit connection: Connection): Seq[UserApiKey] = {
    SQL"""
      SELECT seq, `user`, keyHash, keyPrefix, name, dateInserted, dateLastUsed, dateRevoked
      FROM UserApiKey
      WHERE `user` = $user
      ORDER BY dateInserted DESC, seq DESC
    """.as(rowParser.*).map(flatten).map(UserApiKey.tupled)
  }

  def selectAll()(implicit connection: Connection): Seq[UserApiKey] = {
    SQL"""
      SELECT seq, `user`, keyHash, keyPrefix, name, dateInserted, dateLastUsed, dateRevoked
      FROM UserApiKey
      ORDER BY dateInserted DESC, seq DESC
    """.as(rowParser.*).map(flatten).map(UserApiKey.tupled)
  }

  def insert(user: Long, name: String)(implicit connection: Connection): Created = {
    val rawKey = generateRawKey()
    val keyHash = hash(rawKey)
    val keyPrefix = displayPrefix(rawKey)
    val seq = SQL"""
      INSERT INTO UserApiKey (`user`, keyHash, keyPrefix, name)
      VALUES ($user, $keyHash, $keyPrefix, $name)
    """.executeInsert(scalar[Long].single)

    Created(selectBySeq(seq).get, rawKey)
  }

  def selectNamesBySeqs(seqs: Seq[Long])(implicit connection: Connection): Map[Long, String] = {
    val distinctSeqs = seqs.distinct
    if (distinctSeqs.isEmpty) {
      Map.empty
    } else {
      SQL"""
        SELECT seq, name
        FROM UserApiKey
        WHERE seq IN ($distinctSeqs)
      """.as((long("seq") ~ str("name")).map(flatten).*).toMap
    }
  }

  def selectBySeq(seq: Long)(implicit connection: Connection): Option[UserApiKey] = {
    SQL"""
      SELECT seq, `user`, keyHash, keyPrefix, name, dateInserted, dateLastUsed, dateRevoked
      FROM UserApiKey
      WHERE seq = $seq
    """.as(rowParser.singleOpt).map(flatten).map(UserApiKey.tupled)
  }

  def revokeByUser(seq: Long, user: Long)(implicit connection: Connection): Int = {
    SQL"""
      UPDATE UserApiKey
      SET dateRevoked = COALESCE(dateRevoked, CURRENT_TIMESTAMP)
      WHERE seq = $seq
        AND `user` = $user
    """.executeUpdate()
  }

  def revoke(seq: Long)(implicit connection: Connection): Int = {
    SQL"""
      UPDATE UserApiKey
      SET dateRevoked = COALESCE(dateRevoked, CURRENT_TIMESTAMP)
      WHERE seq = $seq
    """.executeUpdate()
  }

  def touchLastUsed(seq: Long)(implicit connection: Connection): Int = {
    SQL"""
      UPDATE UserApiKey
      SET dateLastUsed = CURRENT_TIMESTAMP
      WHERE seq = $seq
    """.executeUpdate()
  }
}
