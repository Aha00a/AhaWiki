package logics.wikis

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.Instant
import scala.util.Try

object SignedReadUrlLogic {
  private val HmacAlgorithm = "HmacSHA256"
  val QueryParamExpires: String = "sr_exp"
  val QueryParamSignature: String = "sr_sig"
  val ValidDurationSeconds: Long = 24L * 60L * 60L

  def signReadRequest(host: String, name: String, revision: Int, action: String, expiresAtEpochSeconds: Long, secret: String): String = {
    val payload = payloadString(host, name, revision, normalizedAction(action), expiresAtEpochSeconds)
    hmacSha256Hex(payload, secret)
  }

  def verifyReadRequest(host: String,
                        name: String,
                        revision: Int,
                        action: String,
                        expiresAtEpochSeconds: Option[Long],
                        signature: Option[String],
                        secret: String,
                        nowEpochSeconds: Long = Instant.now().getEpochSecond): Boolean = {
    if (secret.isEmpty) {
      false
    } else {
      (expiresAtEpochSeconds, signature.map(_.trim).filter(_.nonEmpty)) match {
        case (Some(exp), Some(sig)) if exp >= nowEpochSeconds =>
          val expected = signReadRequest(host, name, revision, action, exp, secret)
          MessageDigest.isEqual(expected.getBytes(StandardCharsets.UTF_8), sig.getBytes(StandardCharsets.UTF_8))
        case _ =>
          false
      }
    }
  }

  def parseEpochSecond(value: Option[String]): Option[Long] =
    value.flatMap(v => Try(v.toLong).toOption)

  private def normalizedAction(action: String): String = {
    action match {
      case "" | "view" => "view"
      case "raw" => "raw"
      case "history" => "history"
      case "diff" => "diff"
      case _ => ""
    }
  }

  private def payloadString(host: String, name: String, revision: Int, action: String, expiresAtEpochSeconds: Long): String = {
    Seq(host.trim.toLowerCase, name, revision.toString, action, expiresAtEpochSeconds.toString).mkString("|")
  }

  private def hmacSha256Hex(value: String, secret: String): String = {
    val mac = Mac.getInstance(HmacAlgorithm)
    mac.init(new SecretKeySpec(secret.getBytes(StandardCharsets.UTF_8), HmacAlgorithm))
    mac.doFinal(value.getBytes(StandardCharsets.UTF_8)).map("%02x".format(_)).mkString
  }
}
