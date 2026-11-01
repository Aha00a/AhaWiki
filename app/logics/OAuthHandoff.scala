package logics

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.util.Base64

/**
 * Signed values that carry a Google login between a wiki and the one host Google will call back.
 *
 * Google takes no wildcard in a redirect URI, and a site's session cookie is its own host's. So a
 * site under the parent domain sends the browser to Google with the parent as the callback and a
 * signed `state` saying where it came from. The parent reads the profile and sends the browser
 * back with a signed `handoff` holding it, and the site opens its own session. A site on a domain
 * of its own keeps its own redirect URI and none of this happens.
 *
 * Each value names its purpose inside what is signed, so a state cannot be replayed as a handoff.
 * The nonce is also in the site's flash cookie and has to match there, which ties the login to the
 * browser that started it: a handoff address taken from a log, or planted in a link, does nothing
 * in any other browser. Without that it would log the victim in as somebody else.
 */
object OAuthHandoff {
  val MaxAgeMillis: Long = 5 * 60 * 1000L

  case class Value(purpose: String, host: String, redirect: String, nonce: String, connect: Boolean, expires: Long, email: String, profileImageUrl: String)

  private val separator = "\n"
  private def encode(text: String): String =
    Base64.getUrlEncoder.withoutPadding.encodeToString(text.getBytes(StandardCharsets.UTF_8))
  private def decode(text: String): String =
    new String(Base64.getUrlDecoder.decode(text), StandardCharsets.UTF_8)

  def seal(value: Value, sign: String => String): String = {
    val payload = encode(Seq(value.purpose, value.host, value.redirect, value.nonce, value.connect.toString, value.expires.toString, value.email, value.profileImageUrl).mkString(separator))
    s"$payload.${sign(payload)}"
  }

  /** The value, if it is intact, of this purpose, and not expired. */
  def open(token: String, purpose: String, now: Long, sign: String => String): Option[Value] =
    token.split('.') match {
      case Array(payload, signature) if MessageDigest.isEqual(sign(payload).getBytes(StandardCharsets.UTF_8), signature.getBytes(StandardCharsets.UTF_8)) =>
        decode(payload).split(separator, -1) match {
          case Array(p, host, redirect, nonce, connect, expires, email, image) if p == purpose && expires.toLong > now =>
            Some(Value(p, host, redirect, nonce, connect == "true", expires.toLong, email, image))
          case _ => None
        }
      case _ => None
    }

  def isUnderParent(host: String, parentDomain: String): Boolean =
    parentDomain.nonEmpty && host.endsWith(s".$parentDomain")

  /**
   * Only ever a path on the host the handoff lands on. A referer is absolute, so its origin is
   * dropped, and anything left that a browser could read as another host becomes the front page.
   */
  def localPath(redirect: String): String = {
    val path = redirect.replaceFirst("^https?://[^/]*", "")
    if (path.startsWith("/") && !path.startsWith("//") && !path.startsWith("/\\")) path else "/"
  }
}
