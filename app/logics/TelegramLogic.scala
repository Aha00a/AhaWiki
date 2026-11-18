package logics

import logics.wikis.PageNameUrl
import org.apache.pekko.actor.ActorSystem
import play.api.Logging
import play.api.libs.json.Json
import play.api.libs.ws.WSClient

import java.net.URLEncoder
import javax.inject._
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object TelegramLogic {
  /**
   * What sendTo logs about one send, or None when it went through.
   *
   * A transport failure is logged by its class name only. The request URL carries the bot token,
   * and nothing promises that an exception's message leaves the URL out. Until 2026-09-10 such a
   * failure -- DNS, a refused connection, a timeout -- logged nothing at all, because the result
   * was read with foreach, which never runs for a failed Future.
   */
  def sendOutcomeWarning(chatId: String, outcome: Try[(Int, String)]): Option[String] = outcome match {
    case Success((200, _))       => None
    case Success((status, body)) => Some(s"TelegramLogic.sendTo chatId=$chatId failed: status=$status body=${body.take(200)}")
    case Failure(error)          => Some(s"TelegramLogic.sendTo chatId=$chatId failed: ${error.getClass.getName}")
  }

  /** Telegram's limit on one message's text. */
  val messageLimit: Int = 4096

  /**
   * The next message to send: as many pending messages, oldest first, as fit in `limit` when
   * joined by a blank line, and what is left. Always takes at least one, so a message longer than
   * the limit is still tried on its own rather than holding up the queue.
   */
  def takeBatch(pending: Vector[String], limit: Int = messageLimit): (String, Vector[String]) = {
    val separator = "\n\n"
    var length = pending.head.length
    var count = 1
    while (count < pending.size && length + separator.length + pending(count).length <= limit) {
      length += separator.length + pending(count).length
      count += 1
    }
    (pending.take(count).mkString(separator), pending.drop(count))
  }

  /** How long Telegram asked us to wait, when it refused a send for coming too fast. */
  def retryAfterSeconds(status: Int, body: String): Option[Int] =
    if (status != 429) None
    else Try((Json.parse(body) \ "parameters" \ "retry_after").as[Int]).toOption.filter(_ > 0)
}

/**
 * Sends to one chat, one request at a time, at most one every `interval`.
 *
 * Telegram lets a bot post about 20 messages a minute into one group and answers 429 to the rest.
 * Until 2026-11-18 every event was posted the moment it happened, so someone uploading a folder of
 * attachments on sst -- 40 to 70 files a minute, three times that evening -- had 180 of the
 * notifications refused, and they were gone. Now whatever arrives while a send is under way or
 * while the interval runs is joined into the next message (`TelegramLogic.takeBatch`), and a 429
 * waits as long as Telegram says and sends the same text again, up to `maxRetries` times.
 *
 * `post` and `schedule` are parameters so the spec can drive the queue without a network or a clock.
 */
class TelegramChatQueue(
  chatId: String,
  post: String => Future[(Int, String)],
  schedule: (FiniteDuration, () => Unit) => Unit,
  warn: String => Unit,
  interval: FiniteDuration = 3.seconds,
  maxRetries: Int = 5,
)(implicit executionContext: ExecutionContext) {

  private var pending = Vector.empty[String]
  private var busy = false
  private var retries = 0

  def enqueue(message: String): Unit = {
    val start = synchronized {
      pending :+= message
      val idle = !busy
      busy = true
      idle
    }
    if (start) sendNext()
  }

  private def sendNext(): Unit = {
    val batch = synchronized {
      if (pending.isEmpty) {
        busy = false
        None
      } else {
        val (text, rest) = TelegramLogic.takeBatch(pending)
        pending = rest
        Some(text)
      }
    }
    batch.foreach { text =>
      post(text).onComplete { outcome =>
        val retryAfter = outcome.toOption.flatMap { case (status, body) => TelegramLogic.retryAfterSeconds(status, body) }
        retryAfter match {
          case Some(seconds) if synchronized(retries) < maxRetries =>
            synchronized {
              retries += 1
              pending = text +: pending
            }
            schedule(seconds.seconds + 1.second, () => sendNext())
          case _ =>
            synchronized { retries = 0 }
            TelegramLogic.sendOutcomeWarning(chatId, outcome).foreach(warn)
            schedule(interval, () => sendNext())
        }
      }
    }
  }
}

@Singleton
class TelegramLogic @Inject()(
  applicationConf: ApplicationConf,
  wsClient: WSClient,
  actorSystem: ActorSystem,
)(implicit executionContext: ExecutionContext) extends Logging {

  // ── 내부 유틸 ──────────────────────────────────────────────

  private def pageUrl(host: String, pageName: String): String =
    s"https://$host/w/${PageNameUrl.encode(pageName)}"

  private def diffUrl(host: String, pageName: String, revision: Long): String =
    s"${pageUrl(host, pageName)}?action=diff&after=$revision"

  private def h(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  private def b(s: String): String = s"<b>${h(s)}</b>"

  private def commentLine(comment: String): String =
    if (comment.nonEmpty) s"\n${h(comment)}" else ""

  private def post(botToken: String, chatId: String)(message: String): Future[(Int, String)] =
    wsClient
      .url(s"https://api.telegram.org/bot$botToken/sendMessage")
      .post(Map(
        "chat_id"              -> chatId,
        "text"                 -> message,
        "parse_mode"           -> "HTML",
        "disable_notification" -> "true",
      ))
      .map(response => (response.status, response.body))

  private val queues = TrieMap.empty[String, TelegramChatQueue]

  private def sendTo(chatId: String, message: String): Unit = {
    val botToken = applicationConf.AhaWiki.telegram.botToken()
    if (botToken.isEmpty) return

    queues.getOrElseUpdate(chatId, new TelegramChatQueue(
      chatId,
      post(botToken, chatId),
      (delay, run) => actorSystem.scheduler.scheduleOnce(delay)(run()),
      message => logger.warn(message),
    )).enqueue(message)
  }

  private def send(message: String, siteChatId: Option[String]): Unit = {
    val globalChatId = applicationConf.AhaWiki.telegram.chatId()
    val chatIds = (Seq(globalChatId) ++ siteChatId).map(_.trim).filter(_.nonEmpty).distinct
    chatIds.foreach(sendTo(_, message))
  }

  // ── 이벤트별 알림 ──────────────────────────────────────────

  def notifySiteRequested(host: String, siteName: String, domain: String, nickname: String, purpose: String): Unit =
    send(s"🏠 ${b(siteName)} ($domain) requested by ${h(nickname)}\n${h(purpose)}\nhttps://$host/Admin/SiteRequests", None)

  def notifyPageCreated(host: String, pageName: String, nickname: String, comment: String, siteChatId: Option[String] = None): Unit =
    send(s"🆕 ${b(pageName)} created by ${h(nickname)}${commentLine(comment)}\n${pageUrl(host, pageName)}", siteChatId)

  def notifyPageEdited(host: String, pageName: String, revision: Long, nickname: String, comment: String, siteChatId: Option[String] = None): Unit =
    send(s"✏️ ${b(pageName)} r$revision edited by ${h(nickname)}${commentLine(comment)}\n${diffUrl(host, pageName, revision)}", siteChatId)

  def notifyPageDeleted(host: String, pageName: String, nickname: String, siteChatId: Option[String] = None): Unit =
    send(s"🗑️ ${b(pageName)} deleted by ${h(nickname)}\n${pageUrl(host, pageName)}", siteChatId)

  def notifyLastRevisionDeleted(host: String, pageName: String, revision: Long, nickname: String, siteChatId: Option[String] = None): Unit =
    send(s"⏪ ${b(pageName)} r$revision revision deleted by ${h(nickname)}\n${pageUrl(host, pageName)}", siteChatId)

  def notifyPageRenamed(host: String, oldName: String, newName: String, nickname: String, siteChatId: Option[String] = None): Unit =
    send(s"📝 ${b(oldName)} → ${b(newName)} renamed by ${h(nickname)}\n${pageUrl(host, newName)}", siteChatId)

  def notifyAttachmentUploaded(host: String, pageName: String, filename: String, nickname: String, siteChatId: Option[String] = None): Unit =
    send(s"📎 ${b(pageName)} ${h(filename)} attached by ${h(nickname)}\n${pageUrl(host, pageName)}", siteChatId)

  def notifyAttachmentDeleted(host: String, pageName: String, filename: String, nickname: String, siteChatId: Option[String] = None): Unit =
    send(s"🗑️📎 ${b(pageName)} ${h(filename)} attachment deleted by ${h(nickname)}\n${pageUrl(host, pageName)}", siteChatId)

  def notifyClipboardImageUploaded(host: String, pageName: String, nickname: String, siteChatId: Option[String] = None): Unit =
    send(s"🖼️ ${b(pageName)} image pasted by ${h(nickname)}\n${pageUrl(host, pageName)}", siteChatId)

  def notifySpreadsheetSynced(host: String, pageName: String, nickname: String, siteChatId: Option[String] = None): Unit =
    send(s"🔄 ${b(pageName)} spreadsheet synced by ${h(nickname)}\n${pageUrl(host, pageName)}", siteChatId)
}
