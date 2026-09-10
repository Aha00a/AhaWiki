package logics

import logics.wikis.PageNameUrl
import play.api.Logging
import play.api.libs.ws.WSClient

import java.net.URLEncoder
import javax.inject._
import scala.concurrent.ExecutionContext
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
}

@Singleton
class TelegramLogic @Inject()(
  applicationConf: ApplicationConf,
  wsClient: WSClient,
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

  private def sendTo(chatId: String, message: String): Unit = {
    val botToken = applicationConf.AhaWiki.telegram.botToken()
    if (botToken.isEmpty) return

    wsClient
      .url(s"https://api.telegram.org/bot$botToken/sendMessage")
      .post(Map(
        "chat_id"              -> chatId,
        "text"                 -> message,
        "parse_mode"           -> "HTML",
        "disable_notification" -> "true",
      ))
      .onComplete { outcome =>
        TelegramLogic.sendOutcomeWarning(chatId, outcome.map(response => (response.status, response.body)))
          .foreach(message => logger.warn(message))
      }
  }

  private def send(message: String, siteChatId: Option[String]): Unit = {
    val globalChatId = applicationConf.AhaWiki.telegram.chatId()
    val chatIds = (Seq(globalChatId) ++ siteChatId).map(_.trim).filter(_.nonEmpty).distinct
    chatIds.foreach(sendTo(_, message))
  }

  // ── 이벤트별 알림 ──────────────────────────────────────────

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
