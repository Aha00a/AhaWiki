package logics

import play.api.Logging
import play.api.libs.ws.WSClient

import java.net.URLEncoder
import javax.inject._
import scala.concurrent.ExecutionContext

@Singleton
class TelegramLogic @Inject()(
  applicationConf: ApplicationConf,
  wsClient: WSClient,
)(implicit executionContext: ExecutionContext) extends Logging {

  // ── 내부 유틸 ──────────────────────────────────────────────

  private def pageUrl(host: String, pageName: String): String =
    s"https://$host/w/${URLEncoder.encode(pageName, "UTF-8").replace("+", "%20")}"

  private def h(s: String): String =
    s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

  private def b(s: String): String = s"<b>${h(s)}</b>"

  private def commentLine(comment: String): String =
    if (comment.nonEmpty) s"\n${h(comment)}" else ""

  private def send(host: String, message: String): Unit = {
    val botToken = applicationConf.AhaWiki.telegram.botToken()
    val chatId   = applicationConf.AhaWiki.telegram.chatId()
    if (botToken.isEmpty || chatId.isEmpty) return

    wsClient
      .url(s"https://api.telegram.org/bot$botToken/sendMessage")
      .post(Map(
        "chat_id"              -> chatId,
        "text"                 -> message,
        "parse_mode"           -> "HTML",
        "disable_notification" -> "true",
      ))
      .foreach { response =>
        if (response.status != 200)
          logger.warn(s"TelegramLogic.send failed: status=${response.status} body=${response.body.take(200)}")
      }
  }

  // ── 이벤트별 알림 ──────────────────────────────────────────

  def notifyPageCreated(host: String, pageName: String, nickname: String, comment: String): Unit =
    send(host, s"🆕 ${b(pageName)} created by ${h(nickname)}${commentLine(comment)}\n${pageUrl(host, pageName)}")

  def notifyPageEdited(host: String, pageName: String, revision: Long, nickname: String, comment: String): Unit =
    send(host, s"✏️ ${b(pageName)} r$revision edited by ${h(nickname)}${commentLine(comment)}\n${pageUrl(host, pageName)}")

  def notifyPageDeleted(host: String, pageName: String, nickname: String): Unit =
    send(host, s"🗑️ ${b(pageName)} deleted by ${h(nickname)}\n${pageUrl(host, pageName)}")

  def notifyLastRevisionDeleted(host: String, pageName: String, revision: Long, nickname: String): Unit =
    send(host, s"⏪ ${b(pageName)} r$revision revision deleted by ${h(nickname)}\n${pageUrl(host, pageName)}")

  def notifyPageRenamed(host: String, oldName: String, newName: String, nickname: String): Unit =
    send(host, s"📝 ${b(oldName)} → ${b(newName)} renamed by ${h(nickname)}\n${pageUrl(host, newName)}")

  def notifyAttachmentUploaded(host: String, pageName: String, filename: String, nickname: String): Unit =
    send(host, s"📎 ${b(pageName)} ${h(filename)} attached by ${h(nickname)}\n${pageUrl(host, pageName)}")

  def notifyAttachmentDeleted(host: String, pageName: String, filename: String, nickname: String): Unit =
    send(host, s"🗑️📎 ${b(pageName)} ${h(filename)} attachment deleted by ${h(nickname)}\n${pageUrl(host, pageName)}")

  def notifyClipboardImageUploaded(host: String, pageName: String, nickname: String): Unit =
    send(host, s"🖼️ ${b(pageName)} image pasted by ${h(nickname)}\n${pageUrl(host, pageName)}")

  def notifySpreadsheetSynced(host: String, pageName: String, nickname: String): Unit =
    send(host, s"🔄 ${b(pageName)} spreadsheet synced by ${h(nickname)}\n${pageUrl(host, pageName)}")
}
