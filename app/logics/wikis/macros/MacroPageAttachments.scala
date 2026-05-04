package logics.wikis.macros

import com.aha00a.commons.Implicits._
import models.ContextWikiPage
import models.tables.Attachment

object MacroPageAttachments extends TraitMacro {
  override def isBlock: Boolean = true

  override def toHtmlString(argument: String)(implicit wikiContext: ContextWikiPage): String = {
    val attachments = wikiContext.database.withConnection { implicit connection =>
      Attachment.selectUploadedByPage(wikiContext.site.seq, wikiContext.name)
    }

    if (attachments.isEmpty) {
      ""
    } else {
      val items = attachments.map { attachment =>
        val fileName = attachment.originalFilename.escapeHtml()
        val contentType = attachment.contentType.escapeHtml()
        val fileSize = f"${attachment.fileSize}%,d"
        S3AttachmentUrlLogic.generatePresignedUrl(attachment.objectKey).toOption match {
          case Some(url) =>
            val href = url.escapeHtmlAttribute()
            val thumbOrIcon = if (attachment.contentType.startsWith("image/")) {
              s"""<img src="$href" alt="$fileName" class="attachmentThumb"/>"""
            } else {
              """<i class="far fa-file" aria-hidden="true"></i>"""
            }
            s"""<li class="attachmentItem"><div class="attachmentItemBody"><div class="attachmentThumbWrap">$thumbOrIcon</div><div class="attachmentContent" style="flex:1"><div class="attachmentItemMeta"><a href="$href" target="_blank" rel="noopener" class="wikiAttachmentLink"><span class="attachmentName">$fileName</span></a></div><div class="attachmentMeta">$contentType · $fileSize bytes</div></div></div></li>"""
          case None =>
            s"""<li class="attachmentItem"><div class="attachmentItemBody"><div class="attachmentThumbWrap"><i class="fas fa-exclamation-triangle" aria-hidden="true"></i></div><div class="attachmentContent" style="flex:1"><div class="attachmentItemMeta"><span class="attachmentName">$fileName</span></div><div class="attachmentMeta failed">$contentType · $fileSize bytes · DB_ONLY (S3 missing)</div></div></div></li>"""
        }
      }.mkString("\n")

      s"""<div class="wikiAttachmentSection"><ul class="wikiAttachmentList">$items</ul></div>"""
    }
  }
}
