package com.aha00a.logics

import logics.AttachmentLogic
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDateTime

/** The key an upload is written under. Wiki page Dev Attachment shows the same layout, and the
  * pages that already hold `[[Attachment(...)]]` macros depend on it staying put. */
class AttachmentLogicSpec extends AnyFreeSpec {
  private val uploadedAt = LocalDateTime.of(2024, 1, 1, 12, 0, 0)

  "objectKey" - {
    "is the page prefix, the file's name, and the name again with the upload time" in {
      assert(AttachmentLogic.objectKey(1, "MyPage", "photo.jpg", "jpg", uploadedAt) === "Attachment/1/MyPage/photo.jpg/photo.2024-01-01T12-00-00.jpg")
    }

    "puts a pasted image under clipboard" in {
      assert(AttachmentLogic.objectKey(1, "MyPage", "clipboard", "png", uploadedAt) === "Attachment/1/MyPage/clipboard/clipboard.2024-01-01T12-00-00.png")
    }

    "sanitizes the page name and the file name, keeping Hangul" in {
      assert(AttachmentLogic.objectKey(7, "A B/C", "내 사진 (1).jpg", "jpg", uploadedAt) === "Attachment/7/A_B_C/내_사진__1_.jpg/내_사진__1_.2024-01-01T12-00-00.jpg")
    }

    "keeps a name that is nothing but its extension rather than leaving it empty" in {
      assert(AttachmentLogic.objectKey(1, "P", ".png", "png", uploadedAt) === "Attachment/1/P/.png/.png.2024-01-01T12-00-00.png")
    }

    // The listing that page delete and the attachment list run is by this prefix. A key outside it
    // is an upload nothing finds again.
    "lies under the prefix the page's attachments are listed by" in {
      Seq("MyPage", "A B", "A_B", "대문 페이지", "a/b?c").foreach { pageName =>
        assert(AttachmentLogic.objectKey(3, pageName, "x.png", "png", uploadedAt).startsWith(AttachmentLogic.pagePrefix(3, pageName)), pageName)
      }
    }
  }
}
