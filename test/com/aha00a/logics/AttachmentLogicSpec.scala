package com.aha00a.logics

import logics.AttachmentLogic
import logics.AttachmentLogic.PageListing
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDateTime

/** The key an upload is written under, and what the attachment list says about a row's object.
  * Wiki page Dev Attachment shows the same layout, and the pages that already hold
  * `[[Attachment(...)]]` macros depend on it staying put. */
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

  // The attachment list called every row OK as long as a URL could be signed, and signing never
  // asks S3: aha00a.com's first attachment had lost its object and was still OK in the editor. The
  // list already reads the page's listing for S3_ONLY; these pin when that listing may overrule the
  // signature and when it may not.
  "integrityStatus" - {
    val prefix = AttachmentLogic.pagePrefix(1, "MyPage")
    val objectKey = AttachmentLogic.objectKey(1, "MyPage", "clipboard", "png", uploadedAt)
    def listing(keys: String*): Option[PageListing] = Some(PageListing(prefix, keys, truncated = false))

    "is OK when the listing holds the key" in {
      assert(AttachmentLogic.integrityStatus(objectKey, presigned = true, listing(objectKey)) === "OK")
    }

    "is DB_ONLY when a complete listing of the key's prefix does not hold it" in {
      assert(AttachmentLogic.integrityStatus(objectKey, presigned = true, listing(s"${prefix}other.png")) === "DB_ONLY")
    }

    "is DB_ONLY when the URL could not be signed, whatever the listing says" in {
      assert(AttachmentLogic.integrityStatus(objectKey, presigned = false, listing(objectKey)) === "DB_ONLY")
    }

    "stays OK when S3 cut the listing short, since it may have stopped before the key" in {
      assert(AttachmentLogic.integrityStatus(objectKey, presigned = true, Some(PageListing(prefix, Seq.empty, truncated = true))) === "OK")
    }

    "stays OK for a key outside the listed prefix, which the listing never looked at" in {
      assert(AttachmentLogic.integrityStatus(AttachmentLogic.objectKey(1, "Other", "a.png", "png", uploadedAt), presigned = true, listing()) === "OK")
    }

    "stays OK when there is no listing at all" in {
      assert(AttachmentLogic.integrityStatus(objectKey, presigned = true, None) === "OK")
    }
  }
}
