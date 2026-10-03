package logics.wikis

import org.scalatest.freespec.AnyFreeSpec

/** A page's representative image comes from an image macro the page runs -- not from one it only
  * shows. Dev Page lists the image sources as `[[Image(...)]]` in backticks; until 2026-10-03 that
  * made "..." its image, and every graph showing the page asked for /... and got a 404.
  */
class PageLogicImageSpec extends AnyFreeSpec {
  private def image(content: String): Option[String] = PageLogic.extractMacroImage(PageLogic.markupOnly(content))
  private def attachmentKey(content: String): Option[String] = PageLogic.extractMacroAttachmentKey(PageLogic.markupOnly(content))

  "the image macro" - {
    "is read where the page runs it" in {
      assert(image("text\n[[Image(a.png)]]\n") === Some("a.png"))
      assert(image("[[Image(a.png, 100)]]") === Some("a.png"))
    }

    "is read inside a block that renders its body as wiki" in {
      assert(image("[[[#!Paper\n[[Image(c.png)]]\n]]]") === Some("c.png"))
    }

    "is not read from a code span" in {
      assert(image(" 1. `[[Image(...)]]`") === None)
      assert(image("``[[Image(a.png)]]``") === None)
    }

    "is not read from a block that shows its body verbatim" in {
      assert(image("[[[#!Vim text\n[[Image(a.png)]]\n]]]") === None)
      assert(image("[[[\n[[Image(a.png)]]\n]]]") === None)
    }

    "is read after a code span that only mentions it" in {
      assert(image("`[[Image(...)]]` for example:\n[[Image(b.png)]]") === Some("b.png"))
    }
  }

  "the attachment macro" - {
    "follows the same rule" in {
      assert(attachmentKey("[[Attachment(photo.jpg)]]") === Some("photo.jpg"))
      assert(attachmentKey(" 1. `[[Attachment(...)]]`") === None)
    }
  }
}
