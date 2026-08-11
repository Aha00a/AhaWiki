package com.aha00a.logics.wikis.macros

import com.aha00a.tests.provider.EmptyContextWikiPage
import logics.wikis.InlinedSource
import logics.wikis.macros.MacroInlineDays
import models.tables.Page
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDateTime

/**
 * Pushing a spliced page's headings down a level.
 *
 * A month page supplies the heading for each day, so the day's own headings have to sit under
 * it. The shift used to run over the raw markup, where `== ` at the start of a line inside a
 * `[[[ ... ]]]` block — a shell transcript, a diff — is text the author wrote rather than a
 * heading, and would have gained an `=` it never had.
 */
class MacroInlineDaysSpec extends AnyFreeSpec with EmptyContextWikiPage {

  "oneLevelDeeper" - {
    "pushes a heading down one level" in {
      assert(MacroInlineDays.oneLevelDeeper("== Lunch") === "=== Lunch")
    }

    "pushes every level, keeping the depth between them" in {
      val shifted = MacroInlineDays.oneLevelDeeper("= One\n== Two\n====== Six")

      assert(shifted === "== One\n=== Two\n======= Six")
    }

    "leaves text that only looks like a heading alone" in {
      assert(MacroInlineDays.oneLevelDeeper("not == a heading") === "not == a heading")
      assert(MacroInlineDays.oneLevelDeeper("==missing space") === "==missing space")
    }

    "does not touch a heading-shaped line inside an interpreter block" in {
      val markup =
        """== Ate out
          |[[[#!Vim diff
          |== 8 <     the old line
          |== 9 >     the new line
          |]]]
          |== Went home""".stripMargin

      val shifted = MacroInlineDays.oneLevelDeeper(markup)

      assert(shifted.contains("=== Ate out"))
      assert(shifted.contains("=== Went home"))
      assert(shifted.contains("== 8 <     the old line"))
      assert(!shifted.contains("=== 8 <"))
    }

    "puts the block back exactly as it was, delimiters included" in {
      val markup = "[[[#!Html\n<p>hi</p>\n]]]"

      assert(MacroInlineDays.oneLevelDeeper(markup) === markup)
    }

    "handles more than one block" in {
      val markup = "[[[#!Text\n== a\n]]]\n== between\n[[[#!Text\n== b\n]]]"

      val shifted = MacroInlineDays.oneLevelDeeper(markup)

      assert(shifted === "[[[#!Text\n== a\n]]]\n=== between\n[[[#!Text\n== b\n]]]")
    }

    "leaves an empty body alone" in {
      assert(MacroInlineDays.oneLevelDeeper("") === "")
    }
  }

  /**
   * Where a day's body starts in the page as stored.
   *
   * This is the arithmetic the edit links rest on. Get it wrong and the control opens the right
   * page at the wrong place, which is harder to notice than opening nothing.
   */
  "bodyOf" - {
    "starts after a heading the page wrote by hand" in {
      val body = MacroInlineDays.bodyOf("= [2020-01]-04 Sat\nAte out.\nWent home.")

      assert(body.lines === Seq("Ate out.", "Went home."))
      assert(body.firstRawLine === 2)
      assert(body.rawLineCount === 3)
    }

    "starts after a DayHeader macro" in {
      val body = MacroInlineDays.bodyOf("[[DayHeader]]\nAte out.")

      assert(body.lines === Seq("Ate out."))
      assert(body.firstRawLine === 2)
    }

    "counts the directives the reader would count" in {
      val body = MacroInlineDays.bodyOf("#!read all\n#!var mood good\n[[DayHeader]]\nAte out.")

      assert(body.lines === Seq("Ate out."))
      assert(body.firstRawLine === 4)
      assert(body.rawLineCount === 4)
    }

    "keeps a first line that is not a heading" in {
      val body = MacroInlineDays.bodyOf("Ate out.\nWent home.")

      assert(body.lines === Seq("Ate out.", "Went home."))
      assert(body.firstRawLine === 1)
    }

    "keeps a first line that is not a heading, after directives" in {
      val body = MacroInlineDays.bodyOf("#!read all\nAte out.")

      assert(body.lines === Seq("Ate out."))
      assert(body.firstRawLine === 2)
    }
  }

  /**
   * The line-by-line origin the section edit controls are built from.
   *
   * Verified here rather than against the database because the day pages that carry a heading
   * of their own — the case that makes the arithmetic worth anything — are all on wikis this
   * process cannot read anonymously.
   */
  "assemble" - {
    def page(name: String, revision: Long, content: String): Page =
      Page(name, revision, LocalDateTime.of(2020, 1, 1, 0, 0), None, None, "", "", isMinorEdit = false, content)

    "gives every line of a day the day it came from" in {
      val assembled = MacroInlineDays.assemble(Seq(page("2020-01-04", 8, "[[DayHeader]]\nAte out.\nWent home.")))

      assert(assembled.sourceAt(1).map(_.page) === Some("2020-01-04"))
      assert(assembled.sourceAt(2).map(_.page) === Some("2020-01-04"))
      assert(assembled.sourceAt(3).map(_.page) === Some("2020-01-04"))
      assert(assembled.sources.forall(_.revision == 8))
    }

    "points the day's own heading at the whole day page" in {
      val assembled = MacroInlineDays.assemble(Seq(page("2020-01-04", 8, "[[DayHeader]]\nAte out.\nWent home.")))

      assert(assembled.sourceAt(1) === Some(InlinedSource("2020-01-04", 8, 1, 3)))
    }

    "counts a body line from where it sits in the page as stored" in {
      val assembled = MacroInlineDays.assemble(Seq(page("2020-01-04", 8, "#!read all\n[[DayHeader]]\nAte out.\n== Evening\nWent home.")))

      // 조립된 2번째 줄부터가 본문. 저장된 페이지에서는 디렉티브 1줄 + 제목 1줄 뒤인 3번째 줄이다.
      assert(assembled.sourceAt(2).map(_.line) === Some(3))
      assert(assembled.sourceAt(3).map(_.line) === Some(4))
      assert(assembled.sourceAt(4).map(_.line) === Some(5))
    }

    "keeps each day apart" in {
      val assembled = MacroInlineDays.assemble(Seq(
        page("2020-01-05", 2, "[[DayHeader]]\nSecond day."),
        page("2020-01-04", 8, "[[DayHeader]]\nFirst day."),
      ))

      assert(assembled.sourceAt(1).map(_.page) === Some("2020-01-05"))
      assert(assembled.sourceAt(2).map(_.page) === Some("2020-01-05"))
      assert(assembled.sourceAt(3).map(_.page) === Some("2020-01-04"))
      assert(assembled.sourceAt(4).map(_.page) === Some("2020-01-04"))
    }

    "knows nothing about a line past the end" in {
      val assembled = MacroInlineDays.assemble(Seq(page("2020-01-04", 8, "[[DayHeader]]\nAte out.")))

      assert(assembled.sourceAt(99) === None)
    }

    "still shifts the day's own headings down" in {
      val assembled = MacroInlineDays.assemble(Seq(page("2020-01-04", 8, "[[DayHeader]]\n== Evening")))
      val lines = assembled.markup.split("\n")

      // 요일 이름은 읽는 사람의 로케일을 따르므로 글자로 단정하지 않는다.
      assert(lines(0).startsWith("== [2020-01-04] "))
      assert(lines(1) === "=== Evening")
    }
  }

  "registration" - {
    "is named after what it does, not after Include" in {
      assert(MacroInlineDays.name === "InlineDays")
    }
  }
}
