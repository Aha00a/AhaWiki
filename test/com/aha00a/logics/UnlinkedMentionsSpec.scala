package com.aha00a.logics

import logics.UnlinkedMentions
import models.tables.SearchResult
import org.scalatest.freespec.AnyFreeSpec

import java.time.LocalDateTime

/**
 * Which of the pages a search finds are mentions of a page that do not yet link to it.
 *
 * The search matches a name or a text, so the filter has to refuse the page itself, the pages that
 * already link, and the pages that only have the name in *their* name.
 */
class UnlinkedMentionsSpec extends AnyFreeSpec {
  private val now = LocalDateTime.of(2026, 10, 25, 12, 0)
  private def page(name: String, content: String) = SearchResult(name, now, content)
  private val allowed: SearchResult => Boolean = _ => true

  "searchable" - {
    "refuses a name too short to mean anything" in {
      assert(!UnlinkedMentions.searchable("AB"))
      assert(!UnlinkedMentions.searchable("  A "))
      assert(UnlinkedMentions.searchable("Aha"))
    }

    "refuses date pages, whose names are in the text of every page that has a date" in {
      assert(!UnlinkedMentions.searchable("2026-10-25"))
      assert(!UnlinkedMentions.searchable("2026-10"))
      assert(!UnlinkedMentions.searchable("2026"))
      assert(!UnlinkedMentions.searchable("--10-25"))
    }

    "accepts an ordinary name, in any script" in {
      assert(UnlinkedMentions.searchable("성운 빛"))
      assert(UnlinkedMentions.searchable("Dev GraphGlow"))
    }
  }

  "of" - {
    "keeps a page that says the name in its text and does not link" in {
      val m = UnlinkedMentions.of("GraphGlow", Seq(page("Notes", "see the GraphGlow effect")), Set.empty, allowed)
      assert(m.map(_.name) === Seq("Notes"))
      assert(m.head.snippet.contains("GraphGlow"))
    }

    "does not count the name inside a longer name: Macro is not MacroInclude" in {
      val found = Seq(page("Family", "see MacroInclude and MacroPageMap"), page("Real", "the Macro, as a page"))
      assert(UnlinkedMentions.of("Macro", found, Set.empty, allowed).map(_.name) === Seq("Real"))
    }

    "counts the name beside punctuation and brackets" in {
      for (text <- Seq("(Macro)", "Macro.", "[Macro]", "a Macro b", "\"Macro\"", "Macro/Include", "Macro-ish"))
        assert(UnlinkedMentions.of("Macro", Seq(page("P", text)), Set.empty, allowed).size === 1, text)
    }

    "counts a Korean name with a particle stuck to it" in {
      assert(UnlinkedMentions.of("성운", Seq(page("P", "밤하늘의 성운을 본다")), Set.empty, allowed).size === 1)
    }

    "does not count a name stuck to digits or an underscore" in {
      assert(UnlinkedMentions.of("Macro", Seq(page("P", "Macro2 and _Macro")), Set.empty, allowed).isEmpty)
    }

    "centres the snippet on the real mention, not on an earlier substring" in {
      val text = "MacroInclude comes first. " + ("filler " * 40) + "and only here is the Macro itself mentioned"
      val snippet = UnlinkedMentions.of("Macro", Seq(page("P", text)), Set.empty, allowed).head.snippet
      assert(snippet.contains("the Macro itself"))
      assert(!snippet.contains("MacroInclude"))
    }

    "drops the page itself" in {
      assert(UnlinkedMentions.of("GraphGlow", Seq(page("GraphGlow", "GraphGlow is this page")), Set.empty, allowed).isEmpty)
    }

    "drops a page that already links here" in {
      val found = Seq(page("Linker", "GraphGlow, linked"), page("Mentioner", "GraphGlow, not linked"))
      assert(UnlinkedMentions.of("GraphGlow", found, Set("Linker"), allowed).map(_.name) === Seq("Mentioner"))
    }

    "drops a page that has the name only in its own name" in {
      assert(UnlinkedMentions.of("Foo", Seq(page("Foo Bar", "nothing about it here")), Set.empty, allowed).isEmpty)
    }

    "matches whatever the case" in {
      assert(UnlinkedMentions.of("GraphGlow", Seq(page("Notes", "the graphglow effect")), Set.empty, allowed).size === 1)
    }

    "drops a page the reader may not read" in {
      val found = Seq(page("Secret", "GraphGlow, hidden"), page("Public", "GraphGlow, shown"))
      val canRead: SearchResult => Boolean = _.name != "Secret"
      assert(UnlinkedMentions.of("GraphGlow", found, Set.empty, canRead).map(_.name) === Seq("Public"))
    }

    "keeps the order the search gave" in {
      val found = Seq(page("B", "x GraphGlow"), page("A", "x GraphGlow"))
      assert(UnlinkedMentions.of("GraphGlow", found, Set.empty, allowed).map(_.name) === Seq("B", "A"))
    }
  }
}
