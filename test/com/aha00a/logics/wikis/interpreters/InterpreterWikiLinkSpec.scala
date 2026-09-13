package com.aha00a.logics.wikis.interpreters

import logics.wikis.interpreters.InterpreterWiki
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

/** The link grammar, which had no spec until it had twice been got wrong.
  *
  * Its alternatives are tried in order and the order *is* the grammar, so what matters is not that
  * each form parses but that the right one wins. The mistake both times was the same: writing a
  * multi-word page name without quotes, which the `[Page Alias]` alternative used to claim first.
  * It read correctly on the rendered page -- the alias was the rest of the name -- and linked
  * elsewhere. Since 2026-09-14 the space is part of the name, and only a target that is not a
  * page title keeps the label after its first space.
  */
class InterpreterWikiLinkSpec extends AnyFreeSpec with Matchers {

  private def parse(s: String): Option[(String, String)] =
    InterpreterWiki.regexLink.findFirstMatchIn(s).flatMap(InterpreterWiki.uriAndAlias)

  private def escaped(s: String): Boolean =
    InterpreterWiki.regexLink.findFirstMatchIn(s).exists(_.group(1) != null)

  "a name without spaces is the page" in {
    parse("[FrontPage]") mustBe Some(("FrontPage", ""))
    parse("[Dev]") mustBe Some(("Dev", ""))
  }

  "an unquoted name with spaces is one page name" in {
    // Until 2026-09-14 these were the trap: they rendered as their full text and linked to the
    // first word. Links written in that sense were rewritten to [first|rest] before this changed.
    parse("[Dev Api]") mustBe Some(("Dev Api", ""))
    parse("[Page With Three Words]") mustBe Some(("Page With Three Words", ""))
    parse("[Forrest Gump]") mustBe Some(("Forrest Gump", ""))
  }

  "a target that is not a page title keeps the label after its first space" in {
    parse("[schema:Person 사람]") mustBe Some(("schema:Person", "사람"))
    parse("[User:aha00a 아하]") mustBe Some(("User:aha00a", "아하"))
    parse("[wiki:FrontPage Front]") mustBe Some(("wiki:FrontPage", "Front"))
    parse("[Page#section 그 절]") mustBe Some(("Page#section", "그 절"))
    parse("[?q=1 검색]") mustBe Some(("?q=1", "검색"))
    // A time is not a prefix; a prefix starts with a letter.
    parse("[12:30 회의]") mustBe Some(("12:30 회의", ""))
  }

  "a name does not start or end with a space" in {
    parse("[ a b ]") mustBe None
    // Not a link. One of this shape written before 2026-09-14 read as the page a labelled "b ";
    // where a named a page, the migration rewrote it to [a|b ], which still does.
    parse("[a b ]") mustBe None
  }

  "quoting is what makes the space part of the name" in {
    parse("""["Dev Api"]""") mustBe Some(("Dev Api", ""))
    parse("""["Dev Api" 개발 API]""") mustBe Some(("Dev Api", "개발 API"))
  }

  "a pipe says where the alias starts, so the name may hold spaces unquoted" in {
    parse("[Dev Api|개발 API]") mustBe Some(("Dev Api", "개발 API"))
    parse("[Dev|개발]") mustBe Some(("Dev", "개발"))
    // Everything after the first pipe is the alias, pipes included.
    parse("[Dev Api|a|b]") mustBe Some(("Dev Api", "a|b"))
  }

  "the pipe stands aside for the three syntaxes that own the character themselves" in {
    // A URL: `|` is legal in one, and 13 links on the wiki have it inside a description.
    parse("[https://youtu.be/x BURGER KING | Whopper Neutrality]") mustBe
      Some(("https://youtu.be/x", "BURGER KING | Whopper Neutrality"))
    // A query link: `[?Tag=술&Address=서교|합정 술집]` is real on this wiki.
    parse("[?Score=>=6&Address=서교|합정|망원 마포 술집]") mustBe
      Some(("?Score=>=6&Address=서교|합정|망원", "마포 술집"))
    // Quoted wins: the quotes already said where the name ends.
    parse("""["A|B"]""") mustBe Some(("A|B", ""))
    parse("""["A|B" alias]""") mustBe Some(("A|B", "alias"))
  }

  "a bare URL links itself" in {
    parse("https://example.com/plain") mustBe Some(("https://example.com/plain", ""))
    // A URL followed by text is the target and its description -- 6,189 links are shaped this way.
    parse("[https://example.com/a 설명]") mustBe Some(("https://example.com/a", "설명"))
  }

  "prefixes are page names to the parser; the renderer is what treats them specially" in {
    for (s <- Seq("schema:Person", "User:aha00a", "wiki:FrontPage", "#anchor", "?action=edit", "KR"))
      parse(s"[$s]") mustBe Some((s, ""))
    parse("[#Anchor Anchor Alias]") mustBe Some(("#Anchor", "Anchor Alias"))
  }

  "a backslash makes it text" in {
    escaped("""\[FrontPage]""") mustBe true
    escaped("""\["Dev Api" alias]""") mustBe true
    escaped("""\[Dev Api|alias]""") mustBe true
    escaped("[FrontPage]") mustBe false
  }

  "[ ] is not a link" in {
    // replaceLink turns it into a checkbox before the link pattern runs; the pattern must not
    // claim it either, since `[^]\s]+` can take nothing from a single space.
    parse("[ ]") mustBe None
  }

  "a macro would parse as a link, which is why macros are extracted first" in {
    // `[` is neither `]` nor whitespace, so [[Kbd(f)]] matches with a target of "[Kbd(f)".
    // ExtractConvertInjectMacro removes macros before replaceLink ever sees the text; a tool that
    // scans raw source for links has to do the same or it will read every macro as a broken link.
    parse("[[Kbd(f)]]") mustBe Some(("[Kbd(f)", ""))
  }

  "only the first link on a line is this match; the rest follow on their own" in {
    parse("[a][b]") mustBe Some(("a", ""))
    InterpreterWiki.regexLink.findAllMatchIn("[a][b]").flatMap(InterpreterWiki.uriAndAlias).toSeq mustBe
      Seq(("a", ""), ("b", ""))
  }
}
