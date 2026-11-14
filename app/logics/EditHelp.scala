package logics

import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage

import java.util.Locale

/**
 * What the editor's formatting help shows: the markup people reach for most, each with what
 * it draws, and the blocks -- `[[[#!Name ... ]]]` -- with a small sample that works as inserted.
 *
 * The result column is drawn by the renderer on every edit page rather than written down as HTML
 * here. A hand-written result would be a second statement of what the renderer does, and the two
 * would part the first time the renderer changed; drawn, the help cannot show something the page
 * would not. A block the browser draws -- a diagram, a board, a formula, a map -- has nothing to
 * show until its script runs, so it gets a sentence instead (`drawn = false`).
 *
 * It speaks the reader's language: Korean when the browser asks for Korean, English otherwise
 * ([[Language.of]]). That is the one thing the engine already does by browser language -- the
 * names of days and months (`MacroWeekdayName`, `MacroMonthName`, ...) follow the same
 * `RequestWrapper.locale` -- and the help follows it rather than adding a second way to choose.
 * There is no translation layer (`conf/messages` is empty), so both languages are written here,
 * side by side, as [[Text]]: a sentence and its translation change together, and in one place.
 *
 * The longer cheat sheet is the AhaMark Basic page on ahawiki.net, which the help links to first;
 * it is a wiki page with its own readers and lives on one site, so the two are not merged.
 */
object EditHelp {

  sealed trait Language { def code: String }
  object Language {
    case object Korean extends Language { val code = "ko" }
    case object English extends Language { val code = "en" }

    /** Korean for a Korean browser, English for every other. The wiki's content is Korean, but
      * someone whose browser asks for another language reads English more readily than Korean. */
    def of(locale: Locale): Language = if (locale.getLanguage == "ko") Korean else English

    def current(implicit wikiContext: ContextWikiPage): Language = of(wikiContext.requestWrapper.locale)
  }

  /** A text in both languages. An empty Korean side means there is nothing to say in either. */
  case class Text(ko: String, en: String) {
    def apply(language: Language): String = language match {
      case Language.Korean => ko
      case Language.English => en
    }
    def nonEmpty: Boolean = ko.nonEmpty
    def both: String = s"$ko $en"
  }
  val NoText: Text = Text("", "")

  /** The words around the rows, in edit.scala.html. */
  object Ui {
    val summary: Text = Text("서식 도움말", "Formatting help")
    val searchPlaceholder: Text = Text("찾기 — 표, 링크, kanban …", "Find — table, link, kanban …")
    val searchLabel: Text = Text("서식 찾기", "Find a format")
    val lead: Text = Text(
      "줄을 누르면 커서 자리에 넣습니다. 커서가 있는 곳의 서식은 색으로 표시됩니다.",
      "Click a row to put it where the cursor is. The format under the cursor is highlighted.")
    val insert: Text = Text("커서 자리에 넣기", "Insert at the cursor")
    val more: Text = Text("자세히", "More")
    val empty: Text = Text("찾는 서식이 없습니다. 아래 문서에서 찾아보세요.", "No format matches. Try the pages below.")
    val seeAlso: Text = Text("더 보기", "See also")
    val cheatSheet: Text = Text("치트시트", "cheat sheet")
  }

  /**
   * One row.
   *
   *  - `block`: inserted on a line of its own; otherwise where the cursor is.
   *  - `kind`: what `AhaWiki.EditHelpContext.kindAt` answers for a cursor on such markup, so the
   *    row can be lit while the cursor is there. Blocks use the interpreter's name in lower case.
   *  - `drawn`: whether the result column is the renderer's drawing or `description`.
   *  - `page`: the ahawiki.net page that tells the rest.
   */
  case class Example(
    label: Text,
    markup: Text,
    kind: String,
    block: Boolean,
    note: Text = NoText,
    drawn: Boolean = true,
    description: Text = NoText,
    page: String = "",
  )

  case class Group(title: Text, examples: Seq[Example])

  case class Row(example: Example, html: String)

  private def same(markup: String): Text = Text(markup, markup)

  val groups: Seq[Group] = Seq(
    Group(Text("글자와 줄", "Text and lines"), Seq(
      Example(Text("굵게·기울임", "Bold, italic"), Text("'''굵게''' ''기울임''", "'''bold''' ''italic''"), kind = "emphasis", block = false),
      Example(Text("코드", "Code"), Text("`코드`", "`code`"), kind = "code", block = false),
      Example(Text("링크", "Link"), same("[FrontPage]"), kind = "link", block = false,
        note = Text("없는 문서 이름을 적으면, 그 링크를 눌러 새 문서를 만듭니다.", "A page that does not exist yet is created from its link.")),
      Example(Text("링크 별칭", "Link label"), Text("[FrontPage|첫 화면]", "[FrontPage|Home]"), kind = "link", block = false,
        note = Text("| 뒤가 보이는 글자입니다.", "What follows | is the text shown.")),
      Example(Text("제목", "Heading"), Text("== 제목", "== Heading"), kind = "heading", block = true,
        note = Text("= 를 늘리면 한 단계 아래 제목입니다.", "More = for a lower level.")),
      Example(Text("목록", "List"), Text(" * 항목\n  * 안쪽 항목", " * item\n  * nested item"), kind = "list", block = true,
        note = Text("맨 앞에 띄어쓰기 한 칸이 있어야 합니다. 없으면 그냥 글입니다.", "Starts with a space. Without it, it is plain text.")),
      Example(Text("번호 목록", "Numbered list"), Text(" 1. 첫째\n 1. 둘째", " 1. first\n 1. second"), kind = "numbered", block = true,
        note = Text("맨 앞에 띄어쓰기 한 칸. 번호는 저절로 매겨집니다.", "Starts with a space. The numbers count themselves.")),
      Example(Text("체크박스", "Checkbox"), Text(" * [ ] 할 일\n * [[CB(x)]] 끝낸 일", " * [ ] to do\n * [[CB(x)]] done"), kind = "checkbox", block = true,
        note = Text("끝낸 일은 [[CB(x)]] 입니다. [x] 는 x 라는 문서로 가는 링크가 됩니다.", "Done is [[CB(x)]], not [x]: [x] links to a page named x.")),
      Example(Text("가로선", "Horizontal line"), same("----"), kind = "hr", block = true),
    )),
    Group(Text("블록", "Blocks"), Seq(
      Example(Text("표", "Table"), Text("[[[#!Table tsv 1\n이름\t역할\n김하나\t기획\n]]]", "[[[#!Table tsv 1\nName\tRole\nKim\tDesign\n]]]"), kind = "table", block = true,
        note = Text("칸은 탭으로 나눕니다. 스프레드시트에서 복사해 붙여 넣어도 됩니다. 1 은 첫 줄이 머리글이라는 뜻입니다.",
          "Cells are separated by tabs; a paste from a spreadsheet works. 1 means the first line is the header."),
        page = "InterpreterTable"),
      Example(Text("코드 구간", "Code block"), same("[[[#!Vim python\nprint(\"hello\")\n]]]"), kind = "vim", block = true,
        note = Text("python 자리에 언어 이름을 적으면 색을 입힙니다.", "Name the language where python is, to colour it."),
        page = "InterpreterVim"),
      Example(Text("접기", "Fold"), Text("[[[#!Fold 자세히 보기\n접어 둔 내용\n]]]", "[[[#!Fold Show more\nFolded away\n]]]"), kind = "fold", block = true,
        note = Text("Fold 뒤의 글이 단추의 글입니다.", "The words after Fold are the button's."), page = "InterpreterFold"),
      Example(Text("인용", "Quote"), Text("[[[#!Quote\n한 번 적어 두면 두 번 묻지 않는다.\n]]]", "[[[#!Quote\nWrite it down once and nobody asks twice.\n]]]"), kind = "quote", block = true,
        page = "InterpreterQuote"),
      Example(Text("흐름도", "Mermaid"), Text("[[[#!Mermaid\ngraph TD\n    시작-->검토\n    검토-->끝\n]]]", "[[[#!Mermaid\ngraph TD\n    Start-->Review\n    Review-->Done\n]]]"), kind = "mermaid", block = true,
        drawn = false, description = Text("상자와 화살표로 흐름도·순서도를 그립니다. --> 가 화살표입니다.", "Draws a flowchart of boxes and arrows. --> is an arrow."),
        page = "InterpreterMermaid"),
      Example(Text("할 일 보드", "Kanban"), Text("[[[#!Kanban\n=== 할 일\n==== 첫 카드\n=== 하는 중\n=== 끝\n]]]", "[[[#!Kanban\n=== To do\n==== First card\n=== Doing\n=== Done\n]]]"), kind = "kanban", block = true,
        drawn = false, description = Text("=== 는 칸, ==== 는 카드입니다. 저장하면 카드를 끌어 옮길 수 있습니다.", "=== is a column, ==== a card. Once saved, cards can be dragged."),
        page = "InterpreterKanban"),
      Example(Text("점과 선", "Graph"), Text("[[[#!Graph\n가->나\n나->다\n]]]", "[[[#!Graph\nA->B\nB->C\n]]]"), kind = "graph", block = true,
        drawn = false, description = Text("이름을 -> 로 이어 관계 그림을 그립니다.", "Joins names with -> into a picture of how they relate."),
        page = "InterpreterGraph"),
      Example(Text("일정 막대", "Gantt"), Text("[[[#!Gantt\n기획\t2026-12-01\t3\n\t조사\t\t2\n개발\t기획\t5\n]]]", "[[[#!Gantt\nPlan\t2026-12-01\t3\n\tResearch\t\t2\nBuild\tPlan\t5\n]]]"), kind = "gantt", block = true,
        drawn = false, description = Text("줄마다 일, 시작일(또는 앞선 일), 걸리는 날 수. 앞에 탭을 넣으면 안쪽 일입니다.",
          "Each line: a task, its start (or the task before it) and its days. A leading tab nests it."),
        page = "InterpreterGantt"),
      Example(Text("수식", "Math"), same("[[[#!Math\nE = mc^2\n]]]"), kind = "math", block = true,
        drawn = false, description = Text("LaTeX 수식을 그립니다.", "Typesets LaTeX."), page = "InterpreterMath"),
      Example(Text("지도", "Map"), Text("[[[#!Map\nName\tAddress\n시청\t서울 중구 세종대로 110\n]]]", "[[[#!Map\nName\tAddress\nCity Hall\t110 Sejong-daero, Jung-gu, Seoul\n]]]"), kind = "map", block = true,
        drawn = false, description = Text("주소를 지도에 핀으로 꽂습니다. 칸은 탭으로 나눕니다.", "Pins addresses on a map. Cells are separated by tabs."),
        page = "InterpreterMap"),
      Example(Text("다른 문서 넣기", "Include"), Text("[[Include(문서이름)]]", "[[Include(PageName)]]"), kind = "include", block = true,
        drawn = false, description = Text("다른 문서의 내용을 이 자리에 그대로 보여 줍니다.", "Shows another page's content in this place."),
        page = "MacroInclude"),
    )),
  )

  def examples: Seq[Example] = groups.flatMap(_.examples)

  /** A row's name: the Korean one keeps the English after it, which is what the interpreter is
    * called and what a Korean reader may well type into the search. */
  def label(example: Example, language: Language): String = language match {
    case Language.Korean => s"${example.label.ko} (${example.label.en})"
    case Language.English => example.label.en
  }

  /** What the search box looks through: both languages, so either finds the row. */
  def searchText(example: Example): String =
    Seq(example.label.both, example.markup.both, example.note.both, example.description.both, example.page).mkString(" ").toLowerCase

  def rows(group: Group, language: Language)(implicit wikiContext: ContextWikiPage): Seq[Row] =
    group.examples.map { example =>
      Row(example, if (example.drawn) Interpreters.toHtmlString("#!wiki\n" + example.markup(language)) else "")
    }

  def pageUrl(page: String): String =
    "https://ahawiki.net/w/" + java.net.URLEncoder.encode(page, "UTF-8").replace("+", "%20")
}
