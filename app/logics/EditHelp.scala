package logics

import logics.wikis.interpreters.Interpreters
import models.ContextWikiPage

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
 * The text is Korean. The engine already assumes every wiki is written in Korean
 * (`AhaWikiConfig.ContentLanguage`) and has no translation layer; the names keep the English
 * word after them, which is what the interpreter is called and what a search may be typed as.
 * When the engine gets one, these strings are the first to move.
 *
 * The longer cheat sheet is the AhaMark Basic page on ahawiki.net, which the help links to first;
 * it is a wiki page with its own readers and lives on one site, so the two are not merged.
 */
object EditHelp {

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
    label: String,
    markup: String,
    kind: String,
    block: Boolean,
    note: String = "",
    drawn: Boolean = true,
    description: String = "",
    page: String = "",
  )

  case class Group(title: String, examples: Seq[Example])

  case class Row(example: Example, html: String)

  val groups: Seq[Group] = Seq(
    Group("글자와 줄", Seq(
      Example("굵게·기울임 (Bold, italic)", "'''굵게''' ''기울임''", "emphasis", block = false),
      Example("코드 (Code)", "`코드`", "code", block = false),
      Example("링크 (Link)", "[FrontPage]", "link", block = false, "없는 문서 이름을 적으면, 그 링크를 눌러 새 문서를 만듭니다."),
      Example("링크 별칭 (Link label)", "[FrontPage|첫 화면]", "link", block = false, "| 뒤가 보이는 글자입니다."),
      Example("제목 (Heading)", "== 제목", "heading", block = true, "= 를 늘리면 한 단계 아래 제목입니다."),
      Example("목록 (List)", " * 항목\n  * 안쪽 항목", "list", block = true, "맨 앞에 띄어쓰기 한 칸이 있어야 합니다. 없으면 그냥 글입니다."),
      Example("번호 목록 (Numbered list)", " 1. 첫째\n 1. 둘째", "numbered", block = true, "맨 앞에 띄어쓰기 한 칸. 번호는 저절로 매겨집니다."),
      Example("체크박스 (Checkbox)", " * [ ] 할 일\n * [[CB(x)]] 끝낸 일", "checkbox", block = true, "끝낸 일은 [[CB(x)]] 입니다. [x] 는 x 라는 문서로 가는 링크가 됩니다."),
      Example("가로선 (Horizontal line)", "----", "hr", block = true),
    )),
    Group("블록", Seq(
      Example("표 (Table)", "[[[#!Table tsv 1\n이름\t역할\n김하나\t기획\n]]]", "table", block = true, "칸은 탭으로 나눕니다. 스프레드시트에서 복사해 붙여 넣어도 됩니다. 1 은 첫 줄이 머리글이라는 뜻입니다.", page = "InterpreterTable"),
      Example("코드 구간 (Code block)", "[[[#!Vim python\nprint(\"hello\")\n]]]", "vim", block = true, "python 자리에 언어 이름을 적으면 색을 입힙니다.", page = "InterpreterVim"),
      Example("접기 (Fold)", "[[[#!Fold 자세히 보기\n접어 둔 내용\n]]]", "fold", block = true, "Fold 뒤의 글이 단추의 글입니다.", page = "InterpreterFold"),
      Example("인용 (Quote)", "[[[#!Quote\n한 번 적어 두면 두 번 묻지 않는다.\n]]]", "quote", block = true, page = "InterpreterQuote"),
      Example("흐름도 (Mermaid)", "[[[#!Mermaid\ngraph TD\n    시작-->검토\n    검토-->끝\n]]]", "mermaid", block = true,
        drawn = false, description = "상자와 화살표로 흐름도·순서도를 그립니다. --> 가 화살표입니다.", page = "InterpreterMermaid"),
      Example("할 일 보드 (Kanban)", "[[[#!Kanban\n=== 할 일\n==== 첫 카드\n=== 하는 중\n=== 끝\n]]]", "kanban", block = true,
        drawn = false, description = "=== 는 칸, ==== 는 카드입니다. 저장하면 카드를 끌어 옮길 수 있습니다.", page = "InterpreterKanban"),
      Example("점과 선 (Graph)", "[[[#!Graph\n가->나\n나->다\n]]]", "graph", block = true,
        drawn = false, description = "이름을 -> 로 이어 관계 그림을 그립니다.", page = "InterpreterGraph"),
      Example("일정 막대 (Gantt)", "[[[#!Gantt\n기획\t2026-12-01\t3\n\t조사\t\t2\n개발\t기획\t5\n]]]", "gantt", block = true,
        drawn = false, description = "줄마다 일, 시작일(또는 앞선 일), 걸리는 날 수. 앞에 탭을 넣으면 안쪽 일입니다.", page = "InterpreterGantt"),
      Example("수식 (Math)", "[[[#!Math\nE = mc^2\n]]]", "math", block = true,
        drawn = false, description = "LaTeX 수식을 그립니다.", page = "InterpreterMath"),
      Example("지도 (Map)", "[[[#!Map\nName\tAddress\n시청\t서울 중구 세종대로 110\n]]]", "map", block = true,
        drawn = false, description = "주소를 지도에 핀으로 꽂습니다. 칸은 탭으로 나눕니다.", page = "InterpreterMap"),
      Example("다른 문서 넣기 (Include)", "[[Include(문서이름)]]", "include", block = true,
        drawn = false, description = "다른 문서의 내용을 이 자리에 그대로 보여 줍니다.", page = "MacroInclude"),
    )),
  )

  def examples: Seq[Example] = groups.flatMap(_.examples)

  def rows(group: Group)(implicit wikiContext: ContextWikiPage): Seq[Row] =
    group.examples.map { example =>
      Row(example, if (example.drawn) Interpreters.toHtmlString("#!wiki\n" + example.markup) else "")
    }

  def pageUrl(page: String): String =
    "https://ahawiki.net/w/" + java.net.URLEncoder.encode(page, "UTF-8").replace("+", "%20")
}
