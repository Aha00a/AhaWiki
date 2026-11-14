package logics

import logics.ScreenLanguage.{Language, Text, TextTable}

/**
 * The words every page wears, in both languages (ScreenLanguage): the navigation bar and the
 * account menu (`_base.scala.html`), the page's created/modified line (`Wiki/action.scala.html`),
 * the sections a page view adds under the content (`Wiki.getAhaMarkAdditionalInfo`), and the
 * buttons `js.js` puts on each section -- those it reads from `window.AhaWikiFrameText`, the
 * `js.` keys, which the frame writes into every page.
 *
 * The edit screen's own words are EditScreen's. What stays as it is: the wiki's name AhaWiki, key
 * names in the hints, and the section ids (`#See-Also-Generated` and the like), which links point
 * at whatever the title says.
 */
object FrameText extends TextTable("FrameText", Map(
  // Navigation bar.
  "skipToContent" -> Text("본문으로 건너뛰기", "Skip to content"),
  "menu" -> Text("메뉴", "Menu"),
  "menu.open" -> Text("메뉴 열기", "Open main menu"),
  "navigate" -> Text("이동", "Navigate"),
  "navigate.open" -> Text("이동 메뉴 열기", "Open navigation menu"),
  "search" -> Text("찾기", "Search"),
  "random" -> Text("아무 문서", "Random"),
  "log" -> Text("기록", "Log"),
  "page" -> Text("문서", "Page"),
  "page.open" -> Text("문서 메뉴 열기", "Open page menu"),
  "raw" -> Text("원문", "Raw"),
  "history" -> Text("이력", "History"),
  "blame" -> Text("줄별 작성자", "Blame"),
  "edit" -> Text("편집", "Edit"),
  "theme" -> Text("테마", "Theme"),
  "theme.group" -> Text("테마", "Theme"),
  "theme.toggle" -> Text("밝게·어둡게", "Toggle theme"),
  "wide" -> Text("넓게", "Wide"),
  "wide.toggle" -> Text("본문 폭 제한", "Limit content width"),
  "account.open" -> Text("계정 메뉴 열기", "Open account menu"),
  "color" -> Text("색", "Color"),
  "color.hue" -> Text("테마 색조", "Theme color hue"),
  "color.reset" -> Text("사이트 기본값으로 되돌리기", "Reset hue to site default"),
  "profile" -> Text("프로필", "Profile"),
  "account" -> Text("계정", "Account"),
  "admin" -> Text("관리", "Admin"),
  "logout" -> Text("로그아웃", "Logout"),
  "login" -> Text("로그인", "Login"),
  // The one-key hints beside the menu items.
  "hotkey.S" -> Text("찾기 열기", "Open search"),
  "hotkey.R" -> Text("아무 문서나 열기", "Open random page"),
  "hotkey.W" -> Text("기록 쓰기 열기", "Open write log"),
  "hotkey.H" -> Text("이 문서의 이력", "Open page history"),
  "hotkey.B" -> Text("이 문서의 줄별 작성자", "Open page blame"),
  "hotkey.E" -> Text("이 문서 편집", "Open page edit"),

  // The created/modified line beside the title.
  "createdBy" -> Text("처음 쓴 사람", "created by"),
  "lastModifiedBy" -> Text("마지막으로 고친 사람", "last modified by"),
  "at" -> Text("·", "at"),
  "revision" -> Text("리비전:", "revision:"),
  "signedUrl" -> Text("서명된 주소 만들기", "Generate signed URL"),

  // The sections a page view adds under the content.
  // Not 함께 보기: twelve of ahawiki.net's own pages end in a section of that name, and the table
  // of contents showed two of them side by side (2026-11-15). Nothing there is called 이어진 문서.
  "seeAlso" -> Text("이어진 문서", "See Also"),
  "backlinks" -> Text("이 문서를 가리키는 문서", "Backlinks"),
  "twinPages" -> Text("다른 위키의 같은 이름", "Twin Pages"),
  "similarPages" -> Text("비슷한 문서", "Similar Pages"),
  "similarPages.note" -> Text(
    "낱말 빈도의 코사인 유사도로 고른 문서입니다. 이름 뒤의 낱말은 그 문서에 자주 나오는 말입니다.",
    "Similar pages by cosine similarity. Words after page name are term frequency."),
  "adjacentPages" -> Text("가까운 문서", "Adjacent Pages"),

  // js.js, through window.AhaWikiFrameText.
  "js.edit" -> Text("편집", "Edit"),
  "js.copy" -> Text("복사", "Copy"),
  "js.fold" -> Text("이 절 접기", "Fold this section"),
  "js.unfold" -> Text("이 절 펼치기", "Unfold this section"),
)) {

  /** The `js.` keys, for js.js. */
  def scriptTexts(language: Language): Map[String, String] = scriptTexts(language, _.startsWith("js."))
}
