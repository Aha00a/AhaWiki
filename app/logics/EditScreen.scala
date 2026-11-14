package logics

import logics.ScreenLanguage.{Language, Text}

/**
 * The words of the edit screen, in both languages (ScreenLanguage): what edit.scala.html prints
 * and what edit.page.js says in its buttons and alerts. The formatting help's own words are in
 * EditHelp.
 *
 * One table for the template and the script. The script gets the chosen language's half as
 * `AhaWikiEditConfig.text` and reads it with `editText(key)`; a key the script asks for and this table
 * lacks fails test/ahawiki.edit-screen-text.test.mjs by name, and one the template asks for fails
 * the edit screen's render spec, because `apply` throws rather than printing nothing.
 *
 * What stays English on purpose: the names of the wiki's own things (AhaMark, InfoBox's
 * Wikipedia), key names in the hints (Alt, Ctrl, Tab), and the permission summary's values, which
 * are the permission rules as written (`All(*)`, `Exact(...)`).
 */
object EditScreen {

  val texts: Map[String, Text] = Map(
    // The Alt-key hints beside the buttons.
    "hotkey.W" -> Text("줄 바꿈 켜고 끄기", "Toggle word wrap"),
    "hotkey.I" -> Text("위키백과에서 정보 상자 가져오기", "Fetch infobox from Wikipedia"),
    "hotkey.A" -> Text("첨부 파일 올리기", "Open attachment upload"),
    "hotkey.C" -> Text("편집 요약 칸으로", "Focus revision comment input"),
    "hotkey.M" -> Text("사소한 편집 켜고 끄기", "Toggle minor edit"),
    "hotkey.S" -> Text("저장", "Save page"),
    "hotkey.R" -> Text("이름 바꾸기", "Rename page"),
    "hotkey.T" -> Text("삭제", "Delete page"),

    "title" -> Text("편집", "Edit"),
    "dropToUpload" -> Text("파일을 여기에 놓아 올리기", "Drop files here to upload"),
    "partialEdit" -> Text("부분 편집", "Partial edit mode"),
    "switchToTable" -> Text("표 편집기로", "Switch to Table Editor"),
    "switchToRaw" -> Text("글로 편집", "Switch to Raw Editor"),
    "switchToGantt" -> Text("간트 편집기로", "Switch to Gantt Editor"),

    "wordWrap" -> Text("줄 바꿈", "Word Wrap"),
    "format.bold" -> Text("굵게 ('''...''')", "Bold ('''...''')"),
    "format.italic" -> Text("기울임 (''...'')", "Italic (''...'')"),
    "format.underline" -> Text("밑줄 (__...__)", "Underline (__...__)"),
    "format.strike" -> Text("취소선 (~~...~~)", "Strikethrough (~~...~~)"),
    "format.code" -> Text("코드 (`...`)", "Inline code (`...`)"),
    "format.heading" -> Text("제목 (== )", "Heading (== )"),
    "format.ul" -> Text("목록 ( * )", "Bullet list ( * )"),
    "format.link" -> Text("링크 ([...])", "Link ([...])"),
    "infoBox" -> Text("정보 상자", "InfoBox"),
    "attach" -> Text("첨부", "Attach"),

    "permission.show" -> Text("페이지 권한 자세히 보기", "Show page permission details"),
    "permission.hide" -> Text("페이지 권한 접기", "Hide page permission details"),
    "permission.label" -> Text("페이지 권한:", "Page Permission:"),
    "permission.keep" -> Text("지금 그대로", "Keep Current"),
    "permission.general" -> Text("사이트 설정대로", "General"),
    "permission.privateRead" -> Text("나만 읽기", "Only me can read"),
    "permission.privateWrite" -> Text("나만 고치기", "Only me can edit"),
    // The summary's labels, as Wiki.pagePermissionSummary names them.
    "permission.summary.Current User" -> Text("지금 사용자", "Current User"),
    "permission.summary.Anonymous User" -> Text("로그인 안 한 사람", "Anonymous User"),
    "permission.summary.Page Matching" -> Text("이 페이지에 맞는 규칙", "Page Matching"),

    "table" -> Text("표", "Table"),
    "table.addRow" -> Text("+ 행", "+ Row"),
    "table.deleteRow" -> Text("- 행", "- Row"),
    "table.addCol" -> Text("+ 열", "+ Col"),
    "table.deleteCol" -> Text("- 열", "- Col"),

    "gantt" -> Text("간트", "Gantt"),
    "gantt.addRow" -> Text("행 추가 (Enter)", "Add row (Enter)"),
    "gantt.deleteRow" -> Text("고른 행 삭제 (Shift+Del)", "Delete target row(s) (Shift+Del)"),
    "gantt.moveUp" -> Text("고른 행 위로 (Alt+↑)", "Move target row(s) up (Alt+↑)"),
    "gantt.moveDown" -> Text("고른 행 아래로 (Alt+↓)", "Move target row(s) down (Alt+↓)"),
    "gantt.indent" -> Text("들여쓰기 (Tab)", "Indent target row(s) (Tab)"),
    "gantt.outdent" -> Text("내어쓰기 (Shift+Tab)", "Outdent target row(s) (Shift+Tab)"),
    "gantt.today" -> Text("시작 칸에 오늘 날짜 넣기", "Insert today's date into Start field"),
    "gantt.undo" -> Text("되돌리기 (Ctrl+Z)", "Undo (Ctrl+Z)"),
    "gantt.redo" -> Text("다시 하기 (Ctrl+Y / Ctrl+Shift+Z)", "Redo (Ctrl+Y / Ctrl+Shift+Z)"),
    "gantt.toggleRaw" -> Text("글로 편집", "Switch to raw editor"),
    "gantt.task" -> Text("작업", "Task"),
    "gantt.start" -> Text("시작 / 앞선 일", "Start / Ref"),
    "gantt.est" -> Text("기간", "Est"),

    "comment" -> Text("편집 요약", "Comment"),
    "minorEdit" -> Text("사소한 편집", "Minor edit"),
    "save" -> Text("저장", "Save"),
    "rename" -> Text("이름 바꾸기", "Rename"),
    "delete" -> Text("삭제", "Delete"),

    // edit.page.js
    "partialNotTable" -> Text("부분 편집한 글이 #!table 블록이 아닙니다.", "The partial text is not a #!table block."),
    "attachment.deleteConfirm" -> Text("첨부 파일을 삭제할까요?", "Delete this attachment?"),
    "attachment.deleteFailed" -> Text("첨부 파일을 삭제하지 못했습니다.", "Attachment delete failed."),
    "attachment.listFailed" -> Text("첨부 파일 목록을 읽지 못했습니다.", "Attachment list load failed."),
    "upload.fileFailed" -> Text("파일을 올리지 못했습니다.", "File upload failed."),
    "upload.imageFailed" -> Text("그림을 올리지 못했습니다.", "Image upload failed."),
    "save.noRecaptcha" -> Text(
      "reCAPTCHA 토큰을 받지 못해 저장하지 않았습니다. 무언가 reCAPTCHA 를 막고 있을 수 있습니다.",
      "Could not get a reCAPTCHA token, so the page was not saved. Something may be blocking reCAPTCHA."),
    "save.notSaved" -> Text("저장하지 못했습니다", "Not saved"),
    "save.retryToken" -> Text("다시 저장하면 새 토큰을 받습니다.", "Saving again asks for a new token."),
    "save.permissionDenied" -> Text("권한이 없습니다", "Permission denied"),
    "save.conflict" -> Text("충돌", "Conflict"),
    "save.sameContent" -> Text("바뀐 내용이 없습니다.", "You are trying to save the same content"),
    "infoBox.notFound" -> Text("정보 상자를 찾지 못했습니다.", "No infobox found"),
  )

  /** The text for `key`. A key this table lacks is a mistake in the caller, so it throws. */
  def apply(key: String, language: Language): String =
    texts.getOrElse(key, throw new NoSuchElementException(s"EditScreen has no text for '$key'"))(language)

  /** A permission summary label in the reader's language, or as it is when it has no entry. */
  def summaryLabel(label: String, language: Language): String =
    texts.get(s"permission.summary.$label").map(_(language)).getOrElse(label)

  /** The chosen language's half, for edit.page.js. */
  def scriptTexts(language: Language): Map[String, String] =
    texts.collect { case (key, text) if !key.startsWith("hotkey.") => key -> text(language) }
}
