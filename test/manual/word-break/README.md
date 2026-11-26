# Word break harness

Draws a wiki page's text with the real `less.css`, `wiki.css` and `interpreter.css`, without a
server or a login, once at the page's width and once in a 390px box, and checks where its lines
break:

 * **A word that fits on a line is on one line** -- Korean, English, a sum, a date, the label of
   a link, a table cell.
 * **A word wider than its line is broken**: an address, Korean letters with no space among them,
   a page name.
 * **Nothing ends past the page's right edge.**
 * **Inline code still breaks after any letter** and stays in. That is on purpose: code has no
   words to keep whole.
 * **A table with nothing around it is no wider than the page.** It gives way inside a word when
   its columns are squeezed narrower than their words.
 * **Code squeezed in a table keeps four letters or more to a line** -- the Raw side of a
   `#!WikiSyntaxPreview` whose Preview side cannot get narrower.

The page exists because `.wikiContent` was `word-break: break-all` until 2026-11-26, which put a
break after any letter, and the change away from it touches every page of every site. The last
two checks are the ones that guard how it was done. With `overflow-wrap: break-word` instead of
`anywhere`, a squeezed table grows as wide as its longest words -- 671px in the 390px box. And
with `anywhere` left on for code, which keeps `break-all`, the browser counts a code block as
narrow as a letter when it squeezes a table: the first version of the change drew the Raw side
of the examples on a phone one letter to a line -- on this page too, where `break-all` alone
gives five. The wiki page `Dev Less` has the account.

The result is printed under the page as `PASS`/`FAIL` lines, and the last line counts them.

## The markup

It is what a wiki's `/api/renderAhaMark` answered for the text below, unchanged, put in twice.
The bare table at the end of the narrow box is written by hand, as `Wiki/history.scala.html`
writes one. When `InterpreterWiki` changes what it writes, change the page with it. Nothing
checks that they match.

```text
== 줄바꿈 견본
위키는 한 문단을 한 줄로 쓰는 것이 … 100,000,000원 같은 금액이나 2026-11-26 같은 날짜, International Standard Book Number 같은 영어 낱말이 중간에서 끊기지 않아야 한다.

 * 목록의 항목도 …
 * 긴 주소는 줄 끝에서 끊긴다: https://example.com/a/very/long/path/…/in.html
 * 띄어쓰기가 없는 긴 글자열도 넘치지 않는다: 가나다라마바사아자차카타파하…

[https://example.com/guide 링크의 글자도 … 길게 쓴 링크 글자입니다] 그리고 `inline_code_long_enough_…_narrow` 는 낱말 안에서 끊긴다.

[[[#!Quote
인용문 안의 글도 같은 규칙을 따른다. …
]]]

[[[#!Table tsv 1
이름	설명
가나다	표의 칸에 든 긴 문장도 …
라마바	둘째 줄의 설명은 …
]]]

[[[#!WikiSyntaxPreview
 * [[ColorCode(rgba(255, 119, 0, 0.5))]]
 * [[ColorCode(#ff7700)]]
]]]
```

The `#!WikiSyntaxPreview` is 28px wider than the narrow box whatever the stylesheet: neither of
its sides can give way there. On a page `js.js` puts a scrolling box around it, and the check
that nothing ends past the right edge leaves it out.

The 390px box is a phone's width for the breaks only. The stylesheet's phone rules follow the
window, so to see those, narrow the window.

## Running it

The Browser pane refuses `file://`, so serve the repository root:

```bash
python -m http.server 9998
```

and open `http://localhost:9998/test/manual/word-break/`. `.claude/launch.json` has this as
`static`. `?theme=dark` draws the dark theme; nothing is stored.

Headless, for the result and for a picture of it:

```bash
chrome --headless=new --window-size=1300,2100 --virtual-time-budget=8000 --dump-dom "http://localhost:9998/test/manual/word-break/index.html"
chrome --headless=new --window-size=1300,2100 --virtual-time-budget=8000 --screenshot=words.png "http://localhost:9998/test/manual/word-break/index.html"
```

Give each run a new, empty `--user-data-dir`: `http.server` sends no cache headers, and a profile
kept between runs goes on drawing yesterday's `wiki.css`.

Look at the picture too. The checks do not see a line left half empty because the next word
would not fit, which is what breaking between words costs.

## Trusting it

A harness that cannot fail is worth nothing. Before believing a `PASS`, check that it still fails
against the stylesheet it was written to catch. Any commit up to `d66f7562` has it:

```bash
d=test/manual/word-break/_old && mkdir -p $d
git show d66f7562:app/assets/wiki.css > $d/wiki.css
```

then open `index.html?css=_old/wiki.css`. It should end in `FAIL — 2 of 9`: words drawn on two
lines, one at the page's width and thirteen in the narrow box. Delete `_old` afterwards.

The other two checks fail only against a stylesheet nobody committed. To see them fail, copy
the current `wiki.css` into `_old` and, in the copy, turn every `overflow-wrap: anywhere` into
`break-word` (the bare table becomes 671px wide), or take out every `overflow-wrap: normal`
(code is drawn one letter to a line).
