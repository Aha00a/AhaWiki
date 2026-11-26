# #!Paper contract layout harness

Draws a `#!Paper contract` document of three pages with the real `less.css`, `wiki.css` and
`interpreter.css`, without a server or a login, and measures what the contract layout is for:

 * **Two columns.** The `==` titles and the labels start at the left edge of the page, and the
   paragraphs and lists under them start 60mm in.
 * **A label is the body's size**, whichever of `===` to `======` it was written with.
 * **A label of two lines is aligned left**, not spread to both edges.
 * **The title column.** A title of short words is 50mm wide. A title whose longest word is
   wider than that is as wide as the word, up to 57mm, and the word stays whole. A word wider
   than 57mm is broken. No title loses the end of a word -- a heading hides what overflows it --
   and none reaches the body.
 * **No word of the body or of a label is drawn on two lines.**
 * **The header and the footer:** each of the three places on one line, the middle one in the
   middle of the page, the right one ending at the right edge.
 * **All of it above the footer.**

The page exists because, until 2026-11-26, most of those were not so, and no server-side spec
can see any of them. A Paper's `.pageContent` calls `.commonAhaWiki()`, whose heading rules are
one class more specific than the contract's were, so the labels kept that mixin's indent and
size; the page's `word-break: break-all` cut words anywhere; a title word wider than 50mm was
cut off; and the header's three places were equal thirds. The wiki page `Dev Less` has the
account.

The result is printed under the page as `PASS`/`FAIL` lines, and the last line counts them.

## The markup

The first page is what the server answered for the text below, with the `data-edit-link` and
`data-edit-title` attributes left out. The second and third pages are a title and a paragraph
each, written by hand in the same shape: `== SERVICE CONTRACT 용역 계약서`, whose words all fit
in 50mm, and `== INTERNATIONALIZATION 국제화 계획서`, whose first word is wider than 57mm. When
`InterpreterPaper` or `InterpreterWiki` changes what it writes, change the page with it. Nothing
checks that they match.

```text
#!Paper contract S-MM-20260101-01
#!var topLeft MINUTES OF SHAREHOLDERS' MEETING
#!var topCenter 대외비
#!var bottomLeft 견본 주식회사
#!var bottomCenter {{pageNo}} / {{pageTotal}}
#!var bottomRight 견본
== MINUTES OF SHAREHOLDERS' MEETING 임시주주총회 의사록
2026년 01월 01일 10시 00분 본점 회의실에서 임시주주총회를 개최하다.

 * 주주총수: 3명, 총발행주식수: 10,000주
 * 참석주주수: 3명, 참석주식수: 10,000주 (100%)

의장인 대표이사는 … 심의를 구하다.

====== 제1호 의안
'''임원 보수 한도 승인의 건'''

의장은 … 50,000,000원으로 한다.
 1. 이사의 보수한도 : 100,000,000원
 1. 감사의 보수한도 : 50,000,000원
  a. 상여금은 … 지급한다.

====== 제2호 의안 정관 일부 변경 및 본점 이전 승인의 건
의장은 … 승인하다.

=== 폐회
이상으로서 … 폐회를 선언하다.
```

The long word in the title -- `SHAREHOLDERS'` is 55.8mm wide at a title's size -- the label
that takes two lines, the `===` label among `======` ones and the long text in the header's
left place are each there for one of the checks.

## Running it

The Browser pane refuses `file://`, so serve the repository root:

```bash
python -m http.server 9998
```

and open `http://localhost:9998/test/manual/paper-contract/`. `.claude/launch.json` has this as
`static`. `?theme=dark` draws the dark theme; nothing is stored.

Headless, for the result and for a picture of it:

```bash
chrome --headless=new --window-size=1000,3900 --virtual-time-budget=8000 --dump-dom "http://localhost:9998/test/manual/paper-contract/index.html"
chrome --headless=new --window-size=1000,3900 --virtual-time-budget=8000 --screenshot=paper.png "http://localhost:9998/test/manual/paper-contract/index.html"
```

Give each run a new, empty `--user-data-dir`: `http.server` sends no cache headers, and a profile
kept between runs goes on drawing yesterday's `wiki.css`.

Look at the picture too, not only at the result. The checks do not see whether a line of
justified text has been spread too thin, or where a long word was broken.

## Trusting it

A harness that cannot fail is worth nothing. Before believing a `PASS`, check that it still fails
against the stylesheet it was written to catch. Any commit up to `7fe5ba83` has it:

```bash
d=test/manual/paper-contract/_old && mkdir -p $d
git show 7fe5ba83:app/assets/wiki.css > $d/wiki.css
```

then open `index.html?css=_old/wiki.css`. It should end in `FAIL — 8 of 17`: the titles and
labels off the left edge, a `===` label at 22.75px, a two-line label justified, 22px and 134px of
two titles cut off, neither long title word handled, words drawn on two lines, and the header's
left text on two lines. Delete `_old` afterwards.

The stylesheet of `d66f7562`, the first of that day's two changes, fails 2 of 17: it broke a
title word wider than 50mm where the column ended -- «SHAREHOLDE / RS'» -- instead of letting
the column grow to 57mm for it.
