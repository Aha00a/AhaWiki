# Kanban board harness

Draws a `#!Kanban` board with the real `AhaWiki.Kanban.js`, the real Sortable and the stylesheets
a page loads, from the template that writes a board, without a server or a login: the page answers
the board's requests itself. Then it drags a card and a list with the pointer, drags files over the
board, opens a card's dialog, and checks how each looks while that is happening. Then it opens
itself in three frames as wide as phones -- 390, 360 and 320px -- where a board is something else:
one list at a time under a row of tabs, and a dialog that comes up from the bottom.

What it checks:

 * **The board.** Its lists and cards are drawn. An assignee's picture fills its circle -- on the
   board, and in the copies of a dragged card and a dragged list, which have rules of their own.
   A card shows four assignees and counts the rest. The card assigned to whoever is looking has
   a ring.
 * **A card, dragged.** What follows the pointer is a copy on `<body>`, 64% opaque, with a shadow.
   Where the card was, a faded copy of it stays. Until the card is over another place no slot is
   drawn for it, and the list is as tall as it was. Then the slot is a dashed box with nothing
   drawn in it and no shadow -- the card dragged is the one with a ring, which is a shadow. The
   cards the slot pushes aside slide to their places; the faded copy and the slot do not. Over a
   card of another list the slot goes into that list, and the drop is saved there.
 * **A list, dragged.** The same, across the board. And what is hidden in the list is hidden in
   the copy of it: the list is picked up with its «Add a Card» open, which hides the button.
 * **The pointer** is a closed hand wherever it is on the page while either is dragged: over
   another card, a button, a list's title, the board, a link in the page's text, the page.
 * **Files dragged in from outside.** A list they are over is outlined with dashes and tinted; so
   is the board when they are over none of its lists, and a card's dialog, which also says what
   dropping them does. Each is as it was when they leave.
 * **The dialog**, in a window: 720px wide and in the middle, stopping 16px short of the window's
   top and bottom and scrolling, its heading running to its edges. What it keeps hidden until it
   is asked for is hidden. Its button closes it.
 * **On a phone**: tabs and one list, nothing made draggable, the page no wider than the phone.
   The dialog is as wide as the phone, comes up from the bottom a tenth of the phone short of the
   top, and has round corners at the top only.

The result is printed under the page as `PASS`/`FAIL` lines, the phones' after the page's own, and
the last line counts them: `PASS — 86 checks`.

## Why it exists

`kanban.less` carried sixty `!important` until 2026-11-26. Taking them out meant knowing which of
them did anything, and nothing could say: the node tests run the script against a made-up DOM
with no stylesheet, and a server-side spec sees neither. This page was written first, against the
stylesheet as it was, and then run against that stylesheet with every `!important` deleted. Fifteen
of its checks fail there, and they are the ways a board looks wrong without them:

 * An assignee's picture is 30 by 16 in a circle of 28 by 28, on the board and in the copy of a
   dragged list: a card's rule for the images in it sizes and frames it as an attachment.
 * The copy that follows the pointer is 80% opaque, which is what Sortable writes on it.
 * The pointer is a pointing hand over cards, buttons and titles while something is dragged.
 * The faded copy and the slot slide about with the cards.
 * The slot of a card assigned to whoever is looking keeps the ring.
 * A list dragged with its «Add a Card» open shows the button again in the copy.

Fifteen of the sixty were doing that. Of the rest, one repeated another, thirty-one had nothing
to outrank, and thirteen sat in rules that match no element. What became of each is on the wiki
page `Dev Less`, «!important 는 선택자로 이길 수 없는 것에만», and `test/less.important.test.mjs`
lists the seven that stayed, with what each outranks.

## The markup

Nothing here is a copy of the template. The page fetches `InterpreterKanban.scala.html` and writes
what Twirl would: the `@import` and the parameters dropped, each `@name` filled in. When the
template comes to use more than that, an `@` is left over and the first check fails.

What is written by hand is what goes around a board on a page: the page's own
`div.Wiki > div.wikiContent > div.limitWidth`, the `.revision` a page's header has and the board
reads before it saves, and the `InterpreterRenderMetaWrapper` a page writes around a block with
the lines of the page's text the block is on (`ExtractConvertInjectInterpreter.scala`). Nothing
checks that these match, so a change to them has to be made here too.

The stylesheets are the eight a page loads, in the order `_baseSkeleton.scala.html` loads them.
The order matters: of two rules as specific as each other the later wins, and three stylesheets
come after the board's.

## The server

The board asks its server for the page's revision and a token, asks it to render what people
wrote, and saves to it. The page answers all of that itself, by standing in for `fetch` on paths
under `/api/` and `/w/`. A `[User:Name]` comes back with a picture for two of the six people on
the board, which is where an assignee's picture comes from on a real page too. A save is answered
with success, and the checks that a drop «is saved» mean that the board sent one.

## The pointer

Sortable is told to move a copy of what is dragged itself (`forceFallback`), so it listens to
pointer events, and the page sends them: `pointerdown` on what is under the point, `pointermove`
and `pointerup` on the document. Three things about that were found by getting them wrong:

 * **Sortable looks under the pointer every 50ms, not when it moves.** The page waits for the
   board to change, with a `MutationObserver`, and measures as soon as it has, before anything is
   painted. That is the first frame of a move, and «slides» means a transform is in effect on the
   element then.
 * **Dead centre of a card is in neither half of it.** Coming from another list, Sortable asks
   which half of the card the pointer is in, and at the exact middle it does nothing. A hand is
   never there; a script that asks for an element's centre always is. The page goes to a
   quarter of the way down.
 * **No click follows a drop.** A browser sends the click after a drag to what the button went
   down on, and the board has drawn its lists again by then, so that element is gone. A click sent
   to whatever is under the pointer lands on the card just dropped and opens its dialog -- which
   then covers the board, and the next drag picks up nothing.

A mouse works as well: open the page and drag.

## What it does not see

 * **Hover.** A script's events do not hover. The rules for a card under the pointer are not
   checked, the slot's and the faded copy's among them.
 * **Whether it looks right.** The copy's blur, the colours, how the slide feels. Look.
 * **Touch.** On a phone the script does not make anything draggable, and the page checks that
   it does not. Whether a finger scrolls a list is not something it can press.
 * **Every cursor rule there is.** The pointer is asked about in six places. A rule elsewhere
   that outranks the closed hand would be found by dragging over what it is for.
 * **The server.** Whatever a real one would refuse.

## Seeing the checks fail

```bash
mkdir test/manual/kanban-board/_old
git show d030da95:app/assets/kanban.css | sed 's/ !important//g' > test/manual/kanban-board/_old/stripped.css
npm run harnesses -- "kanban-board?css=_old/stripped.css" --lines
```

`FAIL — 15 of 86`, the six above and their repeats on the phones. `d030da95` is the last commit
with the sixty; with its stylesheet as it is (`git show d030da95:app/assets/kanban.css`, saved
the same way) every check passes, because the change was meant to draw what was drawn before.
`?script=` and `?template=` load another `AhaWiki.Kanban.js` and another template the same way,
and `?theme=dark` draws the dark theme. Delete `_old` afterwards.

## A picture of a drag half way

A drag is over in a second and leaves nothing to look at. A script driving the page -- over the
DevTools protocol, with `Page.addScriptToEvaluateOnNewDocument` -- can define `window.atStage`
before the page loads. The page calls it with a name and the document at each stage and waits for
what it returns: `the board`, `a card picked up`, `a card over another place`, `a card over
another list`, `a card dropped`, `a list picked up`, `a list over another place`, `a list
dropped`, `files over a list`, `files over the dialog`, `the dialog`, and in each frame
`390px: the board` and `390px: the dialog`. The slides are over by the time it is called.

That is how the change of 2026-11-26 was compared with what came before it. At each stage the
old stylesheet was swapped for the new one in the open page, and every computed property of
every element and of its `::before` and `::after` compared -- and again with `:hover` forced,
through the same protocol, on everything that has a hover rule. One thing differed, in both
themes: the corners of an assignee's picture on the board and in the copy of a dragged list, 6px
before and its circle's after. That is what its rule had said all along, and lost to a card's
rule for images. The circle cuts the picture either way: with those corners put back to 6px the
two stylesheets draw every stage the same to the pixel, and with them as they are now the
pictures differ by 4 in 255 at most, in a few hundred pixels at the edges of things round.

Two things to know before comparing pictures this way. Take both of the same moment, in the one
open page: of two runs, the dragged copy lands a pixel off in one. And take the first stylesheet's
picture again after the second's, and compare with that one: what a swap makes the page draw
again comes out a shade different at a few corners -- one pixel of the slot's dashed border by a
fifth -- whichever stylesheet it ends on. Swapping a stylesheet for a copy of itself draws
nothing again, and those pictures are the same to the pixel.
