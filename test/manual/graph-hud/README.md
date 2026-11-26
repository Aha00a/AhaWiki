# Graph buttons harness

Draws the buttons that lie over a force graph -- Control and Metrics at its top corners, the bar
along its bottom -- with the real `adjacent.css`, `AhaWiki.D3CanvasControls.js` and, for the
Adjacent Pages graphs, the real `AhaWiki.AdjacentPagesGraph.js`, without a server or a login. Then
it measures them and clicks them.

A graph is as wide as what holds it, so the page draws one at each width a graph has: a page's, a
column's, a table cell's in a help page. Then it opens itself in three frames as wide as phones --
390, 360 and 320px -- and checks all of it again there. A stylesheet asks about the window, and a
frame is a window: in a phone's the Adjacent Pages graph is square, and a `#!Graph`, which is 16:9
wherever it is, is low as well as narrow.

For each graph:

 * **Closed**, which is how a graph starts, nothing reaches out of the graph and no panel lies on
   another. On a graph 700px wide or wider the buttons cover less than 15% of it.
 * **Words or icons.** From 520px across the buttons have their words; below that, icons alone.
   The number is read from the stylesheet, and two graphs a pixel apart stand on either side of it.
 * **The bar** is one line, with every button in it. The legend is whole beside them, or, where it
   does not fit, not shown at all. A graph wide enough for words has its legend.
 * **Open**, a panel is in the graph, off the bar, and nothing of it is cut off: in a graph too low
   for it, its rows scroll. A wide graph has both panels open at once. A narrow one shows one at a
   time -- while one is open the other's button is not there to press -- and if both are left open,
   as leaving fullscreen can leave them, it shows Control.
 * **Closed again** from the same buttons, with both buttons back.
 * **The script**, for Adjacent Pages: it writes the Fullscreen button's word where the stylesheet
   can hide it, a slider in Control reaches it, and Metrics counts the five pages it drew. The page
   answers `/api/links` itself.

A graph smaller than 262px across or 160px down is drawn and not checked: it is too small for its
buttons, and the wiki page `MacroAdjacentPages` says so. Each is a `NOTE` line, and the last line
counts them. The `#!Graph` in a help page's table is one on every phone.

The result is printed under the page as `PASS`/`FAIL` lines, the phones' after the page's own, and
the last line counts them: `PASS — 263 checks, and 3 too small to check`.

## Why it exists

The buttons were folded on 2026-11-26 and checked on three live pages at three window widths, by
scripts that went away with the session. Writing this page the same day found what those had
missed, each on a phone:

 * **The legend ran out of the bar.** By 11px on a 360px phone, the commonest width there is, and by
   51px on a 320px one. The check had been at 390px, where it fits with 20px to spare.
 * **An open panel ran out of a low graph.** A `#!Graph` on a 390px phone is 206px high and Control
   needs 226px: it hung 20px below the graph and lay on the bar. Adjacent Pages is square on a
   phone and had room, and that was the one looked at.
 * **An open panel reached the other's button** in a graph 266px wide, which is Adjacent Pages on a
   320px phone.

No server-side spec can see any of them.

## The markup

Nothing here is a copy of the template. The page fetches `_graphD3CanvasLayout.scala.html`, and
the two templates that call it -- `adjacentPagesD3CanvasHost.scala.html` and `graph.scala.html` --
for the arguments each passes, and writes what Twirl would: the signature and the `@* comments *@`
dropped, each `@if(flag){…}` kept or dropped, `@name` filled in. That is all the template uses.
When it comes to use more, an `@` is left over and the check `this page can draw all of the
template` fails, naming what it did not know.

What is written by hand is what goes around a graph on a page -- `div.Macro.BlockMacro.AdjacentPages`,
a block's `InterpreterRenderMetaWrapper`, the table of a `#!WikiSyntaxPreview` -- and the page's own
`div.Wiki > div.wikiContent > div.limitWidth`. They matter: a block macro is indented 30px, which
is why Adjacent Pages is 306px wide on a 360px phone and not 336px. Nothing checks that these match
the server.

A `#!Graph`'s script is written into its page by `graph.scala.html` and cannot be loaded from a
file. Its buttons are drawn and clicked all the same; what its sliders do is not checked here.

## Running it

The Browser pane refuses `file://`, so serve the repository root:

```bash
python -m http.server 9998
```

and open `http://localhost:9998/test/manual/graph-hud/`. `.claude/launch.json` has this as
`static`. Headless, for the result and for a picture:

```bash
chrome --headless=new --window-size=1100,3000 --virtual-time-budget=60000 --dump-dom "http://localhost:9998/test/manual/graph-hud/index.html"
chrome --headless=new --window-size=1100,3000 --virtual-time-budget=60000 --screenshot=hud.png "http://localhost:9998/test/manual/graph-hud/index.html?open=control"
```

Give each run a new, empty `--user-data-dir`: `http.server` sends no cache headers, and a profile
kept between runs goes on drawing yesterday's `adjacent.css`.

 * `?theme=dark` draws the dark theme. Nothing is stored.
 * `?open=control`, `?open=metrics` and `?open=both` leave those panels open after the checks, which
   close them. Look at the picture too, not only at the result: the checks do not see whether a
   panel reads well, or what the scrollbar of a low graph's panel lies on.
 * The window does not have to be a phone's for the phones: the frames are. A headless Chrome on
   Windows will not make a window narrower than 500px whatever `--window-size` says, which is why
   the page does not depend on one.

Two things that look like faults and are not. The console shows a 404 for
`/assets/img/attachmentFallback.svg`, the picture the script keeps for a thumbnail that fails to
load; nothing here has a thumbnail. And `http.server` now and then refuses one request of the
dozen a page makes at once. A page short of a stylesheet measures as nonsense, so it says `not
every file loaded` instead and is to be reloaded; a frame that says so is opened again by the page.

## Trusting it

A harness that cannot fail is worth nothing. Before believing a `PASS`, check that it still fails
against the files it was written to catch. `?css=`, `?controls=`, `?graph=` and `?template=` each
load another copy of one file, and the frames are opened with the same ones.

The state the three faults above were in is commit `d66f7562`:

```bash
d=test/manual/graph-hud/_old && mkdir -p $d
git show d66f7562:app/assets/adjacent.css > $d/adjacent.css
git show d66f7562:public/js/AhaWiki.D3CanvasControls.js > $d/AhaWiki.D3CanvasControls.js
```

then open `index.html?css=_old/adjacent.css&controls=_old/AhaWiki.D3CanvasControls.js`. It should
end in `FAIL — 54 of 263`, among them `on a 360px phone, Adjacent Pages: the legend is whole in the
bar, beside the buttons: it runs 11px past the bar` and `on a 390px phone, #!Graph: with Control
open, nothing reaches out of the graph: topLeft by 20px`. Thirty of the 54 are the three checks of
one panel at a time, which that stylesheet did not do: its script closed the other panel instead.

The state before the buttons were folded at all is commit `869053f7`, and takes all four files:

```bash
for f in app/assets/adjacent.css public/js/AhaWiki.D3CanvasControls.js public/js/AhaWiki.AdjacentPagesGraph.js app/views/Wiki/_graphD3CanvasLayout.scala.html; do
  git show 869053f7:$f > $d/$(basename $f)
done
```

and `index.html?css=_old/adjacent.css&controls=_old/AhaWiki.D3CanvasControls.js&graph=_old/AhaWiki.AdjacentPagesGraph.js&template=_old/_graphD3CanvasLayout.scala.html`
should end in `FAIL — 30 of 53`: four panels always open, covering 23% of a graph 960px wide and
37% of one 700px wide, on each other in every narrow graph. Delete `_old` afterwards.
