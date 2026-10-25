# Graph glow harness

Draws a force-directed graph with the real `public/js/AhaWiki.GraphGlow.js` and the same call
order as the two graph templates (`graph.scala.html`, `adjacentPagesD3Canvas.scala.html`):
background, glow, links, nodes with a ring. No server, no login, no database — which matters,
because the templates need all three and a local server does not start without the cache.

What it checks, and prints as `PASS`/`FAIL` after every half second:

 * **No exception in a frame.** An undefined cluster colour used to throw halfway through
   `draw()` and leave the frame half painted. That is invisible to the eye, so it is counted.
 * **No empty frame**: one that has links to draw and never called `stroke`.

What it cannot check, and you have to look at: that the glow reads as a cloud and not a ring, that
its colour sits well on the light and the dark background (the toggle), that the nodes stay
legible on top of it, and that it breathes — about seven seconds a swell, each cluster out of
step. Try all three sizes, the stopped simulation (the glow must keep breathing with nothing
ticking), and *add a node every second* (cluster colours must not jump as the graph grows).

## Running it

The Browser pane refuses `file://`, so serve the repository root:

```bash
python -m http.server 9998
```

and open `http://localhost:9998/test/manual/graph-glow/`. `.claude/launch.json` has this as
`static`. `?script=<url>` loads another copy of the module, to check that the harness can fail.

The unit tests in `test/graph.glow.test.mjs` run in `npm test`; this is the part they cannot see.
