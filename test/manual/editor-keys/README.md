# Editor key harness

Presses Tab and Enter in a real CodeMirror, with the real editor scripts loaded, and prints what
the line came out as.

It exists because the keys have gone wrong twice in a way no unit test saw: two handlers both
acted on one key, and the second one edited coordinates the first had already moved.

```
 * [JIH        with JIH0 offered, then Tab    ->  "  * JIH0H]"
 * [JIH        with JIH0 offered, then Enter  ->  " * [JIH0\n * ]"
```

## Running it

```bash
python -m http.server 9998
```

from the repository root, then open
<http://localhost:9998/test/manual/editor-keys/>. It runs by itself and ends in `PASS` or
`FAIL`. Needs the internet — jQuery and CodeMirror come from the same CDNs the edit page uses.

Nothing else. No Play server, no database, no Redis, and no login — which is the point, because
the edit page needs one and that is what made this hard to check. `public/js/` is read straight
off disk, so whatever is in the working tree is what gets tested.

## What is real and what is not

Real: `AhaWiki.Editor.js`, `AhaWiki.CodeMirror.AhaMark.js`, `Wiki/edit.page.js`, CodeMirror, and
the `keydown` events. Faked: the page-name list, because fetching the real one needs a login, and
the surrounding page elements that `edit.page.js` measures on load.

## Trusting it

A harness that cannot fail is worth nothing. Before believing a `PASS`, check that it still
reproduces the bug it was built for:

```bash
mkdir -p /tmp/old && git show 461313f7^ -- public/js/AhaWiki.Editor.js > /tmp/old/AhaWiki.Editor.js
```

Point the two script tags at copies from a commit before the fix and reload. It should print the
two lines at the top of this file. If it prints `PASS` against the broken scripts, the harness is
lying and the result means nothing.

`461313f7` is the fix; `e7d28758` is the last commit before any of it.
