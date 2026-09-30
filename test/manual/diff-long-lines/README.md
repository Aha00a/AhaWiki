# Diff long-line harness

Draws a diff with the real `public/js/Wiki/diff.js` and `app/assets/diff.css`, without a server
or a login, and checks what `diff.less` and `diff.js` are there for: pages are written one
paragraph per line, so a changed line is usually wider than the screen.

 * **No column scrolls sideways**, in either layout — long lines wrap.
 * **Side by side, the rows stay in line.** The two halves are separate tables; a line that
   wraps on one side only would push every row below it out of step. `diff.js` evens them out.

The result is printed on the page as `PASS`/`FAIL` lines, and again after each window resize.

## Running it

The Browser pane refuses `file://`, so serve the repository root:

```bash
python -m http.server 9998
```

and open `http://localhost:9998/test/manual/diff-long-lines/`. `.claude/launch.json` has this
as `static`.

The browser caches `diff.css` and `diff.js`; after changing either, reload without the cache.

Look at the page too, not only at the result: the checks do not see a marked change landing on
the wrong characters, or text sitting away from its line number.
