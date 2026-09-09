# Editor hotkey harness

Presses Alt+S, Alt+R and Alt+H in a page shaped like the edit screen — the editor's own `kbd`
hints inside `.edit`, and the navigation bar's single-key hints outside it, which carry the
same `data-hotkey-alt` attribute — and counts what `edit.hotkeyAlt.js` clicks.

It exists because the dispatcher used to query the whole document. On the real edit page that
meant Alt+S saved *and* opened the search box, Alt+R followed both `?action=rename` and the
Random link `/r`, and Alt+H, Alt+B and Alt+E — not editor hotkeys at all — followed the
History, Blame and Edit links and left the editor with the text unsaved. `edit.hotkeyAlt.js`
now looks only inside the element it listens on.

## Running it

Open `index.html` in a browser. It works straight from disk (`file://`): the script under test
is loaded by a relative path, so whatever is in the working tree is what gets tested. Over
HTTP, serve the repository root — `python -m http.server 9998`, which is also the `static`
entry in `.claude/launch.json` — and open
<http://localhost:9998/test/manual/editor-hotkeys/>. jQuery comes from the CDN the edit page
uses, so it needs the internet. It ends in `PASS` or `FAIL`.

No Play server, no database, no login — the same reason `../editor-keys/` exists.

## Trusting it

A harness that cannot fail is worth nothing. Before believing a `PASS`, check that it still
fails against the script it was written to catch. Any commit up to `ac45dd39` has it:

```bash
d=test/manual/editor-hotkeys/_old && mkdir -p $d
git show ac45dd39:public/js/Wiki/edit.hotkeyAlt.js > $d/edit.hotkeyAlt.js
```

then open `index.html?script=_old/edit.hotkeyAlt.js`. It should print three `MISMATCH` lines
and `FAIL — 3 of 3`. Delete `_old` afterwards. If it prints `PASS` against that script, the
harness is lying and the result means nothing.
