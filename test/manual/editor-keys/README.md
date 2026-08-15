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
reproduces the bug it was built for. Take both scripts from **`e7d28758`**, the last commit
before any of the fix:

```bash
d=test/manual/editor-keys/_old && mkdir -p $d
for f in AhaWiki.Editor.js AhaWiki.CodeMirror.AhaMark.js; do
  git show e7d28758:public/js/$f > $d/$f
done
```

Point those two script tags at `./_old/…`, reload, and it should say:

```
--- Enter | popup visible "JIH0"
    after  " * [JIH0\n * ]" ch=3  MISMATCH
--- Tab | popup visible "JIH0"
    after  "  * [JIH0]" ch=9  MISMATCH
FAIL — 2 of 2
```

Then `git checkout` the harness and delete `_old`. If it prints `PASS` against those scripts,
the harness is lying and the result means nothing.

**Use `e7d28758`, not `461313f7^`.** The fix is two commits — `f688ca83` stops Tab indenting
the line, `461313f7` labels the popup's keys — so `461313f7^` already contains the part that
matters and passes. This paragraph said to use it until 2026-08-16, when following its own
instructions produced a `PASS` and made a working harness look like a broken one. A check
whose failure mode is "everything looks fine" is the kind this file exists to warn about.
