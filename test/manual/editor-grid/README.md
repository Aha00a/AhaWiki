# Editor grid harness

Opens the real editor scripts on a `#!Table` block and then a `#!gantt` block whose cells hold
quotes, edits one other cell, and checks that every cell was shown whole and written back as it
was.

It exists because both grids built each `<input value="...">` with jQuery's `.text().html()`,
which escapes `<`, `>` and `&` but not `"`. A cell holding `27" monitor` showed as `27`, a cell
starting with a quote (`["Name: X"]`, a quoted csv field, a Gantt task `"Phase 1" kickoff`) showed
as nothing, and the next edit of any cell wrote those losses into the page -- silently, on the
block a section's Edit link opens straight into the grid. Fixed on 2026-11-21
(`escapeAttribute` in `public/js/Wiki/edit.page.js`). With the old script this page prints five
FAILs, with the text each cell came out as.

## Running it

```bash
python -m http.server 9998
```

from the repository root, then open
<http://localhost:9998/test/manual/editor-grid/>. It runs by itself and ends in `PASS` or `FAIL`.
Same shape as `../editor-keys/`: the same vendored files, no server, no login.

Two things on the page are not failures: an alert that the attachment list could not be read, and
an exception from the Wikipedia helper. The edit page asks the server for the first and loads the
second from a script this page does not include; neither touches the grids. Driven headless, accept
the alert or the page stops there.
