# #!Table numeric columns and data bars harness

Draws the tables `InterpreterTable` writes, with the real tablesorter and its filter widget and
the real `less.css`, `wiki.css` and `interpreter.css`, without a server. It checks what was
argued from tablesorter's minified source when the numeric columns went in, by running it:

 * **Two header rows:** the `sorter-digit` on the header picks the parser, even when the first
   body cell is `₩1,200`, which tablesorter would otherwise read as text.
 * **Placeholders sort last**, ascending and descending. The empty `data-text` on `-`, `n/a`,
   `#N/A` and an empty cell is what does it.
 * **Spellings sort by value:** units, accounting parentheses and currency marks.
 * **The filter** finds `65,4` in `65,438,723` and does not find `n/a`. That is the price of the
   empty `data-text`, and why number cells do not carry one.
 * **The look that can be measured:** numeric headers and cells right-aligned, an axis line only
   in the column with both signs, a bar in each barred cell and none in a column of zeros.
 * **`heat=`:** the colour's opacity follows `--heat` on every row -- the even rows' background
   does not cover it -- none on a placeholder, a bar and a colour in one cell, and the text on the
   strongest cell at 4.5:1 or better in the theme drawn.
 * **`compact`:** closer cells, type at 0.92, a thinner bar inset -- while the header keeps its
   room for the sort arrow and the filter row its own padding. Those two are what setting the
   padding with a selector instead of `--table-cell-padding` would take away.

That every number spelling reads to tablesorter as the value the server gives it is
`test/table.number-contract.test.mjs`, under `npm test`; this page is for what needs a browser.

The tables are the markup `InterpreterBlockUnit` asserts, with more rows. When
`InterpreterTable` changes what it writes, change them here too. Nothing checks that they match.

The result is printed on the page as `PASS`/`FAIL` lines.

## Running it

The Browser pane refuses `file://`, so serve the repository root:

```bash
python -m http.server 9998
```

and open `http://localhost:9998/test/manual/table-data-bars/`. `.claude/launch.json` has this
as `static`. `?theme=dark` draws the dark theme; nothing is stored.

`?strip` takes `sorter-digit` off the headers and `data-text` off the placeholders before
tablesorter starts. The parser, sort, placeholder and filter checks then fail, which shows they
test something. The look checks do not depend on either, and keep passing.

Look at the `bar=all` and `heat=` tables in both themes, not only at the result. The checks do not see
whether a bar is too faint, or whether the axis line is visible against it.
