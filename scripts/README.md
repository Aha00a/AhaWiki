# Scripts

`lib/ahawiki.net.mjs` holds what the two wiki scripts both need — where the wiki is, where the
local page copies live, and how a page name becomes a filename.

## Front-end admin build

`app/assets/js/admin.jsx` is bundled to `public/js/babel/admin.js`.

### One-time build

```bash
npm install
npm run admin:build
```

### Auto rebuild (watch mode)

```bash
npm run admin:watch
```

This is a Node.js + esbuild flow (`scripts/admin.mjs`); Bun and TypeScript are not required.

## External asset check

`check-cdn-assets.mjs` fetches every external stylesheet, script, and importmap entry the
templates point at, and fails if any does not answer 200.

```bash
npm run check:cdn
```

Run it after changing a pinned version. It is not part of `npm test` — it needs the network.

A 404 stylesheet is silent, which is why this exists: the admin UI pointed at
`mantine-datatable@9.2.2/styles.css`, the package ships that file under `dist/`, and every
admin table rendered unstyled for as long as the link was wrong. The most visible symptom was
the "no records" overlay drawn across real rows, because the rule that hides it lives in the
file that never arrived.

URLs still holding a template placeholder (`https://${host}/...`) are skipped — the server
fills those in and there is nothing to fetch.

## Wiki page sync

`sync.ahawiki.net.mjs` compares the **committed** copies under `docs/ahawiki.net/` against the
live wiki and uploads the ones where only the local side moved. It implements the procedure in
`AGENTS.md` under **AhaWikiDoc Sync**, which is where the rules and the reasoning live.

Set `AHAWIKI_API_KEY` first. Never write a key into this repository.

Report what is out of step, changing nothing:

```bash
npm run sync:ahawiki.net
```

Upload the pages the local side is ahead on. The comment is required — it lands in the page
history, so it should say what actually changed:

```bash
npm run sync:ahawiki.net -- --apply --comment="Dev Testing: record what the H2 schema misses"
```

Restrict to named pages by repeating `--only`, and add `--minor` for an edit not worth
announcing:

```bash
npm run sync:ahawiki.net -- --apply --comment="fix a stale path" --only="Dev Testing" --minor
```

The classification is what decides whether a live page gets overwritten, so it is covered by
`test/ahawiki.sync.test.mjs` (part of `npm test`) against this repository's real git history.

Three outcomes need a person rather than a flag:

* **diverged** — the wiki holds content that was never committed here, so someone edited the
  page in the browser. Read it, merge it into the local file, commit, then sync. The script
  will not overwrite it.
* **local only** — a file that is not a page on the wiki yet.
* **wiki only** — a page with no local copy. `npm run download:ahawiki.net` pulls copies down.

## Wiki page download

`download.ahawiki.net.mjs` replaces everything under `docs/ahawiki.net/` with a fresh copy of
every page, plus a `manifest.json` describing the download.

```bash
npm run download:ahawiki.net
```

It scrapes `PageList` and reads pages with `?action=raw`, so it needs no key and sees only what
a logged-out visitor sees. It is a bulk refresh, not a sync — it overwrites local edits without
looking. Use the sync script for anything else.

## `writer` → `author` (done 2026-09-04)

`writer-to-author.mjs` was a one-off. Reviewing the hand-written Schema blocks against the
shipped schema.org vocabulary found `writer` on 137 pages — 112 films, 23 series, 2 comics — and
schema.org has no such property, nor a screenwriter one. `author` is what a film's writer gets.

It is kept because it says what was changed and how to check it, and because the same shape of
mistake will happen again: a property that reads like schema.org and is not.

```bash
WRITER_PAGES_FILE=pages.tsv node scripts/writer-to-author.mjs                    # report only
WRITER_PAGES_FILE=pages.tsv node scripts/writer-to-author.mjs --apply --comment="..."
```

`WRITER_PAGES_FILE` is a tsv of `site<TAB>page`, from
`SELECT DISTINCT site, page FROM CalculatedSchemaOrg WHERE prop = 'writer'`.

Two things it is careful about, both of which it got wrong first:

- It only rewrites a bare `writer<TAB>` field, never a `# Writer` comment holding the original.
- A page can hold several Schema blocks, so `author` and `writer` clash only inside **one** block.
  구르미 그린 달빛 types the novel as `Book` and the drama as `TVSeries`, each with its own person;
  comparing the whole page called that a clash. `test/writer-to-author.test.mjs` pins both.

Verify by reading the pages back rather than by querying `CalculatedSchemaOrg` — that table is
derived and lags behind the edit. After the run it still reported 121 pages holding `writer`
while every page had already been rewritten.

## Paired software classes (done 2026-09-05)

`add-paired-class.mjs` adds `SoftwareApplication` to a block typed `SoftwareSourceCode` that
carries an application property (`applicationCategory`, `softwareVersion`, `operatingSystem`),
and the reverse (`codeRepository`, `programmingLanguage`, `runtimePlatform`). A library is
source and a thing you use, and six pages already said so by naming both — the interpreter only
started reading the second name in `5fb88dd9`. This makes the rest agree.

```bash
PAIRED_PAGES_FILE=pages.tsv node scripts/add-paired-class.mjs                    # report only
PAIRED_PAGES_FILE=pages.tsv node scripts/add-paired-class.mjs --apply --comment="..."
```

It only ever **adds**, only the paired class, and only when that makes every property in the
block fit the vocabulary. A block it cannot settle that way is reported as `unresolved` and left
alone — `industry` on a Corporation, `duration` on a TVSeries, and anything outside the software
pair fall there.

**A copied line is not a reason to widen.** Graphviz and Homebrew give the program its own
`SoftwareApplication` block and the codebase a `SoftwareSourceCode` one, then repeat
`applicationCategory` in the second, identical down to the value. Adding the class there would
make the page declare the same application twice; the copy is what is wrong. Those come back as
`duplicate-of-sibling`, naming the line, and a person deletes it. Only a byte-identical line in a
sibling block that already declares the class counts — two different values are two facts, and
the block making the second one does need the class that defines the property.

The first run missed this and would have widened both. It was caught by three reviewers reading
each page independently, one per lens: what the subject is, what the author meant, what
schema.org practice is. Two lenses passed both pages; the one reading intent saw the sibling
block. That is the case for reading the page rather than only the property list.

`logo` is ignored when deciding. Its domain is Organization/Brand/Product, so it fits no software
class, but `InterpreterSchema.imageKeys` draws it as a picture on any block — a display
convention, not a claim about the type. Counting it held twelve library pages back for nothing.

The decision is pinned in `test/add-paired-class.test.mjs` against the shipped vocabulary.

## Auditing every page on every site

`audit.wiki.mjs` reports pages that render an error, or render something other than what was
written: a macro or block naming something the app does not register, a link written
`[Page Name]` whose whole text names a page — which renders the words the author meant and goes to
the first one — and a link that misses an existing page by case or spacing.

That last one needs the whole site to see. `Page.name` is `utf8mb4_bin`, so `[PHP]` and the page
`Php` are two different names and always will be; the link renders as an invitation to write a page
that is already written. A red link is normal on a wiki, which is why this reports only the ones
that name a real page under another casing — 8 out of 6,500. What counts as missing is
`AhaMarkLink.toHtmlString`, and the exclusions in the script are its exclusions.

The tests under `test/` check the 116 pages committed here. This checks all of them, including the
sites whose pages exist only on the wiki, so it needs a dump of the database rather than the API —
the API returns only what the key may read.

```bash
ssh <host> 'mysql --defaults-file=~/.my.rds.cnf -B -N -e "
  SELECT p.site, TO_BASE64(p.name), TO_BASE64(p.content)
  FROM Page p JOIN (SELECT site, name, MAX(revision) r FROM Page GROUP BY site, name) m
    ON m.site=p.site AND m.name=p.name AND m.r=p.revision"' > dump.tsv
node scripts/audit.wiki.mjs dump.tsv
```

**Match what the renderer sees, or the report is noise.** Backticks and `[[[blocks]]]` come out of
the text before macros and links are read, so a scan of the raw source calls every documented
example a broken one. The first run reported 236 suspect fragments and nine unregistered macros;
after mirroring that order, four and then one. The header lists the three false-positive classes
and why each happened, including one that came from deriving the macro list with a regex that
required a trailing comma and so missed the last entry.

The lists of macros and interpreters are written out rather than derived, because deriving them is
what produced two of those false positives. `test/ahawiki.docs-links.test.mjs` guards the link
shape for the committed pages on every run; this script is for the rest, run by hand.

**A count that surprises you is a claim about your own script first.** A throwaway written with a
shell heredoc lost a backslash, so `/\\n/g` became `/\n/g`, stripped none of the base64 wrap
markers, and reported 4,109 of 4,616 rows as corrupt. The dump was clean. Write scripts to a file
with an editor, not through the shell.

## Links that miss a page by case (done 2026-09-09)

`fix-case-miss-links.mjs` was a one-off for the eight the audit found: `[PHP]`, `[ASP]`,
`[ASP.NET]` and `[ffmpeg]` on four aha00a.com pages, where the pages are `Php`, `Asp`, `Asp.Net`
and `FFmpeg`.

```bash
node scripts/fix-case-miss-links.mjs                          # report only
node scripts/fix-case-miss-links.mjs --apply --comment="..."  # rewrite
node scripts/fix-case-miss-links.mjs --apply --comment="..." --only="Waveform"
```

It writes `[Php|PHP]`, not `[Php]`. The page names are the odd spelling here — PHP, ASP.NET and
ffmpeg are what those things are called — and one of the four pages is a CV, where changing
`ASP.NET` to `Asp.Net` would be a change to how someone presents themselves rather than a repair.
The pipe form sends the link to the page that exists and leaves the reader seeing what was written.
Renaming the pages instead would fix more links at once, but a rename is not a script's decision.

It rewrites **by offset**, not by replacing text: `[PHP]` also appears in prose about the markup,
and a page that documents the syntax shows the very shape being rewritten. The mask blanks blocks,
backticks and macros to the same length so the offsets stay true — `test/fix-case-miss-links.test.mjs`
pins that, and pins the four kinds of `[PHP]` that must be left alone.

Verified by reading each page back afterwards, rendered: `<a href="/w/FFmpeg">ffmpeg</a>`, with no
`missing` class and the text unchanged.
