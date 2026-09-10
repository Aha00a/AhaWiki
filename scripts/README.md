# Scripts

`lib/ahawiki.net.mjs` holds what the two wiki scripts both need — where the wiki is, where the
local page copies live, and how a page name becomes a filename.

`lib/ahamark.mjs` holds what every script reading AhaMark links needs: the link pattern, and the
mask for what the renderer takes out of the text before it looks for one. Both had been copied
into four files, and the copies had drifted — three masked macros with `\[\[[^\]]*\]\]`, which
stops at the first `]`, so `[[Include(["Aws EC2"])]]` was not masked at all and the link inside
it counted as real. A test caught it. Anything in that file is a claim about
`app/logics/wikis/`; change it only alongside that code.

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

**Two page names that differ only by case are one file here, and neither is written.** `Page.name`
is `utf8mb4_bin`, so the wiki can hold `TODO NewUserFlow` and `ToDo NewUserFlow` at once; Windows
and macOS cannot hold both files. This script empties the directory and writes every page
concurrently, so before the guard the two raced for one file, the loser was gone, and the manifest
still listed both as written — and *which* one lost depended on timing. It happened on 2026-09-09,
minutes after a rename created the second name. The colliding names are now named on the console
and in the manifest under `notWrittenFileNameCollision`, and `npm run sync:ahawiki.net` reports
them as **cannot be mirrored** rather than as diverged or wiki-only — both of which read as "run
the download", which is the thing that loses the page.

Only the whole group is skipped in the download; the sync is finer, and still reports whichever
of the pair the file actually holds as a normal page.

No page collides today — the four that did were deleted, see the ToDo section below — so both
guards are quiet. They are here for the next rename, not for a state anyone is living with.

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

**What it finds on another user's site is reported, not repaired.** Repairs are made on the
owner's own sites, aha00a.com and ahawiki.net. The owner decided that on 2026-09-10, when the
sweep turned up a redirect pointing the other way round on one site, a bare `TODO` page on
another and an empty page on a third: each of those wikis keeps its own conventions, and a fix
by this repository's standard would be an edit to someone else's writing. Which site number is
whose is in the script's `siteHost` map.

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

## Tidying aha00a.com after the audit (done 2026-09-10)

The owner's decisions on what the audit found on aha00a.com, applied by a one-off that is not
kept: it ends in a hard delete, and the reasoning below about committed delete scripts applies.
Each page was read, backed up, rewritten by plan, saved against the revision just read, and read
back to confirm it held the plan.

* `Computer Language` r16: the duplicate `[Php]` on the Perl line is gone; `[PHP], [ASP]` on the
  next line stays, since `[ASP]` lives only there.
* `NginxTlsForLocalhost` r3, `Visualization` r7, `curl` r3, `목소리의 형태` r3: `[TODO]` →
  `[ToDo]`, rewritten by offset through the same mask `fix-case-miss-links.mjs` uses.
* `Graphviz` r22: the two Trac `#!graphviz` blocks are `#!Mermaid` now (`graph TD` and
  `graph LR`, as the DOT said) and hold only the edges. The page shows each example's DOT source
  in a `#!Vim dot` block just above, so nothing is lost and the rendering the page meant to show
  is back. r21 tried `#!Graph` first, and it drew nothing visible: that renderer fixes the root
  node at the screen centre and lets `forceCenter` pull every other node to world `(w/2, h/2)`,
  which is the canvas's bottom-right corner, under the legend box. Two nodes and one edge vanish
  there. `adjacentPagesD3Canvas.scala.html` centres the same way; with dozens of nodes the
  spread hides it. Not fixed here — it is an app change that needs a deploy to look at.
* `AhaImageViewer` r77: the two `#!td` wrappers are gone and the code blocks they held stand on
  their own. The Trac `||= … =||` header line above them was never a table here and is unchanged.
* `TODO` deleted. It was a `#!redirect ToDo` stub with three revisions — r1 `= TODO` /
  `할일들.` / `아래쪽 Backlink참고.`, r2 `#!redirect TODO`, r3 `#!redirect ToDo`. The owner's
  reason: a redirect stub is still a page name, and the editor offered it in autocomplete
  whenever a new page was being written. The four links above were rewritten first, and the
  delete refused to run while any `[TODO]` link remained.

## Pages whose names differ only by case (done 2026-09-09)

`case-duplicate-pages.mjs`. `Page.name` is `utf8mb4_bin`, so `Css` and `CSS` are two pages and
both can hold content. Sixteen such groups on aha00a.com; thirteen were already settled, one page
holding the text and the other `#!redirect`.

```bash
node scripts/case-duplicate-pages.mjs                          # report only
node scripts/case-duplicate-pages.mjs --apply --comment="..."  # write
```

English Wikipedia decided which name keeps the content, and it was **asked rather than
remembered** — its API has two traps. It upper-cases the first letter of every title, so `macOS`
comes back as `MacOS` and `sbt` as `Sbt (software)`; the article's own DISPLAYTITLE is the answer.
And a short name is often a disambiguation page, which means Wikipedia has no opinion: `JSP`,
`ASP` and `Todo` are all disambiguations. That is why `ToDo`/`TODO` was left exactly as it was —
the standard does not answer, and inventing an answer is not what tidying means.

**It swaps content; it does not rename.** `POST /api/v1/rename` is better — it moves every
revision and leaves a redirect — but it refuses when the target name is taken, and the target was
taken in every case here. Clearing the way means `DELETE`, which is `DELETE FROM Page WHERE
name = ?`: every revision of that name, gone. `CSS` held **26 revisions going back to
2008-09-23**, older than `Css` itself — it was the original page, pointed at `Css` in 2016.
Deleting it to make room would have destroyed the older history of the pair. Two ordinary writes
keep both histories; the cost is that the content's own history stays under the old name.

`Jsp` and `Snmp` were pointers written as prose — `see [wiki:JSP]` — so the reader landed on a
near-empty page and had to click again, and `PageList` counted them as pages with content. They
are `#!redirect` now, which answers 303.

Verified by fetching each afterwards: `CSS` 200 with `= CSS`, and `Css`, `Jsp`, `Snmp` all 303 to
the right place.

`AWS`/`Aws` is here too, but it had to wait for the family: `Aws` is the prefix of 21 pages, and
moving the parent alone would have left nineteen children spelled the old way. Those went first,
by rename — see the next section — and the parent came here afterwards because `AWS` was an
occupied name and rename refuses one.

## Renaming to the Wikipedia spelling (done 2026-09-09)

`wikipedia-case-renames.mjs` is the other half. `POST /api/v1/rename` is the better tool whenever
the target name is **free**: it moves every revision, leaves `#!redirect <new>` behind, and
deletes nothing.

```bash
node scripts/wikipedia-case-renames.mjs acronyms                          # report only
node scripts/wikipedia-case-renames.mjs aws --apply --comment="..."       # do it
```

Three plans. `acronyms` moved `Php` → `PHP`, `Asp.Net` → `ASP.NET`, `Asp` → `ASP`. `aws` moved
sixteen `Aws <thing>` children plus `AwsCli` → `AWS CLI`, the last page still spelled the
CamelCase way its siblings left behind years ago.

`todo` is the one where **Wikipedia is not the standard**. Its `Todo` is a disambiguation page and
settles nothing, so the wiki's own WikiWord naming decides, and that is `ToDo` — which aha00a.com
already read as. Four ahawiki.net pages were out of step: `TODO NewUserFlow` and three
`TODO-*` task documents. These are the only ones with committed copies under `docs/ahawiki.net/`,
so the rename is half the job — `git mv` the files and pull the changed pages back down.

**The four redirect stubs were then deleted, on the owner's instruction**, so the mirror is 1:1
again: 117 pages, 117 files, nothing unmirrorable. That is a hard delete —
`DELETE FROM Page WHERE site = ? AND name = ?`, every revision, no undo — so before it ran, each
stub was saved whole (one revision, one line each), and **all 121 pages were read to confirm
nothing linked to any of the four**. Grep over `docs/ahawiki.net/` would not have been enough:
those were 117 of the wiki's 121, and the four with no local copy were exactly the ones in
question. The delete script is deliberately not kept — it has no second use, and a committed
script that hard-deletes wiki pages is a hazard with no upside. What it deleted is recorded here.

The collision guard stays, because the hazard is not specific to these four: any rename to a
name that differs only by case recreates it, and the download bug it exposed was real
independently.

Four things a rename leaves behind, and this is most of what the script is:

* **The heading.** It names the page, so it moves with it. `Asp.Net` opened `= [Asp].Net`, a link
  inside a heading rather than the plain title the others had.
* **A redirect that now points at a redirect.** `AwsCodeCommit` said `#!redirect Aws CodeCommit`;
  rename that target and the reader gets a pointer to a pointer, which the renderer does not
  follow. Retargeted.
* **An index listing the old names.** The parent page is a list of its children.
* **Red links in that index.** Four entries — `Aws IAM`, `Aws Route 53` and two others — are
  pages nobody has written. There is nothing to rename, so the rename pass never sees them, and
  left alone the next person to click one writes a page under the name everything else just
  stopped using.

Links from **elsewhere** are deliberately not rewritten. A redirect is what a wiki leaves behind
on purpose, and every old link keeps working through it.

It also collapses `[Php|PHP]` back to `[PHP]`. That pipe was written by
`fix-case-miss-links.mjs` when the page was still called `Php`: it kept the reader seeing PHP
while the link reached the page. The page is called PHP now, so the pipe says nothing — but only
that exact case, because `[FFmpeg|ffmpeg]` still earns its keep and an alias that is not the new
name is a label, not a workaround.

**Both scripts are idempotent**, because they stay in the repository as the record of what was
changed and running one again has to be safe. A rename whose old name already holds the right
redirect reports "already done" — and still counts as moved, since the passes after it work from
that list. Getting that wrong the first time made the run report seventeen failures and skip the
index it was supposed to fix.

Verified against the wiki's own page list rather than the plan: 26 old names, every one a
redirect; 26 names under the new spelling; no redirect pointing at a redirect; and no old name
still holding content.

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
