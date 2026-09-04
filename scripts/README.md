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

`logo` is ignored when deciding. Its domain is Organization/Brand/Product, so it fits no software
class, but `InterpreterSchema.imageKeys` draws it as a picture on any block — a display
convention, not a claim about the type. Counting it held twelve library pages back for nothing.

The decision is pinned in `test/add-paired-class.test.mjs` against the shipped vocabulary.
