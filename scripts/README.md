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
