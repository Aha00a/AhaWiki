# AhaWiki

A wiki engine written in Scala on Play. One installation serves many wikis, told apart by the
host they are asked for — `something.ahawiki.net` is a wiki of its own, with its own pages,
permissions and look.

It is built for people who want to keep writing things down and linking them together: a personal
knowledge base, a small team's meeting notes and project pages, a research notebook.

Running here: **<https://ahawiki.net>** — the site is itself an AhaWiki, and its pages are mirrored
in [`docs/ahawiki.net/`](docs/ahawiki.net/) (Korean).

## What it does

* **Pages and links first.** Type `[PageName]` and the link is there; follow it to write the page.
  Backlinks, similar pages, a page graph and a title index come from that.
* **Two syntaxes, one page.** [AhaMark](https://ahawiki.net/w/AhaMark) is the native one; a page or
  a block can be Markdown instead.
* **Blocks that do something.** Tables that sort and filter, code with syntax highlighting, math,
  Mermaid diagrams, Gantt charts, a Kanban board that several people can move cards on
  at once, maps fed from a Google Spreadsheet, slides and printable handouts. The list is on
  [Interpreter](https://ahawiki.net/w/Interpreter); the smaller inline ones are on
  [Macro](https://ahawiki.net/w/Macro).
* **Per-site and per-page permissions.** Read and write are decided by rows, not by a global
  switch, so a private page can live in a public wiki —
  [AccessControl](https://ahawiki.net/w/AccessControl).
* **Written by scripts too.** An HTTP API with per-user API keys, which marks what it writes as
  such so human edits stay legible — [Api](https://ahawiki.net/w/Api). The `docs/ahawiki.net/`
  mirror in this repository is kept in step through it.
* **Live while you read.** Other readers' cursors, and a refresh prompt when someone else saves the
  page you have open.

## Running it locally

You need a JDK (the build targets 11), [sbt](https://www.scala-sbt.org/), MySQL, Redis, and Node 18+
for the admin bundle and the JavaScript tests. Library and tool versions are in `build.sbt` and
`project/build.properties`.

The application's configuration — database, Redis, credentials — lives **outside** this repository,
because `sbt stage` packages everything under `conf/` into the release:

```bash
sbt -Dconfig.file="$HOME/.config/ahawiki/application.local.dev.conf" -Dhttp.port=9999 -Duser.timezone=Asia/Seoul run
```

```bash
curl -sL -o /dev/null -w '%{http_code}\n' http://localhost:9999/w/AhaWiki
```

Three things have to be arranged before that first command gives you a working server, and none of
them says its own name when it fails — including why the port above is not a matter of taste.
[Dev RunningLocally](https://ahawiki.net/w/Dev%20RunningLocally) is the page for it, and the source
for these two commands.

## Tests

```bash
sbt test
```

```bash
npm test
```

The Scala specs run against H2 in MySQL mode, on a schema built from the committed dump
`schema/schema.sql` rather than a hand-written copy —
[Dev Testing](https://ahawiki.net/w/Dev%20Testing). The Node tests cover the browser-side pure
functions and the repository's own checks, such as whether the wiki pages still cite names that
exist in this source tree.

## Deploying

`deploy.sh` builds locally and uploads a release. What it does and why each of its awkward steps is
there is on [Dev Deploying](https://ahawiki.net/w/Dev%20Deploying); which machine it deploys to
comes from the environment, so this public repository names none.

## Layout

| | |
|---|---|
| `app/` | Play application — controllers, `logics/` (including the wiki interpreters and macros), `models/`, Twirl views |
| `conf/` | routes, evolutions, base configuration, default pages |
| `public/`, `app/assets/` | static assets; `.less` sources next to the generated `.css` |
| `test/` | Scala specs and Node tests |
| `scripts/` | admin bundle build, wiki sync, and other tooling — [`scripts/README.md`](scripts/README.md) |
| `docs/ahawiki.net/` | copies of the wiki's own pages, which is where the documentation lives |

## Documentation

Developer documentation is written on the wiki, in Korean, under
[Dev](https://ahawiki.net/w/Dev), and mirrored in `docs/ahawiki.net/`.
[AGENTS.md](AGENTS.md) is the exception and the place to start when working in this repository: it
says how changes are made here and indexes everything else.

## License

[GNU General Public License v3.0](LICENSE).
