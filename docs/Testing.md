# Testing

Specs run against H2 in MySQL mode, not MySQL. The schema they run against is built from
`schema/schema.sql`, the committed dump of the real database.

## Shared harness

Two files under `test/com/aha00a/tests` hold what every spec needs:

- `TestApplication` — the in-memory `SyncCacheApi` stand-in (the real cache module is Redis,
  which tests do not run), the H2 URL, a collision-proof database name, and the Guice
  configuration every spec starts from.
- `TestSchema` — `createAll()` builds every table from the dump. Not a named subset: the
  foreign keys tie the tables together, and a spec listing what it believes it needs is how
  such a list goes stale. An unused table in an in-memory database costs nothing.

A spec that differs says so by adding to the shared configuration rather than restating it.
`ApiV1FilterSpec` runs the real filter chain, so it overrides `play.http.filters` and leaves
the rest alone.

The database URL used by the Guice application and by any direct `DriverManager` connection
must come from the same `TestApplication.h2Url`. Two spellings of the same name are two
different in-memory databases, and the spec fails on a missing table rather than on the
mismatch.

## Keeping the schema true

Refresh the dump with `schemaDump.sh` after a schema change. The specs follow automatically.
A column they depend on going away then shows up as a failing spec, instead of as a copy
quietly going stale.

The script scrubs the header `mysqldump` writes, and the two reasons it does are easy to
mistake for one. The host and database name are infrastructure names that do not belong in a
public repository, so they are replaced by a pointer to `.env`. The server version, the
completion timestamp, and `AUTO_INCREMENT` counters are merely noise that changes on every
run, and left in they make an unchanged schema arrive as a diff. Removing a `sed` to get the
"real" header back reintroduces whichever of the two that line was holding down.

Three adjustments are made while loading, each for a difference between MySQL and H2 rather
than a difference of opinion about the schema:

| Adjustment | Why |
|---|---|
| foreign keys moved out of `CREATE TABLE` into `ALTER TABLE` | a dump is ordered alphabetically, so a key routinely points at a table that does not exist yet — and `AccessLog` and `IpDeny` reference each other, which no ordering satisfies |
| `tinyint(1)` read as `BOOLEAN` | MySQL has no boolean and dumps one as `tinyint(1)`; left alone, H2 returns Integer and every parser expecting Boolean fails |
| foreign keys onto non-unique columns dropped | MySQL accepts a key referencing any index prefix. H2 requires the target to be unique and will create a unique index to get there — on `Page (site, name)` that forbids a page having a second revision |

The third one is a real loss of fidelity: three keys onto `Page (site, name)` are not
enforced under test. Everything else, including every `NOT NULL`, default, and ENUM value
list, is exactly what production has.

## What building it this way found

The hand-written copy it replaced disagreed with production in sixteen places — nine integer
widths, two TEXT columns declared VARCHAR(255), three ENUM columns declared VARCHAR so a
spec could store a `targetType` the real column rejects, and a `UserSite` table evolution 55
had dropped.

Switching to the dump surfaced more, this time in the fixtures: specs were inserting `Site`
rows without `abbr` (`NOT NULL`, no default in production), `PageMeta` rows with no `Page`
behind them, and `AccessLog` rows missing eight required columns. Each had been passing
against a schema shaped loosely enough to accept them.

`UserMergeSpec` had also built its own minimal table named `Page` with a foreign key to
`User`, on the belief that production's `Page` has no such key. It does —
`Page_User_seq_fk` — so the stand-in was unnecessary and the spec now uses the real table.

## Known gaps

- Building the schema from the evolutions instead would tie the tests to the migrations
  themselves. It does not work: of the 67 files, **17 fail — 41 statements**, on MySQL
  grammar H2 rejects in MySQL mode.

  | Construct | Example |
  |---|---|
  | column positioning | `ALTER TABLE Page MODIFY comment TEXT NOT NULL AFTER remoteAddress` |
  | `... FIRST` | `ALTER TABLE Link ADD site INT DEFAULT 1 NOT NULL FIRST` |
  | drop and add a key in one statement | `ALTER TABLE Page DROP PRIMARY KEY, ADD PRIMARY KEY (site, name, revision)` |
  | `TRUNCATE` without `TABLE` | `TRUNCATE TermFrequency` |
  | MySQL date functions | `DATE_ADD(...)` in an `UPDATE` |

  All of it is `ALTER`. A dump has none — it is `CREATE TABLE` and nothing else — which is
  why the dump works where the evolutions do not.

- Nothing checks that `schema/schema.sql` is current. It is refreshed by hand, and a schema
  change that nobody dumps leaves the specs testing yesterday's shape. The failure mode is
  gentler than before — the whole suite runs against one stale schema rather than each spec
  against its own invention — but it is still a manual step.
