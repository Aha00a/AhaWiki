# Testing

Specs run against H2 in MySQL mode, not MySQL. Evolutions are MySQL-flavoured and do not
apply to H2, so the schema the specs run against is written by hand.

## Shared harness

Two files under `test/com/aha00a/tests` hold what every spec needs:

- `TestApplication` — the in-memory `SyncCacheApi` stand-in (the real cache module is Redis,
  which tests do not run), the H2 URL, a collision-proof database name, and the Guice
  configuration every spec starts from.
- `TestSchema` — one `CREATE TABLE` per table. A spec asks for the tables it needs:
  `TestSchema.create("Site", "User", "SiteAdmin")`. Referenced tables are created first, so a
  foreign key never precedes what it points at.

A spec that differs says so by adding to the shared configuration rather than restating it.
`ApiV1FilterSpec` runs the real filter chain, so it overrides `play.http.filters` and leaves
the rest alone.

The database URL used by the Guice application and by any direct `DriverManager` connection
must come from the same `TestApplication.h2Url`. Two spellings of the same name are two
different in-memory databases, and the spec fails on a missing table rather than on the
mismatch.

## Why this is shared

It was not. Every spec built its own harness, and the schemas had drifted:

| Table | Copies | Distinct definitions |
|---|---:|---:|
| `User` | 7 | 4 |
| `Site` | 6 | 3 |
| `Page` | 4 | 3 |
| `SiteDomain`, `UserEmail`, `PageMeta`, three `Calculated*` | 2 each | 2 each |

A spec could pass against a table shape no other spec — and no production database — agreed
with. One had grown its user columns to `BIGINT` where production declares `INT`, which H2
accepts right up until a foreign key has to match.

## Known gaps

- `TestSchema` is a hand-written mirror. It drifts from the evolutions under
  `conf/evolutions/default` silently, and nothing checks the two against each other.

  Running the evolutions against H2 instead would remove that whole class of drift, so it
  was tried: of the 67 evolution files, **17 fail — 41 statements in all**. The blockers are
  MySQL grammar H2 does not accept in MySQL mode, and they are structural rather than
  incidental:

  | Construct | Example |
  |---|---|
  | column positioning | `ALTER TABLE Page MODIFY comment TEXT NOT NULL AFTER remoteAddress` |
  | `... FIRST` | `ALTER TABLE Link ADD site INT DEFAULT 1 NOT NULL FIRST` |
  | drop and add a key in one statement | `ALTER TABLE Page DROP PRIMARY KEY, ADD PRIMARY KEY (site, name, revision)` |
  | `TRUNCATE` without `TABLE` | `TRUNCATE TermFrequency` |
  | MySQL date functions | `DATE_ADD(...)` in an `UPDATE` |

  Rewriting seven years of migrations to a common subset is not worth it, and editing an
  already-applied migration is not something to do casually. So the mirror stays.

  `schema/schema.sql` — the committed `mysqldump --no-data` of the real database — is the
  closest thing to an oracle. Comparing `TestSchema` against it by hand found sixteen
  disagreements: nine integer widths, `Page.comment` and `remoteAddress` declared
  VARCHAR(255) against production's TEXT, and three ENUM columns declared VARCHAR, which let
  a spec store a `targetType` the real column would reject. Those are now aligned and the
  suite still passes. Nothing performs that comparison automatically; a test that does would
  be worth writing, and its accuracy would depend on the dump being re-taken after schema
  changes.
- `TestSchema` declares a `UserSite` table that `schema/schema.sql` does not contain, even
  though evolution 55 creates it. `UserMerge` already guards its `UserSite` work with
  `hasColumn`, so the code was written expecting the table to be possibly absent. Which of
  the three is right — the evolutions, the dump, or the guard — is not something the
  repository answers; it needs a look at the real database.
- `UserMergeSpec` builds its own table named `Page` with a foreign key to `User`.
  `UserMerge` finds what to update by walking the foreign keys exported from `User`, and
  production's `Page` has no foreign key to `User` — only to `UserApiKey`. The table is a
  stand-in for "some table that references a user"; the name is misleading and the case it
  proves is not one production has.
