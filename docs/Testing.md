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
  `conf/evolutions/default` silently, and nothing checks the two against each other. Running
  the evolutions against H2 would remove that whole class of drift; the dialect is what
  stands in the way.
- `UserMergeSpec` builds its own table named `Page` with a foreign key to `User`.
  `UserMerge` finds what to update by walking the foreign keys exported from `User`, and
  production's `Page` has no foreign key to `User` — only to `UserApiKey`. The table is a
  stand-in for "some table that references a user"; the name is misleading and the case it
  proves is not one production has.
