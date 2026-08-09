# Database

The schema of record is `conf/evolutions/default/*.sql`. Play applies it at startup.

## Multi-statement writes go through `LocalTransaction`

`models.tables.LocalTransaction` wraps a block so it commits or rolls back as a unit,
whether or not the connection is already in a transaction. Most callers get their connection
from `Database.withConnection`, which leaves autocommit on, but the same methods are also
reached from inside `withTransaction`, so the block has to handle both: it opens a
transaction when there is none and takes a savepoint when there already is one.

The nested case used to differ per table. `Page` took a savepoint; `User` and `UserMerge`
ran the block bare and let the exception through with the block's partial writes still
pending. All three rethrow, so an outer handler that rolls everything back saw no
difference — an outer handler that catches and continues did. The savepoint version is the
one kept, because it is the only one that leaves the connection in a state the caller can
describe.

The database is one schema on a shared RDS instance. Other projects live on the same
instance — **never touch anything outside this schema.**

## Datetime columns hold KST

**Every `datetime` column in this repository holds Korea Standard Time (KST, UTC+9).**

`datetime` carries no timezone, so what it holds is fixed by convention alone.
Unwritten, there is no way for a reader to find out — hence this note.

### Evidence (measured 2026-08-04)

The RDS `time_zone` is `Asia/Seoul`, so `current_timestamp()` yields KST.
Play's JDBC connections do not change the session timezone either, so values written by
the application are KST as well.

The test was the difference between `MAX(value)` and `NOW()`. Zero means KST, nine means UTC.

| Column | Written by | Difference from `NOW()` | Verdict |
|---|---|---|---|
| `AccessLog.dateInserted` | DB default | 0.0h | KST |
| `PageMeta.dateUpdated` | application | 0.0h | KST |

Columns filled by a DB default (23 of them) and columns the application writes directly
(7 of them) are **both KST**. They are not mixed.

### Rules

- New `datetime` columns are **KST** too. Mix them and there is no longer any way to tell
  them apart.
- Do not convert to UTC when the application writes a time. Whether it is
  `current_timestamp()` or `LocalDateTime.now()` on the Scala side, server local (= KST)
  is what belongs there.
- **Do not change the session timezone on a connection.** Change it and the
  `current_timestamp()` default quietly starts writing a different time, leaving rows from
  before and after the change mixed together nine hours apart.

### Other repositories differ — do not assume

Schemas on the same RDS instance follow different conventions (measured 2026-08-04).

| Schema | Time stored |
|---|---|
| this repository's schema | **KST** |
| one sibling schema | **UTC** (all columns, unified 2026-08-04) |
| another sibling schema | **mixed** — nine hours apart within a single row; to be unified on UTC |

Take particular care when viewing several schemas together in a database browser. A global
"timezone +9h" setting in the viewer turns this repository's values into a nine-hour lie.
