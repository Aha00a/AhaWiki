# How the app finds its files

Read resources off the classpath, not off a relative path.

Reading them relatively worked only when the working directory happened to be a source checkout.
A `sbt stage` build has no `app/` or `public/` beside the binary, so every page answered 500 the
first time one was deployed.

| | Before | Now |
|---|---|---|
| `app/logics/SchemaOrg.scala` | `Source.fromFile(new File("public/schema.org/…"))` | `getResourceAsStream("public/schema.org/…")` |
| `app/logics/DefaultPageLogic.scala` | `new File("app/assets/Page", title)` | `getResourceAsStream(s"Page/$title")` |

## Why the default pages live in `conf/Page`

They used to be in `app/assets/Page`, and moved for two reasons.

Anything under `app/assets/` is packaged as a web asset, so every default page was also being
served verbatim at `/assets/Page/<title>` — the source, not the rendering.

And `conf/` ships as its own directory next to `lib/` in a staged build, with the start script
putting it at the front of the classpath (`app_classpath="$lib_dir/../conf/:…"`). That makes
`Page/<title>` resolve with nothing further to arrange.

`schema.org` stayed in `public/`. It is public schema data, so serving it does no harm, and its
path inside the assets jar is `public/schema.org/…` either way — only the reading had to change.

`cache` is still read as a filesystem path, which is right: it is written at runtime. In a
deployed release it is a symlink to a directory that outlives the release.

One consequence worth knowing: a deploy only has to carry the staged output. Nothing needs to be
uploaded beside it.

## In dev mode

```
playMonitoredFiles                    -> public, conf, app
Compile/unmanagedResourceDirectories  -> conf
```

Both `conf/` and `app/` are watched, so moving the default pages changed nothing about how dev
restarts; `conf/` is actually quicker, having nothing to recompile.

Putting a resource somewhere unwatched to avoid the restart backfires. Read from the classpath,
dev serves the copy under `target/scala-2.13/classes`, so an unwatched file is one whose edits
never appear at all.

## Checking that no relative path crept back

```bash
grep -rnoE '(new File|Paths\.get|Source\.fromFile)\("[^"/][^"]*"' app/
```

Anything but `cache` and `.` should be reading from the classpath instead.
