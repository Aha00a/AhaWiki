# Running the app locally

Start from your local config. It is not in this repository and should not be: keep it somewhere
like `~/.config/ahawiki/application.local.dev.conf`. It already names the database, the Redis and
its database number — one per environment, so a local run does not share a cache with production.

```bash
sbt -Dconfig.file="$HOME/.config/ahawiki/application.local.dev.conf" -Dhttp.port=9999 -Duser.timezone=Asia/Seoul run
```

It lived in `conf/` until 2026-08-12, which was a mistake worth not repeating: `sbt stage`
packages everything under `conf/` and gitignore has no say in that, so a file full of credentials
went out with every deploy. The build now refuses untracked files there, but the config has no
reason to be inside the repository at all — nothing reads it by a relative path.

The rest of this page is what goes wrong around that, each in a way that does not name itself.

## A Redis has to be reachable

`build.sbt` pulls in `cacheApi` and `play-redis`, and no cache implementation besides. There is
no in-process fallback: if the configured Redis does not answer, Guice fails to build the
injector and every request answers `500`, with the connection error buried in the stack trace.
The specs do not hit this because `TestApplication` binds its own in-memory `SyncCacheApi` — see
`docs/Testing.md`.

`Connection refused` from the configured host means nothing is listening there rather than
something turning you away: the packet arrived. A Redis listening on loopback only looks exactly
like this, and the app running beside it stays fine because it connects locally.

That state comes back on its own. Opening it with `redis-cli CONFIG SET bind ...` changes the
running server and not `redis.conf`, so the next restart — a reboot, a package upgrade, an OOM
kill — reads the old file and closes it again. `CONFIG REWRITE` afterwards writes the running
configuration back to the file and makes it stick.

A firewall refusing you looks different: the connection hangs for twenty seconds rather than
failing in two, because a dropped packet is answered by nothing at all. Timing tells the two
apart before you go looking in the wrong place.

If you have to stand one up while that is sorted out, anything speaking the protocol will do,
but then the host has to be overridden as well. `-Dplay.cache.redis.host` on the sbt command
line does not take; write a config that includes yours and overrides after it:

```hocon
include file("/path/to/application.local.dev.conf")

play.cache.redis.host = "localhost"
play.cache.redis.database = 15
```

Choosing a spare database number keeps the local run off the entries the configured one holds.

## Evolutions must not run

A local config normally points at a shared development database. Play applies evolutions to
whatever it is given, so a local run can migrate a database other people are using. Turn them
off in the same override file:

```hocon
play.evolutions.db.default.enabled = false
play.evolutions.db.default.autoApply = false
```

## Then

```bash
sbt "-Dconfig.file=/path/to/your-override.conf" -Dhttp.port=9999 -Duser.timezone=Asia/Seoul run
```

The app resolves the site from the `Host` header, so requests need one that matches a row in
`Site`:

```bash
curl -H "Host: ahawiki.net" http://localhost:9999/w/FrontPage
```

`localhost` with no `Host` header reaches no site.

`conf/base.conf` holds the defaults your local config overrides. Your local config itself lives
outside the repository — see the top of this page.

## Loopback is whitelisted, and why that matters

`FilterAccessLog` rate-limits by address and writes an `IpDeny` row when the limit is passed,
and that row keeps returning `403` for ninety days. `AhaWiki.ipWhitelist` in `conf/base.conf`
lists loopback in both families — plain `127.0.0.1` is not enough, because a local request
arrives over IPv6 as `0:0:0:0:0:0:0:1` on a current OS. Half a dozen quick `curl`s used to be
enough to lock yourself out for a quarter.

A whitelisted address now skips the deny lookup as well as the limiter, so an address that was
denied before it was whitelisted is reachable again without touching the table.
