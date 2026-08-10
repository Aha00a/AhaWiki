# Running the app locally

`sbt run` alone does not get you a working server. Three things have to be arranged first, and
each fails in a way that does not name itself.

## A Redis has to be reachable

`build.sbt` pulls in `cacheApi` and `play-redis`, and no cache implementation besides. There is
no in-process fallback: without a reachable Redis, Guice fails to build the injector and every
request answers `500`. The specs do not hit this because `TestApplication` binds its own
in-memory `SyncCacheApi` — see `docs/Testing.md`.

Anything speaking the protocol will do:

```bash
docker run -d --rm --name ahawiki-redis -p 6379:6379 redis:7-alpine
```

Then point the app at it. Overriding `play.cache.redis.host` with `-D` on the sbt command line
does not take — write a config that includes yours and overrides after it:

```hocon
include file("/path/to/AhaWiki/conf/application.local.dev.conf")

play.cache.redis.host = "localhost"
play.cache.redis.database = 15
```

Picking a database number keeps a local run from writing over cache entries a shared Redis is
holding for someone else.

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

Config lives in `conf/`; the local file is gitignored and is not in this repository. Read
`conf/base.conf` for the defaults it overrides.

## Loopback is whitelisted, and why that matters

`FilterAccessLog` rate-limits by address and writes an `IpDeny` row when the limit is passed,
and that row keeps returning `403` for ninety days. `AhaWiki.ipWhitelist` in `conf/base.conf`
lists loopback in both families — plain `127.0.0.1` is not enough, because a local request
arrives over IPv6 as `0:0:0:0:0:0:0:1` on a current OS. Half a dozen quick `curl`s used to be
enough to lock yourself out for a quarter.

A whitelisted address now skips the deny lookup as well as the limiter, so an address that was
denied before it was whitelisted is reachable again without touching the table.
