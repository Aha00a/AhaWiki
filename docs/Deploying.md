# Deploying

`deploy.sh` builds locally and puts the result on the server as a new release. Run it from a
machine that can already `ssh` to the server and `sudo -n` there.

```bash
AHAWIKI_DEPLOY_HOST=<ssh host>  AHAWIKI_HEALTH_HOST=<a site's domain>  bash deploy.sh
```

Everything about where it deploys comes from the environment. This repository is public and
names no machine of its own.

That is the whole boundary, and it is worth stating as one: **this repository owns how a deploy
works, and whatever holds your operations owns which machine it happens to.** Steps, ordering,
health checks, the tag format and the rollback all belong here, next to the code whose shape
decides them. Hostnames, accounts and the reverse proxy in front belong there, and calling this
needs no more than the values and one line:

```bash
AHAWIKI_DEPLOY_HOST=… AHAWIKI_HEALTH_HOST=… AHAWIKI_VERIFY_URLS="… …" bash /path/to/AhaWiki/deploy.sh
```

Keeping a second copy of the procedure on that side would put the same facts in two places, and
they always drift — the copy this replaced had gone stale enough to write to a directory nobody
read.

| | |
|---|---|
| `AHAWIKI_DEPLOY_HOST` | required — the ssh host or alias |
| `AHAWIKI_HEALTH_HOST` | required — a domain one of the wikis answers to, for the health check |
| `AHAWIKI_REMOTE_ROOT` | `/opt/ahawiki` |
| `AHAWIKI_SERVICE_USER` | `ahawiki` |
| `AHAWIKI_PORTS` | `10001 10000` — restart order |
| `AHAWIKI_VERIFY_URLS` | public URLs to check afterwards, space separated |
| `AHAWIKI_KEEP_RELEASES` | `3` |
| `SKIP_TAG` | `1` to skip the deploy tag |

## What it does

Releases sit side by side under `releases/` and `current` is a symlink to one of them, so
deploying moves a symlink and rolling back moves it back:

```bash
ssh <host> "sudo -n -u ahawiki ln -sfn /opt/ahawiki/releases/<previous> /opt/ahawiki/current \
  && sudo -n systemctl restart ahawiki@10001 && sleep 20 && sudo -n systemctl restart ahawiki@10000"
```

`cache/` and `logs/` live in `shared/` and are linked into each release, because they outlive
any one of them. Two systemd instances, `ahawiki@<port>`, sit behind a reverse proxy.

## Why it is shaped the way it is

Each of these cost a broken deploy once.

- **One instance at a time.** Restarting both empties the proxy's pool of healthy upstreams for
  a moment and readers get a 502. Each instance has to answer before the next goes down, and if
  one never does the script stops with the other still serving.
- **The health check sends a `Host` header.** One instance serves many wikis and picks by host.
  A request without one matches no site and answers 404 no matter how healthy the instance is.
- **The external check follows redirects.** A front page answering 303 says nothing about what
  it redirects to. A deploy once passed on the 303 while every page behind it was a 500.
- **Upload is `tar` over ssh, not rsync.** Calling an MSYS2 rsync from Git Bash crosses two MSYS
  runtimes; the arguments arrive mangled and it dies before copying anything.
- **The tag is written last.** After verification, so it means "this reached the server and
  answered" rather than "this built".

## Keep configuration with secrets out of `conf/`

`sbt stage` copies every file under `conf/` into the release. Gitignore has no say in that — it
governs what git tracks, not what the build packages — so a local config kept there rides along
to the server on every deploy.

That is not hypothetical. A development database password and a `play.http.secret.key` sat in
three releases, world readable, in files the app never opened: it is started with an absolute
`-Dconfig.file` pointing outside the release. They had been going out with every deploy since the
current layout began.

`build.sbt` now asks git what it tracks under `conf/` and packages only that, so an ignored file
does not ship — which is what everyone assumed was already true. It names what it leaves out:

```
[info] conf/: not packaged (untracked): conf/application.local.dev.conf
```

Filename patterns were tried first and are kept only as a fallback for a build with no git
available. A pattern catches the shapes someone thought of, and the file that started this had
none of them: it was named after a hostname.

Nothing requires a deployment config to live in `conf/` — the server is told where its config is
with an absolute path — so the safest place for one is still somewhere else entirely.

## What used to be here

Two other mechanisms shipped in this repository until 2026-08-12 and neither matched the server
any more: a script that copied a build over an old path, and a GitHub Actions workflow that
built on the server and started it under pm2. Production had moved to systemd services running
out of `current`, so the script wrote to a directory nobody read, and the workflow would have
raced the running services for their ports. Both were removed, along with the pm2 launch scripts
and process file they depended on.
