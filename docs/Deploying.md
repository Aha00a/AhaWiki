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
H=<host>; S=<a site's domain>
ssh $H "sudo -n -u ahawiki ln -sfn /opt/ahawiki/releases/<previous> /opt/ahawiki/current"
for p in 10001 10000; do
  ssh $H "sudo -n systemctl restart ahawiki@$p"
  until [ "$(ssh $H "curl -s -o /dev/null -w '%{http_code}' --max-time 5 -H 'Host: $S' http://127.0.0.1:$p/w/FrontPage")" = 200 ]; do sleep 3; done
  until [ "$(curl -sL -o /dev/null -w '%{http_code}' --max-time 15 https://$S/)" = 200 ]; do sleep 3; done
done
```

Longer than "restart one, `sleep 20`, restart the other", and the two `until` lines are why. A
rollback runs when something is already wrong, which is the worst moment to discover that
twenty seconds was not enough — or to sit through twenty when the instance came back in four.
They are the two conditions `deploy.sh` waits on, for the reasons below, and the second is not
redundant: an instance answering on loopback is not yet one the proxy will send anything to.
This said `sleep 20` until 2026-08-16, long after the deploy path had stopped counting seconds.

`cache/` and `logs/` live in `shared/` and are linked into each release, because they outlive
any one of them. Two systemd instances, `ahawiki@<port>`, sit behind a reverse proxy.

Old releases beyond `AHAWIKI_KEEP_RELEASES` are pruned at the end, skipping whatever `current`
points at. **Every remote block in the script begins with `set -e`, and the pruning one is the
reason to check that it still does.** Without it a failed `cd` into `releases/` does not stop
anything: the loop goes on listing the login user's home directory instead, nothing there
matches the "is this the current release?" test, and it runs `sudo rm -rf` over the oldest
entries it finds. The deploy reports success either way, because the failure is inside a
command whose exit status nobody was reading.

## Why it is shaped the way it is

Each of these cost a broken deploy once.

- **One instance at a time, and the proxy has to agree.** Restarting both empties the pool of
  healthy upstreams and readers get a 502. Each instance has to answer before the next goes
  down, and if one never does the script stops with the other still serving.

  Its own answer is not enough. A proxy that drops a failing upstream holds it out for a fixed
  interval, so an instance can be serving on loopback while the proxy still refuses to send it
  anything — and taking the other one down in that gap leaves nothing in rotation. It cost about
  a second of 502 on a deploy that was otherwise fine. So between instances the script waits for
  a request *through the proxy* to come back, rather than sleeping a number tuned to the
  interval, which would put that interval in two places at once.
- **The health check sends a `Host` header.** One instance serves many wikis and picks by host.
  A request without one matches no site and answers 404 no matter how healthy the instance is.
- **The external check follows redirects.** A front page answering 303 says nothing about what
  it redirects to. A deploy once passed on the 303 while every page behind it was a 500.
- **The external check retries.** The instances answering does not mean the proxy has noticed.
  A proxy that drops a failing upstream for a fixed interval keeps dropping it for the rest of
  that interval after it recovers, so checking the moment the last restart goes healthy reads
  502 from a deploy that worked — which happened, and cost a tag on a good release.
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
[info] conf/: not packaged (untracked): conf/something.local.conf
```

Filename patterns were tried first and are kept only as a fallback for a build with no git
available. A pattern catches the shapes someone thought of, and the file that started this had
none of them: it was named after a hostname.

That fallback went unexercised until 2026-08-15, when it was run for real by building with git
off the `PATH`, against two planted files — one named `*.local.*` and one named after a host.
It behaves as designed and the design's limit is the one described above: the first was left
out, the **second shipped**. So the fallback warns, and now also lists the top level of `conf/`
it is about to package, because "check the release" is not answerable from a list of what was
*dropped* — the file to worry about is by definition the one the patterns did not recognise.

```
[warn] conf/: git unavailable, falling back to name patterns. Check the release for local configs.
[warn] conf/: packaged on the name rule alone: conf/application.conf
[warn] conf/: packaged on the name rule alone: conf/base.conf
...
```

Six or so lines, and a stray config is the one that does not belong. Subdirectories are left
out of that list — every default page and every evolution is long enough to hide one in. None
of this appears when git is available, which is every normal build.

Nothing requires a deployment config to live in `conf/` — the server is told where its config is
with an absolute path — so the safest place for one is still somewhere else entirely.

## What used to be here

Two other mechanisms shipped in this repository until 2026-08-12 and neither matched the server
any more: a script that copied a build over an old path, and a GitHub Actions workflow that
built on the server and started it under pm2. Production had moved to systemd services running
out of `current`, so the script wrote to a directory nobody read, and the workflow would have
raced the running services for their ports. Both were removed, along with the pm2 launch scripts
and process file they depended on.
