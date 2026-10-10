#!/usr/bin/env bash
# Builds locally and puts the result on the server as a new release.
#
# The server keeps releases side by side and points `current` at one of them, so a deploy is a
# symlink move and a rollback is the same move backwards. One instance serves behind a reverse
# proxy. A deploy starts a second one, the standby, on the new release, restarts the first while
# the standby answers, and stops the standby again.
#
#   AHAWIKI_DEPLOY_HOST=<ssh host>  AHAWIKI_HEALTH_HOST=<a site's domain>  AHAWIKI_PROXY_RELOAD=<command>  bash deploy.sh
#   AHAWIKI_CANARY=1 ... bash deploy.sh        # stop once the standby runs the new release, to compare it
#   AHAWIKI_RESTART_ONLY=1 ... bash deploy.sh  # no build: hand over on whatever `current` points at
#   SKIP_TAG=1 ... bash deploy.sh     # skip the deploy tag
#
# Everything about where it deploys comes from the environment. This repository is public and
# says nothing about the machines it runs on; see docs/ahawiki.net/'Dev Deploying' for the rest.

set -euo pipefail

HOST="${AHAWIKI_DEPLOY_HOST:-}"
HEALTH_HOST="${AHAWIKI_HEALTH_HOST:-}"
ROOT="${AHAWIKI_REMOTE_ROOT:-/opt/ahawiki}"
SERVICE_USER="${AHAWIKI_SERVICE_USER:-ahawiki}"
KEEP_RELEASES="${AHAWIKI_KEEP_RELEASES:-3}"
# How long to wait for a restarted instance to answer /hc, as a count of 3-second polls. A cold
# JVM + Play start binds the port only after module init and the first cache warmup, which on a
# loaded server with the other instance also starting has taken past three minutes; 90s (the old
# 30) timed out on a start that was fine, and the deploy stopped with one instance still on the
# old release. 60 -> 180s. Raise it with AHAWIKI_HEALTH_TRIES on a slower box.
HEALTH_TRIES="${AHAWIKI_HEALTH_TRIES:-60}"
PRIMARY="${AHAWIKI_PRIMARY_PORT:-10000}"
STANDBY="${AHAWIKI_STANDBY_PORT:-10001}"
# Run on the host to make the reverse proxy forget which upstreams it has seen fail; step 5 says
# why. It names the proxy, which is the operations side's business, so it comes from there.
PROXY_RELOAD="${AHAWIKI_PROXY_RELOAD:-}"
CANARY="${AHAWIKI_CANARY:-0}"
RESTART_ONLY="${AHAWIKI_RESTART_ONLY:-0}"
# Falls back to the host the health check already uses, so forgetting the variable costs
# coverage rather than the check itself. Left empty this loop runs zero times, reports nothing,
# and the deploy goes on to prune and tag as if it had passed.
read -r -a VERIFY_URLS <<< "${AHAWIKI_VERIFY_URLS:-https://${AHAWIKI_HEALTH_HOST:-}/}"

for required in AHAWIKI_DEPLOY_HOST AHAWIKI_HEALTH_HOST AHAWIKI_PROXY_RELOAD; do
  if [ -z "${!required}" ]; then
    echo "$required is not set. See docs/ahawiki.net/'Dev Deploying'." >&2
    exit 2
  fi
done
if [ "$CANARY" = "1" ] && [ "$RESTART_ONLY" = "1" ]; then
  echo "AHAWIKI_CANARY and AHAWIKI_RESTART_ONLY are two halves of one deploy; run them one after the other." >&2
  exit 2
fi
# A deploy that stopped half way can leave $STANDBY serving with $PRIMARY down (step 5 says when).
# Restarting $STANDBY first, as step 5 otherwise does, would then leave nothing answering, so in
# that state the hand-over starts from $STANDBY as it is. A canary would need that restart, so it
# refuses before building anything.
STANDBY_SERVING=0
ssh "$HOST" "systemctl is-active --quiet ahawiki@$STANDBY && ! systemctl is-active --quiet ahawiki@$PRIMARY" && STANDBY_SERVING=1
if [ "$STANDBY_SERVING" = "1" ] && [ "$CANARY" = "1" ]; then
  echo "$STANDBY is serving with $PRIMARY down. Hand back first with AHAWIKI_RESTART_ONLY=1, then run the canary." >&2
  exit 2
fi

SRC="$(cd "$(dirname "$0")" && pwd)"
REL="$(date +%Y%m%d-%H%M%S)"

say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }

if [ "$RESTART_ONLY" = "1" ]; then
  say "1-4/7 Skipped: restarting on the release current points at"
  REL="$(ssh "$HOST" "basename \"\$(readlink -f '$ROOT/current')\"")"
  echo "  release $REL"
  # Nothing to go back to: this run did not move current.
  PREV=""
else
say "1/7 Build"
cd "$SRC"
git status --porcelain | grep -q . && echo "  warning: the working tree is not clean" || true
SHA="$(git rev-parse HEAD)"
SHORT="$(git rev-parse --short HEAD)"
echo "  commit $SHORT on $(git rev-parse --abbrev-ref HEAD)"
npm run --silent admin:build
sbt -batch stage

STAGE="$SRC/target/universal/stage"
[ -x "$STAGE/bin/ahawiki" ] || { echo "no build output at $STAGE" >&2; exit 1; }

say "2/7 Make the release directory: $REL"
ssh "$HOST" "sudo -n mkdir -p '$ROOT/releases/$REL' && sudo -n chown $SERVICE_USER:$SERVICE_USER '$ROOT/releases/$REL'"

say "3/7 Upload"
# tar over ssh rather than rsync. Calling an MSYS2 rsync from Git Bash crosses two MSYS
# runtimes, the arguments arrive mangled, and it dies in main.c before copying anything.
echo "  stage ($(du -sh "$STAGE" | cut -f1))"
tar -C "$STAGE" -cf - . | ssh "$HOST" "sudo -n tar -C '$ROOT/releases/$REL' -xf -"

say "4/7 Point current at it"
# Kept so that a standby which comes up unable to render can leave current as it found it.
PREV="$(ssh "$HOST" "readlink -f '$ROOT/current' || true")"
ssh "$HOST" "
  set -e
  R='$ROOT/releases/$REL'
  sudo -n chown -R $SERVICE_USER:$SERVICE_USER \"\$R\"
  # The cache and the logs outlive any one release, so they live outside and are linked in.
  sudo -n -u $SERVICE_USER ln -sfn '$ROOT/shared/cache' \"\$R/cache\"
  sudo -n -u $SERVICE_USER ln -sfn '$ROOT/shared/logs'  \"\$R/logs\"
  sudo -n -u $SERVICE_USER ln -sfn \"\$R\" '$ROOT/current'
  ls -l '$ROOT/current'
"
fi

say "5/7 Hand over: $STANDBY answers while $PRIMARY restarts"
# One instance, $PRIMARY, serves. The proxy lists $STANDBY as a backup, sent requests only while
# $PRIMARY does not answer, and outside a deploy $STANDBY is stopped. Restarting $PRIMARY alone
# would leave nothing to answer for the length of a JVM start, so the standby comes up first, on
# the new release, and is checked before $PRIMARY goes down. A release or a config file that
# cannot start or render stops the deploy there, with $PRIMARY untouched and still running what
# it loaded at its own start.
wait_healthy() {
  local p=$1 i code
  for i in $(seq 1 "$HEALTH_TRIES"); do
    sleep 3
    # /hc rather than a wiki page. It runs SELECT 1 and checks free disk, so it answers for the
    # things a restart can break, and it needs no Host header because it does not match a site.
    #
    # It also has to stay off /w/. IpRateLimiter calls anything under /w/ a page view and
    # everything else a human signal, and bans an IP that asks for 5 pages with fewer than 3
    # human signals in 30s. Polling /w/FrontPage every 3s is precisely that, so on 2026-09-04
    # this loop banned itself on the fifth poll: every later poll got 403 and a tarpit, and the
    # deploy died with "never became healthy" while the instance was serving readers normally.
    # Whether the site renders is checked once it answers, by render_failures below.
    code=$(ssh "$HOST" "curl -s -o /dev/null -w '%{http_code}' --max-time 5 http://127.0.0.1:$p/hc" 2>/dev/null || echo 000)
    if [ "$code" = "200" ]; then echo "    healthy after $i"; return 0; fi
  done
  return 1
}

# /hc renders no page, and every check through the proxy can be answered by the instance not
# restarted. On 2026-10-01 a canary answered 500 on every page while all of this deploy's checks
# passed; a comparison run afterwards was the first thing to see it. So each restarted instance
# renders the verify URLs itself, on its own port over loopback -- the whitelisted address, so
# these requests count toward no ban. Prints "<code> <url>" for each one it does not render.
render_failures() {
  local p=$1 u rest h path code
  for u in "${VERIFY_URLS[@]}"; do
    [ -n "$u" ] || continue
    rest="${u#*://}"; h="${rest%%/*}"; path="/${rest#*/}"
    [ "$rest" = "$h" ] && path=/
    # -L: a site's / answers 303 to a path on the same port, and curl keeps the Host header for it.
    code=$(ssh "$HOST" "curl -sL -o /dev/null -w '%{http_code}' --max-time 60 -H 'Host: $h' 'http://127.0.0.1:$p$path'" 2>/dev/null || echo 000)
    [ "$code" = "200" ] || echo "$code $u"
  done
}

# Starts or restarts one instance and checks it: 0 when it answers and renders, 1 when it answers
# but does not render (the failures are printed), 2 when it never answers.
start_checked() {
  local p=$1 failures
  ssh "$HOST" "sudo -n systemctl restart ahawiki@$p"
  wait_healthy "$p" || { echo "    $p never became healthy" >&2; return 2; }
  failures="$(render_failures "$p")"
  if [ -n "$failures" ]; then
    echo "    $p answers /hc but does not render:" >&2
    printf '%s\n' "$failures" | sed 's/^/      /' >&2
    return 1
  fi
  echo "    renders ${#VERIFY_URLS[@]} verify URL(s)"
}

# A proxy that drops a failing upstream keeps it out for a fixed interval, even after it answers
# again. On 2026-08-12 one instance was restarted while the other was still serving that penalty,
# and readers got a 502 for about a second. Waiting for the proxy to come round needs a request
# only the restarted instance can answer, and every request through the proxy can be answered by
# the other one; sleeping a number tuned to the interval would put the interval in two places.
# A reload starts the proxy with no memory of failures, which makes the condition true instead of
# waiting for it. It is a configuration reload: requests in flight finish on the old workers.
proxy_forget() {
  local out
  out="$(ssh "$HOST" "$PROXY_RELOAD" 2>&1)" || {
    printf '%s\n' "$out" | sed 's/^/      /' >&2
    echo "    the proxy reload failed — stopping here with every instance that is up left up" >&2
    exit 1
  }
}

# True when there is a release to go back to: this run moved current away from one.
can_put_back() { [ -n "$PREV" ] && [ "$PREV" != "$ROOT/releases/$REL" ]; }
put_back() {
  echo "    putting current back to $PREV" >&2
  ssh "$HOST" "sudo -n -u $SERVICE_USER ln -sfn '$PREV' '$ROOT/current'"
}

if [ "$STANDBY_SERVING" = "1" ]; then
  echo "  $STANDBY is serving and $PRIMARY is down: handing back from $STANDBY as it is"
else
  echo "  starting $STANDBY on $REL"
  rc=0; start_checked "$STANDBY" || rc=$?
  if [ "$rc" != 0 ]; then
    # Nothing has reached readers: $PRIMARY was not touched.
    can_put_back && put_back
    ssh "$HOST" "sudo -n systemctl stop ahawiki@$STANDBY"
    echo "    stopping: $PRIMARY was not touched and still serves; $STANDBY is stopped again. Nothing tagged." >&2
    exit 1
  fi
fi

if [ "$CANARY" = "1" ]; then
  echo "  canary: $STANDBY runs $REL; $PRIMARY still runs the release before it and takes the readers."
  echo "    compare: AHAWIKI_DEPLOY_HOST=<ssh host> bash scripts/compare-instances.sh $PRIMARY $STANDBY"
  echo "    finish:  the same environment with AHAWIKI_RESTART_ONLY=1"
  echo "    abandon: point current back at ${PREV:-the previous release}, then stop ahawiki@$STANDBY"
else
  # $STANDBY is about to be the only instance answering. If the proxy wrote it off at some earlier
  # failure, it would still be holding it out.
  proxy_forget
  echo "  restarting $PRIMARY"
  rc=0; start_checked "$PRIMARY" || rc=$?
  if [ "$rc" != 0 ]; then
    # Leaving it like this is not "$STANDBY serves in its place": the proxy goes back to $PRIMARY
    # the moment it answers anything, pages that fail to render included. So either $PRIMARY goes
    # back to the release before, where it was working, or it is stopped and $STANDBY -- which
    # passed the same checks -- takes the readers.
    if can_put_back; then
      put_back
      echo "  restarting $PRIMARY on $PREV" >&2
      rc=0; start_checked "$PRIMARY" || rc=$?
      if [ "$rc" = 0 ]; then
        proxy_forget
        ssh "$HOST" "sudo -n systemctl stop ahawiki@$STANDBY"
        echo "    stopping: $PRIMARY is back on the release before and $STANDBY is stopped — as before this deploy. Nothing tagged." >&2
        exit 1
      fi
    fi
    ssh "$HOST" "sudo -n systemctl stop ahawiki@$PRIMARY"
    echo "    !! stopping: $PRIMARY is stopped and $STANDBY serves alone. Find out why $PRIMARY failed, then hand back with AHAWIKI_RESTART_ONLY=1." >&2
    exit 1
  fi
  # The proxy wrote $PRIMARY off while it restarted. Stopping $STANDBY before it forgets that would
  # leave it no upstream at all.
  proxy_forget
  echo "  stopping $STANDBY"
  ssh "$HOST" "sudo -n systemctl stop ahawiki@$STANDBY"
fi

if [ "$CANARY" = "1" ]; then
  say "6/7 Verify from outside: skipped — the proxy still sends readers to the release before"
else
say "6/7 Verify from outside"
verify_failed=0
for u in "${VERIFY_URLS[@]}"; do
  [ -n "$u" ] || continue
  code=000
  # Retried. It began as waiting out the proxy's penalty on an instance that had just come back,
  # which once cost a good release its tag; step 5 now reloads the proxy instead, and the retries
  # stay because they cost nothing when the first try passes.
  #
  # Follow the redirects. A front page that answers 303 says nothing about what it redirects to,
  # and a deploy once passed this check while every page behind it was a 500.
  for attempt in 1 2 3 4 5 6; do
    code=$(curl -sL -o /dev/null -w '%{http_code}' --max-time 25 "$u" || echo 000)
    [ "$code" = "200" ] && break
    sleep 5
  done
  printf '  %-40s HTTP %s\n' "$u" "$code"
  [ "$code" = "200" ] || verify_failed=1
done
if [ "$verify_failed" = "1" ]; then
  echo "  verification failed — not tagging. Roll back by pointing current at the previous release." >&2
  exit 1
fi
fi

if [ "$RESTART_ONLY" = "1" ]; then
  echo
  echo "done: $PRIMARY restarted on release $REL, $STANDBY stopped. Nothing new was released, so nothing is tagged."
  exit 0
fi

echo "  pruning old releases:"
ssh "$HOST" "
  # The other remote block sets this and so must the one that runs \`rm -rf\`. Without it a
  # failed \`cd\` does not stop anything: the loop keeps going in the login user's home, where
  # nothing matches the 'is this the current release' test, and prunes whatever it lists there.
  set -e
  cd '$ROOT/releases'
  ls -1t | tail -n +$((KEEP_RELEASES+1)) | while read d; do
    [ \"$ROOT/releases/\$d\" = \"\$(readlink -f '$ROOT/current')\" ] && continue
    echo \"    removing \$d\"; sudo -n rm -rf \"\$d\"
  done
  echo '    kept:'; ls -1t | sed 's/^/      /'
"

say "7/7 Tag"
# After verification, so the tag means "this reached the server and answered", not "this built".
# A canary is tagged too: its standby reached the server and answered.
if [ "${SKIP_TAG:-0}" = "1" ]; then
  echo "  SKIP_TAG=1 — not tagging"
else
  DEPLOYER="$(git config user.name 2>/dev/null || true)"
  [ -n "$DEPLOYER" ] || DEPLOYER="$(git config user.email 2>/dev/null || whoami)"
  DEPLOYER="$(printf '%s' "${DEPLOYER%%@*}" | tr -c 'A-Za-z0-9._-' '-')"
  [ -n "$DEPLOYER" ] || DEPLOYER="deploy"
  TAG="v$(date +%Y%m%dT%H%M%S)-${DEPLOYER}"
  if git tag "$TAG" "$SHA" 2>/dev/null; then
    echo "  tagged $TAG -> $SHORT (release $REL)"
    git push origin "$TAG" && echo "  pushed" || echo "  !! tag push failed — run: git push origin $TAG" >&2
  else
    echo "  !! tag $TAG already exists — not tagging" >&2
  fi
fi

echo
echo "done: release $REL, commit $SHORT"
