#!/usr/bin/env bash
# Builds locally and puts the result on the server as a new release.
#
# The server keeps releases side by side and points `current` at one of them, so a deploy is a
# symlink move and a rollback is the same move backwards. Two instances sit behind a reverse
# proxy and are restarted one at a time.
#
#   AHAWIKI_DEPLOY_HOST=<ssh host>  AHAWIKI_HEALTH_HOST=<a site's domain>  bash deploy.sh
#   SKIP_TAG=1 ... bash deploy.sh     # skip the deploy tag
#
# Everything about where it deploys comes from the environment. This repository is public and
# says nothing about the machines it runs on; see docs/Deploying.md for the rest.

set -euo pipefail

HOST="${AHAWIKI_DEPLOY_HOST:-}"
HEALTH_HOST="${AHAWIKI_HEALTH_HOST:-}"
ROOT="${AHAWIKI_REMOTE_ROOT:-/opt/ahawiki}"
SERVICE_USER="${AHAWIKI_SERVICE_USER:-ahawiki}"
KEEP_RELEASES="${AHAWIKI_KEEP_RELEASES:-3}"
read -r -a PORTS <<< "${AHAWIKI_PORTS:-10001 10000}"
# Falls back to the host the health check already uses, so forgetting the variable costs
# coverage rather than the check itself. Left empty this loop runs zero times, reports nothing,
# and the deploy goes on to prune and tag as if it had passed.
read -r -a VERIFY_URLS <<< "${AHAWIKI_VERIFY_URLS:-https://${AHAWIKI_HEALTH_HOST:-}/}"

for required in AHAWIKI_DEPLOY_HOST AHAWIKI_HEALTH_HOST; do
  if [ -z "${!required}" ]; then
    echo "$required is not set. See docs/Deploying.md." >&2
    exit 2
  fi
done

SRC="$(cd "$(dirname "$0")" && pwd)"
REL="$(date +%Y%m%d-%H%M%S)"

say() { printf '\n\033[1m== %s\033[0m\n' "$*"; }

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

say "5/7 Restart, one instance at a time"
# Restarting both at once empties the proxy's pool of healthy upstreams for a moment and readers
# get a 502. Each one has to answer before the next goes down.
for p in "${PORTS[@]}"; do
  echo "  restarting $p"
  ssh "$HOST" "sudo -n systemctl restart ahawiki@$p"
  ok=0
  for i in $(seq 1 30); do
    sleep 3
    # The Host header is not optional: one instance serves many wikis and picks by host, so a
    # request without one matches no site and answers 404 however healthy the instance is.
    code=$(ssh "$HOST" "curl -s -o /dev/null -w '%{http_code}' --max-time 5 -H 'Host: $HEALTH_HOST' http://127.0.0.1:$p/w/FrontPage" 2>/dev/null || echo 000)
    if [ "$code" = "200" ]; then echo "    healthy after $i"; ok=1; break; fi
  done
  [ "$ok" = "1" ] || { echo "    $p never became healthy — stopping here, the other instance is still up" >&2; exit 1; }

  # An instance answering is not the same as the proxy knowing it. A proxy that drops a failing
  # upstream holds it out for a fixed interval, so restarting the next one while this is still
  # serving its penalty leaves none in rotation and readers get a 502 — which happened, for about
  # a second, on a deploy that was otherwise fine.
  #
  # Waiting a number tuned to that interval would put the interval in two places. Wait for the
  # condition instead: keep going once a request through the proxy comes back.
  if [ -n "${VERIFY_URLS[0]:-}" ]; then
    back=0
    for i in $(seq 1 20); do
      [ "$(curl -sL -o /dev/null -w '%{http_code}' --max-time 15 "${VERIFY_URLS[0]}" || echo 000)" = "200" ] && { back=1; break; }
      sleep 3
    done
    [ "$back" = "1" ] || { echo "    the proxy is still not serving after $p came back — stopping before touching the rest" >&2; exit 1; }
  fi
done

say "6/7 Verify from outside"
verify_failed=0
for u in "${VERIFY_URLS[@]}"; do
  [ -n "$u" ] || continue
  code=000
  # Retried, because the instances answering does not mean the proxy has noticed yet. A proxy
  # that drops a failing upstream for a fixed interval keeps dropping it for the rest of that
  # interval after it recovers, so a check run the moment the last restart passes its health
  # check reads 502 from a deploy that worked. Give it longer than that interval before
  # believing it.
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
