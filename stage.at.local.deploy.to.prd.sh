#!/usr/bin/env bash
set -euo pipefail

# SSH host alias from ~/.ssh/config
SERVER_HOST="aws.aharise.com"
APP_DIR="/home/ubuntu/www/wiki.aha00a.com"
REMOTE_UNIVERSAL_DIR="$APP_DIR/target/universal"

require_command() {
  local cmd="$1"
  if ! command -v "$cmd" >/dev/null 2>&1; then
    echo "Required command not found: $cmd" >&2
    exit 1
  fi
}

echo '==> Checking local requirements'
require_command sbt
require_command ssh
require_command scp

echo '==> Building (sbt stage)'
sbt stage

echo "==> Preparing remote stage directory on $SERVER_HOST"
ssh "$SERVER_HOST" "mkdir -p '$REMOTE_UNIVERSAL_DIR' && rm -rf '$REMOTE_UNIVERSAL_DIR/stage'"

echo '==> Uploading stage output (overwrite)'
scp -r target/universal/stage "$SERVER_HOST:$REMOTE_UNIVERSAL_DIR/"

#echo "==> Restarting remote app on $SERVER_HOST"
#ssh "$SERVER_HOST" "bash -lc 'set -euo pipefail; \
#  source ~/.profile >/dev/null 2>&1 || true; \
#  source ~/.bashrc >/dev/null 2>&1 || true; \
#  source ~/.nvm/nvm.sh >/dev/null 2>&1 || true; \
#  command -v pm2 >/dev/null 2>&1 || { echo \"pm2 not found on remote host\" >&2; exit 127; }; \
#  cd \"$APP_DIR\"; \
#  pm2 del start10000 || true; \
#  rm -f /home/ubuntu/www/wiki.aha00a.com/target/universal/stage/RUNNING_PID_10000; \
#  pm2 start start10000.sh; \
#  sleep 60; \
#  pm2 del start10001.sh || true; \
#  rm -f /home/ubuntu/www/wiki.aha00a.com/target/universal/stage/RUNNING_PID_10001; \
#  pm2 start start10001.sh'"
#
#echo '✅ Local build + stage upload + remote restart completed'
