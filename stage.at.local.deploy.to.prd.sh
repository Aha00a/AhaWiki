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

echo '✅ Local build + stage upload completed'
echo "Next step (manual): ssh $SERVER_HOST and restart app with your preferred process"
