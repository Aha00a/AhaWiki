#!/bin/sh
set -eu

if command -v sbt >/dev/null 2>&1; then
  sbt stage
elif [ -x "$HOME/sbt/bin/sbt" ]; then
  "$HOME/sbt/bin/sbt" stage
else
  echo "Error: sbt not found. Install sbt or add it to PATH." >&2
  exit 127
fi
