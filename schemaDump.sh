#!/usr/bin/sh
set -e

source "$(dirname "$0")/.env"

# 임시 파일에 받고 성공했을 때만 교체한다. 예전에는 곧바로 schema/schema.sql 로
# 리다이렉트해서, 접속에 실패하면 mysqldump 가 아무것도 못 쓰기 전에 셸이 파일을
# 0바이트로 비워 버렸다. 덤프는 스키마 대조의 기준이라 잃으면 안 된다.
tmp="$(dirname "$0")/schema/.schema.sql.tmp"
out="$(dirname "$0")/schema/schema.sql"

# The header mysqldump writes is scrubbed on the way in, for two separate reasons, and
# removing either sed puts something back that does not belong in the file.
#   - Host and database name are infrastructure names, and this repository is public
#     (AGENTS.md). The replacement points at where the real values live instead.
#   - Server version, completion time, and AUTO_INCREMENT counters change on every run,
#     so leaving them makes an unchanged schema show up as a change.
mysqldump \
    --default-character-set=utf8mb4 \
    -h "$DB_HOST" \
    -P "$DB_PORT" \
    -u "$DB_USER" \
    -p"$DB_PASS" \
    --no-data --no-tablespaces --set-gtid-purged=OFF --column-statistics=0 \
    "$DB_NAME" \
    | sed 's/ AUTO_INCREMENT=[0-9]*//g' \
    | sed 's/^-- Dump completed on .*//g' \
    | sed 's/Distrib [0-9]*.[0-9]*.[0-9]*, for .*/Distrib #.#.#, for OS/g' \
    | sed 's/^-- Host: .*/-- Host: (from .env)    Database: (from .env)/' \
    > "$tmp"

# 빈 결과를 성공으로 착각하지 않는다.
if ! grep -q "^CREATE TABLE" "$tmp"; then
    echo "schemaDump: CREATE TABLE 이 없다. 기존 $out 를 그대로 둔다." >&2
    rm -f "$tmp"
    exit 1
fi

mv "$tmp" "$out"
echo "schemaDump: $out 갱신 ($(grep -c '^CREATE TABLE' "$out") tables)"
