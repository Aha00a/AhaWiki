#!/usr/bin/sh
set -e

# schemaDump.sh          — 라이브 스키마를 받아 schema/schema.sql 을 갱신한다
# schemaDump.sh --check  — 받아서 대조만 한다. 파일은 건드리지 않고, 다르면 diff 와 함께 1 로 끝난다
#
# --check 가 있는 이유: 덤프는 스펙이 돌아가는 스키마의 기준인데 손으로 갱신한다. 스키마를
# 바꾸고 갱신을 잊으면 스펙은 낡은 스키마에 대고 통과한다 — 초록불이 거짓이 되는 자리다.
# 사람이 대조하려면 값이 필요 없으니, 이건 자격증명만 있으면 언제든 돌릴 수 있다.

# 자격증명은 앱이 이미 쓰는 설정에서 읽는다. 예전에는 저장소 안 `.env` 에 같은 DB
# 비밀번호를 한 벌 더 두었는데, 같은 비밀이 두 곳에 있으면 한쪽만 도는 날이 온다 —
# 그리고 저장소 밖에 있는 쪽이 살아남는다. 2026-09-02 에 이 체크아웃이 통째로 사라졌을 때
# 설정은 `~/.config/ahawiki/` 에 있어 무사했고, `.env` 만 함께 없어졌다.
#
# `AHAWIKI_CONF` 로 다른 환경의 설정을 지정할 수 있다. 설정이 없으면 옛 `.env` 로 물러선다.
conf="${AHAWIKI_CONF:-$HOME/.config/ahawiki/application.local.dev.conf}"
if [ -f "$conf" ]; then
    conf_value() { sed -n "s/^[[:space:]]*$1[[:space:]]*=[[:space:]]*//p" "$conf" | tail -1 | tr -d '"'; }
    url="$(conf_value 'db\.default\.url')"
    # jdbc:mysql://host:port/dbname?params — 포트가 없으면 3306.
    hostport="$(echo "$url" | sed -e 's#^jdbc:mysql://##' -e 's#/.*$##')"
    DB_HOST="$(echo "$hostport" | cut -d: -f1)"
    DB_PORT="$(echo "$hostport" | sed -n 's/^[^:]*:\([0-9]*\)$/\1/p')"
    DB_PORT="${DB_PORT:-3306}"
    DB_NAME="$(echo "$url" | sed -e 's#^jdbc:mysql://[^/]*/##' -e 's#?.*$##')"
    DB_USER="$(conf_value 'db\.default\.username')"
    DB_PASS="$(conf_value 'db\.default\.password')"
elif [ -f "$(dirname "$0")/.env" ]; then
    . "$(dirname "$0")/.env"
else
    echo "schemaDump: 자격증명을 찾지 못했다. $conf 가 없고 .env 도 없다." >&2
    exit 1
fi

for v in DB_HOST DB_PORT DB_NAME DB_USER DB_PASS; do
    eval "test -n \"\$$v\"" || { echo "schemaDump: $v 가 비어 있다 ($conf 를 확인할 것)." >&2; exit 1; }
done

tmp="$(dirname "$0")/schema/.schema.sql.tmp"
out="$(dirname "$0")/schema/schema.sql"

# 갱신과 대조가 **같은 산출물**을 봐야 한다. 파이프라인을 두 벌로 두면 한쪽 sed 만 고치는
# 날이 오고, 그때부터 대조는 있지도 않은 차이를 보고한다.
#
# The header mysqldump writes is scrubbed on the way in, for two separate reasons, and
# removing either sed puts something back that does not belong in the file.
#   - Host and database name are infrastructure names, and this repository is public
#     (AGENTS.md). The replacement points at where the real values live instead.
#   - Server version, completion time, and AUTO_INCREMENT counters change on every run,
#     so leaving them makes an unchanged schema show up as a change.
#
# 비밀번호는 `-p` 가 아니라 MYSQL_PWD 로 넘긴다. `-p"$DB_PASS"` 는 인자라서 실행되는 동안
# 프로세스 목록에 그대로 보이고, mysqldump 자신이 매번 그렇다고 경고한다.
dump_scrubbed() {
    MYSQL_PWD="$DB_PASS" mysqldump \
        --default-character-set=utf8mb4 \
        -h "$DB_HOST" \
        -P "$DB_PORT" \
        -u "$DB_USER" \
        --no-data --no-tablespaces --set-gtid-purged=OFF --column-statistics=0 \
        "$DB_NAME" \
        | sed 's/ AUTO_INCREMENT=[0-9]*//g' \
        | sed 's/^-- Dump completed on .*//g' \
        | sed 's/Distrib [0-9]*.[0-9]*.[0-9]*, for .*/Distrib #.#.#, for OS/g' \
        | sed 's/^-- Host: .*/-- Host: (from local config)    Database: (from local config)/'
}

# 임시 파일에 받고 성공했을 때만 교체한다. 예전에는 곧바로 schema/schema.sql 로
# 리다이렉트해서, 접속에 실패하면 mysqldump 가 아무것도 못 쓰기 전에 셸이 파일을
# 0바이트로 비워 버렸다. 덤프는 스키마 대조의 기준이라 잃으면 안 된다.
dump_scrubbed > "$tmp"

# 빈 결과를 성공으로 착각하지 않는다. --check 에서도 마찬가지다 — 접속에 실패한 것을
# "스키마가 통째로 바뀌었다"로 보고하면 대조가 없느니만 못하다.
if ! grep -q "^CREATE TABLE" "$tmp"; then
    echo "schemaDump: CREATE TABLE 이 없다. 기존 $out 를 그대로 둔다." >&2
    rm -f "$tmp"
    exit 1
fi

if [ "${1:-}" = "--check" ]; then
    if diff -u "$out" "$tmp"; then
        rm -f "$tmp"
        echo "schemaDump: $out 는 최신이다 ($(grep -c '^CREATE TABLE' "$out") tables)"
    else
        rm -f "$tmp"
        echo "schemaDump: $out 가 라이브 스키마와 다르다. 인자 없이 실행해 갱신할 것." >&2
        exit 1
    fi
    exit 0
fi

mv "$tmp" "$out"
echo "schemaDump: $out 갱신 ($(grep -c '^CREATE TABLE' "$out") tables)"
