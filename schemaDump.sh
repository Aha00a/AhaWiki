#!/usr/bin/sh

source "$(dirname "$0")/.env"

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
    > ./schema/schema.sql
