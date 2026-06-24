# Agent Instructions

When creating new files that should be part of the change, remember to stage them with `git add` so they are not omitted from the final commit or PR.

## AhaWiki Docs Sync

Files under `docs/ahawiki.net/` are local copies of pages on `https://ahawiki.net`.

When an agent changes any file in `docs/ahawiki.net/`, it should sync the changed file back to the matching AhaWiki page using the Bot API:

1. Treat the filename as the page name.
2. Read the current remote page with `GET /api/bot/page/<url-encoded-page-name>`.
3. Save the full local file content with `POST /api/bot/page/<url-encoded-page-name>`.
4. Use the remote `revision` from the read response. If the page is missing, use `revision: 0`.
5. On `409 Conflict`, re-read the remote page and resolve the conflict before saving.
6. Use a clear save `comment` and set `minorEdit` according to the change.
7. Verify by reading the page again and comparing the remote `content` with the local file.

Do not commit or write API keys into this repository. Use a user-provided key for the current session or an environment variable such as `AHAWIKI_API_KEY`.

