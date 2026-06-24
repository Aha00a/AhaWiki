# Agent Instructions

When creating new files that should be part of the change, remember to stage them with `git add` so they are not omitted from the final commit or PR.

## AhaWikiDoc Sync

Files under `docs/ahawiki.net/` are local copies of pages on `https://ahawiki.net`.

Documents under `docs/ahawiki.net/` must use AhaMark format. Pay special attention to bullets, checkboxes, links, and code blocks:

```text
= PageTitle

== Section

 * Bullet item
  * Nested bullet item
 * [[CB]] Open checkbox item
 * [[CB(x)]] Checked checkbox item
 * Link to another wiki page: [PageName]
 * Link with label: [PageName Label Text]
 * Inline code uses backticks like `code`.

{{{
code block
multiple lines
}}}
```

When the user asks for `AhaWikiDoc sync`, sync changed files under `docs/ahawiki.net/` to the matching remote pages and also check for newer remote changes that should be pulled down locally. Do not limit this to uncommitted working-tree changes; compare remote pages with local files even when the local changes are already committed.

Use the existing `download:ahawiki.net` script in `package.json` and `scripts/download.ahawiki.net.mjs` as background for the page-list/download behavior, but prefer the Bot API for sync work. Do not use `?action=raw` for this workflow when the Bot API can provide the page metadata and content.

For sync, use remote `dateTime` and `revision` from `GET /api/bot/page/<url-encoded-page-name>` to compare local and remote state:

1. Treat the filename as the page name.
2. Include both uncommitted local files and committed local files in the comparison set when they are under `docs/ahawiki.net/`.
3. Read the current remote page with `GET /api/bot/page/<url-encoded-page-name>`.
4. Compare the remote `content` with the local file content.
5. If only the local file changed, save the full local file content with `POST /api/bot/page/<url-encoded-page-name>`.
6. If only the remote page changed, update the local file from the remote `content`.
7. If both local and remote changed, do not overwrite either side blindly; merge or ask the user.
8. When saving, use the remote `revision` from the read response. If the page is missing, use `revision: 0`.
9. On `409 Conflict`, re-read the remote page and resolve the conflict before saving.
10. Use a clear save `comment` and set `minorEdit` according to the change.
11. Verify by reading the page again and comparing the remote `content` with the local file.

Do not commit or write API keys into this repository. Use a user-provided key for the current session or an environment variable such as `AHAWIKI_API_KEY`.
