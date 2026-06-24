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
 * [ ] Open checkbox item
 * [[CB(x)]] Checked checkbox item
 * Link to another wiki page: [PageName]
 * Link with label: [PageName Label Text]
 * Inline code uses backticks like `code`.

[[[#!Vim text
code block
multiple lines
]]]

Tables should use `InterpreterTable` blocks with TSV by default. Set the heading row count when the table has a header row, and omit it when the table has no header:

[[[#!Table tsv 1
Column A	Column B
Value A	Value B
]]]
```

TODO-style documents under `docs/ahawiki.net/` should be maintained as checkbox lists. When working on an item from a TODO document, update the relevant checkbox state as part of the same change:

 * Use `[ ]` for open work.
 * Use `[[CB(x)]]` for completed work.
 * Do not use `[x]`; only the empty checkbox form uses square brackets.
 * Keep newly discovered follow-up work as new `[ ]` items.
 * Do not leave the TODO document stale when the implementation status changes.

When a TODO-style document is fully completed, do not keep it as a historical checklist by default:

 * Move only the durable result, decisions, API behavior, test outcome, and operational notes into the relevant non-TODO reference documents.
 * Remove transient task-management details such as completed checkbox lists, staging notes, and implementation scratch notes.
 * Remove links or Kanban cards that only point to the completed TODO document.
 * Delete the completed TODO document after its necessary results have been moved.
 * Sync the deletion only after the cleanup commit is reviewed and committed, following the committed-content sync rule below.

When the user asks for `AhaWikiDoc sync`, sync committed files under `docs/ahawiki.net/` to the matching remote pages and also check for newer remote changes that should be pulled down locally. Do not upload uncommitted local edits by default; the user should review and commit local documentation changes before they become the source for remote sync. Only include uncommitted local edits in an upload if the user explicitly asks for that exception.

Use the existing `download:ahawiki.net` script in `package.json` and `scripts/download.ahawiki.net.mjs` as background for the page-list/download behavior, but prefer the Bot API for sync work. Do not use `?action=raw` for this workflow when the Bot API can provide the page metadata and content.

For sync, use remote `dateTime` and `revision` from `GET /api/bot/page/<url-encoded-page-name>` to compare local and remote state:

1. Treat the filename as the page name.
2. Compare remote pages against the committed local content under `docs/ahawiki.net/`, not against in-progress working-tree edits unless explicitly requested.
3. Read the current remote page with `GET /api/bot/page/<url-encoded-page-name>`.
4. Compare the remote `content` with the local file content.
5. If only the local file changed, save the full local file content with `POST /api/bot/page/<url-encoded-page-name>`.
6. If only the remote page changed, update the local file from the remote `content`.
7. If both local and remote changed, do not overwrite either side blindly; merge or ask the user.
8. When saving, use the remote `revision` from the read response. If the page is missing, use `revision: 0`.
9. On `409 Conflict`, re-read the remote page and resolve the conflict before saving.
10. Analyze the local/remote diff before saving, use a clear save `comment` that briefly summarizes the actual content change, and set `minorEdit` according to the change.
11. Verify by reading the page again and comparing the remote `content` with the local file.

Do not commit or write API keys into this repository. Use a user-provided key for the current session or an environment variable such as `AHAWIKI_API_KEY`.
