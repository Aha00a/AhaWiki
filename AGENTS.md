# Agent Instructions

When creating new files that should be part of the change, remember to stage them with `git add` so they are not omitted from the final commit or PR.

## What is written down, and where

**Documentation lives on the wiki.** The home for this repository's documentation is
`docs/ahawiki.net/` and the live pages on `ahawiki.net` it mirrors — a new document is a wiki
page (Korean, AhaMark, synced per **AhaWikiDoc Sync** below), not a new markdown file. The
developer pages hang off the [`Dev`](docs/ahawiki.net/Dev) hub. What stays in the repository as
English markdown is this file and the READMEs that sit next to the files they describe; a
standalone `docs/*.md` was the old shape, and the last five were moved to the wiki on
2026-08-19.

Nothing links to these from anywhere a newcomer looks, so each has been rediscovered at least
once by someone who could have read it instead. Add a line here when you add a document.

| | |
|---|---|
| [docs/ahawiki.net/Dev RunningLocally](docs/ahawiki.net/Dev%20RunningLocally) | What has to be arranged before `sbt run` produces a working server. Three things, each of which fails without naming itself. |
| [docs/ahawiki.net/Dev Deploying](docs/ahawiki.net/Dev%20Deploying) | `deploy.sh`, what it does, and why each of its awkward steps is there. Where it deploys comes from the environment — this repository names no machine. |
| [docs/ahawiki.net/Dev Testing](docs/ahawiki.net/Dev%20Testing) | How the specs get a schema, what building it from the committed dump found, and what it still does not cover. |
| [docs/ahawiki.net/Dev Resources](docs/ahawiki.net/Dev%20Resources) | Why the app reads its files off the classpath, where the default pages live and why, and how to check nothing has gone back to relative paths. |
| [docs/ahawiki.net/Dev Database](docs/ahawiki.net/Dev%20Database) | The database itself: what the columns mean and which timezone they are in. |
| [docs/kanban-fixtures/](docs/kanban-fixtures/) | Input and expected output for the Kanban round-trip tests. They once spent two months gone without a build noticing; `test/kanban.roundtrip-fixtures.test.mjs` now fails by name when one is missing, and runs any new directory without being told about it. |
| [test/manual/editor-keys/](test/manual/editor-keys/) | Presses Tab and Enter in a real CodeMirror with the real editor scripts, without needing a server or a login. Not part of `npm test` — it needs a browser. |
| [scripts/README.md](scripts/README.md) | What each script is for: the admin bundle, the external-asset check (`npm run check:cdn` — run it after changing a pinned CDN version; a 404 stylesheet is silent and one went unnoticed for months), the wiki sync (**AhaWikiDoc Sync** below is its rules), and the bulk download that overwrites local edits without looking. |
| `docs/ahawiki.net/` | Copies of pages on the live wiki. Korean, and kept in step with it — see **AhaWikiDoc Sync** below. |

## Duplicated Code — Refactor at Two, Not Three (Absolute Rule)

**The moment the same code exists in two places is the moment to refactor it.** This repository does not use the common "rule of three." Extract while you are writing the second copy, not the third.

Divergence begins at the second copy, not the third. Once there are two, it becomes possible to fix only one of them, and once they differ the code itself no longer tells you which one is correct. Wait for the third and the job stops being "extract the shared part" and becomes "decide which of these three is right" — a decision with no original to check against. The second copy is also the only moment when extraction is cheap, because the two are still identical.

How to apply it:

 * Extract in the same commit that creates the second copy. "Copy it now, clean it up later" is what produces the third copy.
 * Lift it only as far as the nearest boundary the two call sites share: same file, then same module, then a shared utility. Lifting further than necessary is its own kind of coupling.
 * Similar is not the same as duplicated. The test is not whether two pieces of code look alike but whether they have a reason to change together. Merging two that merely resemble each other forces you to split them again with a flag at the next change, which is worse than the duplication was.
 * If you decide not to merge them, leave one line next to the second copy saying why. Without that, the next person redoes the same reasoning from scratch and usually just makes a third copy.
 * This is not only about code. The same *fact* written in two places — config, scripts, migrations, documentation — is governed by the same rule. Facts recorded in two places always drift apart.

## Documentation — In the Same Commit as the Work (Absolute Rule)

**Documentation is part of the work, not something written after it.** Update every document the change touches **in the same commit**. Do not defer it: no "document it later," no "code first, notes after." A change that lands without its documentation is **incomplete**.

> **Never split a change into a code commit and a documentation commit.** One change is one commit — landing the code first and the documentation after, or batching documentation into a later cleanup, are the same violation. The commit is the unit a reviewer and the next reader open, so *what changed and why* has to close inside it.

Deferred documentation usually never gets written. When it does, **only what was done survives and why it was done is gone.** The reasoning is only in your hands while you are working — what you measured, what turned out to be false, which alternative you rejected and why. A few days later even you cannot reconstruct it. A record without reasoning tells the next person **to investigate it all over again**.

Splitting the commits is its own problem. If code and documentation land separately, then in the window between them the two describe different moments in time — the last item of the duplication rule above (*facts recorded in two places always drift apart*) applies exactly.

What to leave behind:

 * **Why it was done this way.** The diff already says what was done.
 * **What turned out to be false.** Ruling out a wrong hypothesis keeps the next person off that path.
 * **What you decided not to do, and why.** A rejected alternative always comes back.
 * **How to undo it.** If you changed something irreversible, say so.
 * **Do not write values that go stale.** A fact that becomes wrong with time belongs in the code that looks it up, not in prose. A stale value is worse than none — it grants false confidence.
 * The commit message is documentation too: the subject says what, the body says why.
 * If you decide no documentation is needed, put **that decision** in the commit message.
 * Do not accumulate separate "docs cleanup" commits. Documentation detached from its change loses track of which change it describes.
 * Fix stale text the moment you notice it. **Wrong documentation is worse than none.**
 * One kind of staleness is checked for you: `test/ahawiki.docs-citations.test.mjs` fails when a wiki page cites a class, method, or file by a name that no longer exists in the source. It reads names, not structure — it cannot tell that a method moved, only that nothing is called that any more, which is what a rename actually leaves behind. A name that is real but lives outside this repository goes in that file's `namedOutsideTheRepository` list with the reason it is not here.

## Language — English, Except the Wiki Page Copies

This repository is public. Its readers are not limited to Korean speakers.

 * **Commit messages: English.** They are the most visible text a public repository has, and once pushed they cannot be edited without rewriting history. What a message should contain is covered by the documentation rule above.
 * **Code, comments, and identifiers: English.**
 * **Repository documentation: English.** `AGENTS.md` and the READMEs that sit next to the files they describe. Everything else is a wiki page copy — developer documentation is written on the wiki, in Korean, per the section above.
 * **`docs/ahawiki.net/`: Korean.** These are copies of pages on a Korean-language wiki. Translating them would put this repository's convention ahead of the readers those pages exist for. See the AhaWikiDoc Sync section below.

Being public also decides what does not belong in any of them: infrastructure hostnames, schema or service names belonging to other projects, credentials, and facts that go stale such as "service X is currently down." Point at the configuration file that holds the value instead of copying the value into prose.

Assume anything that reaches a public remote is permanent. A force-push moves the branch, but the old commits stay reachable by SHA on the hosting side, and every clone, fork, and cache keeps its own copy. Getting it right before the push is the only reliable control.

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

TODO-style documents under `docs/ahawiki.net/` should be maintained as checkbox lists. Always update the relevant checkbox state immediately after finishing the work, as part of the same change — this is required, never optional. Do not end a task with completed work still marked as an open checkbox:

 * Use `[ ]` for open work.
 * Use `[[CB(x)]]` for completed work; flip an item to `[[CB(x)]]` as soon as that item is done and verified.
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

**`scripts/sync.ahawiki.net.mjs` does this** — see `scripts/README.md` for its flags. Do not sync by hand. It was prose here for long enough that every sync re-implemented it, and the re-implementations kept dropping the sweep below.

```bash
npm run sync:ahawiki.net                                          # report, changes nothing
npm run sync:ahawiki.net -- --apply --comment="what changed"      # upload the local-ahead pages
```

It reads committed content only, compares by the page list's `contentHash` without downloading, re-reads each page immediately before saving so a concurrent browser edit is rejected rather than overwritten, and re-reads again afterwards to confirm the wiki holds what the file holds. The API paths it uses are routes in `conf/routes` under `controllers.ApiV1`; an endpoint rename breaks the script, and `conf/routes` is where the truth is.

Two things it will not decide for you:

 * A **diverged** page holds content that was never committed here — someone edited it in the browser. Read it, merge it into the local file, commit, then sync. Never overwrite either side blindly.
 * The save `comment` is required, and it should say what actually changed. The script only checks that you wrote one.

### Sweep every page, not only the ones the change touched

Syncing only the files a commit touched leaves a page that missed its sync stranded until someone edits it again. Two pages sat that way for months after a commit updated their documentation without syncing, and nothing surfaced it.

This is why the script always reports on **every** page, including with `--only`, and why the report is worth reading past the lines you expected.

Not every difference is drift. `manifest.json` is a download artifact rather than a page, and a wiki-only page may be a redirect stub someone added in the browser.

Neither are line endings. `.gitattributes` normalizes this tree to LF, but a file checked out before that attribute existed keeps its CRLF in the working tree while the committed content is already LF — so hashing the working file reports drift on a page nobody has touched. That is the concrete reason the script hashes the committed content rather than the file on disk. Fix such a file by deleting it and checking it out again, rather than by saving it to the wiki.

Do not commit or write API keys into this repository. Use a user-provided key for the current session or an environment variable such as `AHAWIKI_API_KEY`.
