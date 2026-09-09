// Facts shared by the scripts that talk to the wiki: where it is, where the local page copies
// live, and how a page name becomes a filename. Written once because a second copy of any of
// them drifts silently — a download that names files one way and a sync that looks for them
// another way both "work" and simply never see each other's pages.
import path from "node:path";
import { fileURLToPath } from "node:url";

export const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), "..", "..");

export const baseUrl = "https://ahawiki.net";

/** Slash-separated, so it can be handed to git as-is on every platform. */
export const docsGitPath = "docs/ahawiki.net";

export const docsDir = path.join(rootDir, ...docsGitPath.split("/"));

/** A download artifact that sits among the pages without being one. */
export const manifestFileName = "manifest.json";

/**
 * A page name as a filename. Windows rejects the characters replaced here and a page name may
 * legitimately contain any of them, so they are percent-escaped rather than dropped.
 */
export function safeFileName(pageName) {
  return pageName.replace(/[<>:"/\\|?*\x00-\x1F]/g, (char) => {
    return `%${char.codePointAt(0).toString(16).toUpperCase().padStart(2, "0")}`;
  });
}

/**
 * Groups of page names that would be the same file on a case-insensitive filesystem.
 *
 * `Page.name` is `utf8mb4_bin`, so the wiki can hold `TODO NewUserFlow` and `ToDo NewUserFlow` at
 * once. Windows and macOS cannot hold both files. The download writes every page concurrently
 * into a directory it has just emptied, so without this the two race for one file, the loser is
 * gone, and the manifest lists both names as written — a page silently lost, and which one is
 * lost depends on timing. It happened on 2026-09-09, to a page that had just been renamed.
 *
 * Returns [] when nothing collides, which is the normal case.
 */
export function fileNameCollisions(pageNames) {
  const byFoldedFileName = new Map();

  for (const name of pageNames) {
    const key = safeFileName(name).toLowerCase();
    if (!byFoldedFileName.has(key)) byFoldedFileName.set(key, []);
    byFoldedFileName.get(key).push(name);
  }

  return [...byFoldedFileName.values()]
    .filter((group) => group.length > 1)
    .map((group) => [...group].sort());
}
