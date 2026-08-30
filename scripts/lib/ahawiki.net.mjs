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
