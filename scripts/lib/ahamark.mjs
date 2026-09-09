// The two facts every script that reads AhaMark links needs: what a link looks like, and what
// the renderer has already taken out of the text before it looks for one.
//
// Both were copied into four files -- the audit, two repair scripts and a test -- and the copies
// had already drifted. Three of them masked macros with `\[\[[^\]]*\]\]`, which stops at the
// first `]`, so `[[Include(["Aws EC2"])]]` was not masked at all and the link inside it counted
// as a real link. This repository refactors at the second copy, not the third; that rule was
// waiting on a fourth here.
//
// Anything here is a claim about app/logics/wikis/. Change it only alongside that code.

/**
 * The alternatives of InterpreterWiki.regexLink, in the order it tries them.
 *
 * Groups: 1 escape, 2 URL, 3 ["Page"], 4+5 [Page|alias], 6 [Page], 7+8 ["Page" alias],
 * 9+10 [Page alias].
 */
export const regexLink =
    /((?<!\\)\\)?(?:([a-zA-Z][-a-zA-Z0-9+._]+:\/\/\S+)|\["([^\]"]+)"\]|\[(?![?"])((?:(?!:\/\/)[^\]|])+)\|([^\]]+)\]|\[([^\]\s]+)\]|\["([^\]"]+)"\s+([^\]]+)\]|\[([^\]\s]+)\s+([^\]]+)\])/g;

/** The page an alternative names, whichever alternative matched, or undefined for a URL. */
export function linkTarget(match) {
    if (match[1] || match[2]) return undefined;              // escaped, or a bare URL
    const written = match[3] ?? match[4] ?? match[6] ?? match[7] ?? match[9];
    if (written === undefined) return undefined;
    // AhaMarkLink.uriNormalized drops the prefix, so [wiki:Page] already means the page Page.
    return (written.startsWith('wiki:') ? written.slice(5) : written).trim();
}

/**
 * Blank what the link pattern never sees, keeping every offset where it was.
 *
 * Backticks, `[[[blocks]]]` and `[[macros]]` come out of the text before links are read, so a
 * scan of raw source calls every documented example a real link. Regions are replaced with the
 * same number of characters -- and blocks keep their newlines -- so an offset into the mask is
 * an offset into the source, and a line counted from one is the right line.
 *
 * The macro pattern is ExtractConvertInjectMacro.regex: the argument is lazy-any, so it spans a
 * `]` and `[[Include(["Aws EC2"])]]` masks whole.
 */
export function maskUnlinkable(content) {
    const blank = match => ' '.repeat(match.length);
    return content
        .replace(/`[^`\n]*`/g, blank)
        .replace(/\[\[\[[\s\S]*?\]\]\]/g, match => match.replace(/[^\n]/g, ' '))
        .replace(/\[\[(\w*)(?:\((.*?)\))?\]\]/g, blank);
}
