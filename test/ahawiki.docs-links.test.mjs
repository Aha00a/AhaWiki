// AhaMark reads `[Dev Api]` as a link to `Dev` labelled `Api`, because the alternative that
// claims a target and an alias comes before the one that would take the whole bracket as a name.
// The rendered page then says "Dev Api" and goes somewhere else, so the mistake survives being
// looked at. It reached these pages twice: `[Dev Editor]` in 2026-09, then `[Dev Api]` and
// `[Dev SisterWiki]`, each found only by reading the HTML.
//
// This is the shape that can be checked without judgement: the bracket holds spaces, and the
// whole of it names a page that exists. Quoting it, or writing `[Name|alias]`, says what was
// meant. The grammar itself is pinned in InterpreterWikiLinkSpec.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { docsGitPath, manifestFileName, rootDir } from '../scripts/lib/ahawiki.net.mjs';
import { regexLink, maskUnlinkable } from '../scripts/lib/ahamark.mjs';

/** What the link pattern sees. Variables, blocks, macros and backticks are taken out before it. */
const linkableText = maskUnlinkable;

function committedPages() {
    const directory = path.join(rootDir, ...docsGitPath.split('/'));
    return fs.readdirSync(directory)
        .filter(name => name !== manifestFileName)
        .filter(name => fs.statSync(path.join(directory, name)).isFile());
}

test('a link to a page whose name has spaces says so, with quotes or a pipe', () => {
    const directory = path.join(rootDir, ...docsGitPath.split('/'));
    const pages = committedPages();
    const exists = new Set(pages);
    const wrong = [];

    for (const page of pages) {
        const text = linkableText(fs.readFileSync(path.join(directory, page), 'utf8'));
        for (const match of text.matchAll(regexLink)) {
            if (match[1] || match[2]) continue;          // escaped, or a bare URL
            if (match[9] === undefined) continue;        // not the target-and-alias alternative
            const whole = `${match[9]} ${match[10]}`.trim();
            if (exists.has(whole))
                wrong.push(`${page}: [${whole}] links to "${match[9]}" and labels it "${match[10]}"`);
        }
    }

    assert.deepEqual(wrong, [], `A link reads as the page named by the whole bracket but goes to its first word:\n  ` +
        `${wrong.join('\n  ')}\n\nWrite ["Page Name"] or [Page Name|alias].`);
});

test('the check can tell the two shapes apart', () => {
    // Without this, the test above passes just as well when the pattern stops matching anything.
    const withSpaces = [...linkableText('see [Dev Api] there').matchAll(regexLink)][0];
    assert.equal(withSpaces[9], 'Dev');
    assert.equal(withSpaces[10], 'Api');

    const quoted = [...linkableText('see ["Dev Api"] there').matchAll(regexLink)][0];
    assert.equal(quoted[3], 'Dev Api');
    assert.equal(quoted[9], undefined);

    const piped = [...linkableText('see [Dev Api|개발] there').matchAll(regexLink)][0];
    assert.equal(piped[4], 'Dev Api');

    // A macro is not a link, and would otherwise be read as one -- `[` is neither `]` nor space.
    assert.deepEqual([...linkableText('[[Kbd(f)]]').matchAll(regexLink)], []);
});
