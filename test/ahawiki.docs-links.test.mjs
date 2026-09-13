// Since 2026-09-14 AhaMark reads `[Dev Api]` as the page "Dev Api". Before, it read the page `Dev`
// labelled `Api`, and that trap reached these pages twice -- `[Dev Editor]`, then `[Dev Api]` and
// `[Dev SisterWiki]` -- each found only by reading the HTML, since the page showed the whole text.
//
// The trap now points the other way: a label written in the old habit, `[CSharp C#]`, links to a
// page named "CSharp C#". The shape that can be checked without judgement is a spaced bracket
// whose whole text names no page while its first word does. Write `[CSharp|C#]` for a label.
// The grammar itself is pinned in InterpreterWikiLinkSpec.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { docsGitPath, manifestFileName, rootDir } from '../scripts/lib/ahawiki.net.mjs';
import { regexLink, maskUnlinkable, linkTarget } from '../scripts/lib/ahamark.mjs';

/** What the link pattern sees. Variables, blocks, macros and backticks are taken out before it. */
const linkableText = maskUnlinkable;

function committedPages() {
    const directory = path.join(rootDir, ...docsGitPath.split('/'));
    return fs.readdirSync(directory)
        .filter(name => name !== manifestFileName)
        .filter(name => fs.statSync(path.join(directory, name)).isFile());
}

// A date names a page whether or not one was saved (DefaultPageLogic), so `[2026-09-13 today]`
// in the old habit is the same mistake as `[CSharp C#]`.
const isDatePage = name => /^\d{4}(-\d{2}(-\d{2})?)?$/.test(name) || /^--\d{2}(-\d{2})?$/.test(name);

test('a label is written after a pipe, not after a space', () => {
    const directory = path.join(rootDir, ...docsGitPath.split('/'));
    const pages = committedPages();
    const exists = new Set(pages);
    const wrong = [];

    for (const page of pages) {
        const text = linkableText(fs.readFileSync(path.join(directory, page), 'utf8'));
        for (const match of text.matchAll(regexLink)) {
            if (match[1]) continue;                      // escaped
            const whole = match[11];                     // [Page Name], the whole-text alternative
            if (whole === undefined || !/\s/.test(whole)) continue;
            const first = whole.split(/\s+/)[0];
            if (!exists.has(whole) && (exists.has(first) || isDatePage(first)))
                wrong.push(`${page}: [${whole}] links to a page named "${whole}", not to "${first}"`);
        }
    }

    assert.deepEqual(wrong, [], `A link names no page, but its first word does: a label in the old habit:\n  ` +
        `${wrong.join('\n  ')}\n\nWrite [Page|label] for a label, or ["Page Name"] for the whole name.`);
});

test('the check can tell the shapes apart', () => {
    // Without this, the test above passes just as well when the pattern stops matching anything.
    const withSpaces = [...linkableText('see [Dev Api] there').matchAll(regexLink)][0];
    assert.equal(withSpaces[11], 'Dev Api');

    const quoted = [...linkableText('see ["Dev Api"] there').matchAll(regexLink)][0];
    assert.equal(quoted[3], 'Dev Api');
    assert.equal(quoted[11], undefined);

    const piped = [...linkableText('see [Dev Api|개발] there').matchAll(regexLink)][0];
    assert.equal(piped[4], 'Dev Api');

    // A target that is not a page title keeps the label after its first space.
    const labelled = [...linkableText('see [schema:Person 사람] there').matchAll(regexLink)][0];
    assert.equal(labelled[9], 'schema:Person');
    assert.equal(labelled[10], '사람');

    // A macro is not a link, and would otherwise be read as one -- `[` is neither `]` nor space.
    assert.deepEqual([...linkableText('[[Kbd(f)]]').matchAll(regexLink)], []);
});

// The other shape a page name gets written in by mistake: `[[Dev Kanban Realtime]]`. It is not a
// macro -- a macro name has no spaces, so the renderer does not take it out -- and the link pattern
// then starts at the first bracket: the target is "[Dev Kanban Realtime", a page that cannot
// exist, and the reader sees a red link and a stray `]` after it. Two of these sat on
// Dev WebSocket until 2026-09-12; a scan for links to pages the wiki does not have found them.
test('a page name is not written in double brackets', () => {
    const directory = path.join(rootDir, ...docsGitPath.split('/'));
    const wrong = [];

    for (const page of committedPages()) {
        const content = fs.readFileSync(path.join(directory, page), 'utf8');
        for (const match of linkableText(content).matchAll(regexLink)) {
            const target = linkTarget(match);
            if (target && target.startsWith('[')) {
                const line = content.slice(0, match.index).split('\n').length;
                wrong.push(`${page}:${line}: ${match[0]}]`);
            }
        }
    }

    assert.deepEqual(wrong, [], `A page name in double brackets renders as a red link to "[Name":\n  ${wrong.join('\n  ')}\n\n` +
        'Write ["Page Name"] instead.');
});

test('the double-bracket check sees the mistake and nothing else', () => {
    const targets = text => [...linkableText(text).matchAll(regexLink)].map(linkTarget);
    assert.deepEqual(targets('see [[Dev Kanban Realtime]] there'), ['[Dev Kanban Realtime']);
    assert.deepEqual(targets('see ["Dev Kanban Realtime"] there'), ['Dev Kanban Realtime']);
    assert.deepEqual(targets('[[Br]] and [[Kbd(Alt W)]]'), [], 'macros are taken out first');
});
