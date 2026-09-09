// The risk in this repair is not the links it changes, it is the text that looks like one.
// `[PHP]` appears in prose about markup as often as it appears as a link, and a page that
// documents the syntax shows the very shape being rewritten. So the offsets, and what is left
// alone, are what these pin.
import test from 'node:test';
import assert from 'node:assert/strict';
import { planFor, maskUnlinkable } from '../scripts/fix-case-miss-links.mjs';

test('a link that misses the page by case is pointed at it, keeping what the reader sees', () => {
    const plan = planFor(' * [CSharp C#], [ASP.NET]\n');
    assert.equal(plan.kind, 'rewrite');
    assert.equal(plan.next, ' * [CSharp C#], [Asp.Net|ASP.NET]\n');
});

test('the wiki: prefix is dropped, because the renderer drops it too', () => {
    // AhaMarkLink.uriNormalized strips it, so [wiki:PHP] already means the page PHP.
    assert.equal(planFor('  * [wiki:PHP]\n').next, '  * [Php|PHP]\n');
});

test('a spelling that is already the page name is left alone', () => {
    assert.equal(planFor(' * [Perl], [Python], [Php], [Ruby]\n').kind, 'nothing-to-do');
});

test('markup shown rather than used is not a link', () => {
    for (const source of [
        'write `[PHP]` to link it',
        '[[[#!Text\n[PHP]\n]]]',
        '[[[#!WikiSyntaxPreview\n * [ASP.NET]\n]]]',
        '[[Include([PHP])]]',
    ]) assert.equal(planFor(source).kind, 'nothing-to-do', source);
});

test('an escaped link is text', () => {
    assert.equal(planFor('\\[PHP]').kind, 'nothing-to-do');
});

test('a link that already says what it means is left to its author', () => {
    // Both already carry an alias: the author said where it goes and what to call it.
    assert.equal(planFor('[Php|PHP]').kind, 'nothing-to-do');
    assert.equal(planFor('[Php PHP manual]').kind, 'nothing-to-do');
});

test('masking keeps every offset where it was', () => {
    // Without this the rewrite lands in the wrong place, which is worse than not running at all.
    const source = '[[[#!Text\nfoo\n]]]\n`bar`\n[[Kbd(f)]]\n[PHP]\n';
    const masked = maskUnlinkable(source);
    assert.equal(masked.length, source.length);
    assert.equal(masked.indexOf('[PHP]'), source.indexOf('[PHP]'));
    // Newlines survive, so a line number counted from an offset is still the right line.
    assert.equal(masked.split('\n').length, source.split('\n').length);
});

test('several links on one line are all rewritten, and the later ones do not shift', () => {
    const plan = planFor(' * [PHP], [ASP]\n');
    assert.equal(plan.edits.length, 2);
    assert.equal(plan.next, ' * [Php|PHP], [Asp|ASP]\n');
});
