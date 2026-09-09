// The index rewrite is the part that can quietly damage a page: it edits the text of a page that
// is otherwise fine, and every link it touches is one a reader follows.
import test from 'node:test';
import assert from 'node:assert/strict';
import { rewriteLinks, simplifyAliases } from '../scripts/wikipedia-case-renames.mjs';

const moved = new Map([['Aws EC2', 'AWS EC2'], ['Aws S3', 'AWS S3'], ['AwsCli', 'AWS CLI']]);
const renamed = new Map([['Php', 'PHP'], ['Asp', 'ASP'], ['Asp.Net', 'ASP.NET']]);

test('a quoted link keeps its quotes, and a name that gains a space gets them', () => {
    assert.equal(rewriteLinks(' * ["Aws EC2"]\n', moved).content, ' * ["AWS EC2"]\n');
    // AwsCli has no space and AWS CLI does, so the bare form will not do any more.
    assert.equal(rewriteLinks(' * [AwsCli]\n', moved).content, ' * ["AWS CLI"]\n');
});

test('several links on one line all move, and the later ones do not shift', () => {
    const after = rewriteLinks('["Aws EC2"], ["Aws S3"], [AwsCli]\n', moved);
    assert.equal(after.count, 3);
    assert.equal(after.content, '["AWS EC2"], ["AWS S3"], ["AWS CLI"]\n');
});

test('a page that is not moving is left alone', () => {
    // "Aws Lambda" is not in this map, so nothing about it may change.
    assert.equal(rewriteLinks(' * ["Aws Lambda"]\n', moved).changed, false);
});

test('markup shown rather than used is not a link', () => {
    for (const source of [
        'write `["Aws EC2"]` to link it',
        '[[[#!Text\n["Aws EC2"]\n]]]',
        '[[Include(["Aws EC2"])]]',
    ]) assert.equal(rewriteLinks(source, moved).changed, false, source);
});

test('a link that already carries an alias is left to its author', () => {
    // [Aws EC2|우리 EC2] says what the author wanted it called; retargeting it is a separate
    // decision, and the redirect keeps it working either way.
    assert.equal(rewriteLinks('[Aws EC2|우리 EC2]\n', moved).changed, false);
});

test('the wiki: prefix is understood, because the renderer strips it', () => {
    assert.equal(rewriteLinks('[wiki:AwsCli]\n', moved).content, '["AWS CLI"]\n');
});

test('a pipe the rename made pointless collapses', () => {
    const after = simplifyAliases(' * [Php|PHP], [Asp|ASP]\n', renamed);
    assert.equal(after.count, 2);
    assert.equal(after.content, ' * [PHP], [ASP]\n');
});

test('a pipe that still says something is left alone', () => {
    // FFmpeg was never renamed, so [FFmpeg|ffmpeg] is the author showing the command name.
    assert.equal(simplifyAliases('[FFmpeg|ffmpeg] 설치 필요\n', renamed).changed, false);
    // And an alias that is not the new name is a label, not a workaround.
    assert.equal(simplifyAliases('[Php|피에이치피]\n', renamed).changed, false);
});

test('collapsing does not touch a link written without a pipe', () => {
    // [Php] still reaches the page through the redirect; rewriting it is a different decision.
    assert.equal(simplifyAliases(' * [Perl], [Php], [Ruby]\n', renamed).changed, false);
});
