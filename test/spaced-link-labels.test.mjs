// The rewrite has to keep what each link said the day before the grammar changed, and leave alone
// everything the renderer never read as a link. These pin both, and the offsets.
import test from 'node:test';
import assert from 'node:assert/strict';
import { planRevision, readNow } from '../scripts/spaced-link-labels.mjs';

const pages = new Set(['CSharp', 'Font', 'Font Awesome', 'KR']);
const plan = (content, options = {}) => planRevision({ content, comment: '' }, { exists: name => pages.has(name), ...options });

test('a label written after a space gets a pipe, so it still reads as a label', () => {
    assert.equal(plan(' * [CSharp C#], [KR 한국]\n').content, ' * [CSharp|C#], [KR|한국]\n');
});

test('a link whose first word names no page is left to be read as one name', () => {
    assert.equal(plan('[Forrest Gump] 보다.').content, '[Forrest Gump] 보다.');
});

test('when the whole text names a page as well, the whole name wins', () => {
    assert.equal(plan('[Font Awesome]').content, '[Font Awesome]');
});

test('a target that is not a page title keeps its label anyway, so it is not touched', () => {
    for (const link of ['[schema:Person 사람]', '[#section 절]', '[?q=1 검색]', '[https://aha00a.com 홈]', '[CSharp#History 역사]', '[wiki:CSharp C#]'])
        assert.equal(plan(link).content, link, link);
});

test('markup shown rather than used is not touched', () => {
    for (const source of ['`[CSharp C#]`', '``[CSharp C#]``', '[[Include([CSharp C#])]]', '[[[#!Vim\n[CSharp C#]\n]]]', '\\[CSharp C#]', '#!redirect CSharp\n[CSharp C#]'])
        assert.equal(plan(source).content, source, source);
});

test('a block rendered as wiki is read inside, and one that is not is left', () => {
    assert.equal(plan('[[[#!Quote\n[CSharp C#]\n]]]').content, '[[[#!Quote\n[CSharp|C#]\n]]]');
    assert.equal(plan('[[[#!WikiSyntaxPreview\n[CSharp C#]\n]]]').content, '[[[#!WikiSyntaxPreview\n[CSharp|C#]\n]]]');
    assert.equal(plan('[[[#!Text\n[CSharp C#]\n]]]').content, '[[[#!Text\n[CSharp C#]\n]]]');
});

test('a table splits its cells first, so a bracket across a delimiter or holding a quote is no link', () => {
    assert.equal(plan('[[[#!Table tsv\n[CSharp C#]\tx\n]]]').content, '[[[#!Table tsv\n[CSharp|C#]\tx\n]]]');
    assert.equal(plan('[[[#!Table tsv\n[CSharp\tC#]\n]]]').content, '[[[#!Table tsv\n[CSharp\tC#]\n]]]');
    assert.equal(plan('[[[#!Table csv\n[CSharp C#, x]\n]]]').content, '[[[#!Table csv\n[CSharp C#, x]\n]]]');
    assert.equal(plan('[[[#!Table tsv\n"[""CSharp C#""]"\n]]]').content, '[[[#!Table tsv\n"[""CSharp C#""]"\n]]]');
});

test('a comment is rendered inline, so it is rewritten too', () => {
    const result = planRevision({ content: '', comment: 'fix [CSharp C#]' }, { exists: name => pages.has(name) });
    assert.equal(result.comment, 'fix [CSharp|C#]');
});

test('a link review chose to read as a label is rewritten although its first word names nothing', () => {
    const labelled = before => before === '[la Latin]';
    assert.equal(plan('[la Latin] [el Greek]', { labelled }).content, '[la|Latin] [el Greek]');
});

test('inline code in the label stays where it was', () => {
    // The renderer takes the code out before reading links, so it was part of the label.
    assert.equal(plan('[CSharp `C#` 언어]').content, '[CSharp|`C#` 언어]');
});

test('offsets hold across several links and lines, and the text keeps its length', () => {
    const source = '[KR 한국] and [CSharp C#]\n`x` [CSharp C#]\n';
    const { content } = plan(source);
    assert.equal(content, '[KR|한국] and [CSharp|C#]\n`x` [CSharp|C#]\n');
    assert.equal(content.length, source.length);
});

test('the current grammar is asked what it makes of each old link', () => {
    assert.deepEqual(readNow('[CSharp C#]'), { kind: 'name', name: 'CSharp C#' });
    assert.deepEqual(readNow('[schema:Person 사람]'), { kind: 'label' });
    assert.deepEqual(readNow('[a b ]'), { kind: 'none' });
});
