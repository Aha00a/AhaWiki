// Unit tests for AhaWiki.EditHelpContext.kindAt (public/js/AhaWiki.EditHelpContext.js): which row
// of the editor's formatting help the cursor is on. The kinds are EditHelp.scala's; the last test
// reads that file so a kind the help no longer has fails here by name.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import test from 'node:test';
import vm from 'node:vm';

const code = fs.readFileSync(new URL('../public/js/AhaWiki.EditHelpContext.js', import.meta.url), 'utf8');
const ctx = { window: {} };
vm.createContext(ctx);
vm.runInContext(code, ctx);
const { kindAt } = ctx.window.AhaWiki.EditHelpContext;

// The cursor is where | is; the | is taken out of the text.
function kindOf(textWithCursor) {
    const lines = textWithCursor.split('\n');
    const line = lines.findIndex((l) => l.includes('|'));
    const ch = lines[line].indexOf('|');
    lines[line] = lines[line].replace('|', '');
    return kindAt((i) => lines[i], line, ch);
}

test('a line is what it starts with', () => {
    assert.equal(kindOf('== 제|목'), 'heading');
    assert.equal(kindOf(' * 항|목'), 'list');
    assert.equal(kindOf('  * 안쪽 항|목'), 'list');
    assert.equal(kindOf(' 1. 첫|째'), 'numbered');
    assert.equal(kindOf(' * [ ] 할 |일'), 'checkbox');
    assert.equal(kindOf(' * [[CB(x)]] 끝낸 |일'), 'checkbox');
    assert.equal(kindOf('--|--'), 'hr');
});

test('a line without a leading space is not a list -- the mistake the help warns about', () => {
    assert.equal(kindOf('* 항|목'), '');
    assert.equal(kindOf('1. 첫|째'), '');
});

test('the inline markup under the cursor wins over the line it is on', () => {
    assert.equal(kindOf(' * [Front|Page] 보기'), 'link');
    assert.equal(kindOf(" * '''굵|게'''"), 'emphasis');
    assert.equal(kindOf(' * 여기 `co|de` 있음'), 'code');
    assert.equal(kindOf(' * [Front|Page|첫 화면]'), 'link');
});

test('the same line away from the inline markup is the line', () => {
    assert.equal(kindOf(' * [FrontPage] 보|기'), 'list');
});

test('a checkbox and a macro are not links', () => {
    assert.equal(kindOf(' * [ |] 할 일'), 'checkbox');
    assert.equal(kindOf('[[Include(문서|이름)]]'), 'include');
});

test('markup inside backticks is code', () => {
    assert.equal(kindOf("`'''굵|게'''`"), 'code');
});

test('inside a block, the block is the answer, whatever the line looks like', () => {
    assert.equal(kindOf('[[[#!Kanban\n=== 할 일\n==== 첫 카|드\n]]]'), 'kanban');
    assert.equal(kindOf('[[[#!Table tsv 1\n이름\t역|할\n]]]'), 'table');
    assert.equal(kindOf('[[[#!Vim python\nprint(|1)\n]]]'), 'vim');
    assert.equal(kindOf('[[[#!Mer|maid\ngraph TD\n]]]'), 'mermaid');
    assert.equal(kindOf('[[[#!Fold 자세히\n내용\n]|]]'), 'fold');
});

test('after a block has closed, the line is itself again', () => {
    assert.equal(kindOf('[[[#!Table tsv\na\tb\n]]]\n == 제|목'), '');
    assert.equal(kindOf('[[[#!Table tsv\na\tb\n]]]\n== 제|목'), 'heading');
});

test('a block with no #! line is text, which has no row', () => {
    assert.equal(kindOf('[[[\n그대|로\n]]]'), 'text');
});

test('every kind the help lists can be answered, and every answer but text has a row', () => {
    const scala = fs.readFileSync(new URL('../app/logics/EditHelp.scala', import.meta.url), 'utf8');
    const kinds = new Set([...scala.matchAll(/Example\("[^"]*", "(?:[^"\\]|\\.)*", "([a-z]+)"/g)].map((m) => m[1]));
    assert.ok(kinds.size > 15, `found ${kinds.size} kinds in EditHelp.scala`);
    const answered = new Set(['heading', 'list', 'numbered', 'checkbox', 'hr', 'link', 'emphasis', 'code', 'include',
        'table', 'vim', 'fold', 'quote', 'mermaid', 'kanban', 'graph', 'gantt', 'math', 'map']);
    assert.deepEqual([...kinds].sort(), [...answered].sort());
});
