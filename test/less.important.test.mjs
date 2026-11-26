// `!important` in a stylesheet is for what no selector can do. A selector, however specific,
// loses to a style written on the element itself, and a rule that has to beat every other rule
// on a page cannot know how specific the next one written will be. Anything else is an argument
// between two rules, and the more specific one is meant to win it -- kanban.less carried sixty
// of them until 2026-11-26, and seven had a reason (wiki page Dev Less, «!important 는 선택자로
// 이길 수 없는 것에만»).
//
// This reads every .less and fails, by name, for an `!important` that is not listed below. To
// add one is to add its line here, with what it outranks.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { assetsDir } from '../scripts/less.mjs';

// The ones with a reason: what each outranks, which no selector would.
const outranksWhatNoSelectorCan = [
    { file: 'kanban.less', declaration: 'transform: none', times: 4,
        outranks: 'the transform Sortable writes on an element it slides into place -- the faded copy of a dragged card or list, and the slot for it, are drawn at once where they are' },
    { file: 'kanban.less', declaration: 'opacity: 0.64', times: 1,
        outranks: 'the `opacity: 0.8` Sortable writes on the copy of a card or a list that it drags; one rule for the two copies' },
    { file: 'kanban.less', declaration: 'cursor: grabbing', times: 1,
        outranks: 'every rule on the page that gives something a cursor, while a card or a list is dragged over it' },
];
// The ones written before this test and not looked into since. Whether a selector would do in
// their place is not known; one that is looked into moves to the list above or goes.
const notLookedInto = [
    { file: '_wiki_table.less', declaration: 'text-align: center', times: 1 },
    { file: 'edit.less', declaration: 'cursor: cell', times: 1 },
    { file: 'edit.less', declaration: 'cursor: grabbing', times: 1 },
    { file: 'less.less', declaration: 'display: none', times: 1 },
    { file: 'wiki.less', declaration: 'display: none', times: 1 },
];

// The declarations in a stylesheet's text that carry the flag, each with how often it is there.
// Comments are dropped first: the ones in kanban.less say `!important` to explain it.
const flagged = (text) => {
    const counted = new Map();
    const code = text.replace(/\/\*[\s\S]*?\*\//g, '').replace(/(^|\s)\/\/.*$/gm, '$1');
    for (const [, property, value] of code.matchAll(/([\w-]+)\s*:\s*([^;{}]*?)\s*!important/g)) {
        const declaration = `${property}: ${value.replace(/\s+/g, ' ')}`;
        counted.set(declaration, (counted.get(declaration) || 0) + 1);
    }
    return counted;
};

test('a comment that says !important is not one, and a declaration is', () => {
    const counted = flagged([
        'a { color: red !important; }  // not this: b { color: blue !important; }',
        '/* nor this:',
        '   c { color: green !important; } */',
        'd { background: url(//example.org/x.png) !important; color: red   !important }',
    ].join('\n'));
    assert.deepEqual([...counted], [['color: red', 2], ['background: url(//example.org/x.png)', 1]]);
});

test('every !important in a stylesheet is one this file lists, with what it outranks', () => {
    const listed = new Map();
    for (const { file, declaration, times } of [...outranksWhatNoSelectorCan, ...notLookedInto]) listed.set(`${file}: ${declaration}`, times);

    const written = new Map();
    for (const file of fs.readdirSync(assetsDir).filter(name => name.endsWith('.less')).sort()) {
        for (const [declaration, times] of flagged(fs.readFileSync(path.join(assetsDir, file), 'utf8'))) written.set(`${file}: ${declaration}`, times);
    }

    const unlisted = [...written].filter(([key, times]) => times > (listed.get(key) || 0)).map(([key, times]) => `${key} !important -- ${times} written, ${listed.get(key) || 0} listed`);
    assert.deepEqual(unlisted, [], `A stylesheet has an !important that test/less.important.test.mjs does not list:\n  ${unlisted.join('\n  ')}\n\n`
        + 'If it is winning against another rule, make its selector the more specific one instead. If it outranks a style\n'
        + 'written on the element, or has to beat every rule on a page, add it to the list with what it outranks.');

    const gone = [...listed].filter(([key, times]) => times > (written.get(key) || 0)).map(([key, times]) => `${key} !important -- ${times} listed, ${written.get(key) || 0} written`);
    assert.deepEqual(gone, [], `test/less.important.test.mjs lists an !important that is no longer written; take it off the list:\n  ${gone.join('\n  ')}`);
});
