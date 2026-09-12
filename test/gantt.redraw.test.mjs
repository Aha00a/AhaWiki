// A Gantt chart is drawn again on zoom, 100%, Fit and collapse, from the same parsed nodes. Until
// 2026-09-12 the previous draw's dates were left on them, and calcDates -- which takes a reference
// only when its target already has an end -- read those: a reference to a row further down started
// working from the second draw, and a row referring to its own parent moved a business day later
// with every redraw. Each draw now starts from no dates. These run the real file, not a copy.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync('public/js/AhaWiki.Gantt.js', 'utf8').replace(/^﻿/, '');

function loadGantt() {
    const sandbox = { window: {}, document: { addEventListener() {} } };
    vm.createContext(sandbox);
    vm.runInContext(source, sandbox);
    return sandbox.window.__AhaWikiGanttTestHooks;
}

// B refers to C, which is further down; c2 refers to p, its own parent.
const chart = [
    'A\t2026-01-05\t3',
    'B\tC\t2',
    'C\t2026-02-02\t2',
    'P#p\t2026-01-05',
    '\tc1\t2026-01-05\t2',
    '\tc2\tp\t1',
].join('\n');

function starts(gantt, roots) {
    const out = {};
    const walk = nodes => nodes.forEach(node => { out[node.name] = gantt.fmtDate(node.start); walk(node.children); });
    walk(roots);
    return out;
}

test('drawing a chart again moves none of its dates', () => {
    const gantt = loadGantt();
    const roots = gantt.parseGantt(chart);
    gantt.buildAll(roots, {}, 20, 1);
    const first = starts(gantt, roots);
    gantt.buildAll(roots, {}, 20, 1);
    gantt.buildAll(roots, {}, 20, 1);
    assert.deepEqual(starts(gantt, roots), first);
});

test('a reference to a row below, or to the row\'s own parent, is not used on any draw', () => {
    const gantt = loadGantt();
    const roots = gantt.parseGantt(chart);
    gantt.buildAll(roots, {}, 20, 1);
    gantt.buildAll(roots, {}, 20, 1);
    const at = starts(gantt, roots);
    assert.equal(at.B, '2026-01-08', 'B follows A, as it would with no reference');
    assert.equal(at.c2, '2026-01-07', 'c2 follows c1, not its parent');
});
