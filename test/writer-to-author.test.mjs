// The migration rewrites a property on 137 live pages, so the rule deciding which lines it may
// touch is worth pinning. The first version of it compared the whole page and called two pages a
// clash that were not; the second reset its block counter on "]]]" and gave the next block the
// same id as the first, which produced the same wrong answer for a different reason.
import test from 'node:test';
import assert from 'node:assert/strict';
import {planFor} from '../scripts/writer-to-author.mjs';

const block = (cls, ...lines) => [`[[[#!Schema ${cls}`, ...lines, ']]]'].join('\n');

test('rewrites writer when nothing in that block claims author', () => {
    const plan = planFor(block('Movie', 'director\tPeter Weir', 'writer\tAndrew Niccol'));
    assert.equal(plan.kind, 'rewrite');
    assert.equal(plan.hits, 1);
    assert.match(plan.next, /^author\tAndrew Niccol$/m);
    assert.doesNotMatch(plan.next, /^writer\t/m);
});

test('leaves a block that already says author to a human', () => {
    // Both in one block: author may be the novelist and writer the screenwriter, and merging
    // them would claim they are the same person.
    assert.equal(planFor(block('Movie', 'author\t윤이수', 'writer\t김민정')).kind, 'conflict');
});

test('two blocks on one page do not clash with each other', () => {
    // 구르미 그린 달빛 and 송곳 are shaped like this: the novel or comic is typed separately from
    // the drama, and each carries its own person. Both were wrongly skipped.
    const page = [block('TVSeries', 'writer\t김민정\t임예진'), '', block('Book', 'author\t윤이수')].join('\n');
    const plan = planFor(page);
    assert.equal(plan.kind, 'rewrite');
    assert.match(plan.next, /^author\t김민정\t임예진$/m);
    assert.match(plan.next, /^author\t윤이수$/m);
});

test('a third block is not confused with the first', () => {
    // The counter used to reset when a block closed, so block 3 and block 1 shared an id.
    const page = [block('ComicSeries', 'author\t최규석'), block('Rating', 'ratingValue\t5'), block('TVSeries', 'writer\t이남규')].join('\n');
    assert.equal(planFor(page).kind, 'rewrite');
});

test('a commented original is not a field and stays put', () => {
    // The converter writes the untouched row above the mapped one. Only the bare property moves.
    const plan = planFor(block('Movie', '# Writer\tAndrew Niccol', 'writer\tAndrew Niccol'));
    assert.match(plan.next, /^# Writer\tAndrew Niccol$/m);
    assert.equal(plan.hits, 1);
});

test('a page with no writer line is left alone', () => {
    assert.equal(planFor(block('Movie', 'director\tPeter Weir')).kind, 'no-writer-line');
});
