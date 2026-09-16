// Unit tests for the slide-deck index math (public/js/AhaWiki.Slide.js). These are the pure
// functions the DOM controller navigates with; the file returns before any DOM wiring when there is
// no `document`, so it loads cleanly in a vm context.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import test from 'node:test';
import vm from 'node:vm';

const code = fs.readFileSync(new URL('../public/js/AhaWiki.Slide.js', import.meta.url), 'utf8');
const ctx = { window: {} };
vm.createContext(ctx);
vm.runInContext(code, ctx);
const Slide = ctx.window.AhaWiki.Slide;

test('clampIndex keeps the index within the deck', () => {
    assert.equal(Slide.clampIndex(-3, 5), 0);
    assert.equal(Slide.clampIndex(0, 5), 0);
    assert.equal(Slide.clampIndex(4, 5), 4);
    assert.equal(Slide.clampIndex(9, 5), 4);
    assert.equal(Slide.clampIndex(2, 0), 0); // empty deck
});

test('nextIndex advances and clamps at both ends', () => {
    assert.equal(Slide.nextIndex(0, 3, 1), 1);
    assert.equal(Slide.nextIndex(2, 3, 1), 2); // clamp at last
    assert.equal(Slide.nextIndex(0, 3, -1), 0); // clamp at first
    assert.equal(Slide.nextIndex(1, 3, -1), 0);
});

test('parseHashIndex reads #/n as a zero-based, clamped index', () => {
    assert.equal(Slide.parseHashIndex('#/1', 3), 0);
    assert.equal(Slide.parseHashIndex('#/3', 3), 2);
    assert.equal(Slide.parseHashIndex('#/9', 3), 2); // clamp past the end
    assert.equal(Slide.parseHashIndex('#/0', 3), 0); // 1-based 0 -> -1 -> clamp to 0
});

test('parseHashIndex ignores a hash that is not a slide reference', () => {
    assert.equal(Slide.parseHashIndex('#Section', 3), null);
    assert.equal(Slide.parseHashIndex('', 3), null);
    assert.equal(Slide.parseHashIndex('#/', 3), null);
    assert.equal(Slide.parseHashIndex('#/2x', 3), null);
});

test('hashForIndex is the inverse of parseHashIndex', () => {
    assert.equal(Slide.hashForIndex(0), '#/1');
    assert.equal(Slide.hashForIndex(2), '#/3');
    assert.equal(Slide.parseHashIndex(Slide.hashForIndex(4), 10), 4);
});
