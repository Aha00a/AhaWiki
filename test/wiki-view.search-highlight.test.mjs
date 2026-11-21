// What the page highlights when the reader came from this wiki's search, read out of the page
// script itself. A two-word search highlighted nothing until 2026-11-21: the search box is a GET
// form, so the space arrived as `+`, and decodeURIComponent leaves `+` alone.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const pageScript = fs.readFileSync(new URL('../public/js/AhaWiki.PageView.js', import.meta.url), 'utf8');

function load() {
    const start = pageScript.indexOf('function searchTermFromReferrer(');
    const end = pageScript.indexOf('var searchTerm = ', start);
    assert.ok(start > 0 && end > start, 'the function is where the test expects it');
    const context = { URL };
    vm.createContext(context);
    vm.runInContext(pageScript.slice(start, end) + '\nthis.f = searchTermFromReferrer;', context);
    return context.f;
}

const origin = 'https://wiki.example';

test('a search of several words gives the words, with their spaces', () => {
    const f = load();
    assert.equal(f(`${origin}/search?q=play+framework`, origin), 'play framework');
    assert.equal(f(`${origin}/search?q=play%20framework`, origin), 'play framework');
    assert.equal(f(`${origin}/search?q=%ED%95%9C%EA%B8%80+%EA%B2%80%EC%83%89`, origin), '한글 검색');
});

test('a one-word search still works, and a + typed into the box is kept', () => {
    const f = load();
    assert.equal(f(`${origin}/search?q=scala`, origin), 'scala');
    assert.equal(f(`${origin}/search?q=c%2B%2B`, origin), 'c++');
});

test('only this wiki\'s search page counts', () => {
    const f = load();
    assert.equal(f('https://elsewhere.example/search?q=x', origin), '');
    assert.equal(f(`${origin}/w/Page?q=x`, origin), '');
    assert.equal(f(`${origin}/search`, origin), '');
    assert.equal(f('', origin), '');
    assert.equal(f('not a url', origin), '');
});

test('a bare % does not throw, and another parameter is not part of the term', () => {
    const f = load();
    // decodeURIComponent threw on this; URLSearchParams keeps the % as written.
    assert.equal(f(`${origin}/search?q=100%`, origin), '100%');
    assert.equal(f(`${origin}/search?q=a&x=1`, origin), 'a');
});
