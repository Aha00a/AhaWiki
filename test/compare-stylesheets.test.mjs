// scripts/compare-stylesheets.mjs says whether a stylesheet change made a page worse, and its exit
// status is what a person who did not read the output goes by. Two ways it can lie are tested
// here, because neither shows in a run that happens to go well: calling a page worse that is not
// -- a pixel of rounding, a page a little taller -- which teaches people to ignore it, and calling
// a run clean in which nothing was measured. The measuring itself needs a browser and is not here.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import vm from 'node:vm';
import { execFileSync } from 'node:child_process';
import { assetsDir } from '../scripts/less.mjs';
import {
  exitStatus, inPage, linkedStylesheets, measurePage, muchHeight, readable, replaceStylesheets, reportLines, sameText,
  servedText, summarise, verdict, whenStill, withAbsoluteUrls,
} from '../scripts/lib/stylesheet-comparison.mjs';

const rootDir = path.resolve(assetsDir, '..', '..');

/** One drawing of an ordinary page; `changes` is what a test is about. */
const drawn = (changes = {}) => ({
  overflowX: 0, height: 1000, pastEdge: 0, tables: 2, tablesWiderThanBox: 0, tableWidth: 800,
  words: 100, wordsOnTwoLines: 3, examples: ['2026-11-26'], ...changes,
});
const pair = (before, after) => ({ path: '/w/Page', before: drawn(before), after: drawn(after) });

// ---------------------------------------------------------------------------------------------
// What goes to the page
// ---------------------------------------------------------------------------------------------

test('a function goes to the page as its source, with its arguments', () => {
  // Indirect eval runs in the global scope, as the page does: nothing of this file is in reach.
  assert.equal((0, eval)(inPage((a, b) => a + b, 1, 2)), 3);
  assert.equal((0, eval)(inPage((text, fn) => fn(text), 'x', function shout(s) { return s.toUpperCase(); })), 'X');
});

test('every function sent to the page is an expression on its own', () => {
  for (const fn of [linkedStylesheets, servedText, replaceStylesheets, whenStill, measurePage, withAbsoluteUrls]) {
    assert.doesNotThrow(() => new vm.Script(`(${fn})`), `${fn.name} does not parse outside its module`);
  }
});

test('a stylesheet is matched by its file name, among the links to the page\'s own site', () => {
  const links = [
    '/public/less.css?v=1', '/public/wiki.css?v=2', '/public/vendor/font-awesome/css/all.min.css?v=3',
    'https://elsewhere.example/wiki.css', '',
  ].map((href) => ({ href: href && new URL(href, 'https://wiki.example/w/Page').href }));
  const found = vm.runInNewContext(inPage(linkedStylesheets, ['wiki.css', 'interpreter.css']), {
    document: { querySelectorAll: () => links }, location: { origin: 'https://wiki.example' }, URL,
  });
  assert.deepEqual(JSON.parse(JSON.stringify(found)), [{ href: 'https://wiki.example/public/wiki.css?v=2', name: 'wiki.css' }]);
});

test('nothing else a page could link is named like a stylesheet in app/assets', () => {
  // linkedStylesheets matches by name alone. A vendored wiki.css would be replaced with ours.
  const ours = new Set(fs.readdirSync(assetsDir).filter((name) => name.endsWith('.css')));
  const served = execFileSync('git', ['ls-files', '-z', 'public'], { cwd: rootDir, encoding: 'utf8' }).split('\0').filter(Boolean);
  assert.deepEqual(served.filter((file) => ours.has(path.basename(file))), []);
});

test('a relative url() is written out against the stylesheet, not the page', () => {
  const css = 'a{background:url(img/a.png)} b{background:url("../b.png")} @font-face{src:url(\'f.woff2\') format("woff2")}';
  assert.equal(
    withAbsoluteUrls(css, 'https://wiki.example/public/wiki.css?v=1'),
    'a{background:url("https://wiki.example/public/img/a.png")} b{background:url("https://wiki.example/b.png")} @font-face{src:url("https://wiki.example/public/f.woff2") format("woff2")}',
  );
});

test('a url() that reads the same from the page is left as written', () => {
  const css = 'a{b:url(/public/img/x.png)} c{d:url(https://fonts.example/f.ttf)} e{f:url("data:image/png;base64,AAAA")} g{h:url(#clip)} i{j:url(//cdn.example/x.png)}';
  assert.equal(withAbsoluteUrls(css, 'https://wiki.example/public/wiki.css'), css);
});

test('the page runs the function this file tested', () => {
  const expression = inPage((css, base, fn) => fn(css, base), 'a{b:url(c.png)}', 'https://wiki.example/public/wiki.css', withAbsoluteUrls);
  assert.equal((0, eval)(expression), 'a{b:url("https://wiki.example/public/c.png")}');
});

test('a stylesheet checked out with CRLF is the one served with LF', () => {
  assert.ok(sameText('a {\r\n  b: c;\r\n}\r\n', 'a {\n  b: c;\n}\n'));
  assert.ok(!sameText('a { b: c; }', 'a { b: d; }'));
});

// ---------------------------------------------------------------------------------------------
// What two measurements say
// ---------------------------------------------------------------------------------------------

test('a page drawn the same twice has nothing to say', () => {
  assert.deepEqual(verdict(pair()), { worse: [], moved: [] });
});

test('each thing that sticks out further makes the page worse', () => {
  for (const [before, after, says] of [
    [{}, { overflowX: 300 }, /wider than its window by 0px -> 300px/],
    [{}, { pastEdge: 2 }, /elements past the content's right edge 0 -> 2/],
    [{}, { tablesWiderThanBox: 1 }, /tables wider than their box 0 -> 1/],
    [{ paper: [366, 15] }, { paper: [366, -6] }, /Paper page 2: 15px to spare -> 6px past its footer/],
    [{ paper: [-6] }, { paper: [-20] }, /Paper page 1: 6px past its footer -> 20px past its footer/],
  ]) {
    const { worse } = verdict(pair(before, after));
    assert.equal(worse.length, 1, JSON.stringify(after));
    assert.match(worse[0], says);
  }
});

test('the same things getting better are a change, not a fault', () => {
  for (const [before, after] of [
    [{ overflowX: 300 }, { overflowX: 0 }],
    [{ pastEdge: 2 }, { pastEdge: 0 }],
    [{ tablesWiderThanBox: 1 }, { tablesWiderThanBox: 0 }],
    [{ paper: [-6] }, { paper: [15] }],
    [{ paper: [366] }, { paper: [333] }],
  ]) {
    const { worse, moved } = verdict(pair(before, after));
    assert.deepEqual(worse, [], JSON.stringify(after));
    assert.equal(moved.length, 1, JSON.stringify(after));
  }
});

test('a pixel is not a page wider than its window', () => {
  assert.deepEqual(verdict(pair({}, { overflowX: 1 })), { worse: [], moved: [] });
  assert.deepEqual(verdict(pair({ overflowX: 300 }, { overflowX: 301 })).worse, []);
});

test('a little taller is a change; much taller or much shorter is a page to look at', () => {
  const edge = Math.round(1000 * muchHeight);
  assert.deepEqual(verdict(pair({}, { height: 1046 })), { worse: [], moved: ['height 1000px -> 1046px (+4.6%)'] });
  assert.deepEqual(verdict(pair({}, { height: 1000 + edge })).worse, []);
  assert.deepEqual(verdict(pair({}, { height: 1001 + edge })).moved, []);
  // The page this was written for: nothing out of place, 2.2 times as tall.
  assert.deepEqual(verdict(pair({ height: 2923 }, { height: 6459 })), { worse: ['height 2923px -> 6459px (+121.0%)'], moved: [] });
  assert.equal(verdict(pair({}, { height: 999 - edge })).worse.length, 1);
});

test('what else changes is told and fails nothing', () => {
  const { worse, moved } = verdict(pair({}, { wordsOnTwoLines: 40, tableWidth: 760 }));
  assert.deepEqual(worse, []);
  assert.deepEqual(moved, ["tables' width 800px -> 760px", 'words on two lines 3 -> 40']);
});

test('a page with no wiki content, or a Paper page with no footer, is read without complaint', () => {
  const bare = { path: '/login', before: { overflowX: 0, height: 500 }, after: { overflowX: 0, height: 500 } };
  assert.deepEqual(verdict(bare), { worse: [], moved: [] });
  assert.deepEqual(verdict(pair({ paper: [null, 10] }, { paper: [null, 10] })), { worse: [], moved: [] });
});

// ---------------------------------------------------------------------------------------------
// What the run says
// ---------------------------------------------------------------------------------------------

const same = pair();
const moved = { ...pair({}, { height: 1046, wordsOnTwoLines: 1 }), path: '/w/Moved' };
const worse = { ...pair({}, { pastEdge: 2, height: 1010 }), path: '/w/%ED%95%9C%EA%B8%80' };
const failed = { path: '/w/Gone', error: 'HTTP 404' };

test('the summary counts each page once, by the worst that is true of it', () => {
  const s = summarise([same, moved, worse, failed]);
  assert.equal(s.pages, 4);
  assert.deepEqual([s.worse.length, s.moved, s.same, s.notMeasured.length], [1, 1, 1, 1]);
  assert.deepEqual(s.pastEdge, [0, 2]);
  assert.deepEqual(s.wordsOnTwoLines, [9, 7]);
  assert.deepEqual(s.height, [3000, 3056]);
  assert.deepEqual(s.taller.map((r) => r.path), ['/w/Moved', '/w/%ED%95%9C%EA%B8%80']);
});

test('the pages that changed height most come first, each way', () => {
  const at = (name, height) => ({ ...pair({}, { height }), path: name });
  const s = summarise([at('a', 1010), at('b', 1100), at('c', 990), at('d', 900), at('e', 1000)]);
  assert.deepEqual(s.taller.map((r) => r.path), ['b', 'a']);
  assert.deepEqual(s.shorter.map((r) => r.path), ['d', 'c']);
});

test('the exit status is clean only when pages were measured and none is worse', () => {
  assert.equal(exitStatus(summarise([same, moved])), 0);
  assert.equal(exitStatus(summarise([same, worse])), 1);
  // A page that could not be measured is not a page that is fine.
  assert.equal(exitStatus(summarise([same, failed])), 1);
  // And a run that measured nothing -- every page an error page, say -- found nothing either way.
  assert.equal(exitStatus(summarise([failed, { ...failed, path: '/w/AlsoGone' }])), 2);
  assert.equal(exitStatus(summarise([])), 2);
});

test('the report names the worse page as a person reads it, and why', () => {
  const lines = reportLines(summarise([same, moved, worse, failed]));
  assert.equal(lines[0], '4 pages: 1 worse, 1 moved, 1 the same, 1 not measured');
  const at = lines.indexOf('worse');
  assert.deepEqual(lines.slice(at, at + 3), ['worse', '  /w/한글', "      elements past the content's right edge 0 -> 2"]);
  assert.ok(lines.includes('  /w/Gone: HTTP 404'));
  assert.equal(readable('/w/100%'), '/w/100%', 'a path that is not an encoding is left as it is');
});

test('the report has a line for Paper pages only when a page had some', () => {
  const paper = (lines) => lines.filter((line) => line.includes('Paper pages'));
  assert.deepEqual(paper(reportLines(summarise([same]))), []);
  assert.deepEqual(
    paper(reportLines(summarise([pair({ paper: [366, 15, -6] }, { paper: [366, 15, 12] })]))),
    ['  Paper pages past their footer: 1 -> 0 of 3, in 1 Paper document'],
  );
});

test('a long list is cut, and says by how much', () => {
  const rows = Array.from({ length: 8 }, (_, i) => ({ ...pair({}, { height: 1010 + i }), path: `/w/P${i}` }));
  const lines = reportLines(summarise(rows), 3);
  assert.equal(lines.filter((line) => /^ {6}\/w\/P/.test(line)).length, 3);
  assert.ok(lines.includes('      ... and 5 more'));
});
