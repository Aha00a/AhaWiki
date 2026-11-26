// scripts/harnesses.mjs opens the browser harnesses and says which pass, and what it goes by is
// what each page prints. The pages were written one at a time and do not print alike, so the
// reading (scripts/lib/harness-result.mjs) is where a harness can come out as passing when it did
// not: a last line it does not know, a failure it takes for a detail, a page still printing. Each
// test here is one page's way of printing, as that page prints it. The opening and the waiting
// need a browser and are not here.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { verdictOf } from '../scripts/lib/harness-result.mjs';

const lines = (...all) => all.join('\n');

test('a page that has printed nothing yet is silent, not passing', () => {
  for (const text of ['', 'running…', 'starting…', '   \n', undefined]) {
    assert.equal(verdictOf(text).state, 'silent', JSON.stringify(text));
  }
});

test('paper-contract, word-break, graph-hud: checks, then a line that counts them', () => {
  const passed = verdictOf(lines('PASS the titles and the labels start at the left edge', 'PASS paragraphs and lists start 60mm in (227px)', 'PASS — 2 checks'));
  assert.deepEqual(passed, { state: 'pass', checks: 2, failures: [], summary: 'PASS — 2 checks', summed: true });

  const failed = verdictOf(lines('PASS the bar is one line', 'FAIL the legend is whole in the bar: it runs 11px past the bar', 'FAIL — 1 of 2'));
  assert.equal(failed.state, 'fail');
  assert.equal(failed.checks, 2);
  assert.deepEqual(failed.failures, ['FAIL the legend is whole in the bar: it runs 11px past the bar']);
  assert.equal(failed.summary, 'FAIL — 1 of 2');
});

test('graph-hud: a NOTE is neither a check nor a failure', () => {
  const v = verdictOf(lines('PASS closed, nothing reaches out of the graph', 'NOTE #!Graph in a help page\'s table: at 137×77 it is too small for its buttons, and is not checked', 'PASS — 1 checks, and 1 too small to check'));
  assert.equal(v.state, 'pass');
  assert.equal(v.checks, 1);
  assert.equal(v.summary, 'PASS — 1 checks, and 1 too small to check');
});

test('editor-keys, editor-hotkeys: lines of their own, then PASS or FAIL with a dash', () => {
  const passed = verdictOf(lines('--- Alt+S -> editSave×1  OK', '--- Alt+H -> (nothing)  OK', 'PASS — the editor hotkeys click only the editor\'s own hints'));
  assert.equal(passed.state, 'pass');
  assert.equal(passed.summed, true);
  const failed = verdictOf(lines('--- Alt+S -> editSave×1, navSearch×1  FAIL', 'FAIL — 1 of 3'));
  assert.equal(failed.state, 'fail');
  assert.equal(failed.summary, 'FAIL — 1 of 3');
});

test('editor-keys: a failure it prints before giving up, with no last line', () => {
  const v = verdictOf('FAIL: no CodeMirror instance — check the console for a script that threw');
  assert.equal(v.state, 'fail');
  assert.equal(v.failures.length, 1);
});

test('editor-grid: OK for a check that held, the detail of one that did not under it', () => {
  const passed = verdictOf(lines('OK   the table opens in the grid', 'OK   every cell shows its whole text', 'PASS'));
  assert.deepEqual([passed.state, passed.checks, passed.summary], ['pass', 2, 'PASS']);

  const failed = verdictOf(lines('OK   the table opens in the grid', 'FAIL every cell shows its whole text', '     cell 2 shows «say » for «say "hello"»', 'OK   the gantt grid shows each task name whole', 'FAIL — 1'));
  assert.equal(failed.state, 'fail');
  assert.equal(failed.checks, 3);
  assert.deepEqual(failed.failures, ['FAIL every cell shows its whole text', '  cell 2 shows «say » for «say "hello"»']);
});

test('editor-grid while it is still printing: passing so far, and not summed up', () => {
  const v = verdictOf(lines('OK   the table opens in the grid', 'OK   every cell shows its whole text'));
  assert.equal(v.state, 'pass');
  assert.equal(v.summed, false, 'what tells the runner to go on waiting');
});

test('slide-layout: ALL PASSED, or a count of what failed', () => {
  assert.deepEqual([verdictOf(lines('PASS dark: the slide is dark', 'ALL PASSED')).state, verdictOf(lines('PASS dark: the slide is dark', 'ALL PASSED')).summary], ['pass', 'ALL PASSED']);
  const failed = verdictOf(lines('PASS dark: the slide is dark', 'FAIL picture: fits the slide  (612 of 540)', '1 FAILED'));
  assert.deepEqual([failed.state, failed.summary, failed.failures.length], ['fail', '1 FAILED', 1]);
});

test('admin-vendor, vendor-assets: a detail under a check that held is not a failure', () => {
  const passed = verdictOf(lines('PASS Mantine scheme and the html class follow ahawiki-theme', '     attribute light, class theme-ahawiki-light, stored light', 'PASS no script error', 'ALL PASS'));
  assert.deepEqual([passed.state, passed.checks, passed.failures.length], ['pass', 2, 0]);

  const failed = verdictOf(lines('PASS no script error', 'FAIL nothing fetched off this origin but the wiki\'s fonts', '     https://esm.sh/react', 'SOME FAIL'));
  assert.equal(failed.state, 'fail');
  assert.deepEqual(failed.failures, ['FAIL nothing fetched off this origin but the wiki\'s fonts', '  https://esm.sh/react']);
});

test('diff-long-lines, slide-notes, table-data-bars: the checks and no last line', () => {
  const passed = verdictOf(lines('PASS side-by-side: 0 column(s) scroll sideways', 'PASS line-by-line: 0 column(s) scroll sideways'));
  assert.deepEqual(passed, { state: 'pass', checks: 2, failures: [], summary: '2 checks', summed: false });
  const failed = verdictOf(lines('PASS notes are hidden on the stage', 'FAIL presenter window opened (popup blocked?)'));
  assert.equal(failed.state, 'fail');
  assert.equal(failed.summary, '2 checks');
});

test('graph-glow: one line that is the verdict, and numbers under it', () => {
  assert.equal(verdictOf(lines('PASS 280 frames, 0 exceptions, 0 empty frames', 'nodes 24, links 44', 'groups 3')).state, 'pass');
  const failed = verdictOf(lines('FAIL 280 frames, 3 exceptions, 0 empty frames', 'TypeError: Cannot read properties of undefined', 'nodes 24, links 44', 'groups 3'));
  assert.equal(failed.state, 'fail');
});

test('a check that only mentions PASS or FAIL further in is not counted by it', () => {
  const v = verdictOf(lines('PASS the page says FAIL when a cell is cut', 'PASSED nothing', 'PASS — 1 checks'));
  assert.equal(v.state, 'pass');
  assert.equal(v.checks, 1, '«PASSED nothing» does not start with the word PASS');
});

// Every harness there is prints into one of the three elements the runner looks in. A new one
// that names its element something else would be opened, waited for, and reported as silent.
test('every harness page has an element the runner reads', () => {
  const manual = path.join(path.dirname(fileURLToPath(import.meta.url)), 'manual');
  const pages = fs.readdirSync(manual).filter((name) => fs.existsSync(path.join(manual, name, 'index.html')));
  assert.ok(pages.length >= 13, 'the harnesses are where this test looks for them');
  for (const name of pages) {
    const html = fs.readFileSync(path.join(manual, name, 'index.html'), 'utf8');
    assert.match(html, /id=["'](result|harness|out)["']/, `${name}/index.html has no #result, #harness or #out`);
  }
});
