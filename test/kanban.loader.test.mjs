// AhaWiki.KanbanLoader.js fetches AhaWiki.Kanban.js only for a page with a board. Until
// 2026-11-08 every page carried the 238 KB board script.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const scriptLoader = fs.readFileSync('public/js/AhaWiki.ScriptLoader.js', 'utf8');
const kanbanLoader = fs.readFileSync('public/js/AhaWiki.KanbanLoader.js', 'utf8');
const libraryUrl = '/public/js/AhaWiki.Kanban.js?v=digest';

const page = ({ hasBoard }) => {
  const appended = [];
  let onReady = null;
  const board = { className: 'InterpreterKanban' };
  const documentObj = {
    currentScript: { dataset: { library: libraryUrl } },
    addEventListener: (evt, cb) => { if (evt === 'DOMContentLoaded') onReady = cb; },
    querySelector: (selector) => (hasBoard && selector === '.InterpreterKanban' ? board : null),
    createElement: () => ({}),
    head: { append: (script) => appended.push(script) },
  };
  const sandbox = { window: {}, document: documentObj, Map, Promise, Error };
  vm.createContext(sandbox);
  vm.runInContext(scriptLoader, sandbox);
  vm.runInContext(kanbanLoader, sandbox);
  return { sandbox, appended, ready: () => onReady() };
};

test('a page without a board does not fetch the board script', async () => {
  const { appended, ready } = page({ hasBoard: false });
  await ready();
  assert.equal(appended.length, 0);
});

test('a page with a board fetches it once, and hands the scope to the real initAll', async () => {
  const { sandbox, appended, ready } = page({ hasBoard: true });
  const done = ready();
  const again = sandbox.window.AhaWiki.Kanban.initAll(sandbox.document);
  assert.equal(appended.length, 1, 'asked twice, fetched once');
  assert.equal(appended[0].src, libraryUrl);

  const scopes = [];
  sandbox.window.AhaWiki.Kanban = { initAll: (scope) => scopes.push(scope) };
  appended[0].onload();
  await Promise.all([done, again]);
  assert.deepEqual(scopes, [sandbox.document, sandbox.document]);
});

test('a board script that never replaces the loader does not send it round in a loop', async () => {
  const { appended, ready } = page({ hasBoard: true });
  const done = ready();
  appended[0].onload();
  await done;
});

test('a board script that fails to load rejects rather than hangs', async () => {
  const { appended, ready } = page({ hasBoard: true });
  const done = ready();
  appended[0].onerror();
  await assert.rejects(done, /did not load/);
});
