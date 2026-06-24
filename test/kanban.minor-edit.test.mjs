import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync('public/js/AhaWiki.Kanban.js', 'utf8');

const boot = ({ revision = 1, fetchImpl = async () => ({ ok: true, json: async () => ({}) }) } = {}) => {
  let onReady = null;
  const revisionNode = { textContent: String(revision) };
  const windowObj = {
    location: { hash: '', pathname: '/w/Test', search: '' },
    history: { pushState: () => {}, replaceState: () => {} },
    requestAnimationFrame: (fn) => fn(),
    addEventListener: () => {},
    alert: () => {}
  };
  const documentObj = {
    addEventListener: (evt, cb) => { if (evt === 'DOMContentLoaded') onReady = cb; },
    querySelectorAll: () => [],
    querySelector: (selector) => (selector === '.revision a' ? revisionNode : null)
  };
  const sandbox = {
    window: windowObj,
    document: documentObj,
    console,
    alert: () => {},
    fetch: fetchImpl,
    CustomEvent: class {},
    CSS: { escape: (v) => String(v) },
    URLSearchParams
  };
  vm.createContext(sandbox);
  vm.runInContext(source, sandbox);
  if (onReady) onReady();
  return { hooks: sandbox.window.__AhaWikiKanbanTestHooks, revisionNode };
};

test('minor edit policy marks pure ordering changes as minor', () => {
  const { hooks } = boot();

  assert.equal(hooks.isKanbanSaveMinorEdit('list:move', {}), true);
  assert.equal(hooks.isKanbanSaveMinorEdit('card:move', { fromList: 'Doing', toList: 'Doing' }), true);
});

test('minor edit policy keeps visible content and activity changes as major', () => {
  const { hooks } = boot();

  assert.equal(hooks.isKanbanSaveMinorEdit('card:move', { fromList: 'Todo', toList: 'Done' }), false);
  assert.equal(hooks.isKanbanSaveMinorEdit('card:comment:add', { comment: 'done' }), false);
  assert.equal(hooks.isKanbanSaveMinorEdit('list:add', {}), false);
  assert.equal(hooks.isKanbanSaveMinorEdit('list:rename', {}), false);
  assert.equal(hooks.isKanbanSaveMinorEdit('list:delete', {}), false);
  assert.equal(hooks.isKanbanSaveMinorEdit('card:add', {}), false);
  assert.equal(hooks.isKanbanSaveMinorEdit('card:rename', {}), false);
  assert.equal(hooks.isKanbanSaveMinorEdit('card:delete', {}), false);
  assert.equal(hooks.isKanbanSaveMinorEdit('card:property:update', {}), false);
  assert.equal(hooks.isKanbanSaveMinorEdit('card:description:update', {}), false);
});

test('requestSaveKanban sends action-specific minorEdit value', async () => {
  const seenMinorEdits = [];
  const fetchImpl = async (url, options) => {
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'token' }) };
    if (url.startsWith('/w/')) {
      const params = new URLSearchParams(options.body);
      seenMinorEdits.push(params.get('minorEdit'));
      return { ok: true, status: 200 };
    }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks } = boot({ revision: 10, fetchImpl });
  await hooks.requestSaveKanban('Page', 3, 6, 'content', 'list:move', { listTitle: 'Doing' }, 0);
  await hooks.requestSaveKanban('Page', 3, 6, 'content', 'card:comment:add', { cardId: 'c1', cardTitle: 'Task', comment: 'done' }, 0);

  assert.deepEqual(seenMinorEdits, ['true', 'false']);
});
