import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync('public/js/AhaWiki.Kanban.js', 'utf8');

const loadHooks = () => {
  let onReady = null;
  const windowObj = {
    location: { hash: '', pathname: '/w/Test', search: '' },
    history: { pushState: () => {}, replaceState: () => {} },
    requestAnimationFrame: (fn) => fn(),
    addEventListener: () => {}
  };
  const documentObj = {
    addEventListener: (evt, cb) => { if (evt === 'DOMContentLoaded') onReady = cb; },
    querySelectorAll: () => [],
    querySelector: () => null
  };
  const sandbox = {
    window: windowObj,
    document: documentObj,
    console,
    fetch: async () => ({ ok: true, json: async () => ({}) }),
    CustomEvent: class {},
    CSS: { escape: (v) => String(v) }
  };
  vm.createContext(sandbox);
  vm.runInContext(source, sandbox);
  if (onReady) onReady();
  return sandbox.window.__AhaWikiKanbanTestHooks;
};

const hooks = loadHooks();

test('normalizes same-board card references', () => {
  assert.equal(hooks.normalizeCardReferenceValue('#card-a', 'Card A'), '[#card-a Card A]');
  assert.equal(hooks.normalizeCardReferenceValue('[#card-a Custom label]', 'Card A'), '[#card-a Custom label]');
  assert.equal(hooks.normalizeCardReferenceValue('card-a', ''), '[#card-a]');
  assert.equal(hooks.normalizeCardReferenceValue('bad id', 'Bad'), '');
});

test('normalizes assignee values to User links', () => {
  assert.equal(hooks.normalizeAssigneeValue('Alice'), '[User:Alice]');
  assert.equal(hooks.normalizeAssigneeValue('User:Alice'), '[User:Alice]');
  assert.equal(hooks.normalizeAssigneeValue('[User:Alice]'), '[User:Alice]');
  assert.equal(hooks.getAssigneeName('[User:Alice]'), 'Alice');
});

test('computes reverse card relations from direct properties', () => {
  const target = { id: 'card-a', text: 'Card A', properties: {} };
  const blocker = { id: 'card-b', text: 'Card B', properties: { BlockedBy: ['[#card-a Card A]'] } };
  const child = { id: 'card-c', text: 'Card C', properties: { ParentCard: ['#card-a'] } };
  const columns = [{ title: 'Todo', cards: [target, blocker, child] }];

  const derived = hooks.computeDerivedCardRelationValues(columns, target);
  assert.deepEqual([...derived.Blocks], ['[#card-b Card B]']);
  assert.deepEqual([...derived.SubCards], ['[#card-c Card C]']);
});
