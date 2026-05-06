import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync('public/js/AhaWiki.Kanban.js', 'utf8');

const boot = (hash = '') => {
  let onReady = null;
  const pushes = [];
  const windowObj = {
    location: { hash, pathname: '/w/Test', search: '?a=1' },
    history: { pushState: (_a, _b, url) => pushes.push(url) },
    requestAnimationFrame: () => {},
    addEventListener: () => {}
  };
  const documentObj = {
    addEventListener: (evt, cb) => { if (evt === 'DOMContentLoaded') onReady = cb; },
    querySelectorAll: () => [],
    querySelector: () => null
  };
  const sandbox = { window: windowObj, document: documentObj, console, fetch: async () => ({ ok: true, json: async () => ({}) }), CustomEvent: class {}, CSS: { escape: (v) => String(v) } };
  vm.createContext(sandbox);
  vm.runInContext(source, sandbox);
  if (onReady) onReady();
  return { hooks: sandbox.window.__AhaWikiKanbanTestHooks, pushes, windowObj };
};

test('setHashCardId: 새 카드 해시일 때 pushState 호출', () => {
  const { hooks, pushes } = boot('');
  hooks.setHashCardId('card-1');
  assert.deepEqual(pushes, ['#card-1']);
});

test('setHashCardId: 동일 해시면 pushState 미호출', () => {
  const { hooks, pushes } = boot('#card-1');
  hooks.setHashCardId('card-1');
  assert.equal(pushes.length, 0);
});

test('clearHashCardId: 현재 해시와 일치할 때 base URL로 복귀', () => {
  const { hooks, pushes } = boot('#card-1');
  hooks.clearHashCardId('card-1');
  assert.deepEqual(pushes, ['/w/Test?a=1']);
});

test('clearHashCardId: 다른 카드 id 요청이면 변화 없음', () => {
  const { hooks, pushes } = boot('#card-1');
  hooks.clearHashCardId('card-2');
  assert.equal(pushes.length, 0);
});

test('getHashCardId: # 제거 후 trim 반환', () => {
  const { hooks } = boot('#  card-xyz  ');
  assert.equal(hooks.getHashCardId(), 'card-xyz');
});
