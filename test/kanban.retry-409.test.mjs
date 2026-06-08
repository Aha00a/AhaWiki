import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync('public/js/AhaWiki.Kanban.js', 'utf8');

const boot = ({ revision = 1, fetchImpl }) => {
  let onReady = null;
  const revisionNode = { textContent: String(revision) };
  const alerts = [];
  let reloadCount = 0;
  const documentObj = {
    addEventListener: (evt, cb) => { if (evt === 'DOMContentLoaded') onReady = cb; },
    querySelectorAll: () => [],
    querySelector: (selector) => (selector === '.revision a' ? revisionNode : null)
  };
  const sandbox = {
    window: {
      location: {
        hash: '',
        pathname: '/w/Test',
        search: '',
        reload: () => { reloadCount += 1; }
      },
      history: { pushState: () => {} },
      requestAnimationFrame: () => {},
      addEventListener: () => {},
      alert: (message) => { alerts.push(message); }
    },
    document: documentObj,
    console,
    alert: (message) => { alerts.push(message); },
    fetch: fetchImpl,
    CustomEvent: class {},
    CSS: { escape: (v) => String(v) },
    URLSearchParams
  };
  vm.createContext(sandbox);
  vm.runInContext(source, sandbox);
  if (onReady) onReady();
  return { hooks: sandbox.window.__AhaWikiKanbanTestHooks, revisionNode, alerts, getReloadCount: () => reloadCount };
};

test('409 발생 시 최신 리비전 조회 후 자동 재시도하여 성공', async () => {
  let saveCount = 0;
  let revisionFetchCount = 0;
  const seenRevisions = [];
  const fetchImpl = async (url, options) => {
    if (url.startsWith('/api/pageRevision/')) {
      revisionFetchCount++;
      return { ok: true, json: async () => ({ revision: 11 }) };
    }
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'token' }) };
    if (url.startsWith('/w/')) {
      saveCount++;
      const params = new URLSearchParams(options.body);
      seenRevisions.push(Number(params.get('revision')));
      if (saveCount === 1) return { ok: false, status: 409 };
      return { ok: true, status: 200 };
    }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks, revisionNode, alerts, getReloadCount } = boot({ revision: 10, fetchImpl });
  await hooks.requestSaveKanban('Page', 100, 120, 'A\nB', 'card:move', {}, 0);

  assert.equal(saveCount, 2);
  assert.equal(revisionFetchCount, 1);
  assert.deepEqual(seenRevisions, [10, 11]);
  assert.deepEqual(alerts, []);
  assert.equal(getReloadCount(), 0);
  assert.equal(Number(revisionNode.textContent), 12);
});

test('409 반복 발생 시 최대 재시도 후 reload/alert 없이 에러', async () => {
  let saveCount = 0;
  let revisionFetchCount = 0;
  const fetchImpl = async (url) => {
    if (url.startsWith('/api/pageRevision/')) {
      revisionFetchCount++;
      return { ok: true, json: async () => ({ revision: 20 }) };
    }
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'token' }) };
    if (url.startsWith('/w/')) {
      saveCount++;
      return { ok: false, status: 409 };
    }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks, revisionNode, alerts, getReloadCount } = boot({ revision: 10, fetchImpl });
  await assert.rejects(
    () => hooks.requestSaveKanban('Page', 100, 120, 'content', 'card:move', {}, 0),
    /Conflict: save failed after retries/
  );

  assert.equal(saveCount, 4);
  assert.equal(revisionFetchCount, 3);
  assert.deepEqual(alerts, []);
  assert.equal(getReloadCount(), 0);
  assert.equal(Number(revisionNode.textContent), 20);
});

test('현재 리비전이 0이면 저장 전 최신 리비전 먼저 동기화', async () => {
  const seenRevisions = [];
  const fetchImpl = async (url, options) => {
    if (url.startsWith('/api/pageRevision/')) return { ok: true, json: async () => ({ revision: 15 }) };
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'token' }) };
    if (url.startsWith('/w/')) {
      const params = new URLSearchParams(options.body);
      seenRevisions.push(Number(params.get('revision')));
      return { ok: true, status: 200 };
    }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks, revisionNode } = boot({ revision: 0, fetchImpl });
  await hooks.requestSaveKanban('Page', 3, 3, 'line', 'card:add', {}, 0);
  assert.deepEqual(seenRevisions, [15]);
  assert.equal(Number(revisionNode.textContent), 16);
});
