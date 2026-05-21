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

test('409 발생 시 사용자에게 알리고 최신 페이지로 새로고침', async () => {
  let postCount = 0;
  const fetchImpl = async (url) => {
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'token' }) };
    if (url.startsWith('/w/')) {
      postCount += 1;
      return { ok: false, status: 409 };
    }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks, revisionNode, alerts, getReloadCount } = boot({ revision: 10, fetchImpl });

  await assert.rejects(
    () => hooks.requestSaveKanban('Page', 100, 120, 'A\nB', 'card:move', {}, 0),
    /Conflict: reloading due to stale revision/
  );
  assert.equal(postCount, 1);
  assert.deepEqual(alerts, ['This page has been modified. Refreshing to the latest version.']);
  assert.equal(getReloadCount(), 1);
  assert.equal(Number(revisionNode.textContent), 10);
});

test('409 발생 시 최신 리비전 자동 재조회/재시도 없이 실패', async () => {
  let postCount = 0;
  let revisionFetchCount = 0;
  const fetchImpl = async (url) => {
    if (url.startsWith('/api/pageRevision/')) {
      revisionFetchCount += 1;
      return { ok: true, json: async () => ({ revision: 8 }) };
    }
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'token' }) };
    if (url.startsWith('/w/')) { postCount += 1; return { ok: false, status: 409 }; }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks, revisionNode, getReloadCount } = boot({ revision: 3, fetchImpl });
  await assert.rejects(() => hooks.requestSaveKanban('Page', 1, 1, 'X', 'card:move', {}, 0), /Conflict: reloading due to stale revision/);
  assert.equal(postCount, 1);
  assert.equal(revisionFetchCount, 0);
  assert.equal(getReloadCount(), 1);
  assert.equal(Number(revisionNode.textContent), 3);
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
