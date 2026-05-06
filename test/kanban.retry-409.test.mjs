import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync('public/js/AhaWiki.Kanban.js', 'utf8');

const boot = ({ revision = 1, fetchImpl }) => {
  let onReady = null;
  const revisionNode = { textContent: String(revision) };
  const documentObj = {
    addEventListener: (evt, cb) => { if (evt === 'DOMContentLoaded') onReady = cb; },
    querySelectorAll: () => [],
    querySelector: (selector) => (selector === '.revision a' ? revisionNode : null)
  };
  const sandbox = {
    window: { location: { hash: '', pathname: '/w/Test', search: '' }, history: { pushState: () => {} }, requestAnimationFrame: () => {}, addEventListener: () => {} },
    document: documentObj,
    console,
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

test('409 발생 시 최신 리비전 조회 후 1회 자동 재시도', async () => {
  let postCount = 0;
  const fetchImpl = async (url) => {
    if (url.startsWith('/api/pageRevision/')) return { ok: true, json: async () => ({ revision: 22 }) };
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'token' }) };
    if (url.startsWith('/w/')) {
      postCount += 1;
      if (postCount === 1) return { ok: false, status: 409 };
      return { ok: true, status: 200 };
    }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks, revisionNode } = boot({ revision: 10, fetchImpl });
  const result = await hooks.requestSaveKanban('Page', 100, 120, 'A\nB', 'card:move', {}, 0);

  assert.equal(Number(result.lineEnd), 102);
  assert.equal(postCount, 2);
  assert.equal(Number(revisionNode.textContent), 23);
});

test('409이 연속으로 발생하면 1회 재시도 후 실패', async () => {
  let postCount = 0;
  const fetchImpl = async (url) => {
    if (url.startsWith('/api/pageRevision/')) return { ok: true, json: async () => ({ revision: 8 }) };
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'token' }) };
    if (url.startsWith('/w/')) { postCount += 1; return { ok: false, status: 409 }; }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks, revisionNode } = boot({ revision: 3, fetchImpl });
  await assert.rejects(() => hooks.requestSaveKanban('Page', 1, 1, 'X', 'card:move', {}, 0), /status=409/);
  assert.equal(postCount, 2);
  assert.equal(Number(revisionNode.textContent), 8);
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
