// A 409 means the page moved on since the board last read it. The board reads it again and
// replays its unsaved changes onto it (kanban.rebase.test.mjs, kanban.persist.test.mjs); what is
// pinned here is the request side: a fresh revision before each retry, at most three retries, no
// reload or alert, and nothing sent again but what the replay hands back. Until 2026-09-15 a
// retry resent the text it already had, over whatever another session had saved in between.
import test from 'node:test';
import assert from 'node:assert/strict';
import { loadKanbanHooks } from './lib/kanban-board.mjs';

const replayAs = (lineStart, lineEnd, content) => async () => ({ lineStart, lineEnd, content });

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

  const { hooks, revisionNode, alerts, reloadCount } = loadKanbanHooks({ revision: 10, fetchImpl });
  await hooks.requestSaveKanban('Page', 100, 120, 'A\nB', 'card:move', {}, 0, replayAs(100, 120, 'A\nB'));

  assert.equal(saveCount, 2);
  assert.equal(revisionFetchCount, 1);
  assert.deepEqual(seenRevisions, [10, 11]);
  assert.deepEqual(alerts, []);
  assert.equal(reloadCount(), 0);
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

  const { hooks, revisionNode, alerts, reloadCount } = loadKanbanHooks({ revision: 10, fetchImpl });
  await assert.rejects(
    () => hooks.requestSaveKanban('Page', 100, 120, 'content', 'card:move', {}, 0, replayAs(100, 120, 'content')),
    /Conflict: save failed after retries/
  );

  assert.equal(saveCount, 4);
  assert.equal(revisionFetchCount, 3);
  assert.deepEqual(alerts, []);
  assert.equal(reloadCount(), 0);
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

  const { hooks, revisionNode } = loadKanbanHooks({ revision: 0, fetchImpl });
  await hooks.requestSaveKanban('Page', 3, 3, 'line', 'card:add', {}, 0);
  assert.deepEqual(seenRevisions, [15]);
  assert.equal(Number(revisionNode.textContent), 16);
});

test('a retry sends what the replay hands back, at the lines it hands back', async () => {
  const posts = [];
  const fetchImpl = async (url, options) => {
    if (url.startsWith('/api/pageRevision/')) return { ok: true, json: async () => ({ revision: 11 }) };
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'token' }) };
    if (url.startsWith('/w/')) {
      posts.push(Object.fromEntries(new URLSearchParams(options.body)));
      return posts.length === 1 ? { ok: false, status: 409 } : { ok: true, status: 200 };
    }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks } = loadKanbanHooks({ revision: 10, fetchImpl });
  const result = await hooks.requestSaveKanban('Page', 100, 120, 'mine', 'card:move', {}, 0, replayAs(7, 9, 'mine and theirs'));

  assert.deepEqual(posts.map(p => [p.lineStart, p.lineEnd, p.text]), [['100', '120', 'mine'], ['7', '9', 'mine and theirs']]);
  assert.equal(result.lineEnd, 8);
});

test('without a replay, a 409 is not sent again', async () => {
  let saveCount = 0;
  const fetchImpl = async (url) => {
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'token' }) };
    if (url.startsWith('/w/')) {
      saveCount++;
      return { ok: false, status: 409 };
    }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks } = loadKanbanHooks({ revision: 10, fetchImpl });
  await assert.rejects(() => hooks.requestSaveKanban('Page', 1, 2, 'x', 'card:add', {}, 0), /Conflict/);
  assert.equal(saveCount, 1);
});

test('a replay with nothing left to send ends the save without another request', async () => {
  let saveCount = 0;
  const fetchImpl = async (url) => {
    if (url.startsWith('/api/pageRevision/')) return { ok: true, json: async () => ({ revision: 11 }) };
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'token' }) };
    if (url.startsWith('/w/')) {
      saveCount++;
      return { ok: false, status: 409 };
    }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks } = loadKanbanHooks({ revision: 10, fetchImpl });
  const result = await hooks.requestSaveKanban('Page', 1, 2, 'x', 'card:move', {}, 0, async () => null);
  assert.equal(result.unchanged, true);
  assert.equal(saveCount, 1);
});
