import test from 'node:test';
import assert from 'node:assert/strict';
import { loadKanbanHooks } from './lib/kanban-board.mjs';

// Kanban asks AhaWiki.ReCaptcha for a token on every save attempt, the 409 retries included,
// because a token passes one check. When no token can be had it says so and does not post.
// Until 2026-09-13 it always sent an empty token, which the server let through unchecked.

test('every save attempt carries its own token, the 409 retry included', async () => {
  let issued = 0;
  const actions = [];
  const reCaptcha = {
    token: async (action) => {
      actions.push(action);
      issued += 1;
      return `token-${issued}`;
    },
  };
  const seenTokens = [];
  let saveCount = 0;
  const fetchImpl = async (url, options) => {
    if (url.startsWith('/api/pageRevision/')) return { ok: true, json: async () => ({ revision: 11 }) };
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'csrf' }) };
    if (url.startsWith('/w/')) {
      saveCount += 1;
      seenTokens.push(new URLSearchParams(options.body).get('recaptcha'));
      return saveCount === 1 ? { ok: false, status: 409 } : { ok: true, status: 200 };
    }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks, alerts } = loadKanbanHooks({ revision: 10, fetchImpl, reCaptcha });
  await hooks.requestSaveKanban('Page', 100, 120, 'A\nB', 'card:move', {}, 0, async () => ({ lineStart: 100, lineEnd: 120, content: 'A\nB' }));

  assert.deepEqual(seenTokens, ['token-1', 'token-2']);
  assert.deepEqual(actions, ['kanban_save', 'kanban_save']);
  assert.deepEqual(alerts, []);
});

test('no token means no save, and the reader is told why', async () => {
  const reCaptcha = { token: async () => { throw new Error('reCAPTCHA script failed to load'); } };
  let posted = 0;
  const fetchImpl = async (url) => {
    if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'csrf' }) };
    if (url.startsWith('/w/')) {
      posted += 1;
      return { ok: true, status: 200 };
    }
    throw new Error(`unexpected url: ${url}`);
  };

  const { hooks, alerts } = loadKanbanHooks({ revision: 10, fetchImpl, reCaptcha });
  await assert.rejects(
    () => hooks.requestSaveKanban('Page', 1, 2, 'x', 'card:add', {}, 0),
    /failed to load/,
  );

  assert.equal(posted, 0);
  assert.equal(alerts.length, 1);
  assert.match(alerts[0], /reCAPTCHA/);
});
