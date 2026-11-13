import test from 'node:test';
import assert from 'node:assert/strict';
import { loadKanbanHooks } from './lib/kanban-board.mjs';

// The board asks /api/csrf for a token before every POST. A browser whose session holds no token
// yet is given a new one by each /api/csrf it asks, and keeps the cookie of whichever answer
// arrived last. So two asked at the same moment came back with two tokens, only one of which
// matched the cookie, and the POST carrying the other was refused with 403.
//
// That is what opening a card did on a first visit: the description and the comments are drawn
// by /api/renderAhaMark requests sent together, each after a token of its own. One of them
// failed, the failure was swallowed, and that text stayed as raw markup. Found 2026-11-14 on the
// live wiki; reproduced there with curl (two cookie-less fetches, cookie of one with the token
// of the other: 403).
//
// This is that server, reduced to the two things it does with a token.
const firstVisitServer = () => {
  let cookieToken = null;
  let issued = 0;
  const csrfRequests = [];
  const refused = [];

  const fetchImpl = (url, options = {}) => {
    if (url === '/api/csrf') {
      csrfRequests.push(url);
      // Decided when the request is sent, as the server does: no token in the session it came
      // with means a new one. The cookie changes when the answer's headers arrive, and the body
      // is read after that -- which is the gap the other answer's cookie lands in.
      const token = cookieToken || `token-${++issued}`;
      return new Promise((resolve) => setTimeout(() => {
        cookieToken = token;
        resolve({ ok: true, json: () => new Promise((read) => setTimeout(() => read({ value: token }), 5)) });
      }, 5));
    }
    if (url.startsWith('/api/renderAhaMark/')) {
      const sent = options.headers['Csrf-Token'];
      if (sent !== cookieToken) {
        refused.push(sent);
        return Promise.resolve({ ok: false, status: 403, json: async () => ({}) });
      }
      return Promise.resolve({ ok: true, status: 200, json: async () => ({ html: `<p>${JSON.parse(options.body).comment}</p>` }) });
    }
    return Promise.reject(new Error(`unexpected url: ${url}`));
  };

  return {
    fetchImpl,
    csrfRequests,
    refused,
    // What a login in another tab does to this one: the session, and the token in it, is replaced.
    replaceSession: () => { cookieToken = `token-${++issued}`; },
  };
};

test('two texts drawn together on a first visit are both drawn', async () => {
  const server = firstVisitServer();
  const { hooks } = loadKanbanHooks({ fetchImpl: server.fetchImpl });

  const drawn = await Promise.all([
    hooks.requestRenderInlineComment('Page', 'description'),
    hooks.requestRenderInlineComment('Page', 'comment'),
  ]);

  assert.deepEqual(drawn, ['<p>description</p>', '<p>comment</p>']);
  assert.deepEqual(server.refused, []);
  assert.equal(server.csrfRequests.length, 1, 'calls that start together share one request for the token');
});

// Sharing one request is not the same as remembering its answer. A board stays open for hours,
// and the session under it can change -- a login in another tab is enough. A token kept from
// before would then be refused on every save until the page was reloaded.
test('the token is asked for again once the last answer is in', async () => {
  const server = firstVisitServer();
  const { hooks } = loadKanbanHooks({ fetchImpl: server.fetchImpl });

  assert.equal(await hooks.requestRenderInlineComment('Page', 'first'), '<p>first</p>');
  server.replaceSession();
  assert.equal(await hooks.requestRenderInlineComment('Page', 'second'), '<p>second</p>');

  assert.deepEqual(server.refused, []);
  assert.equal(server.csrfRequests.length, 2);
});

test('a token request that fails is not the one the next call waits on', async () => {
  const server = firstVisitServer();
  let failNext = true;
  const fetchImpl = (url, options) => {
    if (url === '/api/csrf' && failNext) {
      failNext = false;
      return Promise.reject(new Error('network down'));
    }
    return server.fetchImpl(url, options);
  };
  const { hooks } = loadKanbanHooks({ fetchImpl });

  await assert.rejects(() => hooks.requestRenderInlineComment('Page', 'lost'), /network down/);
  assert.equal(await hooks.requestRenderInlineComment('Page', 'found'), '<p>found</p>');
});
