import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync('public/js/AhaWiki.Kanban.js', 'utf8');

const boot = (hash = '') => {
  let onReady = null;
  const pushes = [];
  const replaces = [];
  const windowObj = {
    location: {
      origin: 'https://example.test',
      href: 'https://example.test/w/Test?a=1' + hash,
      hash,
      pathname: '/w/Test',
      search: '?a=1'
    },
    history: {
      pushState: (_a, _b, url) => pushes.push(url),
      replaceState: (_a, _b, url) => {
        replaces.push(url);
        const [pathAndSearch, nextHash = ''] = String(url).split('#');
        const queryIndex = pathAndSearch.indexOf('?');
        windowObj.location.pathname = queryIndex >= 0 ? pathAndSearch.slice(0, queryIndex) : pathAndSearch;
        windowObj.location.search = queryIndex >= 0 ? pathAndSearch.slice(queryIndex) : '';
        windowObj.location.hash = nextHash ? '#' + nextHash : '';
        windowObj.location.href = windowObj.location.origin + windowObj.location.pathname + windowObj.location.search + windowObj.location.hash;
      }
    },
    requestAnimationFrame: () => {},
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
  return { hooks: sandbox.window.__AhaWikiKanbanTestHooks, pushes, replaces, windowObj };
};

test('setHashCardId replaces the current URL hash without pushing history', () => {
  const { hooks, pushes, replaces, windowObj } = boot('');
  hooks.setHashCardId('card-1');
  assert.deepEqual(pushes, []);
  assert.deepEqual(replaces, ['/w/Test?a=1#card-1']);
  assert.equal(windowObj.location.hash, '#card-1');
});

test('setHashCardId is a no-op when the hash already matches', () => {
  const { hooks, pushes, replaces, windowObj } = boot('#card-1');
  hooks.setHashCardId('card-1');
  assert.deepEqual(pushes, []);
  assert.deepEqual(replaces, []);
  assert.equal(windowObj.location.hash, '#card-1');
});

test('clearHashCardId replaces the current URL without pushing history', () => {
  const { hooks, pushes, replaces, windowObj } = boot('#card-1');
  hooks.clearHashCardId('card-1');
  assert.deepEqual(pushes, []);
  assert.deepEqual(replaces, ['/w/Test?a=1']);
  assert.equal(windowObj.location.hash, '');
});

test('clearHashCardId is a no-op for another card id', () => {
  const { hooks, pushes, replaces, windowObj } = boot('#card-1');
  hooks.clearHashCardId('card-2');
  assert.deepEqual(pushes, []);
  assert.deepEqual(replaces, []);
  assert.equal(windowObj.location.hash, '#card-1');
});

test('getHashCardId strips # and trims whitespace', () => {
  const { hooks } = boot('#  card-xyz  ');
  assert.equal(hooks.getHashCardId(), 'card-xyz');
});

test('getHashCardId decodes URL-encoded card ids', () => {
  const { hooks } = boot('#card%2Fxyz');
  assert.equal(hooks.getHashCardId(), 'card/xyz');
});

test('buildCardUrl returns an absolute encoded card URL', () => {
  const { hooks } = boot('');
  assert.equal(hooks.buildCardUrl('card/xyz'), 'https://example.test/w/Test?a=1#card%2Fxyz');
});
