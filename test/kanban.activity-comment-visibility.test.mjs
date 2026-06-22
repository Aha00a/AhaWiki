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

test('activity classifier recognizes current system event detail', () => {
  assert.equal(hooks.isKanbanSystemActivityDetail('["Page#card-1" Page#Build server] moved - ToDo to Done'), true);
});

test('activity classifier recognizes legacy system event detail', () => {
  assert.equal(hooks.isKanbanSystemActivityDetail('Card Property Update - #AWS dev infra - Assignee - [User:alice]'), true);
});

test('activity classifier treats manual comments as user activity comments', () => {
  assert.equal(hooks.isUserActivityComment({
    header: '[User:Aha00a] [2026-06-08]T21:30:08',
    details: ['t3a.medium, 2 vCPU / 4 GiB RAM']
  }), true);
});

test('activity classifier keeps system-only rows as activity rows', () => {
  assert.equal(hooks.isUserActivityComment({
    header: '[User:Aha00a] [2026-06-09]T18:58:05',
    details: ['Card Move - #AWS dev infra - ToDo to Done']
  }), false);
});
