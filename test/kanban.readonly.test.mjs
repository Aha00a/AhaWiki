import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync('public/js/AhaWiki.Kanban.js', 'utf8');

class ClassList {
  constructor(element) {
    this.element = element;
    this.values = new Set();
  }

  add(...names) {
    names.forEach((name) => this.values.add(name));
    this.element.className = [...this.values].join(' ');
  }

  remove(...names) {
    names.forEach((name) => this.values.delete(name));
    this.element.className = [...this.values].join(' ');
  }

  contains(name) {
    return this.values.has(name);
  }
}

class Element {
  constructor(tagName) {
    this.tagName = tagName.toUpperCase();
    this.children = [];
    this.parentElement = null;
    this.parentNode = null;
    this.attributes = new Map();
    this.style = {
      setProperty: () => {},
      removeProperty: () => {}
    };
    this.classList = new ClassList(this);
    this.className = '';
    this.textContent = '';
    this.innerHTML = '';
    this.eventListeners = {};
  }

  setAttribute(name, value) {
    this.attributes.set(name, String(value));
  }

  getAttribute(name) {
    return this.attributes.has(name) ? this.attributes.get(name) : null;
  }

  appendChild(child) {
    child.parentElement = this;
    child.parentNode = this;
    this.children.push(child);
    return child;
  }

  insertBefore(child, reference) {
    child.parentElement = this;
    child.parentNode = this;
    if (!reference) {
      this.children.push(child);
      return child;
    }
    const index = this.children.indexOf(reference);
    if (index < 0) {
      this.children.push(child);
      return child;
    }
    this.children.splice(index, 0, child);
    return child;
  }

  removeChild(child) {
    const index = this.children.indexOf(child);
    if (index >= 0) {
      this.children.splice(index, 1);
      child.parentElement = null;
      child.parentNode = null;
    }
    return child;
  }

  addEventListener(type, handler) {
    this.eventListeners[type] = this.eventListeners[type] || [];
    this.eventListeners[type].push(handler);
  }

  querySelector(selector) {
    return this.querySelectorAll(selector)[0] || null;
  }

  querySelectorAll(selector) {
    const results = [];
    const visit = (node) => {
      node.children.forEach((child) => {
        if (matches(child, selector)) {
          results.push(child);
        }
        visit(child);
      });
    };
    visit(this);
    return results;
  }

  closest(selector) {
    let node = this;
    while (node) {
      if (matches(node, selector)) {
        return node;
      }
      node = node.parentElement;
    }
    return null;
  }

  scrollIntoView() {}
  focus() {}
  select() {}
  getBoundingClientRect() {
    return { left: 0, top: 0, width: 100, height: 100 };
  }
}

const matches = (element, selector) => {
  if (selector.startsWith('.')) {
    const className = selector.slice(1);
    return element.classList.contains(className) || String(element.className || '').split(/\s+/).includes(className);
  }
  if (selector === 'pre[data-shebang]') {
    return element.tagName === 'PRE' && element.getAttribute('data-shebang') !== null;
  }
  return false;
};

const boot = ({ writable }) => {
  let onReady = null;
  let sortableCreateCount = 0;
  const root = new Element('div');
  root.classList.add('InterpreterKanban');
  root.setAttribute('data-page-name', 'Page');
  root.id = 'kanban-test';

  const wikiContent = new Element('div');
  wikiContent.classList.add('wikiContent');
  wikiContent.setAttribute('data-wiki-writable', writable ? 'true' : 'false');
  wikiContent.appendChild(root);

  const pre = new Element('pre');
  pre.setAttribute('data-shebang', 'Kanban');
  pre.textContent = [
    '=== ToDo',
    '==== Card ==== #c1',
    '===== Property',
    '===== Activity'
  ].join('\n');
  const board = new Element('div');
  board.classList.add('kanban-board');
  root.appendChild(pre);
  root.appendChild(board);

  const documentObj = {
    body: new Element('body'),
    addEventListener: (evt, cb) => { if (evt === 'DOMContentLoaded') onReady = cb; },
    createElement: (tagName) => new Element(tagName),
    querySelectorAll: (selector) => selector === '.InterpreterKanban' ? [root] : [],
    querySelector: () => null,
  };

  const sandbox = {
    window: {
      location: { hash: '', pathname: '/w/Page', search: '' },
      history: { pushState: () => {} },
      requestAnimationFrame: (cb) => cb(),
      addEventListener: () => {},
      Sortable: {
        create: () => {
          sortableCreateCount += 1;
          return {};
        }
      },
    },
    document: documentObj,
    console,
    CustomEvent: class {},
    CSS: { escape: (v) => String(v) },
    URLSearchParams,
    fetch: async () => ({ ok: true, json: async () => ({}) }),
    alert: () => {},
  };
  sandbox.Sortable = sandbox.window.Sortable;
  vm.createContext(sandbox);
  vm.runInContext(source, sandbox);
  if (onReady) onReady();
  return { root, board, sortableCreateCount };
};

test('read-only Kanban hides mutation controls and does not create Sortable instances', () => {
  const { root, board, sortableCreateCount } = boot({ writable: false });

  assert.equal(root.getAttribute('data-kanban-read-only'), 'true');
  assert.equal(board.querySelectorAll('.kanban-read-only-notice').length, 1);
  assert.equal(board.querySelectorAll('.kanban-add-list-button').length, 0);
  assert.equal(board.querySelectorAll('.kanban-add-card-button').length, 0);
  assert.equal(board.querySelectorAll('.kanban-icon-button').length, 0);
  assert.equal(board.classList.contains('kanban-draggable'), true);
  assert.equal(sortableCreateCount, 0);
});

test('writable Kanban keeps mutation controls and Sortable instances', () => {
  const { root, board, sortableCreateCount } = boot({ writable: true });

  assert.equal(root.getAttribute('data-kanban-read-only'), 'false');
  assert.equal(board.querySelectorAll('.kanban-read-only-notice').length, 0);
  assert.equal(board.querySelectorAll('.kanban-add-list-button').length, 1);
  assert.equal(board.querySelectorAll('.kanban-add-card-button').length, 1);
  assert.equal(board.querySelectorAll('.kanban-icon-button').length, 1);
  assert.equal(board.classList.contains('kanban-draggable'), true);
  assert.equal(sortableCreateCount, 2);
});
