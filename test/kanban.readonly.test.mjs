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

  syncFromClassName() {
    String(this.element.className || '')
      .split(/\s+/)
      .filter(Boolean)
      .forEach((name) => this.values.add(name));
  }

  writeClassName() {
    this.element.className = [...this.values].join(' ');
  }

  add(...names) {
    this.syncFromClassName();
    names.forEach((name) => this.values.add(name));
    this.writeClassName();
  }

  remove(...names) {
    this.syncFromClassName();
    names.forEach((name) => this.values.delete(name));
    this.writeClassName();
  }

  contains(name) {
    this.syncFromClassName();
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
      values: {},
      setProperty: (name, value) => { this.style.values[name] = value; },
      removeProperty: (name) => { delete this.style.values[name]; }
    };
    this.classList = new ClassList(this);
    this.className = '';
    this.id = '';
    this.textContent = '';
    this.innerHTML = '';
    this.eventListeners = {};
    this.rect = null;
  }

  setAttribute(name, value) {
    this.attributes.set(name, String(value));
    if (name === 'id') {
      this.id = String(value);
    }
  }

  getAttribute(name) {
    return this.attributes.has(name) ? this.attributes.get(name) : null;
  }

  appendChild(child) {
    if (child.parentNode) {
      child.parentNode.removeChild(child);
    }
    child.parentElement = this;
    child.parentNode = this;
    this.children.push(child);
    return child;
  }

  insertBefore(child, reference) {
    if (child === reference) {
      return child;
    }
    if (child.parentNode) {
      child.parentNode.removeChild(child);
    }
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

  removeEventListener(type, handler) {
    const listeners = this.eventListeners[type] || [];
    this.eventListeners[type] = listeners.filter((candidate) => candidate !== handler);
  }

  dispatchEvent(event) {
    const listeners = this.eventListeners[event.type] || [];
    listeners.forEach((handler) => handler(event));
    return true;
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
  get previousElementSibling() {
    if (!this.parentElement) return null;
    const siblings = this.parentElement.children;
    const index = siblings.indexOf(this);
    return index > 0 ? siblings[index - 1] : null;
  }
  get nextElementSibling() {
    if (!this.parentElement) return null;
    const siblings = this.parentElement.children;
    const index = siblings.indexOf(this);
    return index >= 0 && index < siblings.length - 1 ? siblings[index + 1] : null;
  }
  getBoundingClientRect() {
    const rect = this.rect || { left: 0, top: 0, width: 100, height: 100 };
    const left = Number.isFinite(rect.left) ? rect.left : 0;
    const top = Number.isFinite(rect.top) ? rect.top : 0;
    const width = Number.isFinite(rect.width) ? rect.width : 0;
    const height = Number.isFinite(rect.height) ? rect.height : 0;
    return {
      left,
      top,
      width,
      height,
      right: Number.isFinite(rect.right) ? rect.right : left + width,
      bottom: Number.isFinite(rect.bottom) ? rect.bottom : top + height,
    };
  }
}

const matches = (element, selector) => {
  const idMatch = selector.match(/^\.([a-zA-Z0-9_-]+)\[id="([^"]+)"\]$/);
  if (idMatch) {
    return matches(element, `.${idMatch[1]}`) && element.id === idMatch[2];
  }
  if (selector.startsWith('.')) {
    const className = selector.slice(1);
    return element.classList.contains(className) || String(element.className || '').split(/\s+/).includes(className);
  }
  if (selector === 'pre[data-shebang]') {
    return element.tagName === 'PRE' && element.getAttribute('data-shebang') !== null;
  }
  return false;
};

const boot = ({ writable, content } = {}) => {
  let onReady = null;
  let sortableCreateCount = 0;
  let fetchCalls = 0;
  const sortableCreates = [];
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
  pre.textContent = content || [
    '=== ToDo',
    '==== Card ==== #c1',
    '===== Property',
    '===== Activity'
  ].join('\n');
  const board = new Element('div');
  board.classList.add('kanban-board');
  root.appendChild(pre);
  root.appendChild(board);
  const revisionLink = new Element('a');
  revisionLink.textContent = '1';

  const documentObj = {
    body: new Element('body'),
    addEventListener: (evt, cb) => { if (evt === 'DOMContentLoaded') onReady = cb; },
    removeEventListener: () => {},
    createElement: (tagName) => new Element(tagName),
    querySelectorAll: (selector) => selector === '.InterpreterKanban' ? [root] : [],
    querySelector: (selector) => selector === '.revision a' ? revisionLink : null,
  };

  const sandbox = {
    window: {
      location: { hash: '', pathname: '/w/Page', search: '' },
      history: { pushState: () => {} },
      requestAnimationFrame: (cb) => cb(),
      addEventListener: () => {},
      removeEventListener: () => {},
      Sortable: {
        create: (element, options) => {
          sortableCreateCount += 1;
          sortableCreates.push({ element, options });
          return {};
        }
      },
    },
    document: documentObj,
    console,
    CustomEvent: class {
      constructor(type, init = {}) {
        this.type = type;
        this.detail = init.detail;
      }
    },
    CSS: { escape: (v) => String(v) },
    URLSearchParams,
    fetch: async () => {
      fetchCalls += 1;
      return { ok: true, json: async () => ({}) };
    },
    alert: () => {},
  };
  sandbox.Sortable = sandbox.window.Sortable;
  vm.createContext(sandbox);
  vm.runInContext(source, sandbox);
  if (onReady) onReady();
  return { root, board, sortableCreateCount, sortableCreates, getFetchCalls: () => fetchCalls };
};

const cardIds = (list) => list.querySelectorAll('.kanban-card').map((card) => card.id);

const setColumnRects = (columns) => {
  columns.forEach((column, index) => {
    const left = index * 120;
    column.rect = { left, top: 0, width: 100, height: 360 };
    const list = column.querySelector('.kanban-card-list');
    if (list) {
      list.rect = { left, top: 48, width: 100, height: 260 };
    }
  });
};

const threeListContent = [
  '=== List 1',
  '==== One ==== #c1',
  '===== Property',
  '===== Activity',
  '=== List 2',
  '==== Two ==== #c2',
  '===== Property',
  '===== Activity',
  '=== List 3',
  '==== Three ==== #c3',
  '===== Property',
  '===== Activity'
].join('\n');

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

test('card drag outside expanded drop targets cancels stale list move', () => {
  const { board, sortableCreates, getFetchCalls } = boot({ writable: true, content: threeListContent });
  const columns = board.querySelectorAll('.kanban-column');
  const lists = board.querySelectorAll('.kanban-card-list');
  setColumnRects(columns);

  const dragged = lists[2].querySelectorAll('.kanban-card')[0];
  lists[1].appendChild(dragged);

  const sourceSortable = sortableCreates.find((entry) => entry.element === lists[2]);
  sourceSortable.options.onEnd({
    item: dragged,
    from: lists[2],
    to: lists[1],
    oldIndex: 1,
    newIndex: 2,
    originalEvent: { clientX: 110, clientY: 180 }
  });

  const refreshedLists = board.querySelectorAll('.kanban-card-list');
  assert.deepEqual(cardIds(refreshedLists[0]), ['c1']);
  assert.deepEqual(cardIds(refreshedLists[1]), ['c2']);
  assert.deepEqual(cardIds(refreshedLists[2]), ['c3']);
  assert.equal(getFetchCalls(), 0);
});

test('card drag uses the column as the expanded drop target', () => {
  const { board, sortableCreates } = boot({ writable: true, content: threeListContent });
  const columns = board.querySelectorAll('.kanban-column');
  const lists = board.querySelectorAll('.kanban-card-list');
  setColumnRects(columns);

  const dragged = lists[2].querySelectorAll('.kanban-card')[0];
  lists[1].appendChild(dragged);

  const sourceSortable = sortableCreates.find((entry) => entry.element === lists[2]);
  sourceSortable.options.onEnd({
    item: dragged,
    from: lists[2],
    to: lists[1],
    oldIndex: 1,
    newIndex: 2,
    originalEvent: { clientX: 50, clientY: 340 }
  });

  assert.equal(dragged.parentElement, lists[0]);
  assert.deepEqual(cardIds(lists[0]), ['c1', 'c3']);
  assert.deepEqual(cardIds(lists[1]), ['c2']);
});
