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

  removeAttribute(name) {
    this.attributes.delete(name);
    if (name === 'id') {
      this.id = '';
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

  cloneNode(deep = false) {
    const clone = new Element(this.tagName);
    clone.className = this.className;
    clone.classList.syncFromClassName();
    clone.id = this.id;
    clone.textContent = this.textContent;
    clone.innerHTML = this.innerHTML;
    clone.rect = this.rect ? { ...this.rect } : null;
    this.attributes.forEach((value, key) => clone.setAttribute(key, value));
    if (deep) {
      this.children.forEach((child) => clone.appendChild(child.cloneNode(true)));
    }
    return clone;
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
      requestAnimationFrame: () => 1,
      cancelAnimationFrame: () => {},
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
  return { root, board, body: documentObj.body, sortableCreateCount, sortableCreates, getFetchCalls: () => fetchCalls };
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

test('card Sortable uses Trello-like drag preview classes', () => {
  const { board, sortableCreates } = boot({ writable: true });
  const list = board.querySelector('.kanban-card-list');
  const cardSortable = sortableCreates.find((entry) => entry.element === list);

  assert.ok(cardSortable);
  assert.equal(cardSortable.options.draggable, '.kanban-card:not(.kanban-card-drag-origin)');
  assert.equal(cardSortable.options.ghostClass, 'kanban-card-drag-placeholder');
  assert.equal(cardSortable.options.chosenClass, 'kanban-card-drag-chosen');
  assert.equal(cardSortable.options.dragClass, 'kanban-card-drag-placeholder');
  assert.equal(cardSortable.options.forceFallback, true);
  assert.equal(cardSortable.options.fallbackClass, 'kanban-card-drag-preview');
  assert.equal(cardSortable.options.fallbackOnBody, true);

  const previewClone = list.querySelector('.kanban-card').cloneNode(true);
  cardSortable.options.onClone({ clone: previewClone });
  assert.equal(previewClone.id, '');
  assert.equal(previewClone.getAttribute('data-line-number'), null);
  assert.equal(previewClone.getAttribute('aria-hidden'), 'true');
  assert.equal(previewClone.classList.contains('kanban-card-drag-preview'), true);
});

test('card drag keeps an origin clone only while dragging', () => {
  const { root, board, body, sortableCreates } = boot({ writable: true });
  const columns = board.querySelectorAll('.kanban-column');
  const lists = board.querySelectorAll('.kanban-card-list');
  setColumnRects(columns);

  const dragged = lists[0].querySelectorAll('.kanban-card')[0];
  dragged.classList.add('kanban-card-drag-placeholder', 'kanban-card-drag-chosen');
  const sourceSortable = sortableCreates.find((entry) => entry.element === lists[0]);
  sourceSortable.options.onStart({
    item: dragged,
    from: lists[0],
    oldIndex: 1,
    originalEvent: { clientX: 50, clientY: 180 }
  });

  assert.equal(root.classList.contains('kanban-card-dragging'), true);
  assert.equal(body.classList.contains('kanban-card-dragging-global'), true);

  const originClone = lists[0].querySelector('.kanban-card-drag-origin');
  assert.ok(originClone);
  assert.equal(originClone.id, '');
  assert.equal(originClone.getAttribute('aria-hidden'), 'true');
  assert.equal(originClone.parentElement, lists[0]);
  assert.equal(originClone.classList.contains('kanban-card-drag-origin'), true);
  assert.equal(originClone.classList.contains('kanban-card-drag-placeholder'), false);
  assert.equal(originClone.classList.contains('kanban-card-drag-chosen'), false);
  assert.equal(lists[0].children.indexOf(originClone), 1);
  assert.equal(lists[0].children.indexOf(dragged), 2);

  sourceSortable.options.onMove({
    item: dragged,
    from: lists[0],
    to: lists[0],
    oldIndex: 1,
    newIndex: 2,
    originalEvent: { clientX: 50, clientY: 180 }
  });

  assert.equal(lists[0].style.values.outline, undefined);
  assert.equal(lists[0].style.values.background, undefined);

  sourceSortable.options.onMove({
    item: dragged,
    from: lists[0],
    to: lists[0],
    oldIndex: 1,
    newIndex: 2,
    originalEvent: { clientX: -50, clientY: -50 }
  });

  assert.equal(lists[0].children.indexOf(originClone), 1);
  assert.equal(lists[0].children.indexOf(dragged), 2);

  sourceSortable.options.onEnd({
    item: dragged,
    from: lists[0],
    to: lists[0],
    oldIndex: 1,
    newIndex: 1,
    originalEvent: { clientX: 50, clientY: 180 }
  });

  assert.equal(board.querySelectorAll('.kanban-card-drag-origin').length, 0);
  assert.equal(root.classList.contains('kanban-card-dragging'), false);
  assert.equal(body.classList.contains('kanban-card-dragging-global'), false);
  assert.equal(dragged.classList.contains('kanban-card-drag-placeholder'), false);
  assert.equal(dragged.classList.contains('kanban-card-drag-chosen'), false);
});

test('list Sortable uses Trello-like drag preview classes', () => {
  const { board, sortableCreates } = boot({ writable: true });
  const boardSortable = sortableCreates.find((entry) => entry.element === board);

  assert.ok(boardSortable);
  assert.equal(boardSortable.options.draggable, '.kanban-column:not(.kanban-list-drag-origin)');
  assert.equal(boardSortable.options.ghostClass, 'kanban-list-drag-placeholder');
  assert.equal(boardSortable.options.chosenClass, 'kanban-list-drag-chosen');
  assert.equal(boardSortable.options.dragClass, 'kanban-list-drag-placeholder');
  assert.equal(boardSortable.options.forceFallback, true);
  assert.equal(boardSortable.options.fallbackClass, 'kanban-list-drag-preview');
  assert.equal(boardSortable.options.fallbackOnBody, true);

  const previewClone = board.querySelector('.kanban-column').cloneNode(true);
  previewClone.classList.add('kanban-list-drag-placeholder', 'kanban-list-drag-chosen');
  boardSortable.options.onClone({ clone: previewClone });
  assert.equal(previewClone.getAttribute('data-column-index'), null);
  assert.equal(previewClone.getAttribute('data-column-line-number'), null);
  assert.equal(previewClone.getAttribute('aria-hidden'), 'true');
  assert.equal(previewClone.classList.contains('kanban-list-drag-preview'), true);
  assert.equal(previewClone.classList.contains('kanban-list-drag-placeholder'), false);
  assert.equal(previewClone.classList.contains('kanban-list-drag-chosen'), false);
});

test('list drag keeps an origin clone only while dragging', () => {
  const { root, board, body, sortableCreates } = boot({ writable: true, content: threeListContent });
  const columns = board.querySelectorAll('.kanban-column');
  const dragged = columns[0];
  dragged.classList.add('kanban-list-drag-placeholder', 'kanban-list-drag-chosen');
  const boardSortable = sortableCreates.find((entry) => entry.element === board);

  boardSortable.options.onStart({
    item: dragged,
    from: board,
    oldIndex: 0,
    oldDraggableIndex: 0,
    originalEvent: { clientX: 50, clientY: 180 }
  });

  const originClone = board.querySelector('.kanban-list-drag-origin');
  assert.ok(originClone);
  assert.equal(originClone.getAttribute('aria-hidden'), 'true');
  assert.equal(originClone.parentElement, board);
  assert.equal(originClone.classList.contains('kanban-list-drag-origin'), true);
  assert.equal(originClone.classList.contains('kanban-list-drag-placeholder'), false);
  assert.equal(originClone.classList.contains('kanban-list-drag-chosen'), false);
  assert.equal(board.children.indexOf(originClone), 0);
  assert.equal(board.children.indexOf(dragged), 1);
  assert.equal(root.classList.contains('kanban-list-dragging'), true);
  assert.equal(body.classList.contains('kanban-list-dragging-global'), true);

  boardSortable.options.onEnd({
    item: dragged,
    from: board,
    to: board,
    oldIndex: 0,
    oldDraggableIndex: 0,
    newIndex: 0,
    newDraggableIndex: 0,
    originalEvent: { clientX: 50, clientY: 180 }
  });

  assert.equal(board.querySelectorAll('.kanban-list-drag-origin').length, 0);
  assert.equal(root.classList.contains('kanban-list-dragging'), false);
  assert.equal(body.classList.contains('kanban-list-dragging-global'), false);
  assert.equal(dragged.classList.contains('kanban-list-drag-placeholder'), false);
  assert.equal(dragged.classList.contains('kanban-list-drag-chosen'), false);
});

test('list drag computes drop index without the origin clone', () => {
  const { board, sortableCreates } = boot({ writable: true, content: threeListContent });
  const columns = board.querySelectorAll('.kanban-column');
  const dragged = columns[0];
  const boardSortable = sortableCreates.find((entry) => entry.element === board);

  boardSortable.options.onStart({
    item: dragged,
    from: board,
    oldIndex: 0,
    oldDraggableIndex: 0,
    originalEvent: { clientX: 50, clientY: 180 }
  });
  board.insertBefore(dragged, columns[2]);

  boardSortable.options.onEnd({
    item: dragged,
    from: board,
    to: board,
    oldIndex: 0,
    oldDraggableIndex: 0,
    newIndex: 2,
    newDraggableIndex: 1,
    originalEvent: { clientX: 180, clientY: 180 }
  });

  const titles = board.querySelectorAll('.kanban-column-title').map((title) => title.textContent);
  assert.deepEqual(titles, ['List 2', 'List 1', 'List 3']);
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
