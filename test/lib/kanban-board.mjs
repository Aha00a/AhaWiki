// A fake of just enough DOM to build a Kanban board in node:vm, and two ways to load
// public/js/AhaWiki.Kanban.js: bootBoard builds a board, and loadKanbanHooks loads the script with
// no board on the page, for the functions it publishes to tests. Moved here on 2026-09-15 from
// kanban.readonly.test.mjs and kanban.remote-merge.test.mjs, when the save-path tests needed the
// same board and the same bare load.
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
    this._className = '';
    Object.defineProperty(this, 'className', {
      get: () => this._className,
      set: (value) => {
        this._className = String(value || '');
        this.applyLayoutDefaults();
      }
    });
    this.id = '';
    this.textContent = '';
    this.innerHTML = '';
    this.eventListeners = {};
    this.rect = null;
    this.scrollLeft = 0;
    this.scrollTop = 0;
    this.scrollWidth = 100;
    this.scrollHeight = 100;
    this.clientWidth = 100;
    this.clientHeight = 100;
    this.scrollIntoViewCalls = [];
    this.className = '';
  }

  applyLayoutDefaults() {
    const names = String(this._className || '').split(/\s+/);
    if (names.includes('kanban-card-list')) {
      this.clientHeight = 160;
      this.scrollHeight = 720;
    }
    if (names.includes('kanban-card')) {
      this.clientHeight = 56;
      this.scrollHeight = 56;
    }
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

  scrollIntoView(options) {
    this.scrollIntoViewCalls.push(options || {});
  }
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
    let rect = this.rect;
    if (!rect && this.classList.contains('kanban-board')) {
      rect = { left: 0, top: 0, width: this.clientWidth, height: this.clientHeight };
    }
    if (!rect && this.classList.contains('kanban-column')) {
      const board = this.parentElement;
      const boardRect = board ? board.getBoundingClientRect() : { left: 0, top: 0 };
      const index = Number(this.getAttribute('data-column-index')) || 0;
      rect = {
        left: boardRect.left + (index * 320) - (board ? board.scrollLeft : 0),
        top: boardRect.top,
        width: 300,
        height: 360
      };
    }
    if (!rect && this.classList.contains('kanban-card-list')) {
      const columnRect = this.parentElement ? this.parentElement.getBoundingClientRect() : { left: 0, top: 0 };
      rect = {
        left: columnRect.left + 12,
        top: columnRect.top + 48,
        width: 276,
        height: this.clientHeight
      };
    }
    if (!rect && this.classList.contains('kanban-card')) {
      const list = this.parentElement;
      const listRect = list ? list.getBoundingClientRect() : { left: 0, top: 0 };
      const cards = list ? list.children.filter((child) => child.classList.contains('kanban-card')) : [];
      const index = Math.max(0, cards.indexOf(this));
      rect = {
        left: listRect.left,
        top: listRect.top + (index * 72) - (list ? list.scrollTop : 0),
        width: 276,
        height: 56
      };
    }
    rect = rect || { left: 0, top: 0, width: 100, height: 100 };
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
  if (selector === '[data-wiki-writable]') {
    return element.getAttribute('data-wiki-writable') !== null;
  }
  return false;
};

export const bootBoard = ({ writable, content, hash = '', currentUserNickname = '', fetchImpl = null, revision = 1, wrap = null } = {}) => {
  let onReady = null;
  let sortableCreateCount = 0;
  let fetchCalls = 0;
  const copiedTexts = [];
  const sortableCreates = [];
  const root = new Element('div');
  root.classList.add('InterpreterKanban');
  root.setAttribute('data-page-name', 'Page');
  root.id = 'kanban-test';

  const wikiContent = new Element('div');
  wikiContent.classList.add('wikiContent');
  wikiContent.setAttribute('data-wiki-writable', writable ? 'true' : 'false');
  // The render puts an interpreter's output in a wrapper that says which lines it came from.
  let metaWrapper = null;
  if (wrap) {
    metaWrapper = new Element('div');
    metaWrapper.classList.add('InterpreterRenderMetaWrapper');
    metaWrapper.setAttribute('data-line-start', String(wrap.lineStart));
    metaWrapper.setAttribute('data-line-end', String(wrap.lineEnd));
    metaWrapper.appendChild(root);
    wikiContent.appendChild(metaWrapper);
  } else {
    wikiContent.appendChild(root);
  }

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
  board.clientWidth = 240;
  board.scrollWidth = 1040;
  board.clientHeight = 360;
  board.scrollHeight = 360;
  root.appendChild(pre);
  root.appendChild(board);
  const revisionLink = new Element('a');
  revisionLink.textContent = String(revision);

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
      location: {
        origin: 'https://example.test',
        href: 'https://example.test/w/Page' + hash,
        hash,
        pathname: '/w/Page',
        search: ''
      },
      history: {
        pushState: () => {},
        replaceState: (_a, _b, url) => {
          const [pathAndSearch, nextHash = ''] = String(url).split('#');
          const queryIndex = pathAndSearch.indexOf('?');
          sandbox.window.location.pathname = queryIndex >= 0 ? pathAndSearch.slice(0, queryIndex) : pathAndSearch;
          sandbox.window.location.search = queryIndex >= 0 ? pathAndSearch.slice(queryIndex) : '';
          sandbox.window.location.hash = nextHash ? '#' + nextHash : '';
          sandbox.window.location.href = sandbox.window.location.origin + sandbox.window.location.pathname + sandbox.window.location.search + sandbox.window.location.hash;
        }
      },
      navigator: {
        clipboard: {
          writeText: async (text) => {
            copiedTexts.push(text);
          }
        }
      },
      requestAnimationFrame: () => 1,
      cancelAnimationFrame: () => {},
      addEventListener: () => {},
      removeEventListener: () => {},
      AhaWikiCurrentUserNickname: currentUserNickname,
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
    fetch: fetchImpl || (async () => {
      fetchCalls += 1;
      return { ok: true, json: async () => ({}) };
    }),
    alert: () => {},
  };
  sandbox.Sortable = sandbox.window.Sortable;
  vm.createContext(sandbox);
  vm.runInContext(source, sandbox);
  if (onReady) onReady();
  return { root, board, body: documentObj.body, copiedTexts, sortableCreateCount, sortableCreates, getFetchCalls: () => fetchCalls, revisionLink, metaWrapper };
};

export const cardIds = (list) => list.querySelectorAll('.kanban-card').map((card) => card.getAttribute('data-card-id'));

export const setColumnRects = (columns) => {
  columns.forEach((column, index) => {
    const left = index * 120;
    column.rect = { left, top: 0, width: 100, height: 360 };
    const list = column.querySelector('.kanban-card-list');
    if (list) {
      list.rect = { left, top: 48, width: 100, height: 260 };
    }
  });
};

// The script with no board on the page: querySelectorAll finds none, so none is built. The
// revision link, the alerts, fetch and reCAPTCHA are what the save path touches.
export function loadKanbanHooks({ fetchImpl = () => Promise.reject(new Error('this test makes no requests')), revision = 1, reCaptcha = null } = {}) {
    let onReady = null;
    let reloads = 0;
    const revisionNode = { textContent: String(revision) };
    const alerts = [];
    const sandbox = {
        window: {
            location: { hash: '', pathname: '/w/Test', search: '', reload() { reloads += 1; } },
            history: { pushState() {} },
            requestAnimationFrame() {},
            addEventListener() {},
            alert(message) { alerts.push(message); },
        },
        document: {
            addEventListener: (event, callback) => { if (event === 'DOMContentLoaded') onReady = callback; },
            querySelectorAll: () => [],
            querySelector: selector => (selector === '.revision a' ? revisionNode : null),
        },
        console,
        alert(message) { alerts.push(message); },
        fetch: fetchImpl,
        CustomEvent: class {},
        CSS: { escape: value => String(value) },
        URLSearchParams,
    };
    vm.createContext(sandbox);
    vm.runInContext(source, sandbox);
    onReady();
    // Set after the script has run, as the page does: AhaWiki.ReCaptcha.js is its own script.
    if (reCaptcha) {
        sandbox.window.AhaWiki = sandbox.window.AhaWiki || {};
        sandbox.window.AhaWiki.ReCaptcha = reCaptcha;
    }
    return { hooks: sandbox.window.__AhaWikiKanbanTestHooks, revisionNode, alerts, reloadCount: () => reloads };
}
