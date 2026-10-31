// The admin UI's colour scheme and the wiki's theme are one setting (app/assets/js/admin/theme.js).
// The browser half -- Mantine actually repainting -- is test/manual/admin-vendor/ with ?theme=dark.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { rootDir } from '../scripts/lib/ahawiki.net.mjs';
import { createWikiThemeManager } from '../app/assets/js/admin/theme.js';

function fakeStorage(initial = {}) {
    const map = new Map(Object.entries(initial));
    return {
        map,
        getItem: key => (map.has(key) ? map.get(key) : null),
        setItem: (key, value) => { map.set(key, value); },
        removeItem: key => { map.delete(key); },
    };
}

function fakeRoot(...initial) {
    const classes = new Set(initial);
    return {
        classes,
        classList: { toggle: (name, on) => { if (on) classes.add(name); else classes.delete(name); } },
    };
}

function fakeEvents() {
    const listeners = new Set();
    return {
        listeners,
        addEventListener: (type, listener) => { assert.equal(type, 'storage'); listeners.add(listener); },
        removeEventListener: (type, listener) => { listeners.delete(listener); },
        fire: event => { for (const listener of listeners) listener(event); },
    };
}

const manager = (storage = fakeStorage(), root = fakeRoot(), events = fakeEvents()) =>
    createWikiThemeManager({ getStorage: () => storage, root, events });

test('reads what the wiki toggle stored', () => {
    assert.equal(manager(fakeStorage({ 'ahawiki-theme': 'ahawiki-dark' })).get('light'), 'dark');
    assert.equal(manager(fakeStorage({ 'ahawiki-theme': 'ahawiki-light' })).get('dark'), 'light');
});

test('nothing stored, or something unknown, is the light theme, as it is on a wiki page', () => {
    assert.equal(manager().get('dark'), 'light');
    assert.equal(manager(fakeStorage({ 'ahawiki-theme': 'solarized' })).get('dark'), 'light');
});

test('blocked storage falls back to the default rather than throwing', () => {
    const blocked = createWikiThemeManager({
        getStorage: () => { throw new Error('storage is blocked'); },
        root: fakeRoot(),
        events: fakeEvents(),
    });
    assert.equal(blocked.get('light'), 'light');
    assert.doesNotThrow(() => blocked.set('dark'));
    assert.doesNotThrow(() => blocked.clear());
});

test('setting writes the wiki value and moves the class on <html>', () => {
    const storage = fakeStorage();
    const root = fakeRoot('theme-ahawiki-light', 'unrelated');
    manager(storage, root).set('dark');
    assert.equal(storage.map.get('ahawiki-theme'), 'ahawiki-dark');
    assert.deepEqual([...root.classes].sort(), ['theme-ahawiki-dark', 'unrelated']);
});

test('the class still moves when storage is blocked', () => {
    const root = fakeRoot('theme-ahawiki-light');
    createWikiThemeManager({
        getStorage: () => { throw new Error('storage is blocked'); },
        root,
        events: fakeEvents(),
    }).set('dark');
    assert.deepEqual([...root.classes], ['theme-ahawiki-dark']);
});

test('a change in another tab arrives; other keys do not, and unsubscribing stops it', () => {
    const events = fakeEvents();
    const seen = [];
    const subject = manager(fakeStorage(), fakeRoot(), events);
    subject.subscribe(scheme => seen.push(scheme));

    events.fire({ key: 'ahawiki-theme', newValue: 'ahawiki-dark' });
    events.fire({ key: 'ahawiki-hue', newValue: '120' });
    events.fire({ key: 'ahawiki-theme', newValue: null });
    assert.deepEqual(seen, ['dark', 'light']);

    subject.unsubscribe();
    assert.equal(events.listeners.size, 0);
});

test('clearing removes the stored choice', () => {
    const storage = fakeStorage({ 'ahawiki-theme': 'ahawiki-dark' });
    manager(storage).clear();
    assert.equal(storage.map.has('ahawiki-theme'), false);
});

// theme.js cannot share a constant with the two templates that also spell these out: one is an
// inline script that has to run before the first paint, the other the wiki's toggle.
test('the templates spell the key and the values the way theme.js does', () => {
    for (const template of ['app/views/_themeClass.scala.html', 'app/views/_base.scala.html']) {
        const text = fs.readFileSync(path.join(rootDir, template), 'utf8');
        for (const name of ['ahawiki-theme', 'ahawiki-light', 'ahawiki-dark']) {
            assert.ok(text.includes(name), `${template} does not name ${name}`);
        }
    }
});
