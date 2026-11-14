// The edit screen's words live in one table, app/logics/EditScreen.scala, in both languages. The
// template asks for them with t("key") and the script with editText('key'). A key asked for and
// missing from the table would show the key itself in the script and break the template's render;
// this finds it by name first, from the sources alone.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import test from 'node:test';

const read = (path) => fs.readFileSync(new URL(path, import.meta.url), 'utf8');
const table = read('../app/logics/EditScreen.scala');
const keys = new Set([...table.matchAll(/^\s*"([^"]+)" -> Text\(/gm)].map((m) => m[1]));

test('the table has entries, each with both languages', () => {
    assert.ok(keys.size > 50, `found ${keys.size} keys`);
    for (const m of table.matchAll(/^\s*"([^"]+)" -> Text\("([^"]*)", "([^"]*)"\)/gm)) {
        assert.ok(m[2].length > 0 && m[3].length > 0, `${m[1]} has an empty side`);
    }
});

test('every key the script asks for is in the table', () => {
    const script = read('../public/js/Wiki/edit.page.js');
    const asked = [...script.matchAll(/editText\('([^']+)'\)/g)].map((m) => m[1]);
    assert.ok(asked.length > 15, `found ${asked.length} calls`);
    assert.deepEqual(asked.filter((key) => !keys.has(key)), []);
});

test('every key the template asks for is in the table, the Alt hints included', () => {
    const template = read('../app/views/Wiki/edit.scala.html');
    const asked = [...template.matchAll(/@t\("([^"]+)"\)/g)].map((m) => m[1]);
    const hints = [...template.matchAll(/@kbdHotKeyAlt\("([A-Z])"\)/g)].map((m) => `hotkey.${m[1]}`);
    assert.ok(asked.length > 40, `found ${asked.length} calls`);
    assert.deepEqual([...asked, ...hints].filter((key) => !keys.has(key)), []);
});
