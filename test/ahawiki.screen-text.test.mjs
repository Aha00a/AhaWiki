// The screens' words live in tables in both languages (logics.ScreenLanguage.TextTable):
// app/logics/EditScreen.scala for the edit screen and app/logics/FrameText.scala for what every
// page wears. Templates ask for them by key, scripts too. A key asked for and missing from its
// table would break the template's render or show the key itself in the script; this finds it by
// name first, from the sources alone.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import test from 'node:test';

const read = (path) => fs.readFileSync(new URL(path, import.meta.url), 'utf8');

function tableOf(path) {
    const source = read(path);
    return {
        source,
        keys: new Set([...source.matchAll(/^\s*"([^"]+)" -> Text\(/gm)].map((m) => m[1])),
    };
}

// Each table, and the places that ask it: [file, pattern whose first group is a key, prefix].
const tables = [
    {
        path: '../app/logics/EditScreen.scala',
        askers: [
            ['../public/js/Wiki/edit.page.js', /editText\('([^']+)'\)/g, ''],
            ['../app/views/Wiki/edit.scala.html', /@t\("([^"]+)"\)/g, ''],
            ['../app/views/Wiki/edit.scala.html', /@kbdHotKeyAlt\("([A-Z])"\)/g, 'hotkey.'],
        ],
    },
    {
        path: '../app/logics/FrameText.scala',
        askers: [
            ['../public/js/js.js', /frameText\('([^']+)'\)/g, ''],
            ['../public/js/js.js', /frameText\(isCollapsed \? '([^']+)'/g, ''],
            ['../public/js/js.js', /: '(js\.[a-z]+)'\)\)/g, ''],
            ['../app/views/_base.scala.html', /@f\("([^"]+)"\)/g, ''],
            ['../app/views/_base.scala.html', /@kbdHotKey\("([A-Z])"\)/g, 'hotkey.'],
            ['../app/views/Wiki/action.scala.html', /@f\("([^"]+)"\)/g, ''],
            ['../app/controllers/Wiki.scala', /\bt\("([^"]+)"\)/g, ''],
        ],
    },
];

for (const { path, askers } of tables) {
    const { source, keys } = tableOf(path);
    const name = path.split('/').pop();

    test(`${name}: entries, each with both languages`, () => {
        assert.ok(keys.size > 20, `found ${keys.size} keys`);
        for (const m of source.matchAll(/^\s*"([^"]+)" -> Text\("([^"]*)", "([^"]*)"\)/gm)) {
            assert.ok(m[2].length > 0 && m[3].length > 0, `${m[1]} has an empty side`);
        }
    });

    for (const [file, pattern, prefix] of askers) {
        test(`${name}: every key ${file.split('/').pop()} asks for (${pattern.source}) is in it`, () => {
            const asked = [...read(file).matchAll(pattern)].map((m) => prefix + m[1]);
            assert.ok(asked.length > 0, `no calls matched in ${file}`);
            assert.deepEqual(asked.filter((key) => !keys.has(key)), []);
        });
    }
}
