// Every theme colour token used anywhere is declared somewhere. A var() of a name nobody declares
// is not an error in CSS: the property is dropped and the element takes its parent's colour, so a
// renamed or misspelt token fails without a word. One did -- the Gantt editor's column headings
// asked for --color-text-subtle from 2026-06-01, which never existed, until 2026-11-21.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';

function filesUnder(dir, extensions) {
    const out = [];
    for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
        const full = path.join(dir, entry.name);
        if (entry.isDirectory()) {
            if (entry.name === 'vendor' || entry.name === 'babel' || entry.name === 'node_modules') continue;
            out.push(...filesUnder(full, extensions));
        } else if (extensions.some((ext) => entry.name.endsWith(ext))) {
            out.push(full);
        }
    }
    return out;
}

test('every --color-* token a stylesheet, template or script asks for is declared in a stylesheet', () => {
    const less = filesUnder('app/assets', ['.less']);
    const declared = new Set(less.flatMap((file) => [...fs.readFileSync(file, 'utf8').matchAll(/(--color-[a-z0-9-]+)\s*:/g)].map((m) => m[1])));
    const users = [...less, ...filesUnder('app/views', ['.html']), ...filesUnder('public/js', ['.js']), ...filesUnder('app/assets/js', ['.js', '.jsx'])];
    const missing = [];
    for (const file of users) {
        for (const m of fs.readFileSync(file, 'utf8').matchAll(/var\((--color-[a-z0-9-]+)/g)) {
            if (!declared.has(m[1])) missing.push(`${m[1]} in ${file}`);
        }
    }
    assert.ok(declared.size > 50, `read the declarations (${declared.size})`);
    assert.deepEqual([...new Set(missing)], []);
});
