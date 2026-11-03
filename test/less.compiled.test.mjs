// The repository commits each stylesheet twice: the .less that is edited and the .css the pages
// load. Nothing but discipline kept the second equal to the first -- a hand-edited .css passed
// every check and surfaced as a surprise diff on the next recompile, which is how kanban.css lost
// its sourceMappingURL line and adjacent.css came to hold a rule out of lessc's order. This
// compiles every deterministic stylesheet with the pinned lessc and compares bytes.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { assetsDir, compile, remoteImportOnly, stylesheets } from '../scripts/less.mjs';

test('every committed stylesheet is what lessc makes of its .less', () => {
    // Compiled in a copy, not in place: the test must not rewrite the tree it is checking, and
    // the map's sources are relative to the output, so sources and output share a directory.
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'ahawiki-less-'));
    try {
        for (const file of fs.readdirSync(assetsDir).filter(file => file.endsWith('.less'))) {
            fs.copyFileSync(path.join(assetsDir, file), path.join(dir, file));
        }
        const stale = [];
        for (const name of stylesheets) {
            compile(name, dir);
            for (const extension of ['.css', '.css.map']) {
                const fresh = fs.readFileSync(path.join(dir, name + extension));
                const committed = fs.readFileSync(path.join(assetsDir, name + extension));
                if (!fresh.equals(committed)) stale.push(name + extension);
            }
        }
        assert.deepEqual(stale, [], `Committed stylesheets differ from what their .less compiles to:\n  ${stale.join('\n  ')}\n\n` +
            'Run `npm run less` and commit the result, or move the hand edit into the .less file.');
    } finally {
        fs.rmSync(dir, { recursive: true, force: true });
    }
});

test('the list covers every stylesheet whose output is deterministic', () => {
    const compiled = fs.readdirSync(assetsDir)
        .filter(file => file.endsWith('.less') && !file.startsWith('_'))
        .map(file => file.slice(0, -'.less'.length));
    const unlisted = compiled.filter(name => !stylesheets.includes(name) && !(name in remoteImportOnly));
    assert.deepEqual(unlisted, [], `Stylesheets neither listed nor excused in scripts/less.mjs: ${unlisted.join(', ')}`);

    for (const name of stylesheets) {
        const source = fs.readFileSync(path.join(assetsDir, `${name}.less`), 'utf8');
        assert.ok(!/@import\s+(?:\([^)]*\)\s*)?url\(/.test(source), `${name}.less has a remote @import; its output changes with the day, so it cannot be listed`);
    }
    for (const [name, reason] of Object.entries(remoteImportOnly)) {
        assert.ok(reason.length > 20, `${name} needs a reason for staying out of the list`);
        assert.ok(fs.existsSync(path.join(assetsDir, `${name}.less`)), `${name}.less is excused but does not exist`);
    }
});
