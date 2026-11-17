import assert from 'node:assert/strict';
import fs from 'node:fs';
import test from 'node:test';
import vm from 'node:vm';

/**
 * The autocomplete after `[[[#!table `: which names fit depends on which word is being typed.
 *
 * One flat list used to be offered at every word, so after `[[[#!table tsv 1 ` it offered `tsv`
 * and `tsv 1 bar=all` again, and picking one wrote `tsv 1 tsv 1 bar=all`.
 */

function load() {
    const sandbox = { window: {} };
    vm.runInNewContext(fs.readFileSync('public/js/Wiki/edit.tableOptions.js', 'utf8'), sandbox);
    return sandbox.window.AhaWiki.TableOptions;
}
const { suggest, options, presets } = load();
const names = (left) => [...suggest(left).names];

test('outside a table shebang there is nothing to offer', () => {
    assert.equal(suggest('[[[#!Vim tsv '), null);
    assert.equal(suggest('[[[#!table'), null, 'the interpreter name is still being typed');
    assert.equal(suggest('[[[#!table tsv\nbar'), null, 'the shebang is one line');
});

test('the first word is the format, or a whole first line to start from', () => {
    assert.deepEqual(names('[[[#!table '), [...presets]);
    const s = suggest('[[[#!Table ts');
    assert.equal(s.prefix, 'ts');
    assert.equal(s.start, '[[[#!Table '.length);
});

test('after the format: another header count, then the options', () => {
    assert.deepEqual(names('[[[#!table tsv '), ['1', ...options]);
    assert.deepEqual(names('[[[#!table tsv 1 '), ['1', ...options]);
    assert.deepEqual(names('[[[#!table tsv 1 1 '), [...options], 'two counts at most');
    assert.deepEqual(names('[[[#!table csv compact '), ['bar=all', 'heat=all', 'tag='], 'no count after an option');
});

test('the format is never offered again after the first word', () => {
    for (const left of ['[[[#!table tsv ', '[[[#!table tsv 1 ', '[[[#!table tsv 1 1 bar=all ']) {
        const offered = names(left);
        assert.ok(!offered.includes('tsv') && !offered.includes('csv'), `${left} -> ${offered}`);
        assert.ok(!offered.some(n => n.includes(' ')), `no whole first lines after the first word: ${offered}`);
    }
});

test('what is already written is not offered again, case aside', () => {
    assert.deepEqual(names('[[[#!table tsv 1 Bar=all '), ['heat=all', 'tag=', 'compact']);
    assert.deepEqual(names('[[[#!table tsv 1 tag=5 heat=2 '), ['bar=all', 'compact'], 'by key, whatever the value');
});

test('the word being typed is replaced from where it starts', () => {
    const left = '[[[#!table tsv 1 he';
    const s = suggest(left);
    assert.equal(s.prefix, 'he');
    assert.equal(left.slice(0, s.start) + 'heat=all', '[[[#!table tsv 1 heat=all');
});

test('every option key the interpreter reads is offered', () => {
    const scala = fs.readFileSync('app/logics/wikis/interpreters/InterpreterTable.scala', 'utf8');
    const keys = [...scala.matchAll(/parseRules\(tokens, "([a-z]+)"\)/g)].map(m => m[1]);
    assert.ok(keys.length >= 2, `found ${keys}`);
    for (const key of keys)
        assert.ok(options.some(o => o.startsWith(`${key}=`)), `${key}= is read by InterpreterTable but not offered`);
});
