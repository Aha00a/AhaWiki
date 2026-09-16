// Unit tests for the formatting-toolbar text operations (public/js/AhaWiki.Format.js). Pure text
// in, minimal edit out; the DOM wiring in edit.page.js applies the same result to CodeMirror or the
// textarea.
import assert from 'node:assert/strict';
import fs from 'node:fs';
import test from 'node:test';
import vm from 'node:vm';

const code = fs.readFileSync(new URL('../public/js/AhaWiki.Format.js', import.meta.url), 'utf8');
const ctx = { window: {} };
vm.createContext(ctx);
vm.runInContext(code, ctx);
const Format = ctx.window.AhaWiki.Format;

// The module runs in a vm context, so its objects carry that realm's Object.prototype; spread each
// result into a plain object before deepEqual, which otherwise rejects the prototype mismatch.
test('wrap: a selection is wrapped and stays selected', () => {
    assert.deepEqual({ ...Format.wrap('abc', 0, 3, "'''", "'''") }, {
        rangeStart: 0, rangeEnd: 3, replacement: "'''abc'''", selectionStart: 3, selectionEnd: 6,
    });
});

test('wrap: an empty selection puts the caret between prefix and suffix', () => {
    assert.deepEqual({ ...Format.wrap('abc', 1, 1, "''", "''") }, {
        rangeStart: 1, rangeEnd: 1, replacement: "''''", selectionStart: 3, selectionEnd: 3,
    });
});

test('wrap: link wraps the selection in [ ]', () => {
    const r = Format.wrap('Home', 0, 4, '[', ']');
    assert.equal(r.replacement, '[Home]');
    assert.equal(r.selectionStart, 1);
    assert.equal(r.selectionEnd, 5);
});

test('prefixLines: a multi-line selection prefixes each line', () => {
    assert.deepEqual({ ...Format.prefixLines('a\nb\nc', 0, 3, ' * ') }, {
        rangeStart: 0, rangeEnd: 3, replacement: ' * a\n * b', selectionStart: 0, selectionEnd: 9,
    });
});

test('prefixLines: heading on a single line, no selection', () => {
    assert.deepEqual({ ...Format.prefixLines('hello', 2, 2, '== ') }, {
        rangeStart: 0, rangeEnd: 5, replacement: '== hello', selectionStart: 0, selectionEnd: 8,
    });
});

test('prefixLines: only the caret line among many is prefixed', () => {
    const r = Format.prefixLines('x\nhello\ny', 4, 4, '== ');
    assert.equal(r.rangeStart, 2);
    assert.equal(r.rangeEnd, 7);
    assert.equal(r.replacement, '== hello');
});
