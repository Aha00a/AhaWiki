import assert from 'node:assert/strict';
import fs from 'node:fs';
import test from 'node:test';
import vm from 'node:vm';

class StringStream {
    constructor(line) {
        this.string = line;
        this.pos = 0;
    }

    sol() {
        return this.pos === 0;
    }

    match(pattern) {
        if (typeof pattern === 'string') {
            if (!this.string.startsWith(pattern, this.pos))
                return false;
            this.pos += pattern.length;
            return true;
        }

        const match = this.string.slice(this.pos).match(pattern);
        if (!match || match.index !== 0)
            return false;
        this.pos += match[0].length;
        return true;
    }

    skipToEnd() {
        this.pos = this.string.length;
    }

    next() {
        if (this.pos < this.string.length)
            this.pos += 1;
    }
}

function loadMode() {
    let modeFactory = null;
    const sandbox = {
        window: {},
        CodeMirror: {
            modes: {},
            defineMode(name, factory) {
                this.modes[name] = factory;
                modeFactory = factory;
            },
        },
    };
    sandbox.window.CodeMirror = sandbox.CodeMirror;
    vm.runInNewContext(fs.readFileSync('public/js/AhaWiki.CodeMirror.AhaMark.js', 'utf8'), sandbox);
    sandbox.window.AhaWikiCodeMirrorAhaMarkMode();
    return modeFactory();
}

function tokensFor(line) {
    const mode = loadMode();
    const state = mode.startState();
    const stream = new StringStream(line);
    const tokens = [];
    while (stream.pos < line.length) {
        const start = stream.pos;
        const style = mode.token(stream, state);
        const text = line.slice(start, stream.pos);
        if (style)
            tokens.push({ text, style });
        assert.ok(stream.pos > start, `tokenizer did not advance at ${start}`);
    }
    return tokens;
}

test('highlights AhaMark Basic inline formatting', () => {
    const tokens = tokensFor("''italic'' '''bold''' '''''bold italic''''' __underline__ ~~strikeout~~ `code` ``copiable``");
    assert.deepEqual(tokens, [
        { text: "''italic''", style: 'ahamark-italic' },
        { text: "'''bold'''", style: 'ahamark-bold' },
        { text: "'''''bold italic'''''", style: 'ahamark-bold-italic' },
        { text: '__underline__', style: 'ahamark-underline' },
        { text: '~~strikeout~~', style: 'ahamark-strikeout' },
        { text: '`code`', style: 'ahamark-code' },
        { text: '``copiable``', style: 'ahamark-copiable' },
    ]);
});

test('highlights AhaMark Basic block and link syntax', () => {
    assert.deepEqual(tokensFor('== Heading == #anchor'), [
        { text: '== Heading == #anchor', style: 'ahamark-header' },
    ]);
    assert.deepEqual(tokensFor(' * bullet'), [
        { text: ' * ', style: 'ahamark-list' },
    ]);
    assert.deepEqual(tokensFor(' 1. decimal'), [
        { text: ' 1. ', style: 'ahamark-list' },
    ]);
    assert.deepEqual(tokensFor('----'), [
        { text: '----', style: 'ahamark-hr' },
    ]);
    assert.deepEqual(tokensFor('#!var class landscape'), [
        { text: '#!var class landscape', style: 'ahamark-interpreter-shebang' },
    ]);
    assert.deepEqual(tokensFor('  #!portrait'), [
        { text: '  #!portrait', style: 'ahamark-interpreter-shebang' },
    ]);
    assert.deepEqual(tokensFor('[Page Alias] http://aha00a.com'), [
        { text: '[Page Alias]', style: 'ahamark-link' },
        { text: 'http://aha00a.com', style: 'ahamark-link' },
    ]);
    assert.deepEqual(tokensFor('[2026-05-22 2026-05-22T10:20:30]'), [
        { text: '[2026-05-22 2026-05-22T10:20:30]', style: 'ahamark-date' },
    ]);
});

test('does not highlight escaped AhaMark links or URLs', () => {
    assert.deepEqual(tokensFor('\\[Page Alias] \\http://aha00a.com'), []);
});
