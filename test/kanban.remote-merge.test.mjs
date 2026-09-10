// A remote Kanban update is merged three ways: base (last agreed state), local (this tab),
// server (just fetched). What the merge hands back as the next base decides whether the
// second remote update sees phantom conflicts. Until 2026-09-10 the base was refreshed only
// by a save, so a card the server had changed once read as changed on both sides forever
// after: a conflict badge on every later update, and an edit made here in between was lost
// to the server's older copy. Each test below runs the merge with the base it hands back and
// with the stale one, so it shows what was wrong and not only what is right now.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync('public/js/AhaWiki.Kanban.js', 'utf8');

// The merge lives inside the script's DOMContentLoaded closure, so the script is run in a
// sandbox and the function taken from the hooks it publishes for tests. This is the least
// that lets the script load: no board is built, because querySelectorAll finds none. Four
// other tests carry their own boot, each shaped for what it drives (a fake DOM, the location,
// the alerts); one boot serving all five shapes was not attempted here.
function loadMerge() {
    return loadHooks().mergeRemoteKanbanColumns;
}

function loadHooks() {
    let onReady = null;
    const sandbox = {
        window: {
            location: { hash: '', pathname: '/w/Test', search: '', reload() {} },
            history: { pushState() {} },
            requestAnimationFrame() {},
            addEventListener() {},
            alert() {},
        },
        document: {
            addEventListener: (event, callback) => { if (event === 'DOMContentLoaded') onReady = callback; },
            querySelectorAll: () => [],
            querySelector: selector => (selector === '.revision a' ? { textContent: '1' } : null),
        },
        console,
        alert() {},
        fetch: () => Promise.reject(new Error('this test makes no requests')),
        CustomEvent: class {},
        CSS: { escape: value => String(value) },
        URLSearchParams,
    };
    vm.createContext(sandbox);
    vm.runInContext(source, sandbox);
    onReady();
    return sandbox.window.__AhaWikiKanbanTestHooks;
}

const card = (id, text) => ({ id, text, classNames: [], lineNumber: 0, description: [], comments: [], properties: {} });
const board = (...cards) => [{ title: 'Todo', lineNumber: 1, cards }];
// Results come from the sandbox realm, whose prototypes strict deepEqual rejects; JSON brings
// them back as plain host objects.
const plain = value => JSON.parse(JSON.stringify(value));

test('a card the server changed once is not a conflict on the next remote update', () => {
    const merge = loadMerge();
    const base = board(card('a', 'A'), card('b', 'B'));
    const first = merge(base, board(card('a', 'A'), card('b', 'B')), board(card('a', 'A2'), card('b', 'B')), '');
    assert.deepEqual(plain(first.conflictCardIds), []);
    assert.equal(first.columns[0].cards[0].text, 'A2');

    // The next update changes b only. With the base the first merge handed back, a is untouched.
    const second = merge(first.baseColumns, first.columns, board(card('a', 'A2'), card('b', 'B2')), '');
    assert.deepEqual(plain(second.conflictCardIds), []);
    assert.equal(second.columns[0].cards[0].__remoteConflict, undefined);
    assert.equal(second.columns[0].cards[1].text, 'B2');

    // With the base a save had left -- what the code kept until 2026-09-10 -- a reads as
    // changed on both sides and is flagged though nobody here touched it.
    const stale = merge(base, first.columns, board(card('a', 'A2'), card('b', 'B2')), '');
    assert.deepEqual(plain(stale.conflictCardIds), ['a']);
});

test('an edit made here after a remote change survives the next remote update', () => {
    const merge = loadMerge();
    const base = board(card('a', 'A'));
    const first = merge(base, board(card('a', 'A')), board(card('a', 'A2')), '');

    // This tab edits the card the server just changed, and has not saved.
    const local = plain(first.columns);
    local[0].cards[0].text = 'A2, and mine';

    const second = merge(first.baseColumns, local, board(card('a', 'A2'), card('b', 'B')), '');
    assert.equal(second.columns[0].cards[0].text, 'A2, and mine');
    assert.equal(second.columns[0].cards[1].text, 'B');
    assert.deepEqual(plain(second.conflictCardIds), []);

    // With the stale base the server's older copy won and the edit was gone.
    const stale = merge(base, local, board(card('a', 'A2'), card('b', 'B')), '');
    assert.equal(stale.columns[0].cards[0].text, 'A2');
    assert.deepEqual(plain(stale.conflictCardIds), ['a']);
});

test('a real conflict is still a conflict, and the server still wins', () => {
    const merge = loadMerge();
    const first = merge(board(card('a', 'A')), board(card('a', 'A')), board(card('a', 'A2')), '');
    const local = plain(first.columns);
    local[0].cards[0].text = 'mine';

    const second = merge(first.baseColumns, local, board(card('a', 'A3')), '');
    assert.deepEqual(plain(second.conflictCardIds), ['a']);
    assert.equal(second.columns[0].cards[0].text, 'A3');
    assert.equal(second.columns[0].cards[0].__remoteConflict, true);
});

test('the base handed back is the server state, as its own copy', () => {
    const merge = loadMerge();
    const server = board(card('a', 'A2'));
    const result = merge(board(card('a', 'A')), board(card('a', 'A')), server, '');
    assert.deepEqual(plain(result.baseColumns), server);
    assert.notEqual(result.baseColumns[0].cards[0], server[0].cards[0]);
    assert.notEqual(result.baseColumns[0].cards[0], result.columns[0].cards[0]);
});

// Where the merge's server side comes from. The finder has to end a board where the server does:
// a whole-page board (#!Kanban on line 1) is the page's own interpreter and runs to the end, and
// an embedded one ends at the first ]]] after its opener, because the server's block extraction
// does not count nesting either. Until 2026-09-10 a whole-page board stopped at any ]]] line, so
// a code block in a card description hid every card after it from the merge.
test('a whole-page board runs to the end of the page, past a code block in a description', () => {
    const find = loadHooks().findKanbanBlockInRaw;
    const raw = ['#!Kanban', '=== Todo', '==== A ==== #a', '===== Description', '[[[#!Vim', 'code', ']]]', '==== B ==== #b'].join('\n');
    const block = find(raw, 2);
    assert.equal(block.interpreterLineStart, 2);
    assert.equal(block.lineEnd, 9, 'one past the last line, as for a board with no ]]] at all');
    assert.ok(block.contentText.endsWith('==== B ==== #b'), 'card B, after the code block, is part of the board');
});

test('an embedded board still ends at the first ]]] after its opener, where the server ends it', () => {
    const find = loadHooks().findKanbanBlockInRaw;
    const raw = ['= Page', '[[[#!Kanban', '=== Todo', '==== A ==== #a', ']]]', 'after the board'].join('\n');
    const block = find(raw, 3);
    assert.equal(block.interpreterLineStart, 3);
    assert.equal(block.lineEnd, 5);
    assert.equal(block.contentText, '=== Todo\n==== A ==== #a');
});
