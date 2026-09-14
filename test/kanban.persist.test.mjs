// The save path end to end, on a board built in node:vm against a fake server that holds the page
// text, checks the revision and splices a save into its lines as Wiki.save does. Two things went
// wrong here until 2026-09-15. On a page opening with #! lines the board was saved that many lines
// too high, because the numbers a page is drawn with start after those lines while a save counts
// from the top of the text. And a 409 sent the same text again, over whatever another session had
// saved in between.
import test from 'node:test';
import assert from 'node:assert/strict';
import { bootBoard, loadKanbanHooks, setColumnRects } from './lib/kanban-board.mjs';

const board = [
    '=== List 1', '==== One ==== #c1', '===== Property', '===== Activity',
    '=== List 2', '==== Two ==== #c2', '===== Property', '===== Activity',
    '=== List 3', '==== Three ==== #c3', '===== Property', '===== Activity',
];
// A #!var line, which the render takes off before it numbers lines; a heading; the board; a line
// after it. Drawn, the board's opener is line 2; in the text it is line 3.
const page = lines => ['#!var x y', '= Title', '[[[#!Kanban', ...lines, ']]]', 'after the board'].join('\n');
const drawnAt = { lineStart: 2, lineEnd: 15 };

function fakeServer({ text, revision, beforeSave = () => {} }) {
    const server = { text, revision, posts: [] };
    server.fetch = async (url, options = {}) => {
        if (url === '/api/csrf') return { ok: true, json: async () => ({ value: 'csrf' }) };
        if (url.startsWith('/api/pageRevision/')) return { ok: true, json: async () => ({ revision: server.revision }) };
        if (url.endsWith('?action=raw')) return { ok: true, text: async () => server.text };
        if (url.startsWith('/w/') && options.method === 'POST') {
            const params = Object.fromEntries(new URLSearchParams(options.body));
            server.posts.push(params);
            beforeSave(server, server.posts.length);
            if (Number(params.revision) !== server.revision) return { ok: false, status: 409 };
            const lines = server.text.split('\n');
            server.text = [...lines.slice(0, params.lineStart - 1), ...params.text.split('\n'), ...lines.slice(params.lineEnd - 1)].join('\n');
            server.revision += 1;
            return { ok: true, status: 200 };
        }
        throw new Error(`unexpected request: ${url}`);
    };
    return server;
}

// Drags the card in List 3 into List 1, as kanban.readonly.test.mjs does.
function dragThreeIntoOne({ board: boardElement, sortableCreates }) {
    const columns = boardElement.querySelectorAll('.kanban-column');
    const lists = boardElement.querySelectorAll('.kanban-card-list');
    setColumnRects(columns);
    const dragged = lists[2].querySelectorAll('.kanban-card')[0];
    lists[1].appendChild(dragged);
    sortableCreates.find(entry => entry.element === lists[2]).options.onEnd({
        item: dragged, from: lists[2], to: lists[1], oldIndex: 1, newIndex: 2,
        originalEvent: { clientX: 50, clientY: 340 },
    });
}
const settle = async () => {
    for (let i = 0; i < 100; i++) await new Promise(resolve => setImmediate(resolve));
};

// The page around the board is untouched, and the third card now sits in the first list.
function assertMovedIntoPlace(text) {
    const lines = text.split('\n');
    assert.deepEqual(lines.slice(0, 3), ['#!var x y', '= Title', '[[[#!Kanban']);
    assert.deepEqual(lines.slice(-2), [']]]', 'after the board']);
    const index = line => lines.indexOf(line);
    assert.ok(index('=== List 1') < index('==== Three ==== #c3') && index('==== Three ==== #c3') < index('=== List 2'), text);
}

test("the first save on a page that opens with #! lines writes the board's own lines", async () => {
    const server = fakeServer({ text: page(board), revision: 5 });
    const ui = bootBoard({ writable: true, content: board.join('\n'), revision: 5, fetchImpl: server.fetch, wrap: drawnAt });
    dragThreeIntoOne(ui);
    await settle();

    assert.equal(server.posts.length, 1);
    assert.equal(server.posts[0].lineStart, '4');
    assert.equal(server.posts[0].lineEnd, '16');
    assertMovedIntoPlace(server.text);
});

test('a 409 replays the move onto what another session saved, and both stay', async () => {
    const server = fakeServer({
        text: page(board),
        revision: 5,
        beforeSave: (state, count) => {
            if (count !== 1) return;
            // Another session renames the second card and gets its save in first.
            state.text = state.text.replace('==== Two ==== #c2', '==== Two, renamed ==== #c2');
            state.revision = 6;
        },
    });
    const ui = bootBoard({ writable: true, content: board.join('\n'), revision: 5, fetchImpl: server.fetch, wrap: drawnAt });
    dragThreeIntoOne(ui);
    await settle();

    assert.equal(server.posts.length, 2);
    assert.equal(server.posts[1].revision, '6');
    assert.match(server.text, /^==== Two, renamed ==== #c2$/m);
    assertMovedIntoPlace(server.text);
});

test('a board older than the page is brought up to date before its first save', async () => {
    const server = fakeServer({ text: page(board).replace('==== One ==== #c1', '==== One, renamed ==== #c1'), revision: 7 });
    const ui = bootBoard({ writable: true, content: board.join('\n'), revision: 5, fetchImpl: server.fetch, wrap: drawnAt });
    dragThreeIntoOne(ui);
    await settle();

    assert.equal(server.posts.length, 1);
    assert.equal(server.posts[0].revision, '7');
    assert.match(server.text, /^==== One, renamed ==== #c1$/m);
    assertMovedIntoPlace(server.text);
});

test('a board that is no longer on the page is not written anywhere', async () => {
    const server = fakeServer({ text: '#!var x y\n= Title\nthe board was taken out', revision: 6 });
    const ui = bootBoard({ writable: true, content: board.join('\n'), revision: 5, fetchImpl: server.fetch, wrap: drawnAt });
    dragThreeIntoOne(ui);
    await settle();

    assert.equal(server.posts.length, 0);
    assert.ok(ui.body.querySelector('.kanban-conflict-toast'));
});

test('a whole-page board is found after the #! lines before it', () => {
    const { findKanbanBlockInRaw } = loadKanbanHooks().hooks;
    const block = findKanbanBlockInRaw(['#!read Aha00a', '#!Kanban', '=== ToDo', '==== A ==== #a'].join('\n'), 1);
    assert.equal(block.interpreterLineStart, 3);
    assert.equal(block.lineEnd, 5);
    assert.equal(block.contentText, '=== ToDo\n==== A ==== #a');
});

test('a page whose interpreter is not Kanban has only its embedded boards', () => {
    const { findKanbanBlockInRaw } = loadKanbanHooks().hooks;
    const text = ['#!Wiki', '[[[#!Kanban', '=== A', ']]]', 'text', '[[[#!Kanban', '=== B', ']]]'].join('\n');
    assert.equal(findKanbanBlockInRaw(text, 3).contentText, '=== A');
    assert.equal(findKanbanBlockInRaw(text, 7).contentText, '=== B');
});
