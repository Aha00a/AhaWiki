// A save that met a 409 replays this board's unsaved changes onto the board the server holds now
// (rebaseKanbanColumns). What must hold: nothing another session saved in between is lost, each
// kind of change made here lands, and where both sides changed the same thing the server's copy
// stays. The last three tests check it over random boards and random changes against what is
// known without the function: a server that changed nothing gives back this board, a board that
// changed nothing gives back the server's, and whatever both did, no card turns up twice and none
// the server added goes missing.
import test from 'node:test';
import assert from 'node:assert/strict';
import { loadKanbanHooks } from './lib/kanban-board.mjs';

const { rebaseKanbanColumns } = loadKanbanHooks().hooks;

const card = (id, text, extra = {}) => ({ id, text, classNames: [], lineNumber: 0, description: [], comments: [], properties: {}, ...extra });
const list = (title, ...cards) => ({ title, lineNumber: 0, cards });
// Results come from the sandbox realm, whose prototypes strict deepEqual rejects; JSON brings
// them back as plain host objects. Inputs are copied too, because a rebase keeps this board's
// card objects and writes the server's fields into them.
const plain = value => JSON.parse(JSON.stringify(value));
const rebase = (base, local, server, activeCardId = '') => rebaseKanbanColumns(plain(base), plain(local), plain(server), activeCardId);
// What a board says: its lists, and each card's id, title and comments.
const shape = columns => plain(columns).map(column => [column.title, column.cards.map(c => [c.id, c.text, c.comments.map(comment => comment.header)])]);
const titles = columns => plain(columns).map(column => [column.title, column.cards.map(c => `${c.id}:${c.text}`)]);

test('a card moved here stays moved, beside a card the server renamed', () => {
    const base = [list('L1', card('a', 'A'), card('b', 'B')), list('L2', card('c', 'C'))];
    const local = [list('L1', card('a', 'A')), list('L2', card('c', 'C'), card('b', 'B'))];
    const server = [list('L1', card('a', 'A2'), card('b', 'B')), list('L2', card('c', 'C'))];
    const result = rebase(base, local, server);
    assert.deepEqual(titles(result.columns), [['L1', ['a:A2']], ['L2', ['c:C', 'b:B']]]);
    assert.deepEqual(plain(result.conflictCardIds), []);
});

test('a card added here goes after the card before it, next to one the server added', () => {
    const base = [list('L1', card('a', 'A'), card('b', 'B'))];
    const local = [list('L1', card('a', 'A'), card('x', 'X'), card('b', 'B'))];
    const server = [list('L1', card('a', 'A'), card('b', 'B'), card('y', 'Y'))];
    assert.deepEqual(titles(rebase(base, local, server).columns), [['L1', ['a:A', 'x:X', 'b:B', 'y:Y']]]);
});

test('a card deleted here goes, unless the server changed it since', () => {
    const base = [list('L1', card('a', 'A'), card('b', 'B'))];
    const local = [list('L1', card('a', 'A'))];
    assert.deepEqual(titles(rebase(base, local, base).columns), [['L1', ['a:A']]]);

    const edited = rebase(base, local, [list('L1', card('a', 'A'), card('b', 'B2'))]);
    assert.deepEqual(titles(edited.columns), [['L1', ['a:A', 'b:B2']]]);
    assert.deepEqual(plain(edited.conflictCardIds), ['b']);
});

test('a card edited here keeps the edit where the server moved it', () => {
    const base = [list('L1', card('a', 'A')), list('L2')];
    const local = [list('L1', card('a', 'A2')), list('L2')];
    const server = [list('L1'), list('L2', card('a', 'A'))];
    assert.deepEqual(titles(rebase(base, local, server).columns), [['L1', []], ['L2', ['a:A2']]]);
});

test("where both edited the same card the server's copy stays, and the card is flagged", () => {
    const base = [list('L1', card('a', 'A'))];
    const result = rebase(base, [list('L1', card('a', 'here'))], [list('L1', card('a', 'there'))]);
    assert.deepEqual(titles(result.columns), [['L1', ['a:there']]]);
    assert.deepEqual(plain(result.conflictCardIds), ['a']);
    assert.equal(result.columns[0].cards[0].__remoteConflict, true);
});

test('comments added on both sides are all kept, the server’s first', () => {
    const comment = header => ({ header, details: [] });
    const base = [list('L1', card('a', 'A', { comments: [comment('h1')] }))];
    const local = [list('L1', card('a', 'A', { comments: [comment('h1'), comment('here')] }))];
    const server = [list('L1', card('a', 'A', { comments: [comment('h1'), comment('there')] }))];
    assert.deepEqual(shape(rebase(base, local, server).columns), [['L1', [['a', 'A', ['h1', 'there', 'here']]]]]);
});

test('a card the server deleted stays deleted, and an edit made here to it is reported', () => {
    const base = [list('L1', card('a', 'A'), card('b', 'B'))];
    const result = rebase(base, [list('L1', card('a', 'A'), card('b', 'B2'))], [list('L1', card('a', 'A'))]);
    assert.deepEqual(titles(result.columns), [['L1', ['a:A']]]);
    assert.deepEqual(plain(result.dropped), ['card:edit b']);
});

test('both moved the same card: it stays where the server put it, and that is reported', () => {
    const base = [list('L1', card('a', 'A')), list('L2'), list('L3')];
    const result = rebase(base, [list('L1'), list('L2', card('a', 'A')), list('L3')], [list('L1'), list('L2'), list('L3', card('a', 'A'))]);
    assert.deepEqual(titles(result.columns), [['L1', []], ['L2', []], ['L3', ['a:A']]]);
    assert.deepEqual(plain(result.dropped), ['card:move a']);
});

test('lists added, renamed, reordered or deleted here, around what the server did', () => {
    const added = rebase([list('L1'), list('L2')], [list('L1'), list('New'), list('L2')], [list('L1'), list('L2', card('z', 'Z'))]);
    assert.deepEqual(titles(added.columns), [['L1', []], ['New', []], ['L2', ['z:Z']]]);

    const renamed = rebase([list('L1', card('a', 'A'))], [list('Renamed', card('a', 'A'))], [list('L1', card('a', 'A'), card('z', 'Z'))]);
    assert.deepEqual(titles(renamed.columns), [['Renamed', ['a:A', 'z:Z']]]);

    const reordered = rebase([list('L1'), list('L2'), list('L3')], [list('L3'), list('L1'), list('L2')], [list('L1'), list('L2', card('z', 'Z')), list('L3')]);
    assert.deepEqual(titles(reordered.columns), [['L3', []], ['L1', []], ['L2', ['z:Z']]]);

    const deleted = rebase([list('L1', card('a', 'A')), list('L2', card('b', 'B'))], [list('L1', card('a', 'A'))], [list('L1', card('a', 'A')), list('L2', card('b', 'B'))]);
    assert.deepEqual(titles(deleted.columns), [['L1', ['a:A']]]);
});

test('a list deleted here stays for a card the server has added to it since, and nothing else', () => {
    // b went with the list here and nobody changed it there, so it goes; the list stays for z.
    const base = [list('L1', card('a', 'A')), list('L2', card('b', 'B'))];
    const result = rebase(base, [list('L1', card('a', 'A'))], [list('L1', card('a', 'A')), list('L2', card('b', 'B'), card('z', 'Z'))]);
    assert.deepEqual(titles(result.columns), [['L1', ['a:A']], ['L2', ['z:Z']]]);
    assert.deepEqual(plain(result.dropped), ['list:delete L2']);
});

test('a card moved out of a list before the list was deleted here is kept', () => {
    const base = [list('L1', card('a', 'A')), list('L2', card('b', 'B'), card('c', 'C'))];
    const local = [list('L1', card('a', 'A'), card('b', 'B'))];
    assert.deepEqual(titles(rebase(base, local, base).columns), [['L1', ['a:A', 'b:B']]]);
});

test("the open card keeps this board's object, and the server's fields go to its modal", () => {
    const local = [list('L1', card('a', 'A'))];
    const result = rebaseKanbanColumns(plain(local), local, [list('L1', card('a', 'A2'))], 'a');
    assert.equal(result.columns[0].cards[0], local[0].cards[0]);
    assert.equal(local[0].cards[0].text, 'A');
    assert.equal(result.activeCardServerEntry.card.text, 'A2');
});

// Deterministic random numbers (mulberry32), so a failing round can be run again by its seed.
const random = seed => () => {
    seed = (seed + 0x6D2B79F5) | 0;
    let t = Math.imul(seed ^ (seed >>> 15), 1 | seed);
    t = (t + Math.imul(t ^ (t >>> 7), 61 | t)) ^ t;
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
};

let fresh = 0;
const randomBoard = rnd => {
    const lists = [];
    const listCount = 1 + Math.floor(rnd() * 4);
    for (let i = 0; i < listCount; i++) {
        const cards = [];
        const cardCount = Math.floor(rnd() * 5);
        for (let j = 0; j < cardCount; j++) {
            fresh += 1;
            cards.push(card(`c${fresh}`, `Card ${fresh}`, { comments: [{ header: `h${fresh}`, details: [] }] }));
        }
        lists.push(list(`L${fresh}-${i}`, ...cards));
    }
    return lists;
};

// Changes a copy of the board the ways the board's own buttons do. New ids and titles are never
// reused, as on the page.
const change = (board, rnd, times) => {
    const lists = plain(board);
    const pick = items => items[Math.floor(rnd() * items.length)];
    const at = items => Math.floor(rnd() * (items.length + 1));
    for (let k = 0; k < times; k++) {
        const withCards = lists.filter(l => l.cards.length > 0);
        fresh += 1;
        switch (Math.floor(rnd() * 9)) {
            case 0: if (withCards.length) {
                const from = pick(withCards);
                const [moved] = from.cards.splice(Math.floor(rnd() * from.cards.length), 1);
                const to = pick(lists);
                to.cards.splice(at(to.cards), 0, moved);
            } break;
            case 1: { const to = pick(lists); to.cards.splice(at(to.cards), 0, card(`n${fresh}`, `New ${fresh}`)); } break;
            case 2: if (withCards.length) { const from = pick(withCards); from.cards.splice(Math.floor(rnd() * from.cards.length), 1); } break;
            case 3: if (withCards.length) { const target = pick(pick(withCards).cards); target.text = `${target.text} r${fresh}`; } break;
            case 4: if (withCards.length) { pick(pick(withCards).cards).comments.push({ header: `x${fresh}`, details: [] }); } break;
            case 5: lists.splice(at(lists), 0, list(`N${fresh}`)); break;
            case 6: pick(lists).title = `R${fresh}`; break;
            case 7: if (lists.length > 1) { lists.splice(Math.floor(rnd() * lists.length), 1); } break;
            default: if (lists.length > 1) {
                const [moved] = lists.splice(Math.floor(rnd() * lists.length), 1);
                lists.splice(at(lists), 0, moved);
            }
        }
    }
    return lists;
};

test('a server that changed nothing gives back this board, whatever this board did', () => {
    const rnd = random(1);
    for (let round = 0; round < 500; round++) {
        const base = randomBoard(rnd);
        const local = change(base, rnd, 1 + Math.floor(rnd() * 3));
        const result = rebase(base, local, base);
        assert.deepEqual(shape(result.columns), shape(local), `seed 1, round ${round}\nbase ${JSON.stringify(titles(base))}\nlocal ${JSON.stringify(titles(local))}`);
        assert.deepEqual(plain(result.conflictCardIds), [], `seed 1, round ${round}`);
    }
});

test("a board that changed nothing gives back the server's, whatever the server did", () => {
    const rnd = random(2);
    for (let round = 0; round < 500; round++) {
        const base = randomBoard(rnd);
        const server = change(base, rnd, 1 + Math.floor(rnd() * 3));
        const result = rebase(base, base, server);
        assert.deepEqual(shape(result.columns), shape(server), `seed 2, round ${round}\nbase ${JSON.stringify(titles(base))}\nserver ${JSON.stringify(titles(server))}`);
    }
});

test('whatever both sides did, no card turns up twice and none the server added goes missing', () => {
    const rnd = random(3);
    for (let round = 0; round < 500; round++) {
        const base = randomBoard(rnd);
        const local = change(base, rnd, 1 + Math.floor(rnd() * 3));
        const server = change(base, rnd, 1 + Math.floor(rnd() * 3));
        const result = rebase(base, local, server);
        const ids = plain(result.columns).flatMap(column => column.cards.map(c => c.id));
        const where = `seed 3, round ${round}`;
        assert.equal(new Set(ids).size, ids.length, `${where}: a card appears twice`);
        const baseIds = new Set(plain(base).flatMap(column => column.cards.map(c => c.id)));
        for (const id of plain(server).flatMap(column => column.cards.map(c => c.id))) {
            if (!baseIds.has(id)) assert.ok(ids.includes(id), `${where}: the server's new card ${id} is missing`);
        }
        const serverIds = new Set(plain(server).flatMap(column => column.cards.map(c => c.id)));
        for (const id of baseIds) {
            if (!serverIds.has(id)) assert.ok(!ids.includes(id), `${where}: ${id}, which the server deleted, came back`);
        }
        for (const id of plain(local).flatMap(column => column.cards.map(c => c.id))) {
            if (!baseIds.has(id)) assert.ok(ids.includes(id) || plain(result.dropped).includes(`card:add ${id}`), `${where}: the new card ${id} is neither here nor reported`);
        }
    }
});
