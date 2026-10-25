// The MCP server that lets an agent use the wiki. The protocol half is tested apart from the
// wiki half: the first is a function from a message to its answer, the second is the tools over a
// fake of the wiki's API. What is asserted is what an agent would trip on -- a write tool it was
// never meant to have, a save with no revision, a failure that kills the call instead of being
// read by the model. The reasons are on the wiki page Dev Mcp.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createServer } from '../scripts/lib/mcp.mjs';
import { wikiTools, wikiCaller } from '../scripts/mcp.ahawiki.mjs';

function serverWith({ write = false, answers = {} } = {}) {
    const calls = [];
    const call = async (method, path, body) => {
        calls.push({ method, path, body });
        const key = `${method} ${path.split('?')[0]}`;
        if (!(key in answers)) throw new Error(`404 no such route in the fake: ${key}`);
        const answer = answers[key];
        return typeof answer === 'function' ? answer(path, body) : answer;
    };
    const server = createServer({ name: 'ahawiki', version: '0', tools: wikiTools({ call, write }) });
    const rpc = (method, params, id = 1) => server.handle({ jsonrpc: '2.0', id, method, params });
    return { rpc, calls, server };
}

test('initialize answers with its name, its tools capability and a protocol version', async () => {
    const { rpc } = serverWith();
    const { result } = await rpc('initialize', { protocolVersion: '2024-11-05' });
    assert.equal(result.serverInfo.name, 'ahawiki');
    assert.deepEqual(result.capabilities, { tools: {} });
    assert.match(result.protocolVersion, /^\d{4}-\d{2}-\d{2}$/);
});

test('a notification gets no answer, and an unknown request gets a method-not-found error', async () => {
    const { server } = serverWith();
    assert.equal(await server.handle({ jsonrpc: '2.0', method: 'notifications/initialized' }), null);
    const answer = await server.handle({ jsonrpc: '2.0', id: 7, method: 'resources/list' });
    assert.equal(answer.id, 7);
    assert.equal(answer.error.code, -32601);
});

test('without the write switch the agent is not offered a way to change a page', async () => {
    const names = async write => (await serverWith({ write }).rpc('tools/list')).result.tools.map(t => t.name);
    assert.ok(!(await names(false)).includes('ahawiki_save_page'));
    assert.ok((await names(true)).includes('ahawiki_save_page'));
    assert.ok((await names(false)).includes('ahawiki_search'), 'the read tools are there either way');
});

test('every tool says what it is for and what it takes', async () => {
    for (const tool of (await serverWith({ write: true }).rpc('tools/list')).result.tools) {
        assert.ok(tool.description.length > 30, `${tool.name} needs a description a model can choose by`);
        assert.equal(tool.inputSchema.type, 'object');
    }
});

test('search asks the API for the query, encoded, and shows each hit with its words', async () => {
    const { rpc, calls } = serverWith({
        answers: { 'GET /api/v1/search': { q: '날짜 & 속성', truncated: false, pages: [{ name: 'Alpha', dateTime: '2026-10-24T10:00:00', snippet: '…the hit…' }] } },
    });
    const { result } = await rpc('tools/call', { name: 'ahawiki_search', arguments: { q: '날짜 & 속성', limit: 5 } });
    assert.equal(calls[0].path, `/api/v1/search?q=${encodeURIComponent('날짜 & 속성')}&limit=5`);
    assert.match(result.content[0].text, /- Alpha \(2026-10-24T10:00:00\)\n  …the hit…/);
});

test('search says so when nothing matches, and when the limit cut the list', async () => {
    const empty = serverWith({ answers: { 'GET /api/v1/search': { q: 'zzz', truncated: false, pages: [] } } });
    assert.match((await empty.rpc('tools/call', { name: 'ahawiki_search', arguments: { q: 'zzz' } })).result.content[0].text, /No page matches "zzz"/);
    const cut = serverWith({ answers: { 'GET /api/v1/search': { q: 'a', truncated: true, pages: [{ name: 'A', dateTime: 'x', snippet: 's' }] } } });
    assert.match((await cut.rpc('tools/call', { name: 'ahawiki_search', arguments: { q: 'a' } })).result.content[0].text, /more pages match/);
});

test('reading a page reports the revision a later save needs, and encodes the name', async () => {
    const { rpc, calls } = serverWith({ answers: { 'GET /api/v1/page/Dev%20Less%2FA': { revision: 12, content: '= Less' } } });
    const { result } = await rpc('tools/call', { name: 'ahawiki_read_page', arguments: { name: 'Dev Less/A' } });
    assert.equal(calls[0].path, '/api/v1/page/Dev%20Less%2FA');
    assert.equal(result.content[0].text, 'revision: 12\n\n= Less');
});

test('a save sends the revision it was given, and is minor unless told otherwise', async () => {
    const { rpc, calls } = serverWith({ write: true, answers: { 'POST /api/v1/page/Notes': { name: 'Notes', revision: 5 } } });
    const { result } = await rpc('tools/call', { name: 'ahawiki_save_page', arguments: { name: 'Notes', text: 'new', revision: 4, comment: 'why' } });
    assert.deepEqual(calls[0].body, { revision: 4, text: 'new', comment: 'why', minorEdit: true });
    assert.equal(result.content[0].text, 'Saved Notes as revision 5.');
});

test('a save with no revision, or no comment, is refused before it reaches the wiki', async () => {
    const { rpc, calls } = serverWith({ write: true, answers: { 'POST /api/v1/page/Notes': { name: 'Notes', revision: 5 } } });
    const noRevision = (await rpc('tools/call', { name: 'ahawiki_save_page', arguments: { name: 'Notes', text: 't', comment: 'c' } })).result;
    assert.equal(noRevision.isError, true);
    assert.match(noRevision.content[0].text, /revision is required/);
    const noComment = (await rpc('tools/call', { name: 'ahawiki_save_page', arguments: { name: 'Notes', text: 't', revision: 1, comment: '  ' } })).result;
    assert.equal(noComment.isError, true);
    assert.equal(calls.length, 0, 'nothing was sent');
});

test('a failure in a tool comes back for the model to read, not as a protocol error', async () => {
    const { rpc } = serverWith({ answers: { 'GET /api/v1/page/Gone': () => { throw new Error('404 Page not found.'); } } });
    const answer = await rpc('tools/call', { name: 'ahawiki_read_page', arguments: { name: 'Gone' } });
    assert.equal(answer.error, undefined);
    assert.equal(answer.result.isError, true);
    assert.match(answer.result.content[0].text, /404 Page not found/);
});

test('calling a tool that does not exist is a protocol error', async () => {
    const answer = await serverWith().rpc('tools/call', { name: 'ahawiki_delete_everything', arguments: {} });
    assert.equal(answer.error.code, -32602);
});

test('the caller sends the key as a bearer token and turns a non-2xx into an error naming the status', async () => {
    const seen = [];
    const fetchImpl = async (url, init) => {
        seen.push({ url, init });
        return { ok: false, status: 409, text: async () => JSON.stringify({ error: 'revision conflict' }) };
    };
    const call = wikiCaller({ baseUrl: 'https://wiki.example', apiKey: 'k', fetchImpl });
    await assert.rejects(call('POST', '/api/v1/page/X', { revision: 1 }), /409 revision conflict/);
    assert.equal(seen[0].url, 'https://wiki.example/api/v1/page/X');
    assert.equal(seen[0].init.headers.authorization, 'Bearer k');
    assert.equal(seen[0].init.headers['content-type'], 'application/json');
});
