// The page's WebSocket reconnect decision, read out of the view template itself so what is
// tested is what ships. A socket that never opened was refused at the handshake (the server's
// 403 for a page this visitor may not read reaches the browser only as a close), and until
// 2026-09-26 that was retried every 1.5s for as long as the tab lived -- 44,249 attempts from
// one address in August. Now the delay doubles and the fifth refusal is the last; a socket that
// had been open and dropped keeps reconnecting as before.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const template = fs.readFileSync(new URL('../app/views/Wiki/view.scala.html', import.meta.url), 'utf8');

function loadReconnectPolicy() {
    const start = template.indexOf('var reconnectDelayMs = 1500;');
    const end = template.indexOf('var socket;', start);
    assert.ok(start > 0 && end > start, 'the reconnect policy is where the test expects it');
    const source = template.slice(start, end);
    const context = {};
    vm.createContext(context);
    vm.runInContext(source + '\nthis.reconnectDelayAfterClose = reconnectDelayAfterClose; this.reset = function () { failedHandshakes = 0; };', context);
    return context;
}

test('a refused handshake backs off and the fifth refusal is the last', () => {
    const policy = loadReconnectPolicy();
    const delays = [1, 2, 3, 4, 5].map(() => policy.reconnectDelayAfterClose(false));
    assert.deepEqual(delays, [1500, 3000, 6000, 12000, -1]);
});

test('a socket that had been open reconnects at the base delay, and does not count against the refusals', () => {
    const policy = loadReconnectPolicy();
    assert.equal(policy.reconnectDelayAfterClose(true), 1500);
    assert.equal(policy.reconnectDelayAfterClose(true), 1500);
    // A refusal after two open-then-dropped sockets is the first refusal, not the third.
    assert.equal(policy.reconnectDelayAfterClose(false), 1500);
});

test('the template resets the count when a socket opens', () => {
    // onopen sets failedHandshakes back to 0, so a page that was refused a few times and then
    // let in starts over on the next drop.
    assert.match(template, /socket\.onopen = function \(\) \{\s*wasOpen = true;\s*failedHandshakes = 0;/);
});
