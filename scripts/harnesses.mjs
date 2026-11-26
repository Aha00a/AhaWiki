#!/usr/bin/env node
// Opens every browser harness under test/manual/ in headless Chrome and says which pass.
//
//   node scripts/harnesses.mjs                       every harness
//   node scripts/harnesses.mjs graph-hud slide-notes     these
//   node scripts/harnesses.mjs "graph-hud?theme=dark"    one, with what it takes after a ?
//   node scripts/harnesses.mjs --width=1920          in a window that wide (1280 by default)
//
// Why it was written. A harness finds nothing unless someone opens it, and until 2026-11-26 each
// was opened by hand, by whoever remembered, when the code it stands over changed: start a static
// server, open a page, read its last line, thirteen times over. That day the buttons over the
// graphs were changed and looked at on live pages, and a harness written for them the same
// afternoon found three faults in what had already been deployed. It found them because it was
// run. This is the running, as one command.
//
// It serves the repository root itself, on a port of its own, so it needs nothing started first
// and does not meet another run on a port they both wanted. `python -m http.server`, which the
// harnesses' READMEs name, now and then refuses one of the dozen requests a page makes at once,
// and a page short of a stylesheet measures as nonsense.
//
// It runs in real time, not Chrome's virtual time: slide-notes opens a second window, and the two
// do not hear each other under virtual time. Chrome is started with its popup blocking off for
// the same harness.
//
// Exit status 0 when every harness passed, 1 when one failed or printed nothing, 2 when it could
// not run. See scripts/README.md.
import { createReadStream, existsSync, readdirSync, statSync } from 'node:fs';
import { createServer } from 'node:http';
import { extname, join, resolve, sep } from 'node:path';
import { fileURLToPath } from 'node:url';
import { cannotRun, openHeadlessChrome, sleep } from './lib/headless-chrome.mjs';
import { verdictOf } from './lib/harness-result.mjs';

const root = resolve(fileURLToPath(new URL('..', import.meta.url)));
const harnessRoot = join(root, 'test', 'manual');

// A harness is a directory with an index.html. A new one is run without being named here.
const all = readdirSync(harnessRoot).filter((name) => existsSync(join(harnessRoot, name, 'index.html'))).sort();

const options = '--width=<px>, --height=<px> and --lines are the ones there are';
const flags = new Map();
const asked = [];
for (const arg of process.argv.slice(2)) {
  const sized = /^--(width|height)=(\d+)$/.exec(arg);
  if (sized) flags.set(sized[1], Number(sized[2]));
  else if (arg === '--lines') flags.set('lines', true);
  else if (arg.startsWith('--')) cannotRun(`unknown option ${arg}; ${options}`);
  else asked.push(arg);
}
// A name may carry what the page takes after a ?: graph-hud?theme=dark.
const runs = (asked.length ? asked : all).map((arg) => {
  const [name, ...query] = arg.split('?');
  return { name, query: query.length ? '?' + query.join('?') : '', label: arg };
});
for (const run of runs) if (!all.includes(run.name)) cannotRun(`no harness named ${run.name}; there are: ${all.join(', ')}`);

// When a page is done. No page says so in a way the next one does, so it is read off the result:
// what is printed has to have stood unchanged for a while. For how long depends on what it says.
//   - A last line that sums up -- «PASS — 17 checks» -- is the page saying it is done.
//   - Without one the page may be printing as it goes: editor-grid prints a check, works for
//     nearly half a second, prints the next. The longest any page goes quiet between two lines
//     is 1.2 seconds, and this waits twice that.
//   - A failure may only be a slow start: a page that reports every second says FAIL until what
//     it waits for has come. So a failure has to stand longest.
const stands = { summedUp: 500, printing: 2500, failing: 6000 };
const giveUpMs = 90000;
// Three pages are not done by then, because they never are: they report again and again for as
// long as they are open. What they print in their first second is not yet what they will print --
// admin-vendor and vendor-assets while their libraries are still arriving -- so each is open at
// least this long before it is read. graph-glow counts frames, so its text never stands at all,
// and what has to stand there is the verdict; its glow regroups the nodes every 2.5 seconds, and
// a fault in the regrouping shows only after one.
const openAtLeastMs = { 'admin-vendor': 5000, 'vendor-assets': 5000, 'graph-glow': 6000 };
const neverStill = new Set(['graph-glow']);

const types = {
  '.html': 'text/html; charset=utf-8', '.css': 'text/css; charset=utf-8', '.js': 'text/javascript; charset=utf-8',
  '.mjs': 'text/javascript; charset=utf-8', '.json': 'application/json', '.map': 'application/json',
  '.svg': 'image/svg+xml', '.png': 'image/png', '.jpg': 'image/jpeg', '.jpeg': 'image/jpeg', '.gif': 'image/gif',
  '.webp': 'image/webp', '.ico': 'image/x-icon', '.woff': 'font/woff', '.woff2': 'font/woff2', '.ttf': 'font/ttf',
  '.txt': 'text/plain; charset=utf-8',
};
// The repository root, read only, to this machine only.
const server = createServer((request, response) => {
  let path;
  try { path = decodeURIComponent(new URL(request.url, 'http://localhost').pathname); } catch { path = null; }
  let file = path === null ? null : resolve(join(root, path));
  if (file && file !== root && !file.startsWith(root + sep)) file = null;
  if (file && existsSync(file) && statSync(file).isDirectory()) file = join(file, 'index.html');
  if (!file || !existsSync(file) || !statSync(file).isFile()) {
    response.writeHead(404, { 'Content-Type': 'text/plain' }).end('not found');
    return;
  }
  response.writeHead(200, { 'Content-Type': types[extname(file).toLowerCase()] || 'application/octet-stream', 'Cache-Control': 'no-store' });
  createReadStream(file).pipe(response);
});
await new Promise((done) => server.listen(0, '127.0.0.1', done));
const origin = `http://127.0.0.1:${server.address().port}`;

let browser;
try {
  browser = await openHeadlessChrome({
    profilePrefix: 'ahawiki-harnesses-',
    width: flags.get('width') || 1280,
    height: flags.get('height') || 1000,
    domains: ['Page', 'Runtime', 'Network'],
    flags: ['--disable-popup-blocking'],
  });
} catch (e) {
  server.close();
  cannotRun(`Could not start Chrome: ${e.message}`);
}

// A page with a dialog open answers nothing until the dialog is, and the editor's scripts open
// one: with no server behind them they say so in an alert(). Each dialog is accepted as it comes
// and kept, to show under a harness that did not pass.
let dialogs = [];
const answered = new Set();
const dialogWatch = setInterval(() => {
  for (const event of browser.events) {
    if (event.method !== 'Page.javascriptDialogOpening' || answered.has(event)) continue;
    answered.add(event);
    dialogs.push(`${event.params.type}: ${event.params.message}`);
    browser.send('Page.handleJavaScriptDialog', { accept: true });
  }
}, 100);

// The page's result: the element the harnesses print into, by the three names they gave it.
const printed = `(() => { const e = document.querySelector('#result, #harness, #out'); return e ? e.textContent : ''; })()`;

async function runOne({ name, query }) {
  // An empty page first. The page before this one is still running until another has taken its
  // place, and what it throws or asks for in that moment would be counted as this one's.
  await browser.open('about:blank', 5000);
  dialogs = [];
  const started = Date.now();
  const opened = await browser.open(`${origin}/test/manual/${name}/index.html${query}`, 30000);
  if (!opened.answered) return { verdict: { state: 'silent', failures: [], summary: 'the page did not open' }, threw: [], lost: [], said: [] };

  let verdict = { state: 'silent', failures: [], summary: '' };
  let text = '';
  let since = Date.now();
  let before = '';
  while (Date.now() - started < giveUpMs) {
    await sleep(250);
    const read = await browser.evaluate(printed, 10000);
    text = typeof read === 'string' ? read : '';
    verdict = verdictOf(text);
    const standing = neverStill.has(name) ? verdict.state + '\n' + verdict.failures.join('\n') : text;
    if (standing !== before) { before = standing; since = Date.now(); }
    if (verdict.state === 'silent') continue;
    const stood = Date.now() - since;
    const open = Date.now() - started;
    const needed = verdict.state === 'fail' ? stands.failing : verdict.summed ? stands.summedUp : stands.printing;
    if (open >= (openAtLeastMs[name] || 0) && stood >= needed) break;
  }

  // What the page threw, and what it asked for and did not get. A harness that threw has not run
  // all of its checks, whatever the ones before the throw said.
  const threw = browser.events.filter((e) => e.method === 'Runtime.exceptionThrown')
    .map((e) => { const d = e.params.exceptionDetails; return `${d.text} ${(d.exception?.description || '').split('\n')[0]}`.trim(); });
  // (Every page asks for /favicon.ico, and there is none: that one says nothing about the page.)
  const lost = [
    ...browser.events.filter((e) => e.method === 'Network.responseReceived' && e.params.response.status >= 400 && !e.params.response.url.endsWith('/favicon.ico'))
      .map((e) => `${e.params.response.status} ${e.params.response.url.replace(origin, '')}`),
    ...browser.events.filter((e) => e.method === 'Network.loadingFailed' && !e.params.canceled).map((e) => `failed: ${e.params.errorText}`),
  ];
  return { verdict, text, threw, lost, said: dialogs };
}

const width = Math.max(...runs.map((run) => run.label.length));
const counts = { pass: 0, fail: 0, silent: 0 };
for (const run of runs) {
  const { verdict, text, threw, lost, said: inDialogs } = await runOne(run);
  const state = verdict.state === 'pass' && threw.length ? 'fail' : verdict.state;
  counts[state]++;
  const said = state === 'silent' ? `printed no result in ${giveUpMs / 1000}s${verdict.summary ? ` (it says «${verdict.summary}»)` : ''}` : verdict.summary;
  console.log(`${{ pass: 'PASS', fail: 'FAIL', silent: 'NONE' }[state]}  ${run.label.padEnd(width)}  ${said}`);
  // --lines: all of what the page printed, as it printed it. A page that failed without a line
  // starting FAIL -- editor-keys marks a step at its end, not its start -- gets the same unasked.
  const unmarked = state === 'fail' && !verdict.failures.length && !threw.length;
  if (flags.get('lines') || unmarked) for (const line of text.split('\n')) if (line.trim()) console.log(`      | ${line.trimEnd()}`);
  if (state === 'pass') continue;
  const shown = verdict.failures.slice(0, 12);
  for (const line of shown) console.log(`        ${line}`);
  if (verdict.failures.length > shown.length) console.log(`        ... and ${verdict.failures.length - shown.length} more; --lines prints all of it`);
  for (const line of threw) console.log(`        threw: ${line}`);
  for (const line of [...new Set(lost)]) console.log(`        did not get: ${line}`);
  for (const line of inDialogs) console.log(`        said in a dialog: ${line.replace(/\s+/g, ' ')}`);
}

clearInterval(dialogWatch);
await browser.close();
server.close();

const total = runs.length;
console.log(`\n${total} harness${total === 1 ? '' : 'es'}: ${counts.pass} passed`
  + (counts.fail ? `, ${counts.fail} failed` : '') + (counts.silent ? `, ${counts.silent} printed nothing` : ''));
process.exit(counts.fail || counts.silent ? 1 : 0);
