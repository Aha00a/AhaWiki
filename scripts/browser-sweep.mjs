#!/usr/bin/env node
// Opens every page of a site in headless Chrome and reports, page by page, what went wrong in the
// browser: uncaught exceptions, console.error calls, responses of 400 and over, and requests that
// failed outright.
//
//   node scripts/browser-sweep.mjs https://ahawiki.net              # every page the site lists
//   node scripts/browser-sweep.mjs https://ahawiki.net paths.txt    # one path per line: /w/FrontPage
//
// Exit status 0 when every page was clean, 1 when any was not, 2 when it could not run.
//
// Why it exists: compare-instances.sh compares the HTML two releases send, so it cannot see what
// the browser does with it. On 2026-10-03 every ahawiki.net page compared clean while four of them
// drew a site list that asked each listed site for /favicon.ico, a route no AhaWiki site had; pages
// opened by hand that day had shown a representative image of "/..." and a links request for
// "?q=10". Each was a failed request in the browser and nothing anywhere else.
//
// It opens the pages one after another from wherever it runs. From an address the wiki does not
// whitelist, that is what IpRateLimiter bans (wiki page Dev BotDetection) -- run it from one it does.
//
// Chrome is the one at $CHROME, or at its usual install path. It runs with a fresh profile in the
// temp directory, removed afterwards, and a DevTools port of its own choosing.
import { spawn } from 'node:child_process';
import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

const [, , origin, pathsFile] = process.argv;
const settleMs = Number(process.env.SWEEP_SETTLE_MS || 2500);
const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

function fail(message) {
  console.error(message);
  process.exit(2);
}

if (!origin || !/^https?:\/\/[^/]+$/.test(origin)) fail('usage: node scripts/browser-sweep.mjs <origin, e.g. https://ahawiki.net> [paths-file]');

const chromePath = [
  process.env.CHROME,
  'C:/Program Files/Google/Chrome/Application/chrome.exe',
  'C:/Program Files (x86)/Google/Chrome/Application/chrome.exe',
  '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
  '/usr/bin/google-chrome',
  '/usr/bin/chromium',
].find((p) => p && existsSync(p));
if (!chromePath) fail('Chrome not found; set CHROME to its path.');

async function pagePaths() {
  if (pathsFile) return readFileSync(pathsFile, 'utf8').split(/\r?\n/).filter(Boolean);
  const names = await (await fetch(`${origin}/api/pageNames`)).json();
  return names.map((name) => `/w/${encodeURIComponent(name)}`);
}

const profile = mkdtempSync(join(tmpdir(), 'browser-sweep-'));
const chrome = spawn(chromePath, [
  '--headless=new', '--remote-debugging-port=0', `--user-data-dir=${profile}`,
  '--no-first-run', '--no-default-browser-check', '--disable-extensions', '--window-size=1280,900',
  'about:blank',
], { stdio: 'ignore' });

async function finish(code) {
  chrome.kill();
  await sleep(500);
  try { rmSync(profile, { recursive: true, force: true }); } catch { /* Chrome may still hold a file; the OS cleans temp */ }
  process.exit(code);
}

// Chrome writes the port it picked to DevToolsActivePort in the profile once it listens. On Windows
// the file can be locked while Chrome is still writing it -- EBUSY, seen when three sweeps started
// at once -- so a failed read is retried like a missing file.
async function devtoolsPort() {
  const file = join(profile, 'DevToolsActivePort');
  for (let i = 0; i < 150; i++) {
    try {
      const port = Number(readFileSync(file, 'utf8').split('\n')[0]);
      if (port) return port;
    } catch { /* not there yet, or still being written */ }
    await sleep(100);
  }
  throw new Error('Chrome did not open a DevTools port');
}

let paths;
let ws;
try {
  paths = await pagePaths();
  const port = await devtoolsPort();
  const target = (await (await fetch(`http://127.0.0.1:${port}/json/list`)).json()).find((t) => t.type === 'page');
  ws = new WebSocket(target.webSocketDebuggerUrl);
  await new Promise((resolve, reject) => {
    ws.addEventListener('open', resolve, { once: true });
    ws.addEventListener('error', reject, { once: true });
  });
} catch (e) {
  console.error(String(e));
  await finish(2);
}

let nextId = 0;
const waiting = new Map();
let events = [];
ws.addEventListener('message', (ev) => {
  const m = JSON.parse(ev.data);
  if (m.id && waiting.has(m.id)) { waiting.get(m.id)(m); waiting.delete(m.id); }
  else if (m.method) events.push(m);
});
// Every command has a deadline. A page whose script holds the renderer can leave a navigation
// unanswered, and without one the whole sweep waited on it.
const send = (method, params = {}, timeoutMs = 30000) => new Promise((resolve) => {
  const id = ++nextId;
  const timer = setTimeout(() => { waiting.delete(id); resolve({ timedOut: true }); }, timeoutMs);
  waiting.set(id, (m) => { clearTimeout(timer); resolve(m); });
  ws.send(JSON.stringify({ id, method, params }));
});
for (const domain of ['Page', 'Runtime', 'Log', 'Network']) await send(`${domain}.enable`);

function problemsIn(events) {
  // Only requests this page made: one still running from the previous page reports its abort after
  // the navigation and must not be charged to this one.
  const urlOf = new Map();
  const done = new Set();
  const problems = [];
  for (const { method, params: p } of events) {
    if (method === 'Network.requestWillBeSent') urlOf.set(p.requestId, p.request.url);
    else if (method === 'Network.loadingFinished') done.add(p.requestId);
    else if (method === 'Runtime.exceptionThrown') {
      const d = p.exceptionDetails;
      problems.push(`exception: ${(d.exception?.description || d.text || '').split('\n')[0]} @ ${d.url || ''}:${d.lineNumber}`);
    } else if (method === 'Runtime.consoleAPICalled' && p.type === 'error') {
      problems.push(`console.error: ${p.args.map((a) => a.value ?? a.description ?? '').join(' ').slice(0, 200)}`);
    } else if (method === 'Log.entryAdded' && p.entry.level === 'error' && p.entry.source !== 'network') {
      // A failed load is also logged with source "network"; it is reported once, below.
      problems.push(`log(${p.entry.source}): ${p.entry.text.slice(0, 200)}`);
    } else if (method === 'Network.responseReceived' && p.response.status >= 400) {
      problems.push(`HTTP ${p.response.status}: ${p.response.url.slice(0, 200)}`);
    } else if (method === 'Network.loadingFailed') {
      done.add(p.requestId);
      if (!p.canceled && urlOf.has(p.requestId)) problems.push(`failed ${p.errorText}: ${urlOf.get(p.requestId).slice(0, 200)}`);
    }
  }
  if (!events.some((e) => e.method === 'Page.loadEventFired')) {
    // What the load event is still waiting for -- usually an external host that never answers.
    const pending = [...urlOf].filter(([id]) => !done.has(id)).map(([, url]) => url.slice(0, 160));
    problems.push(`no load event in 20s${pending.length ? `; still loading: ${pending.slice(0, 3).join(' , ')}` : ''}`);
  }
  return problems;
}

let withProblems = 0;
for (const path of paths) {
  events = [];
  const started = Date.now();
  const navigation = await send('Page.navigate', { url: origin + path });
  while (Date.now() - started < 20000 && !events.some((e) => e.method === 'Page.loadEventFired')) await sleep(100);
  await sleep(settleMs); // what the page fetches after load -- the adjacent-pages graph, previews
  const problems = problemsIn(events);
  if (navigation.timedOut) problems.unshift('the navigation was not answered in 30s');
  if (problems.length) withProblems++;
  const seconds = ((Date.now() - started) / 1000).toFixed(1);
  console.log(`${problems.length ? 'PROBLEM' : 'ok     '} ${path} (${seconds}s)${problems.map((x) => `\n    ${x}`).join('')}`);
  // Whatever the page still runs or loads stops here, so it cannot hold up the next one.
  await send('Page.stopLoading', {}, 5000);
}
console.log(`\n${paths.length} pages, ${withProblems} with problems`);
ws.close();
await finish(withProblems ? 1 : 0);
