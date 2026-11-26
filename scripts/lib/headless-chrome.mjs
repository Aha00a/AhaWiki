// Headless Chrome, driven over its DevTools protocol with Node's own WebSocket and nothing else.
// What browser-sweep.mjs and compare-stylesheets.mjs share: finding Chrome, starting it with a
// fresh profile, talking to its first page, and the list of a site's pages. It was the first half
// of browser-sweep.mjs until 2026-11-26, when a second script needed the same forty lines.
import { spawn } from 'node:child_process';
import { existsSync, mkdtempSync, readFileSync, rmSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';

export const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

/** How long a command waits for Chrome's answer unless it says otherwise. */
export const answerMs = 30000;

/** Whether `text` is a site's address and nothing after it: https://ahawiki.net, no path. */
export const isOrigin = (text) => /^https?:\/\/[^/]+$/.test(text || '');

/** Says why and exits 2: the status of a script that could not run, as against one that found something. */
export function cannotRun(message) {
  console.error(message);
  process.exit(2);
}

/** Chrome's path: the one at $CHROME, or at its usual install path. Undefined when there is none. */
export function findChrome() {
  return [
    process.env.CHROME,
    'C:/Program Files/Google/Chrome/Application/chrome.exe',
    'C:/Program Files (x86)/Google/Chrome/Application/chrome.exe',
    '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
    '/usr/bin/google-chrome',
    '/usr/bin/chromium',
  ].find((p) => p && existsSync(p));
}

/** The paths to open: one per line of `pathsFile` (/w/FrontPage), or every page the site lists. */
export async function sitePaths(origin, pathsFile) {
  if (pathsFile) return readFileSync(pathsFile, 'utf8').split(/\r?\n/).filter(Boolean);
  const names = await (await fetch(`${origin}/api/pageNames`)).json();
  return names.map((name) => `/w/${encodeURIComponent(name)}`);
}

// Chrome writes the port it picked to DevToolsActivePort in the profile once it listens. On Windows
// the file can be locked while Chrome is still writing it -- EBUSY, seen when three sweeps started
// at once -- so a failed read is retried like a missing file.
async function devtoolsPort(profile) {
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

/**
 * Starts Chrome with a fresh profile in the temp directory, removed by close(), and a DevTools
 * port of its own choosing, and connects to its first page. Throws when there is no Chrome or it
 * does not come up; nothing is left running then.
 *
 * `width` and `height` are the window's. Chrome does not make a window narrower than about 500px;
 * a script that wants a phone's width asks for it with Emulation.setDeviceMetricsOverride.
 * `domains` are the protocol domains to switch on: what they report is what `events` holds.
 * `flags` are more of Chrome's own, for the one script that needs one: harnesses.mjs lets pages
 * open windows, which a page under test does.
 */
export async function openHeadlessChrome({ profilePrefix, width, height, domains, flags = [] }) {
  const chromePath = findChrome();
  if (!chromePath) throw new Error('Chrome not found; set CHROME to its path.');

  const profile = mkdtempSync(join(tmpdir(), profilePrefix));
  const chrome = spawn(chromePath, [
    '--headless=new', '--remote-debugging-port=0', `--user-data-dir=${profile}`,
    '--no-first-run', '--no-default-browser-check', '--disable-extensions', `--window-size=${width},${height}`,
    ...flags,
    'about:blank',
  ], { stdio: 'ignore' });
  const stop = async () => {
    chrome.kill();
    await sleep(500);
    try { rmSync(profile, { recursive: true, force: true }); } catch { /* Chrome may still hold a file; the OS cleans temp */ }
  };

  let ws;
  try {
    const port = await devtoolsPort(profile);
    const target = (await (await fetch(`http://127.0.0.1:${port}/json/list`)).json()).find((t) => t.type === 'page');
    ws = new WebSocket(target.webSocketDebuggerUrl);
    await new Promise((resolve, reject) => {
      ws.addEventListener('open', resolve, { once: true });
      ws.addEventListener('error', reject, { once: true });
    });
  } catch (e) {
    await stop();
    throw e;
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
  // unanswered, and without one the whole run waited on it. A command that ran out of time
  // resolves to { timedOut: true }.
  const send = (method, params = {}, timeoutMs = answerMs) => new Promise((resolve) => {
    const id = ++nextId;
    const timer = setTimeout(() => { waiting.delete(id); resolve({ timedOut: true }); }, timeoutMs);
    waiting.set(id, (m) => { clearTimeout(timer); resolve(m); });
    ws.send(JSON.stringify({ id, method, params }));
  });
  for (const domain of domains) await send(`${domain}.enable`);

  return {
    send,
    /** What Chrome has reported since the last open(). */
    get events() { return events; },
    /**
     * Goes to `url` and waits for its load event, up to `loadMs`. `answered` is false when the
     * navigation itself got no answer in `answerMs`; `loaded`, when the load event did not come.
     */
    async open(url, loadMs) {
      events = [];
      const started = Date.now();
      const navigation = await send('Page.navigate', { url });
      const loaded = () => events.some((e) => e.method === 'Page.loadEventFired');
      while (Date.now() - started < loadMs && !loaded()) await sleep(100);
      return { answered: !navigation.timedOut, loaded: loaded() };
    },
    /** Runs an expression in the page and returns its value, or { error } when it threw or timed out. */
    async evaluate(expression, timeoutMs = 60000) {
      const answer = await send('Runtime.evaluate', { expression, awaitPromise: true, returnByValue: true }, timeoutMs);
      if (answer.timedOut) return { error: 'timed out' };
      const thrown = answer.result && answer.result.exceptionDetails;
      if (thrown) return { error: `${thrown.text} ${(thrown.exception?.description || '').split('\n')[0]}`.trim() };
      return answer.result ? answer.result.result.value : { error: 'no result' };
    },
    async close() {
      ws.close();
      await stop();
    },
  };
}
