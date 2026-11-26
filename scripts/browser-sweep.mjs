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
// Which Chrome it runs, and how, is lib/headless-chrome.mjs.
import { answerMs, cannotRun, isOrigin, openHeadlessChrome, sitePaths, sleep } from './lib/headless-chrome.mjs';

const [, , origin, pathsFile] = process.argv;
const settleMs = Number(process.env.SWEEP_SETTLE_MS || 2500);
const loadMs = 20000;

if (!isOrigin(origin)) cannotRun('usage: node scripts/browser-sweep.mjs <origin, e.g. https://ahawiki.net> [paths-file]');

let paths;
let browser;
try {
  paths = await sitePaths(origin, pathsFile);
  browser = await openHeadlessChrome({ profilePrefix: 'browser-sweep-', width: 1280, height: 900, domains: ['Page', 'Runtime', 'Log', 'Network'] });
} catch (e) {
  cannotRun(String(e));
}

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
    problems.push(`no load event in ${loadMs / 1000}s${pending.length ? `; still loading: ${pending.slice(0, 3).join(' , ')}` : ''}`);
  }
  return problems;
}

let withProblems = 0;
for (const path of paths) {
  const started = Date.now();
  const { answered } = await browser.open(origin + path, loadMs);
  await sleep(settleMs); // what the page fetches after load -- the adjacent-pages graph, previews
  const problems = problemsIn(browser.events);
  if (!answered) problems.unshift(`the navigation was not answered in ${answerMs / 1000}s`);
  if (problems.length) withProblems++;
  const seconds = ((Date.now() - started) / 1000).toFixed(1);
  console.log(`${problems.length ? 'PROBLEM' : 'ok     '} ${path} (${seconds}s)${problems.map((x) => `\n    ${x}`).join('')}`);
  // Whatever the page still runs or loads stops here, so it cannot hold up the next one.
  await browser.send('Page.stopLoading', {}, 5000);
}
console.log(`\n${paths.length} pages, ${withProblems} with problems`);
await browser.close();
process.exit(withProblems ? 1 : 0);
