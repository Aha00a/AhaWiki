#!/usr/bin/env node
// Draws each page of a site twice in headless Chrome -- as the site serves it, then with this
// checkout's stylesheets in place of the site's -- and reports, page by page, what that moves.
//
//   node scripts/compare-stylesheets.mjs https://ahawiki.net             # every page the site lists
//   node scripts/compare-stylesheets.mjs https://ahawiki.net paths.txt   # one path per line: /w/FrontPage
//     --phone          in a phone 390px wide; --phone=360 for another width
//     --shots=<dir>    and two pictures of every page that changed: before, after
//     --assets=<dir>   the stylesheets of <dir> instead of app/assets
//
// Exit status 0 when no page is worse -- sticks out further than it did, or changed height by more
// than a fifth -- 1 when one is or one could not be measured, 2 when it could not run, which
// includes having nothing to compare.
//
// Why it exists: a rule every page inherits cannot be judged on a harness, because a harness holds
// the text someone thought of. On 2026-11-26 `overflow-wrap: anywhere` went into the mixin that
// every wiki page's text takes, and every check under test/manual passed. This, a scratch file
// then, drew the public pages of two wikis in a 390px phone and found one of them more than twice
// as tall as before: `pre` had inherited the property, stopped counting towards the least width
// of its column, and the Raw column of a #!WikiSyntaxPreview had shrunk to one letter a line.
// The wiki page Dev Less has what was measured that day.
//
// What it replaces: every stylesheet a page links that is also a .css file in app/assets, unless
// the site serves that very text already. It reads the compiled .css -- after editing a .less
// file, `npm run less` first. When the first page links nothing that differs it stops with status
// 2: a stylesheet compared with itself moves nothing, and "nothing moved" would read as "safe".
//
// What it cannot see: a page that needs a login, what a page draws only after a click, and print.
// It changes nothing on the site -- the stylesheets are swapped in the page Chrome has open.
//
// It opens the pages one after another from wherever it runs, as browser-sweep.mjs does, and the
// same warning holds: from an address the wiki does not whitelist, that is what IpRateLimiter bans
// (wiki page Dev BotDetection) -- run it from one it does.
import { mkdirSync, readdirSync, readFileSync, writeFileSync } from 'node:fs';
import { join, relative, resolve } from 'node:path';
import { answerMs, cannotRun, isOrigin, openHeadlessChrome, sitePaths } from './lib/headless-chrome.mjs';
import {
  exitStatus, inPage, linkedStylesheets, measurePage, readable, replaceStylesheets, reportLines, sameText,
  servedText, summarise, verdict, whenStill, withAbsoluteUrls,
} from './lib/stylesheet-comparison.mjs';
import { assetsDir } from './less.mjs';

const usage = 'usage: node scripts/compare-stylesheets.mjs <origin, e.g. https://ahawiki.net> [paths-file] [--phone[=<width>]] [--shots=<dir>] [--assets=<dir>]';
const args = process.argv.slice(2);
const [origin, pathsFile, ...tooMany] = args.filter((a) => !a.startsWith('--'));
const flags = new Map(args.filter((a) => a.startsWith('--')).map((a) => {
  const at = a.indexOf('=');
  return at < 0 ? [a.slice(2), true] : [a.slice(2, at), a.slice(at + 1)];
}));
const phoneWidth = flags.get('phone') === true ? 390 : Number(flags.get('phone'));
if (
  !isOrigin(origin) || tooMany.length
  || [...flags.keys()].some((name) => !['phone', 'shots', 'assets'].includes(name))
  || ['shots', 'assets'].some((name) => flags.has(name) && (flags.get(name) === true || !flags.get(name)))
  || (flags.has('phone') && !(Number.isInteger(phoneWidth) && phoneWidth >= 240 && phoneWidth <= 1000))
) cannotRun(usage);

const size = flags.has('phone') ? { width: phoneWidth, height: 844, mobile: true } : { width: 1280, height: 900, mobile: false };
const shots = flags.get('shots');
const assets = flags.has('assets') ? resolve(flags.get('assets')) : assetsDir;
// As a person would type it from here: app/assets, or the whole path of a directory elsewhere.
const assetsName = relative(process.cwd(), assets).startsWith('..') ? assets : relative(process.cwd(), assets) || '.';
const loadMs = 20000;
// Of a page longer than this, a picture shows the top.
const pictureHeight = 8000;

let local;
let paths;
let browser;
try {
  local = new Map(readdirSync(assets).filter((name) => name.endsWith('.css')).map((name) => [name, readFileSync(join(assets, name), 'utf8')]));
  if (!local.size) throw new Error(`no .css file in ${assetsName}`);
  if (shots) mkdirSync(shots, { recursive: true });
  paths = await sitePaths(origin, pathsFile);
  if (!paths.length) throw new Error('no pages to open');
  browser = await openHeadlessChrome({ profilePrefix: 'compare-stylesheets-', width: size.width, height: size.height, domains: ['Page', 'Runtime', 'Network'] });
} catch (e) {
  cannotRun(String(e));
}
// The window alone does not go as narrow as a phone, and a phone lays a page out by its viewport
// meta, which a desktop window ignores.
await browser.send('Emulation.setDeviceMetricsOverride', { width: size.width, height: size.height, deviceScaleFactor: 1, mobile: size.mobile });

// Whether the checkout's file differs from the text the site serves at an address. Asked of the
// page, so it is the text the page itself was drawn with, and once for each address.
const differing = new Map();
async function differs({ name, href }) {
  if (!differing.has(href)) {
    const served = await browser.evaluate(inPage(servedText, href));
    if (typeof served !== 'string') throw new Error(`could not read ${href}: ${served.error}`);
    differing.set(href, !sameText(served, local.get(name)));
  }
  return differing.get(href);
}

async function picture(height) {
  const answer = await browser.send('Page.captureScreenshot', {
    format: 'png', captureBeyondViewport: true, clip: { x: 0, y: 0, width: size.width, height: Math.min(height, pictureHeight), scale: 1 },
  });
  return answer.result?.data ? Buffer.from(answer.result.data, 'base64') : null;
}

/** One page: `{ before, after, replaced }`, or `{ error }` when it could not be measured. */
async function compare(path, index) {
  const { answered } = await browser.open(origin + path, loadMs);
  if (!answered) return { error: `the navigation was not answered in ${answerMs / 1000}s` };
  // An error page has a height too, and would compare as "the same" -- as would every page of a
  // run the wiki has banned. Only a page that answered 200 is measured.
  const answer = browser.events.find((e) => e.method === 'Network.responseReceived' && e.params.type === 'Document');
  if (!answer) return { error: 'the page did not answer' };
  if (answer.params.response.status !== 200) return { error: `HTTP ${answer.params.response.status}` };

  const linked = await browser.evaluate(inPage(linkedStylesheets, [...local.keys()]));
  if (!Array.isArray(linked)) return { error: linked.error };
  const replacements = [];
  for (const sheet of linked) if (await differs(sheet)) replacements.push({ ...sheet, css: local.get(sheet.name) });
  if (!replacements.length) return { error: 'links no stylesheet that differs', nothingDiffers: true };

  const heldBefore = await browser.evaluate(inPage(whenStill));
  const before = await browser.evaluate(inPage(measurePage));
  if (before.error) return before;
  const pictureBefore = shots && await picture(before.height);
  const swapped = await browser.evaluate(inPage(replaceStylesheets, replacements.map(({ href, css }) => ({ href, css })), withAbsoluteUrls));
  if (swapped.error) return swapped;
  const heldAfter = await browser.evaluate(inPage(whenStill));
  const after = await browser.evaluate(inPage(measurePage));
  if (after.error) return after;

  const { worse, moved } = verdict({ before, after });
  if (pictureBefore && (worse.length || moved.length)) {
    const pictureAfter = await picture(after.height);
    const stem = join(shots, `${String(index + 1).padStart(4, '0')}-${readable(path).replace(/[^\p{L}\p{N}]+/gu, '_').replace(/^_+|_+$/g, '').slice(0, 80)}`);
    writeFileSync(`${stem}.before.png`, pictureBefore);
    if (pictureAfter) writeFileSync(`${stem}.after.png`, pictureAfter);
  }
  return { before, after, replaced: replacements.map((r) => r.name), unsettled: heldBefore !== true || heldAfter !== true };
}

const rows = [];
const announced = new Set();
for (const [index, path] of paths.entries()) {
  const started = Date.now();
  let row;
  try {
    row = { path, ...(await compare(path, index)) };
  } catch (e) {
    row = { path, error: String(e.message || e) };
  }
  if (row.nothingDiffers && !rows.some((r) => r.before)) {
    await browser.close();
    cannotRun(`${readable(path)} links no stylesheet that differs from the file of that name in ${assetsName}: there is nothing to compare.\nAfter editing a .less file, \`npm run less\` writes the .css this reads.`);
  }
  for (const name of row.replaced || []) {
    if (announced.has(name)) continue;
    announced.add(name);
    console.log(`in place of the site's ${name}: ${join(assetsName, name)}`);
  }
  rows.push(row);
  const { worse, moved } = row.before ? verdict(row) : { worse: [], moved: [] };
  const state = row.error ? 'FAILED' : worse.length ? 'WORSE ' : moved.length ? 'moved ' : 'same  ';
  const lines = [
    ...(row.error ? [row.error] : []),
    ...worse.map((x) => `worse: ${x}`),
    ...moved,
    ...(row.unsettled ? ['its height was still changing when it was measured'] : []),
  ];
  const seconds = ((Date.now() - started) / 1000).toFixed(1);
  console.log(`${state} ${readable(path)} (${seconds}s)${lines.map((x) => `\n    ${x}`).join('')}`);
  // Whatever the page still runs or loads stops here, so it cannot hold up the next one.
  await browser.send('Page.stopLoading', {}, 5000);
}

const summary = summarise(rows);
console.log(`\nat ${size.width}px${size.mobile ? ', as a phone' : ''}: ${reportLines(summary).join('\n')}`);
if (shots) console.log(`\npictures of the pages that changed: ${shots}`);
await browser.close();
process.exit(exitStatus(summary));
