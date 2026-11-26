// What compare-stylesheets.mjs measures in a page, and how it reads two measurements of one page.
//
// The first half runs in the page. Each function there is sent as its source text, so it can use
// nothing but its arguments and what a page has. The second half is what the script does with the
// numbers that come back; it needs no browser, and test/compare-stylesheets.test.mjs runs it.

/** `fn(...args)` as an expression for the page. A function among `args` is sent as its source. */
export const inPage = (fn, ...args) =>
  `(${fn})(${args.map((a) => (typeof a === 'function' ? String(a) : JSON.stringify(a))).join(', ')})`;

// ---------------------------------------------------------------------------------------------
// In the page
// ---------------------------------------------------------------------------------------------

/**
 * The stylesheets the page links from its own site whose file name is one of `names`. By name
 * alone: nothing else the pages link is called what a stylesheet in app/assets is, and the test
 * fails the day something is.
 */
export function linkedStylesheets(names) {
  return [...document.querySelectorAll('link[rel~="stylesheet"]')]
    .filter((link) => link.href && new URL(link.href).origin === location.origin)
    .map((link) => ({ href: link.href, name: new URL(link.href).pathname.split('/').pop() }))
    .filter(({ name }) => names.includes(name));
}

/** The text the page gets at `href`: what it is drawn with now. */
export async function servedText(href) {
  const answer = await fetch(href);
  if (!answer.ok) throw new Error(`HTTP ${answer.status}`);
  return answer.text();
}

/**
 * Puts each `css` where the link to its `href` is: a <style> in the link's place, so the cascade
 * keeps its order.
 */
export function replaceStylesheets(replacements, withAbsoluteUrls) {
  for (const { href, css } of replacements) {
    const link = [...document.querySelectorAll('link[rel~="stylesheet"]')].find((l) => l.href === href);
    if (!link) return { error: `the link to ${href} is gone` };
    const style = document.createElement('style');
    if (link.media) style.media = link.media;
    style.textContent = withAbsoluteUrls(css, href);
    link.after(style);
    link.remove();
    // A page whose Content-Security-Policy forbids inline styles would drop the text and be
    // measured with no stylesheet at all, which moves everything and means nothing.
    if (!style.sheet || !style.sheet.cssRules.length) return { error: `the page did not take a stylesheet in place of ${href}` };
  }
  return { replaced: replacements.length };
}

/**
 * Waits for the page's fonts and then for its height to hold for half a second. False when it was
 * still changing after five: what is measured then is a moment, not the page.
 */
export async function whenStill() {
  if (document.fonts && document.fonts.ready) await Promise.race([document.fonts.ready, new Promise((r) => setTimeout(r, 5000))]);
  const height = () => document.documentElement.scrollHeight;
  let last = height();
  let held = 0;
  for (let i = 0; i < 20 && held < 2; i++) {
    await new Promise((r) => setTimeout(r, 250));
    const now = height();
    held = now === last ? held + 1 : 0;
    last = now;
  }
  return held >= 2;
}

/** One drawing of the page, in numbers. Everything but the first two is about `.wikiContent`. */
export function measurePage() {
  const root = document.documentElement;
  const out = { overflowX: root.scrollWidth - root.clientWidth, height: root.scrollHeight };
  const content = document.querySelector('.wikiContent');
  if (!content) return out;

  // Elements past the content's right edge. One inside something that scrolls or clips is not out.
  const edge = content.getBoundingClientRect().right;
  const contained = (el) => {
    for (let e = el.parentElement; e && e !== content; e = e.parentElement) {
      if (getComputedStyle(e).overflowX !== 'visible') return true;
    }
    return false;
  };
  out.pastEdge = [...content.querySelectorAll('*')].filter((el) => {
    const r = el.getBoundingClientRect();
    return r.width > 0 && r.right > edge + 1 && !contained(el);
  }).length;

  const tables = [...content.querySelectorAll('table')];
  out.tables = tables.length;
  out.tablesWiderThanBox = tables.filter((t) => t.getBoundingClientRect().width > t.parentElement.getBoundingClientRect().width + 1).length;
  out.tableWidth = Math.round(tables.reduce((sum, t) => sum + t.getBoundingClientRect().width, 0));

  // Words drawn on more than one line, in the innermost blocks of text. Code is left out: it
  // breaks after any letter on purpose. Counting stops after `enough` words, so a very long page
  // is sampled from its top -- the same words both times.
  const blocks = 'p, li, td, th, dd, dt, blockquote';
  const enough = 6000;
  out.words = 0;
  out.wordsOnTwoLines = 0;
  out.examples = [];
  for (const block of content.querySelectorAll(blocks)) {
    if (out.words > enough) break;
    if (block.querySelector(blocks)) continue;
    const walker = document.createTreeWalker(block, NodeFilter.SHOW_TEXT);
    while (walker.nextNode()) {
      const node = walker.currentNode;
      if (node.parentElement.closest('code, pre, .headingNumber, script, style')) continue;
      for (const match of node.nodeValue.matchAll(/\S+/g)) {
        const range = document.createRange();
        range.setStart(node, match.index);
        range.setEnd(node, match.index + match[0].length);
        const lines = new Set([...range.getClientRects()].filter((r) => r.width > 0).map((r) => Math.round(r.top)));
        if (!lines.size) continue;
        out.words++;
        if (lines.size > 1) {
          out.wordsOnTwoLines++;
          if (out.examples.length < 3) out.examples.push(match[0].slice(0, 30));
        }
      }
    }
  }

  // A Paper document: how far above its footer each page's text ends. Negative is past it.
  const rooms = [...document.querySelectorAll('.paperContent .page')].map((page) => {
    const footer = page.querySelector('.pageFooter');
    const text = page.querySelector('.pageContent > div');
    return footer && text ? Math.round(footer.getBoundingClientRect().top - text.getBoundingClientRect().bottom) : null;
  });
  if (rooms.length) out.paper = rooms;
  return out;
}

// ---------------------------------------------------------------------------------------------
// In the page and here
// ---------------------------------------------------------------------------------------------

/**
 * `css` with each url() that is relative to the stylesheet written out in full. A <link> resolves
 * such a path against the stylesheet's address and a <style> against the page's, so a stylesheet
 * moved from one to the other would lose its fonts and pictures -- and be measured without them.
 * Paths that are whole already, or start at the site's root, read the same from both.
 */
export function withAbsoluteUrls(css, base) {
  return css.replace(/url\(\s*(?:"([^"]*)"|'([^']*)'|([^'")\s][^)\s]*))\s*\)/g, (whole, double, single, bare) => {
    const target = double ?? single ?? bare;
    if (!target || /^([a-z][a-z0-9+.-]*:|\/|#)/i.test(target)) return whole;
    return `url("${new URL(target, base).href}")`;
  });
}

// ---------------------------------------------------------------------------------------------
// Here
// ---------------------------------------------------------------------------------------------

/** Whether two stylesheets are the same text. A checkout on Windows may hold the file with CRLF. */
export const sameText = (a, b) => a.replace(/\r\n/g, '\n') === b.replace(/\r\n/g, '\n');

/** A path as a person reads it: /w/한글 rather than /w/%ED%95%9C%EA%B8%80. */
export function readable(path) {
  try {
    return decodeURIComponent(path);
  } catch {
    return path;
  }
}

const room = (px) => (px < 0 ? `${-px}px past its footer` : `${px}px to spare`);
const percent = (was, now) => (was ? ` (${now >= was ? '+' : ''}${(((now - was) / was) * 100).toFixed(1)}%)` : '');

/**
 * How far a page's height may change before that alone makes it a page to look at. The change
 * this was written for put nothing out of place on the page it broke: a column had shrunk to one
 * letter a line, and all there was to see in numbers was a page 2.2 times as tall. What that
 * day's change was meant to do moved no page by more than 4.6%.
 */
export const muchHeight = 0.2;

/**
 * What one page's two measurements say. `worse` is what fails a run: the page sticks out further
 * than it did, or its height changed by more than `muchHeight`. `moved` is everything else that
 * changed -- a page a little taller, or breaking fewer words, is what a change is usually for,
 * and whether that is good is for whoever reads the line to say.
 */
export function verdict({ before, after }) {
  const worse = [];
  const moved = [];
  const note = (isWorse, text) => (isWorse ? worse : moved).push(text);

  // A page is not wider than its window for a pixel of rounding, either way.
  if (after.overflowX !== before.overflowX && (after.overflowX > 1 || before.overflowX > 1)) {
    note(after.overflowX > 1 && after.overflowX > before.overflowX + 1, `wider than its window by ${before.overflowX}px -> ${after.overflowX}px`);
  }
  for (const [key, label] of [['pastEdge', "elements past the content's right edge"], ['tablesWiderThanBox', 'tables wider than their box']]) {
    const [was, now] = [before[key] ?? 0, after[key] ?? 0];
    if (now !== was) note(now > was, `${label} ${was} -> ${now}`);
  }
  (after.paper || []).forEach((now, i) => {
    const was = before.paper?.[i];
    if (now === null || was === null || was === undefined || now === was) return;
    note(now < 0 && now < was, `Paper page ${i + 1}: ${room(was)} -> ${room(now)}`);
  });

  if (Math.abs((after.tableWidth ?? 0) - (before.tableWidth ?? 0)) > 2) moved.push(`tables' width ${before.tableWidth}px -> ${after.tableWidth}px`);
  if ((after.wordsOnTwoLines ?? 0) !== (before.wordsOnTwoLines ?? 0)) moved.push(`words on two lines ${before.wordsOnTwoLines} -> ${after.wordsOnTwoLines}`);
  if (after.height !== before.height) {
    const change = Math.abs(after.height - before.height) / (before.height || 1);
    note(change > muchHeight, `height ${before.height}px -> ${after.height}px${percent(before.height, after.height)}`);
  }
  return { worse, moved };
}

const moreBroken = (r) => (r.after.wordsOnTwoLines ?? 0) - (r.before.wordsOnTwoLines ?? 0);

/**
 * The run in numbers. A row is `{ path, before, after }` when the page was measured and
 * `{ path, error }` when it was not.
 */
export function summarise(rows) {
  const measured = rows.filter((r) => r.before && r.after).map((r) => ({ ...r, ...verdict(r) }));
  const total = (key, side) => measured.reduce((n, r) => n + (r[side][key] ?? 0), 0);
  const both = (key) => [total(key, 'before'), total(key, 'after')];
  const pagesWhere = (test) => ['before', 'after'].map((side) => measured.filter((r) => test(r[side])).length);
  const papers = measured.filter((r) => r.before.paper && r.after.paper);
  const pastFooter = (side) => papers.reduce((n, r) => n + r[side].paper.filter((px) => px !== null && px < 0).length, 0);
  const growth = (r) => (r.after.height - r.before.height) / (r.before.height || 1);
  const byGrowth = measured.filter((r) => r.after.height !== r.before.height).sort((a, b) => growth(b) - growth(a));
  return {
    pages: rows.length,
    notMeasured: rows.filter((r) => !(r.before && r.after)),
    worse: measured.filter((r) => r.worse.length),
    moved: measured.filter((r) => !r.worse.length && r.moved.length).length,
    same: measured.filter((r) => !r.worse.length && !r.moved.length).length,
    widerThanWindow: pagesWhere((m) => m.overflowX > 1),
    pastEdge: both('pastEdge'),
    tables: total('tables', 'before'),
    tablesWiderThanBox: both('tablesWiderThanBox'),
    paperDocuments: papers.length,
    paperPages: papers.reduce((n, r) => n + r.before.paper.length, 0),
    paperPagesPastFooter: [pastFooter('before'), pastFooter('after')],
    words: total('words', 'before'),
    wordsOnTwoLines: both('wordsOnTwoLines'),
    height: both('height'),
    taller: byGrowth.filter((r) => growth(r) > 0),
    shorter: byGrowth.filter((r) => growth(r) < 0).reverse(),
    moreWordsOnTwoLines: measured.filter((r) => moreBroken(r) > 0).sort((a, b) => moreBroken(b) - moreBroken(a)),
  };
}

/**
 * 0 when no page is worse, 1 when one is or one could not be measured, 2 when no page was
 * measured at all -- which is a run that says nothing, not a clean one.
 */
export const exitStatus = (summary) =>
  (summary.notMeasured.length === summary.pages ? 2 : summary.worse.length || summary.notMeasured.length ? 1 : 0);

/** The summary as the lines the script prints last. `top` is how many pages each list names. */
export function reportLines(s, top = 5) {
  const arrow = ([was, now]) => `${was} -> ${now}`;
  const list = (rows, line) => [
    ...rows.slice(0, top).map((r) => `      ${readable(r.path)}: ${line(r)}`),
    ...(rows.length > top ? [`      ... and ${rows.length - top} more`] : []),
  ];
  const heights = (r) => `${r.before.height}px -> ${r.after.height}px${percent(r.before.height, r.after.height)}`;
  return [
    `${s.pages} pages: ${s.worse.length} worse, ${s.moved} moved, ${s.same} the same${s.notMeasured.length ? `, ${s.notMeasured.length} not measured` : ''}`,
    '',
    'what sticks out -- more of any of these makes a page worse',
    `  pages wider than their window: ${arrow(s.widerThanWindow)}`,
    `  elements past the content's right edge: ${arrow(s.pastEdge)}`,
    `  tables wider than their box: ${arrow(s.tablesWiderThanBox)} of ${s.tables}`,
    // No line for what the pages did not have: "0 -> 0 of 0" reads as a thing that was checked.
    ...(s.paperPages ? [`  Paper pages past their footer: ${arrow(s.paperPagesPastFooter)} of ${s.paperPages}, in ${s.paperDocuments} Paper document${s.paperDocuments === 1 ? '' : 's'}`] : []),
    '',
    `what else moved -- a height changed by more than ${muchHeight * 100}% makes a page worse too`,
    `  words drawn on two lines: ${arrow(s.wordsOnTwoLines)} of ${s.words}; pages with more than before: ${s.moreWordsOnTwoLines.length}`,
    ...list(s.moreWordsOnTwoLines, (r) => `${r.before.wordsOnTwoLines} -> ${r.after.wordsOnTwoLines} of ${r.after.words}, e.g. ${r.after.examples.map((x) => JSON.stringify(x)).join(' ')}`),
    `  height of all pages: ${arrow(s.height)}px${percent(...s.height)}; ${s.taller.length} taller, ${s.shorter.length} shorter`,
    ...list(s.taller, heights),
    ...list(s.shorter, heights),
    ...(s.worse.length ? ['', 'worse', ...s.worse.flatMap((r) => [`  ${readable(r.path)}`, ...r.worse.map((x) => `      ${x}`)])] : []),
    ...(s.notMeasured.length ? ['', 'not measured', ...s.notMeasured.map((r) => `  ${readable(r.path)}: ${r.error}`)] : []),
  ];
}
