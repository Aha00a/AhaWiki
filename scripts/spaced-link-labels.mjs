// Rewrites the links written `[first rest]` in the sense AhaMark gave them until 2026-09-14 -- the
// page `first`, labelled `rest` -- to `[first|rest]`, which reads the same under the grammar that
// replaced it. That grammar takes the whole bracket as one page name, spaces included, so
// `[Forrest Gump]` goes to "Forrest Gump" instead of to "Forrest" showing "Gump".
//
//   node scripts/spaced-link-labels.mjs <dump.tsv> <site-names.tsv> <out-dir>
//
// It writes SQL rather than saving through the API, and it rewrites every revision, not only the
// latest. There is no parser version: an old revision is rendered by today's parser, so a save
// through the API would add one correct revision and leave every earlier one misread.
// scripts/README.md has the run, the numbers and the owner's instruction.
//
// The dump is every row of Page, one per line, base64 so newlines survive:
//
//   SELECT site, TO_BASE64(name), revision, TO_BASE64(content), TO_BASE64(comment) FROM Page
//
// and <site-names.tsv> is `SELECT seq, name FROM Site`, which only labels the report.
//
// WHICH LINKS. A link is rewritten when the old reading found a page and the new one does not: its
// first word names a page on its site, or one every site has (conf/Page, a date, schema:), and its
// whole text does not. When the whole text names a page too, the new reading is the likelier
// intent and the link is left to it; so is a link whose first word names nothing, such as
// [Forrest Gump]. A few of those read as a label all the same, and `labelledByReview` lists the
// ones chosen when the dry run was read.
//
// WHERE, as the renderer reads it. Wiki text outside backticks, macros and verbatim blocks, one line
// at a time, because InterpreterWiki applies the pattern per line. The bodies of the blocks it
// renders as wiki: Wiki, Quote, AhaTracQuote, Fold, Paper, Kanban, Table, a WikiSyntaxPreview of
// one of those, and a block that opens with a directive such as #!var. And every revision's
// comment, which the history view renders inline. A table splits its cells before links are read,
// so there a candidate holding the delimiter, or a double quote -- CSV quoting, so [""X Y""] is the
// cell text ["X Y"] -- is not a link.
//
// The mask is not lib/ahamark.mjs's maskUnlinkable. That one blanks every [[[block]]] whole, and
// this has to read inside the blocks rendered as wiki.
import crypto from 'node:crypto';
import fs from 'node:fs';
import path from 'node:path';
import { regexLink } from './lib/ahamark.mjs';
import { rootDir } from './lib/ahawiki.net.mjs';

/** InterpreterWiki.regexLink until 2026-09-14: the grammar every revision before then was written for. */
export const regexLinkUntil20260914 =
    /((?<!\\)\\)?(?:([a-zA-Z][-a-zA-Z0-9+._]+:\/\/\S+)|\["([^\]"]+)"\]|\[(?![?"])((?:(?!:\/\/)[^\]|])+)\|([^\]]+)\]|\[([^\]\s]+)\]|\["([^\]"]+)"\s+([^\]]+)\]|\[([^\]\s]+)\s+([^\]]+)\])/g;

const regexLinkNow = new RegExp(regexLink.source);

/**
 * What the current grammar makes of one old `[first rest]` link: the label reading it keeps for a
 * target that is not a page title, one page name, or no link at all -- `[a b ]` ends in a space,
 * and a name may not.
 */
export function readNow(text) {
    const m = regexLinkNow.exec(text);
    if (!m || m.index !== 0 || m[0].length !== text.length) return { kind: 'none' };
    if (m[9] !== undefined) return { kind: 'label' };
    if (m[11] !== undefined) return { kind: 'name', name: m[11] };
    throw new Error(`the current grammar reads ${text} some third way`);
}

/**
 * Links whose first word names no page but which read as a label all the same: a language or
 * country code followed by its name, a word followed by itself in lower case, a page name
 * followed by a description. Chosen on 2026-09-13 from the dry run's list, and keyed by site,
 * page and the link as written.
 */
export const labelledByReview = new Set([
    ['1', 'Arity', '[la Latin]'],
    ['1', 'Arity', '[el Greek]'],
    ['1', 'Font Awesome', '[Library library]'],
    ['1', 'Font Awesome', '[Toolkit toolkit]'],
    ['1', 'Go', '[ComputerLanguage programming language]'],
    ['1', 'The Edge of Democracy', '[BR 브라질]'],
].map(key => key.join('\t')));

// PageContent: the #! lines at the top are directives, and read, write, redirect and var are not
// the interpreter.
function readDirectives(text) {
    let offset = text.startsWith('\n#!') ? 1 : 0;
    const lines = [];
    while (offset < text.length && text.startsWith('#!', offset)) {
        const newline = text.indexOf('\n', offset);
        lines.push(text.slice(offset + 2, newline < 0 ? text.length : newline).replace(/\r$/, ''));
        offset = newline < 0 ? text.length : newline + 1;
    }
    const shebang = lines
        .filter(line => !line.startsWith('read') && !line.startsWith('write') && !line.startsWith('redirect') && !/^var(\s|$)/.test(line))
        .flatMap(line => line.split(/\s+/))
        .filter(Boolean);
    return {
        hasDirectives: lines.length > 0,
        redirect: lines.some(line => line.startsWith('redirect')),
        interpreter: shebang.length ? shebang[0].toLowerCase() : 'wiki',
        args: shebang.slice(1),
        contentOffset: lines.length ? offset : 0,
    };
}

const renderedAsWiki = new Set(['wiki', 'quote', 'ahatracquote', 'fold', 'paper', 'kanban', 'table']);

/** How a body is read: as wiki, as table cells split on a delimiter, or -- null -- not for links. */
function bodyReading({ interpreter, args }) {
    const [kind, format] = interpreter === 'wikisyntaxpreview'
        ? [(args[0] || 'wiki').toLowerCase(), args[1]]
        : [interpreter, args[0]];
    if (!renderedAsWiki.has(kind)) return null;
    if (kind !== 'table') return { delimiter: undefined };
    // InterpreterTable.csvPreferenceFrom: any other format renders an error instead of cells.
    const delimiter = { tsv: '\t', csv: ',' }[format];
    return delimiter ? { delimiter } : null;
}

// What the link pattern never sees, blanked in place so every offset still holds: backtick spans,
// the double copyable kind first, and macros.
const blank = match => match.replace(/[^\n]/g, ' ');
const mask = text => text
    .replace(/``[^\n]*?``/g, blank)
    .replace(/`[^`\n]*`/g, blank)
    .replace(/\[\[(\w*)(?:\((.*?)\))?\]\]/g, blank);

function judgeLink({ seen, before, first, after, where }, exists, labelled) {
    const now = readNow(seen);
    if (now.kind === 'label') return null;
    const firstNamesPage = exists(first);
    const wholeNamesPage = now.kind === 'name' && exists(now.name);
    const category = firstNamesPage ? (wholeNamesPage ? 'both' : 'first') : (wholeNamesPage ? 'name' : 'neither');
    const byReview = category === 'neither' && labelled(before);
    return {
        before, after, where, first,
        name: now.kind === 'name' ? now.name : null,
        category, byReview,
        rewrite: category === 'first' || byReview,
    };
}

/** The old `[first rest]` links in `text`, one line at a time, each judged and placed at `base`. */
function scanLines(text, base, where, delimiter, masked, judge) {
    const found = [];
    const seen = masked ? mask(text) : text;
    let lineStart = 0;
    for (const line of seen.split('\n')) {
        for (const m of line.matchAll(regexLinkUntil20260914)) {
            if (m[1] || m[9] === undefined) continue;
            const at = lineStart + m.index;
            const end = at + m[0].length;
            const before = text.slice(at, end);
            if (delimiter !== undefined && (before.includes('"') || before.includes(delimiter))) continue;
            const firstEnd = at + 1 + m[9].length;
            // Where the mask blanked part of a word, the renderer sees another link than the mask.
            const gap = /^\s+/.exec(text.slice(firstEnd, end - 1));
            if (!gap) continue;
            const first = text.slice(at + 1, firstEnd);
            const link = judge({
                seen: m[0], before, first, where,
                after: `[${first}|${text.slice(firstEnd + gap[0].length, end - 1)}]`,
            });
            if (link) found.push({ ...link, start: base + at, end: base + end });
        }
        lineStart += line.length + 1;
    }
    return found;
}

// A block pairs with the first `]]]` after it, as the renderer pairs them; with none, the rest is text.
function scanWiki(text, base, where, delimiter, judge) {
    const found = [];
    let cursor = 0;
    for (;;) {
        const open = text.indexOf('[[[', cursor);
        const close = open < 0 ? -1 : text.indexOf(']]]', open + 3);
        const textEnd = open < 0 || close < 0 ? text.length : open;
        found.push(...scanLines(text.slice(cursor, textEnd), base + cursor, where, delimiter, true, judge));
        if (open < 0 || close < 0) break;
        const body = text.slice(open + 3, close);
        const block = readDirectives(body);
        const reading = block.hasDirectives ? bodyReading(block) : null;
        if (reading) {
            const at = open + 3 + block.contentOffset;
            found.push(...scanWiki(body.slice(block.contentOffset), base + at, `${where}>${block.interpreter}`, reading.delimiter, judge));
        }
        cursor = close + 3;
    }
    return found;
}

function applyEdits(text, links) {
    let out = '';
    let cursor = 0;
    for (const link of [...links].sort((a, b) => a.start - b.start)) {
        out += text.slice(cursor, link.start) + link.after;
        cursor = link.end;
    }
    return out + text.slice(cursor);
}

/**
 * Every old `[first rest]` link in one revision, judged, and the revision with the ones to
 * rewrite rewritten. `exists(name)` says whether the site has that page; `labelled(before)`
 * whether review chose the label reading for a link whose first word names nothing.
 */
export function planRevision({ content, comment }, { exists, labelled = () => false }) {
    const judge = candidate => judgeLink(candidate, exists, labelled);
    const page = readDirectives(content);
    const reading = page.redirect ? null : bodyReading(page);
    const contentLinks = reading
        ? scanWiki(content.slice(page.contentOffset), page.contentOffset, page.interpreter, reading.delimiter, judge)
        : [];
    const commentLinks = scanLines(comment, 0, 'comment', undefined, false, judge);
    const rewritten = links => links.filter(link => link.rewrite);
    return {
        links: [...contentLinks, ...commentLinks],
        content: applyEdits(content, rewritten(contentLinks)),
        comment: applyEdits(comment, rewritten(commentLinks)),
    };
}

function main(dumpFile, siteNamesFile, outDir) {
    const decode = s => Buffer.from((s || '').replace(/\\n/g, ''), 'base64').toString('utf8');
    const rows = fs.readFileSync(dumpFile, 'utf8').split('\n').filter(Boolean).map(line => {
        const [site, name, revision, content, comment] = line.split('\t');
        return { site, name: decode(name), revision: Number(revision), content: decode(content), comment: decode(comment) };
    });
    const siteNames = new Map(fs.readFileSync(siteNamesFile, 'utf8').split('\n').filter(Boolean).map(line => line.split('\t')));
    const siteLabel = site => `${site} ${siteNames.get(site) ?? '?'}`;

    const namesBySite = new Map();
    const latest = new Map();
    for (const r of rows) {
        if (!namesBySite.has(r.site)) namesBySite.set(r.site, new Set());
        namesBySite.get(r.site).add(r.name);
        const key = `${r.site}\t${r.name}`;
        latest.set(key, Math.max(latest.get(key) ?? 0, r.revision));
    }
    // DefaultPageLogic: pages every site has without saving one.
    const confPages = new Set(fs.readdirSync(path.join(rootDir, 'conf', 'Page')));
    const everySiteHas = name => confPages.has(name) || name.startsWith('schema:')
        || /^\d{4}(-\d{2}(-\d{2})?)?$/.test(name) || /^--\d{2}(-\d{2})?$/.test(name) || /^---\d{2}$/.test(name);

    const links = [];
    const changed = [];
    for (const r of rows) {
        const isLatest = latest.get(`${r.site}\t${r.name}`) === r.revision;
        const plan = planRevision(r, {
            exists: name => namesBySite.get(r.site).has(name) || everySiteHas(name),
            labelled: before => labelledByReview.has(`${r.site}\t${r.name}\t${before}`),
        });
        links.push(...plan.links.map(link => ({ ...link, site: r.site, page: r.name, revision: r.revision, isLatest })));
        if (plan.content !== r.content || plan.comment !== r.comment)
            changed.push({ r, isLatest, content: plan.content, comment: plan.comment });
    }

    // PageMeta.size is the content's length, so a rewrite that changed it would have to change that too.
    const resized = changed.filter(c => c.content.length !== c.r.content.length);
    if (resized.length)
        throw new Error(`these change length, and PageMeta.size would have to follow: ${resized.map(c => `${c.r.site}/${c.r.name}@${c.r.revision}`).join(', ')}`);

    const sha = s => crypto.createHash('sha256').update(s, 'utf8').digest('hex');
    const literal = s => `CONVERT(FROM_BASE64('${Buffer.from(s, 'utf8').toString('base64')}') USING utf8mb4)`;
    // Page.name is utf8mb4_bin and the connection's collation is not, so the literal says which to use.
    const revisionOf = r => `site = ${Number(r.site)} AND name = ${literal(r.name)} COLLATE utf8mb4_bin AND revision = ${Number(r.revision)}`;
    const update = (r, from, to) => {
        const set = [];
        const guard = [];
        for (const column of ['content', 'comment']) {
            if (from[column] === to[column]) continue;
            set.push(`${column} = ${literal(to[column])}`);
            guard.push(`SHA2(${column}, 256) = '${sha(from[column])}'`);
        }
        return `UPDATE Page SET ${set.join(', ')} WHERE ${revisionOf(r)} AND ${guard.join(' AND ')};\nSET @n = @n + ROW_COUNT();`;
    };
    // One transaction, counted: it commits only if every revision was found as the dump saw it.
    const transaction = (updates, end) => [
        'SET NAMES utf8mb4;',
        'START TRANSACTION;',
        'SET @n = 0;',
        ...updates,
        `SELECT @n AS updated, ${updates.length} AS planned;`,
        'DELIMITER //',
        `BEGIN NOT ATOMIC IF @n <> ${updates.length} THEN SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'not every revision was as planned, so nothing is committed'; END IF; END//`,
        'DELIMITER ;',
        end,
        '',
    ].join('\n');
    const migrated = c => ({ content: c.content, comment: c.comment });
    const forward = changed.map(c => update(c.r, c.r, migrated(c)));
    const backward = changed.map(c => update(c.r, migrated(c), c.r));
    const check = changed.map(c => `SELECT CONCAT_WS(' ', site, revision, CASE`
        + ` WHEN SHA2(content, 256) = '${sha(c.content)}' AND SHA2(comment, 256) = '${sha(c.comment)}' THEN 'migrated'`
        + ` WHEN SHA2(content, 256) = '${sha(c.r.content)}' AND SHA2(comment, 256) = '${sha(c.r.comment)}' THEN 'original'`
        + ` ELSE 'other' END) FROM Page WHERE ${revisionOf(c.r)};`);

    fs.mkdirSync(outDir, { recursive: true });
    const write = (file, text) => fs.writeFileSync(path.join(outDir, file), text);
    write('rehearse.sql', transaction(forward, 'ROLLBACK;'));
    write('apply.sql', transaction(forward, 'COMMIT;'));
    write('undo.sql', transaction(backward, 'COMMIT;'));
    write('check.sql', ['SET NAMES utf8mb4;', ...check, ''].join('\n'));
    write('originals.jsonl', changed.map(c => JSON.stringify({ site: c.r.site, name: c.r.name, revision: c.r.revision, content: c.r.content, comment: c.r.comment })).join('\n') + '\n');
    write('migrated.jsonl', changed.map(c => JSON.stringify({ site: c.r.site, name: c.r.name, revision: c.r.revision, current: c.isLatest, content: c.content, comment: c.comment })).join('\n') + '\n');
    const cell = value => String(value ?? '').replace(/\t/g, '\\t');
    write('links.tsv', [
        'site\tpage\trevision\tcurrent\taction\tcategory\tby review\twhere\tbefore\tafter or name',
        ...links.map(l => [siteLabel(l.site), l.page, l.revision, l.isLatest ? 'current' : 'old', l.rewrite ? 'rewrite' : 'keep',
            l.category, l.byReview ? 'yes' : '', l.where, l.before, l.rewrite ? l.after : (l.name ?? '(no link now)')].map(cell).join('\t')),
    ].join('\n') + '\n');

    const lines = [];
    const say = line => lines.push(line);
    say(`${rows.length} revisions of ${latest.size} pages on ${namesBySite.size} sites`);
    say(`rewritten: ${changed.length} revisions, ${changed.filter(c => c.isLatest).length} of them current, ${changed.filter(c => c.comment !== c.r.comment).length} with the comment`);
    say('');
    say('site                          links rewritten (current/old)   left (current/old)   revisions   current pages');
    const bySite = new Map();
    const bump = (site, key) => {
        const counts = bySite.get(site) ?? {};
        counts[key] = (counts[key] ?? 0) + 1;
        bySite.set(site, counts);
    };
    for (const l of links) bump(l.site, `${l.rewrite ? 'rewrite' : 'keep'}${l.isLatest ? 'Current' : 'Old'}`);
    for (const c of changed) {
        bump(c.r.site, 'revisions');
        if (c.isLatest) bump(c.r.site, 'pages');
    }
    for (const [site, n] of [...bySite].sort((a, b) => Number(a[0]) - Number(b[0])))
        say(`${siteLabel(site).padEnd(30)}${`${n.rewriteCurrent ?? 0}/${n.rewriteOld ?? 0}`.padStart(16)}${`${n.keepCurrent ?? 0}/${n.keepOld ?? 0}`.padStart(21)}${String(n.revisions ?? 0).padStart(12)}${String(n.pages ?? 0).padStart(16)}`);
    say('');
    const categories = new Map();
    for (const l of links) {
        const key = `${l.isLatest ? 'current' : 'old'} ${l.rewrite ? 'rewrite' : 'keep'} ${l.category}${l.byReview ? ' (by review)' : ''}`;
        categories.set(key, (categories.get(key) ?? 0) + 1);
    }
    for (const [key, n] of [...categories].sort()) say(`${key}: ${n}`);
    const distinct = rewrite => {
        const seen = new Map();
        for (const l of links.filter(l => l.isLatest && l.rewrite === rewrite)) {
            const key = `${siteLabel(l.site)} | ${l.page} | ${l.before} -> ${rewrite ? l.after : (l.name ?? '(no link now)')}`
                + (l.byReview ? '  [label by review]' : '') + (l.category === 'both' ? '  [the whole name is a page too]' : '');
            seen.set(key, (seen.get(key) ?? 0) + 1);
        }
        return [...seen].map(([key, n]) => (n > 1 ? `${key}  (x${n})` : key));
    };
    say('');
    say('CURRENT PAGES, LEFT TO THE NEW READING:');
    for (const line of distinct(false)) say(`  ${line}`);
    say('');
    say('CURRENT PAGES, REWRITTEN:');
    for (const line of distinct(true)) say(`  ${line}`);
    const met = new Set(links.map(l => `${l.site}\t${l.page}\t${l.before}`));
    const unmet = [...labelledByReview].filter(key => !met.has(key));
    say('');
    say(`labelledByReview entries that matched nothing: ${unmet.length ? unmet.map(key => key.replace(/\t/g, ' | ')).join(' / ') : 'none'}`);
    write('report.txt', lines.join('\n') + '\n');
    console.log(lines.join('\n'));
}

if (process.argv[1] && import.meta.url.endsWith(process.argv[1].replace(/\\/g, '/').split('/').pop())) {
    if (process.argv.length < 5) {
        console.error('usage: node scripts/spaced-link-labels.mjs <dump.tsv> <site-names.tsv> <out-dir>   (see the header)');
        process.exit(2);
    }
    main(...process.argv.slice(2, 5));
}
