// Reads every current page on every site and reports the things that render as an error, or as
// something other than what was written. The tests under test/ cover the 116 pages committed to
// this repository; this covers all of them, including the sites whose pages live only on the wiki.
//
//   node scripts/audit.wiki.mjs <dump.tsv>
//
// The dump is one page per line, tab separated, base64 so newlines survive:
//
//   ssh <host> 'mysql --defaults-file=~/.my.rds.cnf -B -N -e "
//     SELECT p.site, TO_BASE64(p.name), TO_BASE64(p.content)
//     FROM Page p JOIN (SELECT site, name, MAX(revision) r FROM Page GROUP BY site, name) m
//       ON m.site=p.site AND m.name=p.name AND m.r=p.revision"' > dump.tsv
//
// The database rather than the API, because the API returns only pages the key may read and the
// point is to see every site. TO_BASE64 wraps its output every 76 characters and mysql -B escapes
// those newlines, which is what the \n stripping below is for.
//
// Three false positives cost an hour the first time this ran, and each is a rule the renderer has:
//
//   * Macros and links are only seen outside [[[blocks]]] and backticks, because those come out
//     of the text first. Scanning the raw source calls every documented example a broken one.
//   * Set and Get are not in mapMacros -- they were named directly in convert -- so a list built
//     from that map called them unregistered. They have since been removed, and the entry stays
//     as the reason the list below is written out rather than derived.
//   * The macro list ends without a trailing comma, so a regex demanding one dropped MacroSuccess.
import fs from 'node:fs';

const registeredMacros = new Set([
    'AdjacentPages', 'AhaWikiSiteList', 'Attachment', 'Backlinks', 'Br', 'CB', 'Calendar',
    'CheckBox', 'ColorCode', 'Copyable', 'DayHeader', 'Embed', 'Error', 'Html', 'Image', 'Include',
    'Info', 'InlineDays', 'Kbd', 'LinkDate', 'MonthName', 'Navigation', 'NavigationYear',
    'NavigationYearMonth', 'PageCount', 'PageList', 'PageMap', 'PercentLinkTitle', 'Period',
    'RecentChanges', 'Ruby', 'SimilarPages', 'SiteName', 'Success', 'Themes', 'TitleIndex',
    'Trivial', 'TwinPages', 'Uptime', 'WeekdayName', 'WikiStatistics', 'Years',
]);

// Interpreters.map lowercases every name, and carries one alias.
const registeredInterpreters = new Set([
    'wiki', 'paper', 'wikisyntaxpreview', 'comment', 'html', 'text', 'markdown', 'quote', 'fold',
    'vim', 'table', 'graph', 'math', 'map', 'mermaid', 'kanban', 'gantt', 'schema', 'ahatracquote',
]);

// Read from the page header, never from a block, so a block naming one is still an error.
const directives = new Set(['read', 'write', 'redirect', 'var']);

// A red link is normal on a wiki -- it is how a page gets asked for. A link that misses an
// existing page by case or spacing is not: Page.name is utf8mb4_bin, so `[PHP]` never reaches
// `Php` and never will, and it is spelled the way the reader expects, so nothing looks wrong.
// What counts as missing is decided by AhaMarkLink.toHtmlString; these are its exclusions.
const defaultPages = new Set(['AhaWiki', 'FrontPage', 'PageList', 'PageMap', 'RecentChanges', 'TitleIndex', 'WikiStatistics']);
const dateShapes = [/^\d{4}-\d{2}-\d{2}$/, /^\d{4}-\d{2}$/, /^\d{4}$/, /^---\d{2}$/, /^--\d{2}-\d{2}$/, /^--\d{2}$/];
const fold = name => name.toLowerCase().replace(/\s+/g, ' ').trim();

const siteHost = {
    '1': 'aha00a.com', '2': 'ahawiki.net', '3': 'wiki.aharise.com', '6': 'fuerinha.ahawiki.net',
    '8': 'cellivery.ahawiki.net', '9': 'oc.ahawiki.net', '11': 'whohow.net',
    '13': 'agriina.ahawiki.net', '14': 'kumhotire.ahawiki.net',
};
const hostOf = site => siteHost[site] || `site ${site}`;

/** The alternatives of InterpreterWiki.regexLink, in the order it tries them. */
const regexLink = /((?<!\\)\\)?(?:([a-zA-Z][-a-zA-Z0-9+._]+:\/\/\S+)|\["([^\]"]+)"\]|\[(?![?"])((?:(?!:\/\/)[^\]|])+)\|([^\]]+)\]|\[([^\]\s]+)\]|\["([^\]"]+)"\s+([^\]]+)\]|\[([^\]\s]+)\s+([^\]]+)\])/g;

function readDump(file) {
    return fs.readFileSync(file, 'utf8').trim().split('\n').map(line => {
        const [site, name64, content64] = line.split('\t');
        const decode = s => Buffer.from((s || '').replace(/\\n/g, ''), 'base64').toString('utf8');
        return {site, name: decode(name64), content: decode(content64)};
    });
}

function main(file) {
    const rows = readDump(file);
    const pagesBySite = new Map();
    for (const r of rows) {
        if (!pagesBySite.has(r.site)) pagesBySite.set(r.site, new Set());
        pagesBySite.get(r.site).add(r.name);
    }

    // The names of each site's pages, folded, so a link can be asked whether it is one of them
    // under a different case or spacing.
    const foldedBySite = new Map();
    for (const [site, names] of pagesBySite) {
        const folded = new Map();
        for (const name of names) if (!folded.has(fold(name))) folded.set(fold(name), name);
        foldedBySite.set(site, folded);
    }

    const unknownMacro = new Map(), unknownBlock = new Map(), silentLink = [], caseMiss = [];
    const note = (map, key, where) => {
        if (!map.has(key)) map.set(key, new Set());
        map.get(key).add(where);
    };

    for (const r of rows) {
        const where = `${hostOf(r.site)} ${r.name}`;
        const withoutBackticks = r.content.replace(/`[^`\n]*`/g, ' ');

        for (const m of withoutBackticks.matchAll(/^\[\[\[#!\s*([A-Za-z][A-Za-z0-9]*)/gm)) {
            const name = m[1].toLowerCase();
            if (!registeredInterpreters.has(name) && !directives.has(name)) note(unknownBlock, m[1], where);
        }

        const afterBlocks = withoutBackticks.replace(/\[\[\[[\s\S]*?\]\]\]/g, ' ');
        for (const m of afterBlocks.matchAll(/\[\[([A-Za-z][A-Za-z0-9]*)(\([^\]]*\))?\]\]/g))
            if (!registeredMacros.has(m[1])) note(unknownMacro, m[1], where);

        const linkable = afterBlocks.replace(/\[\[[^\]]*\]\]/g, ' ');
        const known = pagesBySite.get(r.site);
        const folded = foldedBySite.get(r.site);
        for (const m of linkable.matchAll(regexLink)) {
            if (m[1] || m[2]) continue;                          // escaped, or a bare URL

            if (m[9] !== undefined) {
                const whole = `${m[9]} ${m[10]}`.trim();
                if (known.has(whole)) silentLink.push(`${where}: [${whole}] goes to "${m[9]}"`);
            }

            // Every alternative names a page in its first group; the alias does not matter here.
            const raw = m[3] ?? m[4] ?? m[6] ?? m[7] ?? m[9];
            if (raw === undefined) continue;
            const uri = (raw.startsWith('wiki:') ? raw.slice(5) : raw).trim();
            if (!uri || uri.startsWith('#') || uri.startsWith('?')) continue;
            if (/^[a-zA-Z][-a-zA-Z0-9+._]+:\/\//.test(uri)) continue;
            if (uri.startsWith('schema:') || uri.startsWith('User:')) continue;
            const target = uri.replace(/[#?].+$/, '');
            if (!target || known.has(target) || defaultPages.has(target)) continue;
            if (dateShapes.some(shape => shape.test(target))) continue;

            const real = folded.get(fold(target));
            if (real !== undefined) caseMiss.push(`${where}: [${target}] -- the page is "${real}"`);
        }
    }

    const report = (title, map) => {
        console.log(`\n=== ${title}: ${map.size} ===`);
        [...map].sort((a, b) => b[1].size - a[1].size).forEach(([name, pages]) =>
            console.log(`  ${name}  ${pages.size} page(s): ${[...pages].slice(0, 4).join(' | ')}`));
        if (!map.size) console.log('  (none)');
    };

    const list = (title, lines) => {
        const unique = [...new Set(lines)].sort();
        console.log(`\n=== ${title}: ${unique.length} ===`);
        unique.forEach(l => console.log(`  ${l}`));
        if (!unique.length) console.log('  (none)');
        return unique.length;
    };

    console.log(`${rows.length} pages across ${pagesBySite.size} sites`);
    const silent = list('a link written as target and alias whose whole text names a page', silentLink);
    const missed = list('a link that misses an existing page by case or spacing', caseMiss);
    report('macro names the app does not register', unknownMacro);
    report('block names the app does not register', unknownBlock);

    return silent + missed + unknownMacro.size + unknownBlock.size ? 1 : 0;
}

if (process.argv.length < 3) {
    console.error('usage: node scripts/audit.wiki.mjs <dump.tsv>   (see the header for the query)');
    process.exit(2);
}
process.exit(main(process.argv[2]));
