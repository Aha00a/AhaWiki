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

    const unknownMacro = new Map(), unknownBlock = new Map(), silentLink = [];
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
        for (const m of linkable.matchAll(regexLink)) {
            if (m[1] || m[2] || m[9] === undefined) continue;
            const whole = `${m[9]} ${m[10]}`.trim();
            if (known.has(whole)) silentLink.push(`${where}: [${whole}] goes to "${m[9]}"`);
        }
    }

    const report = (title, map) => {
        console.log(`\n=== ${title}: ${map.size} ===`);
        [...map].sort((a, b) => b[1].size - a[1].size).forEach(([name, pages]) =>
            console.log(`  ${name}  ${pages.size} page(s): ${[...pages].slice(0, 4).join(' | ')}`));
        if (!map.size) console.log('  (none)');
    };

    console.log(`${rows.length} pages across ${pagesBySite.size} sites`);
    console.log(`\n=== a link written as target and alias whose whole text names a page: ${silentLink.length} ===`);
    silentLink.forEach(l => console.log(`  ${l}`));
    if (!silentLink.length) console.log('  (none)');
    report('macro names the app does not register', unknownMacro);
    report('block names the app does not register', unknownBlock);

    return silentLink.length + unknownMacro.size + unknownBlock.size ? 1 : 0;
}

if (process.argv.length < 3) {
    console.error('usage: node scripts/audit.wiki.mjs <dump.tsv>   (see the header for the query)');
    process.exit(2);
}
process.exit(main(process.argv[2]));
