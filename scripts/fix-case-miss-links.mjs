// Repairs a link that misses an existing page by case: `[PHP]` where the page is `Php`.
//
// Page.name is utf8mb4_bin, so the two are different names and always will be. The link renders
// as an invitation to write a page that is already written, and nothing about it looks wrong --
// it is spelled the way the reader expects the product to be spelled. That is why these sat
// unnoticed: PHP, ASP.NET and ffmpeg are the correct names of the things. The pages are just
// called something else.
//
//   node scripts/fix-case-miss-links.mjs                          report only, changes nothing
//   node scripts/fix-case-miss-links.mjs --apply --comment="..."  rewrite
//   node scripts/fix-case-miss-links.mjs --apply --comment="..." --only="Waveform"
//
// Needs AHAWIKI_API_KEY. Never write a key into this repository.
//
// It writes `[Php|PHP]`, not `[Php]`: the link goes to the page that exists and the reader still
// sees what the author wrote. One of these pages is a CV, where "Asp.Net" for ASP.NET would be a
// change to how someone presents themselves, not a repair. The pipe form is exactly for this.
//
// It rewrites by offset rather than by replacing the text, because a page also *documents* this
// markup, and `[PHP]` inside a [[[block]]] or backticks must not be touched. The masking below is
// the renderer's order, with regions blanked to the same length so offsets stay true.

const apiKey = process.env.AHAWIKI_API_KEY;
const apply = process.argv.includes('--apply');
const comment = (process.argv.find(a => a.startsWith('--comment=')) || '').slice('--comment='.length);
// Repeat --only to name pages. The first run of any rewrite should be one page, read back.
const only = process.argv.filter(a => a.startsWith('--only=')).map(a => a.slice('--only='.length));

// Found by scripts/audit.wiki.mjs's near-miss pass. Each was checked by reading the target page:
// Php, Asp and Asp.Net are typed ComputerLanguage, FFmpeg describes the tool.
const Repairs = [
    {host: 'aha00a.com', page: 'Aha00aResume'},
    {host: 'aha00a.com', page: 'Computer Language'},
    {host: 'aha00a.com', page: 'Waveform'},
    {host: 'aha00a.com', page: '끄적끄적201201'},
];

/** The written spelling, and the page it was meant to reach. */
const RealPage = new Map([
    ['PHP', 'Php'],
    ['ASP', 'Asp'],
    ['ASP.NET', 'Asp.Net'],
    ['ffmpeg', 'FFmpeg'],
]);

/** The alternatives of InterpreterWiki.regexLink, in the order it tries them. */
const regexLink = /((?<!\\)\\)?(?:([a-zA-Z][-a-zA-Z0-9+._]+:\/\/\S+)|\["([^\]"]+)"\]|\[(?![?"])((?:(?!:\/\/)[^\]|])+)\|([^\]]+)\]|\[([^\]\s]+)\]|\["([^\]"]+)"\s+([^\]]+)\]|\[([^\]\s]+)\s+([^\]]+)\])/g;

/** Blank what the link pattern never sees, keeping every offset where it was. */
export function maskUnlinkable(content) {
    const blank = match => ' '.repeat(match.length);
    return content
        .replace(/`[^`\n]*`/g, blank)
        .replace(/\[\[\[[\s\S]*?\]\]\]/g, m => m.replace(/[^\n]/g, ' '))
        .replace(/\[\[[^\]]*\]\]/g, blank);
}

export function planFor(content) {
    const masked = maskUnlinkable(content);
    const edits = [];

    for (const m of masked.matchAll(regexLink)) {
        if (m[1] || m[2]) continue;                              // escaped, or a bare URL
        // Only the bare `[Target]` alternative. A link that already carries an alias says what
        // its author meant, and one written `[Target Alias]` is a different repair.
        if (m[6] === undefined) continue;
        const written = m[6].startsWith('wiki:') ? m[6].slice(5) : m[6];
        const real = RealPage.get(written);
        if (!real || real === written) continue;
        edits.push({start: m.index, end: m.index + m[0].length, from: m[0], to: `[${real}|${written}]`});
    }

    if (!edits.length) return {kind: 'nothing-to-do'};
    let next = content;
    for (const e of [...edits].reverse()) next = next.slice(0, e.start) + e.to + next.slice(e.end);
    return {kind: 'rewrite', edits, next};
}

async function api(host, method, path, body) {
    const response = await fetch(`https://${host}${path}`, {
        method,
        headers: {
            authorization: `Bearer ${apiKey}`,
            ...(body ? {'content-type': 'application/json'} : {}),
        },
        ...(body ? {body: JSON.stringify(body)} : {}),
    });
    const text = await response.text();
    let json = null;
    try { json = JSON.parse(text); } catch { /* not json */ }
    return {ok: response.ok, status: response.status, text, json};
}

async function main() {
    if (!apiKey) { console.error('AHAWIKI_API_KEY is not set.'); return 1; }
    if (apply && !comment) { console.error('--apply needs --comment="..."'); return 1; }

    const failed = [];
    let changed = 0;

    for (const {host, page} of Repairs) {
        if (only.length && !only.includes(page)) continue;
        const path = `/api/v1/page/${encodeURIComponent(page)}`;
        const current = await api(host, 'GET', path);
        if (!current.ok || !current.json) { failed.push(`${host} ${page}: read ${current.status}`); continue; }

        const plan = planFor(current.json.content);
        if (plan.kind !== 'rewrite') { console.log(`\n${host} ${page}: nothing to do`); continue; }

        console.log(`\n${host} ${page}: ${plan.edits.length} link(s)`);
        for (const e of plan.edits) {
            const line = current.json.content.slice(0, e.start).split('\n').length;
            console.log(`  line ${line}: ${e.from}  ->  ${e.to}`);
        }
        changed += plan.edits.length;

        if (!apply) continue;
        const saved = await api(host, 'POST', path, {
            revision: current.json.revision,
            text: plan.next,
            comment,
            minorEdit: false,
        });
        if (saved.ok) console.log(`  saved r${current.json.revision} -> r${current.json.revision + 1}`);
        else failed.push(`${host} ${page}: save ${saved.status} ${saved.text.slice(0, 120)}`);
    }

    console.log(`\n${changed} link(s) ${apply ? 'rewritten' : 'to rewrite'}`);
    if (failed.length) { console.log(`\nfailed: ${failed.length}`); failed.forEach(f => console.log('  ' + f)); }
    if (!apply) console.log('Report only. Pass --apply --comment="..." to rewrite.');
    return failed.length ? 1 : 0;
}

// Importing this file must not start rewriting pages -- the test only wants planFor.
if (process.argv[1] && import.meta.url.endsWith(process.argv[1].replace(/\\/g, '/').split('/').pop()))
    process.exit(await main());
