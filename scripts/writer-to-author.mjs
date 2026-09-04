// Rewrites the hand-written `writer` property in Schema blocks to `author`.
//
// schema.org has no `writer`, and no screenwriter property either, so 137 pages were carrying a
// line that reads like structured data and is not. The converter never produced it -- `Writer`
// was unmapped and passed through capitalised, and was lowercased by hand. The mapping table now
// sends those labels to `author`; this fixes what was already written.
//
//   node scripts/writer-to-author.mjs                          report only, changes nothing
//   node scripts/writer-to-author.mjs --apply --comment="..."  rewrite the clean pages
//
// Needs AHAWIKI_API_KEY. Never write a key into this repository.
//
// A page that already has its own `author` line is reported and skipped: `author` may hold the
// novelist while `writer` holds the screenwriter, and merging them would claim they are the same
// person. Those are for a human to decide.

// The pages to visit: a tsv of "site<TAB>page", from CalculatedSchemaOrg where prop = 'writer'.
const PageList = process.env.WRITER_PAGES_FILE;

const apiKey = process.env.AHAWIKI_API_KEY;
const apply = process.argv.includes('--apply');
const comment = (process.argv.find(a => a.startsWith('--comment=')) || '').slice('--comment='.length);

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

// Only a line that starts a field with the bare property name; a "# Writer" comment is left alone.
const isWriter = line => line.startsWith('writer\t');
const isAuthor = line => line.startsWith('author\t');

// A page holds several Schema blocks -- 구르미 그린 달빛 types the novel as Book and the drama as
// TVSeries -- so the two properties clash only inside one block. Checking the whole page called
// both of those a conflict when the author was the novelist and the writer the screenwriter.
export function planFor(content) {
    const lines = content.split('\n');
    const blockOf = [];
    // The counter only ever goes up. Resetting it when a block closes gave the next block the
    // same id as the first, so two properties in different blocks looked like one clash.
    let opened = 0;
    let inside = false;
    for (const line of lines) {
        if (/^\[\[\[#!\s*Schema\b/i.test(line)) { opened += 1; inside = true; }
        else if (line.startsWith(']]]')) inside = false;
        blockOf.push(inside ? opened : null);
    }

    const writers = lines.map((l, i) => isWriter(l) ? i : -1).filter(i => i >= 0);
    if (!writers.length) return {kind: 'no-writer-line'};

    const authorBlocks = new Set(lines.map((l, i) => isAuthor(l) ? blockOf[i] : null).filter(b => b !== null));
    const clashing = writers.filter(i => authorBlocks.has(blockOf[i]));
    if (clashing.length) return {kind: 'conflict', hits: clashing.length};

    const next = lines.map((l, i) => writers.includes(i) ? 'author\t' + l.slice('writer\t'.length) : l);
    return {kind: 'rewrite', hits: writers.length, next: next.join('\n')};
}

async function main() {
    if (!apiKey) { console.error('AHAWIKI_API_KEY is not set.'); return 1; }
    if (apply && !comment) { console.error('--apply needs --comment="..."'); return 1; }
    if (!PageList) { console.error('WRITER_PAGES_FILE is not set (tsv of "site<TAB>page").'); return 1; }

    const fs = await import('node:fs');
    const siteHost = {'1': 'aha00a.com', '6': 'fuerinha.ahawiki.net'};
    const rows = fs.readFileSync(PageList, 'utf8').trim().split('\n')
        .map(l => l.split('\t'))
        .map(([site, page]) => ({host: siteHost[site.trim()], page: page.trim()}))
        .filter(r => r.host && r.page);

    const buckets = {rewrite: [], conflict: [], 'no-writer-line': [], failed: []};

    for (const {host, page} of rows) {
        const path = `/api/v1/page/${encodeURIComponent(page)}`;
        const current = await api(host, 'GET', path);
        if (!current.ok || !current.json) { buckets.failed.push(`${host} ${page}: read ${current.status}`); continue; }

        const plan = planFor(current.json.content);
        if (plan.kind !== 'rewrite') { buckets[plan.kind].push(`${host} ${page}`); continue; }

        if (!apply) { buckets.rewrite.push(`${host} ${page} (${plan.hits} line(s))`); continue; }

        const saved = await api(host, 'POST', path, {
            revision: current.json.revision,
            text: plan.next,
            comment,
            minorEdit: false,
        });
        if (saved.ok) buckets.rewrite.push(`OK   ${host} ${page} r${current.json.revision} -> r${current.json.revision + 1}`);
        else buckets.failed.push(`${host} ${page}: save ${saved.status} ${saved.text.slice(0, 120)}`);
    }

    for (const [name, list] of Object.entries(buckets)) {
        console.log(`\n${name}: ${list.length}`);
        list.forEach(l => console.log('  ' + l));
    }
    if (!apply) console.log('\nReport only. Pass --apply --comment="..." to rewrite.');
    return buckets.failed.length ? 1 : 0;
}

// Importing this file must not start rewriting pages -- the test below only wants planFor.
if (process.argv[1] && import.meta.url.endsWith(process.argv[1].replace(/\\/g, '/').split('/').pop()))
    process.exit(await main());
