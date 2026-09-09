// Pages whose names differ only by case, settled against English Wikipedia.
//
//   node scripts/case-duplicate-pages.mjs                          report only, changes nothing
//   node scripts/case-duplicate-pages.mjs --apply --comment="..."  write
//
// Needs AHAWIKI_API_KEY. Never write a key into this repository.
//
// `Page.name` is `utf8mb4_bin`, so `Css` and `CSS` are two pages and both can hold content. Most
// of the sixteen groups on aha00a.com were already settled -- one page holds the text and the
// other holds `#!redirect` -- so the work is the ones pointing the wrong way, and the ones where
// neither is a redirect.
//
// **Wikipedia was asked, not remembered.** Two traps in its API: it upper-cases the first letter
// of every title, so `macOS` comes back `MacOS` and `sbt` comes back `Sbt (software)` -- the
// article's DISPLAYTITLE is the real answer. And a short name is often a disambiguation page,
// which means Wikipedia has no opinion on it. That is what decided `ToDo`, below.
//
// **This swaps content, it does not rename.** The rename endpoint moves every revision and leaves
// a redirect, which is better, but it refuses when the target name is taken -- and it is taken in
// every case here. Clearing the way means DELETE, which is `DELETE FROM Page WHERE name = ?`:
// every revision, gone. `CSS` turned out to hold 26 revisions going back to 2008-09-23, older than
// `Css` itself -- it was the original page and was pointed at `Css` in 2016. Deleting it to make
// room would have destroyed the older history of the two. Writing both pages instead keeps every
// revision of both names; the cost is that the content's history stays under the old name.

const apiKey = process.env.AHAWIKI_API_KEY;
const apply = process.argv.includes('--apply');
const comment = (process.argv.find(a => a.startsWith('--comment=')) || '').slice('--comment='.length);
const host = 'aha00a.com';

// What English Wikipedia calls each, checked 2026-09-09 through its API, with the display title
// and the disambiguation flag. `article` means Wikipedia has a page under that exact name.
//
//   HTML CSS XML HTTP Git Firefox macOS PHP ASP.NET FFmpeg ETPFEST   article
//   sbt -> "sbt (software)", Less -> "Less (style sheet language)"   article, displayed as written
//   SNMP -> "Simple Network Management Protocol"                     article, SNMP redirects to it
//   AWS  -> "Amazon Web Services"                                    article, AWS redirects to it
//   JSP, ASP, Todo                                                   DISAMBIGUATION -- no opinion
//   SIPp                                                             no article
//
// So HTML, HTTP, XML, Git, Firefox, macOS, sbt, Less, SIPp, ETPFEST and Aha00a already hold their
// content under the right name and are not touched. `ToDo`/`TODO` is left alone deliberately:
// Wikipedia's `Todo` is a disambiguation page, so it does not answer the question, and inventing
// an answer is not what was asked for.
const Moves = [
    {
        from: 'Css', to: 'CSS',
        why: 'Wikipedia titles the article CSS',
        // The heading names the page, so it moves with it.
        heading: [/^= Css$/m, '= CSS'],
    },
    {
        from: 'Aws', to: 'AWS',
        // Wikipedia's article is "Amazon Web Services" and AWS redirects to it, so neither name
        // here matches the title; what Wikipedia never writes is "Aws". The family of eighteen
        // children moved to the `AWS ` prefix first, by rename -- see wikipedia-case-renames.mjs.
        // This page could not, because `AWS` was already a redirect and rename refuses a taken
        // name. Its 56 revisions therefore stay readable under `Aws`.
        why: 'Wikipedia writes the acronym AWS; the family of children moved first',
        heading: [/^= Aws$/m, '= AWS'],
    },
];

// A page that is only a pointer, written as prose rather than as a redirect. `see [wiki:JSP]`
// renders as a sentence with a link in it: the reader lands on a near-empty page and has to click
// again, and PageList shows it as a page with content. `#!redirect` answers 303 instead.
const Pointers = [
    {page: 'Jsp', to: 'JSP', was: 'see [wiki:JSP]'},
    {page: 'Snmp', to: 'SNMP', was: 'see [wiki:SNMP]'},
];

async function api(method, path, body) {
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

const read = name => api('GET', `/api/v1/page/${encodeURIComponent(name)}`);

async function write(name, text, revision) {
    return api('POST', `/api/v1/page/${encodeURIComponent(name)}`,
        {revision, text, comment, minorEdit: false});
}

/** The heading names the page; a move that leaves it behind says the old name in a new place. */
export function retitle(content, [pattern, replacement]) {
    if (!pattern.test(content)) return {ok: false, reason: `no heading matching ${pattern}`};
    return {ok: true, content: content.replace(pattern, replacement)};
}

export const redirectTo = name => `#!redirect ${name}\n`;

const failures = [];

async function moveContent({from, to, why, heading}) {
    const source = await read(from);
    const target = await read(to);
    if (!source.ok || !source.json) { failures.push(`${from}: read ${source.status}`); return; }
    if (!target.ok || !target.json) { failures.push(`${to}: read ${target.status}`); return; }

    // Already done: the old name points at the new one and the new one holds the text. Say so
    // rather than reporting the guard below as a failure -- this script stays in the repository
    // as the record of what was changed, so running it again has to be safe and quiet.
    if (source.json.content.trim() === redirectTo(to).trim()) {
        console.log(`\n${from} -> ${to}: already done`);
        return;
    }

    // Refuse to overwrite anything but a redirect. If the target grew content since this was
    // planned, the two need merging by a person, not clobbering by a script.
    if (!/^#!redirect\s/.test(target.json.content.trim())) {
        failures.push(`${to}: holds content, not a redirect -- not overwriting`);
        return;
    }

    const retitled = retitle(source.json.content, heading);
    if (!retitled.ok) { failures.push(`${from}: ${retitled.reason}`); return; }

    console.log(`\n${from} -> ${to}  (${why})`);
    console.log(`  ${to}   r${target.json.revision}  "${target.json.content.trim()}"  ->  the content, ${retitled.content.length} bytes`);
    console.log(`  ${from}  r${source.json.revision}  ${source.json.content.length} bytes  ->  "${redirectTo(to).trim()}"`);
    if (!apply) return;

    const wroteTarget = await write(to, retitled.content, target.json.revision);
    if (!wroteTarget.ok) { failures.push(`${to}: save ${wroteTarget.status} ${wroteTarget.text.slice(0, 120)}`); return; }
    console.log(`  ${to} saved r${target.json.revision} -> r${target.json.revision + 1}`);

    const wroteSource = await write(from, redirectTo(to), source.json.revision);
    if (!wroteSource.ok) { failures.push(`${from}: save ${wroteSource.status} ${wroteSource.text.slice(0, 120)}`); return; }
    console.log(`  ${from} saved r${source.json.revision} -> r${source.json.revision + 1}`);
}

async function makeRedirect({page, to, was}) {
    const current = await read(page);
    if (!current.ok || !current.json) { failures.push(`${page}: read ${current.status}`); return; }
    if (current.json.content.trim() === redirectTo(to).trim()) {
        console.log(`\n${page}: already done`);
        return;
    }
    if (current.json.content.trim() !== was) {
        failures.push(`${page}: says ${JSON.stringify(current.json.content.trim())}, expected ${JSON.stringify(was)}`);
        return;
    }
    console.log(`\n${page}  r${current.json.revision}  "${was}"  ->  "${redirectTo(to).trim()}"`);
    if (!apply) return;
    const saved = await write(page, redirectTo(to), current.json.revision);
    if (saved.ok) console.log(`  saved r${current.json.revision} -> r${current.json.revision + 1}`);
    else failures.push(`${page}: save ${saved.status} ${saved.text.slice(0, 120)}`);
}

async function main() {
    if (!apiKey) { console.error('AHAWIKI_API_KEY is not set.'); return 1; }
    if (apply && !comment) { console.error('--apply needs --comment="..."'); return 1; }

    for (const move of Moves) await moveContent(move);
    for (const pointer of Pointers) await makeRedirect(pointer);

    if (failures.length) {
        console.log(`\nfailed: ${failures.length}`);
        failures.forEach(f => console.log(`  ${f}`));
    }
    if (!apply) console.log('\nReport only. Pass --apply --comment="..." to write.');
    return failures.length ? 1 : 0;
}

// Importing this file must not start writing pages -- the test only wants the pure helpers.
if (process.argv[1] && import.meta.url.endsWith(process.argv[1].replace(/\\/g, '/').split('/').pop()))
    process.exit(await main());
