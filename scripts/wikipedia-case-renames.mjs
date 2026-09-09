// Moves pages to the name English Wikipedia uses, where that is a rename rather than a merge.
//
//   node scripts/wikipedia-case-renames.mjs <plan>                          report only
//   node scripts/wikipedia-case-renames.mjs <plan> --apply --comment="..."  do it
//
// <plan> is `acronyms` or `aws`. Needs AHAWIKI_API_KEY; never write a key into this repository.
//
// `POST /api/v1/rename` is the right tool when the target name is free: it moves every revision,
// leaves `#!redirect <new>` at the old name, and deletes nothing. It refuses when the target
// exists, and that refusal is the whole shape of this script -- the pages where the target was
// taken are handled by scripts/case-duplicate-pages.mjs instead, by writing both pages, because
// freeing a name means DELETE FROM Page and that takes every revision of it.
//
// Two things a rename leaves behind, both of which this fixes:
//
//   * A redirect that pointed at the old name now points at a redirect. `AwsCodeCommit` said
//     `#!redirect Aws CodeCommit`; rename that target and the reader gets a redirect to a
//     redirect, which the renderer does not follow. Retarget those.
//   * A page that lists its children by name is now listing redirects. The parent `AWS` page is
//     an index of eighteen `["Aws ..."]` links, so it gets rewritten to the new names.
//
// Links from elsewhere are deliberately NOT rewritten. A redirect is what a wiki leaves behind on
// purpose, and every old link keeps working through it.

import { maskUnlinkable } from './lib/ahamark.mjs';

const apiKey = process.env.AHAWIKI_API_KEY;
const apply = process.argv.includes('--apply');
const comment = (process.argv.find(a => a.startsWith('--comment=')) || '').slice('--comment='.length);
const planName = process.argv[2];
const host = 'aha00a.com';

// English Wikipedia, checked through its API on 2026-09-09 with displaytitle and the
// disambiguation flag: PHP and ASP.NET are articles under exactly these names. `ASP` is a
// disambiguation, so Wikipedia does not title an article that -- but it never writes `Asp` for
// the technology either, and the wiki's own page is typed ComputerLanguage with Microsoft as
// designer. The acronym is the name; the lower-case form is a snake.
const Plans = {
    acronyms: {
        renames: [
            {from: 'Php', to: 'PHP'},
            {from: 'Asp.Net', to: 'ASP.NET'},
            {from: 'Asp', to: 'ASP'},
        ],
        retarget: [],
        // The heading names the page, so it moves with it. `Asp.Net` opens `= [Asp].Net`, which
        // is a link to the other page inside the heading, not the plain title the others have.
        headings: [
            {page: 'PHP', from: /^= Php$/m, to: '= PHP'},
            {page: 'ASP', from: /^= Asp$/m, to: '= ASP'},
            {page: 'ASP.NET', from: /^= \[Asp\]\.Net$/m, to: '= [ASP].NET'},
        ],
        indexes: [],
        alsoInIndex: [],
        // `[Php|PHP]` was written by fix-case-miss-links.mjs when the page was called Php and the
        // link said PHP: the pipe kept the reader seeing PHP while the link reached the page. The
        // page is called PHP now, so the pipe says nothing -- collapse it back to `[PHP]`.
        simplifyIn: ['Aha00aResume', 'Computer Language', '끄적끄적201201'],
    },
    aws: {
        // Sixteen children spelled `Aws <thing>`, plus the last CamelCase page. Every target was
        // confirmed free before this list was written.
        renames: [
            'Certificate Manager', 'CloudFront', 'CodeCommit', 'EC2', 'EC2 Replace Ssh Key', 'EFS',
            'ElastiCache', 'Elastic Beanstalk', 'Elastic Kubernetes Service',
            'Elastic Load Balancing', 'Find EC2 Public IP in Auto Scaling Group', 'Lambda',
            'MemoryDB for Redis', 'RDS', 'S3', 'VPC',
        ].map(thing => ({from: `Aws ${thing}`, to: `AWS ${thing}`}))
            // AwsCli is the last page still spelled the CamelCase way; its siblings AwsCodeCommit
            // and AwsEc2 were moved to spaced names years ago and left as redirects. `AWS CLI` is
            // both the spaced convention and what Amazon calls it.
            .concat([{from: 'AwsCli', to: 'AWS CLI'}]),
        // Redirects that pointed at a name this plan moves.
        retarget: [
            {page: 'AwsCodeCommit', to: 'AWS CodeCommit'},
            {page: 'AwsEc2', to: 'AWS EC2'},
        ],
        headings: [],
        // The parent page is an index of eighteen ["Aws ..."] links.
        indexes: ['Aws', 'AWS'],
        simplifyIn: [],
        // Four entries in that index are red links -- pages nobody has written yet. There is
        // nothing to rename, so the rename pass never sees them, and left alone the next person
        // to click one writes a page under the name the rest just stopped using.
        alsoInIndex: [
            {from: 'Aws Elastic Container Registry', to: 'AWS Elastic Container Registry'},
            {from: 'Aws Elastic Container Service', to: 'AWS Elastic Container Service'},
            {from: 'Aws IAM', to: 'AWS IAM'},
            {from: 'Aws Route 53', to: 'AWS Route 53'},
        ],
    },
};

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
const write = (name, text, revision) =>
    api('POST', `/api/v1/page/${encodeURIComponent(name)}`, {revision, text, comment, minorEdit: false});

/**
 * Collapse `[old|alias]` to `[alias]` where the rename made the alias the page's own name.
 *
 * Only that exact case. A pipe whose alias is anything else is what its author wanted the link
 * called, and `[FFmpeg|ffmpeg]` still earns its keep -- that page was never renamed.
 */
export function simplifyAliases(content, moved) {
    const masked = maskUnlinkable(content);
    const edits = [];
    for (const m of masked.matchAll(/\[(?![?"])((?:(?!:\/\/)[^\]|])+)\|([^\]]+)\]/g)) {
        const [, target, alias] = m;
        if (moved.get(target.trim()) !== alias.trim()) continue;
        edits.push({start: m.index, end: m.index + m[0].length, to: `[${alias.trim()}]`});
    }
    if (!edits.length) return {changed: false, content};
    let next = content;
    for (const e of [...edits].reverse()) next = next.slice(0, e.start) + e.to + next.slice(e.end);
    return {changed: true, content: next, count: edits.length};
}

/** Rewrite every link naming a page this plan moves, in text the renderer reads as a link. */
export function rewriteLinks(content, moved) {
    const masked = maskUnlinkable(content);

    // Only the two shapes that name a page and nothing else: ["A Name"] and [Name].
    const edits = [];
    for (const m of masked.matchAll(/\["([^\]"]+)"\]|\[([^\]\s|]+)\]/g)) {
        const written = m[1] ?? m[2];
        const target = written.startsWith('wiki:') ? written.slice(5) : written;
        const to = moved.get(target);
        if (!to) continue;
        edits.push({start: m.index, end: m.index + m[0].length, to: /\s/.test(to) ? `["${to}"]` : `[${to}]`});
    }
    if (!edits.length) return {changed: false, content};
    let next = content;
    for (const e of [...edits].reverse()) next = next.slice(0, e.start) + e.to + next.slice(e.end);
    return {changed: true, content: next, count: edits.length};
}

const failures = [];

async function renameOne({from, to}) {
    const source = await read(from);
    if (!source.ok || !source.json) { failures.push(`${from}: read ${source.status}`); return false; }
    const target = await read(to);

    // Already done: the old name is the redirect the rename leaves behind, and the new name is
    // there. This script stays in the repository as the record of what was changed, so running it
    // again has to be safe and quiet -- and it still counts as moved, because the passes after
    // this one work from that list.
    if (target.status !== 404 && source.json.content.trim() === `#!redirect ${to}`) {
        console.log(`  ${from} -> ${to}: already done`);
        return true;
    }
    if (target.status !== 404) {
        failures.push(`${to}: already exists (r${target.json?.revision}) -- rename would be refused`);
        return false;
    }
    console.log(`  ${from}  r${source.json.revision}  ->  ${to}`);
    if (!apply) return true;
    const done = await api('POST', '/api/v1/rename', {name: from, newName: to, revision: source.json.revision, comment});
    if (!done.ok) { failures.push(`${from} -> ${to}: ${done.status} ${done.text.slice(0, 140)}`); return false; }
    return true;
}

async function editPage(name, transform, describe) {
    const current = await read(name);
    // In report mode the renames have not run, so a page this plan is about to create is not
    // there yet. That is the dry run working, not a failure.
    if (!apply && current.status === 404) { console.log(`  ${name}: ${describe} (after the rename)`); return; }
    if (!current.ok || !current.json) { failures.push(`${name}: read ${current.status}`); return; }
    const next = transform(current.json.content);
    if (next === null || next === current.json.content) { console.log(`  ${name}: nothing to change`); return; }
    console.log(`  ${name}  r${current.json.revision}  ${describe}`);
    if (!apply) return;
    const saved = await write(name, next, current.json.revision);
    if (!saved.ok) failures.push(`${name}: save ${saved.status} ${saved.text.slice(0, 140)}`);
}

async function main() {
    if (!apiKey) { console.error('AHAWIKI_API_KEY is not set.'); return 1; }
    if (apply && !comment) { console.error('--apply needs --comment="..."'); return 1; }
    const plan = Plans[planName];
    if (!plan) { console.error(`usage: node scripts/wikipedia-case-renames.mjs <${Object.keys(Plans).join('|')}> [--apply --comment="..."]`); return 2; }

    console.log(`=== rename: ${plan.renames.length} ===`);
    const moved = new Map();
    for (const rename of plan.renames) if (await renameOne(rename)) moved.set(rename.from, rename.to);

    if (plan.headings.length) {
        console.log(`\n=== heading follows the page: ${plan.headings.length} ===`);
        for (const h of plan.headings)
            await editPage(h.page, text => (h.from.test(text) ? text.replace(h.from, h.to) : null), `heading -> ${h.to}`);
    }

    if (plan.retarget.length) {
        console.log(`\n=== redirect that would now point at a redirect: ${plan.retarget.length} ===`);
        for (const r of plan.retarget)
            await editPage(r.page, text => (/^#!redirect\s/.test(text.trim()) ? `#!redirect ${r.to}\n` : null),
                `-> #!redirect ${r.to}`);
    }

    // A page that lists the pages just moved is listing redirects now. Only the index named by
    // the plan -- links from elsewhere keep working through the redirect, which is what a wiki
    // leaves one behind for.
    if ((plan.simplifyIn || []).length && moved.size) {
        console.log(`\n=== a pipe the rename made pointless: ${plan.simplifyIn.length} page(s) ===`);
        for (const name of plan.simplifyIn)
            await editPage(name, text => {
                const result = simplifyAliases(text, moved);
                return result.changed ? result.content : null;
            }, 'collapse [old|Alias] to [Alias]');
    }

    for (const also of plan.alsoInIndex || []) moved.set(also.from, also.to);
    if (plan.indexes.length && moved.size) {
        console.log(`\n=== index page listing the moved names: ${plan.indexes.length} ===`);
        for (const name of plan.indexes)
            await editPage(name, text => {
                const result = rewriteLinks(text, moved);
                return result.changed ? result.content : null;
            }, 'links -> the new names');
    }

    if (failures.length) {
        console.log(`\nfailed: ${failures.length}`);
        failures.forEach(f => console.log(`  ${f}`));
    }
    if (!apply) console.log('\nReport only. Pass --apply --comment="..." to do it.');
    return failures.length ? 1 : 0;
}

// Importing this file must not start renaming pages -- the test only wants rewriteLinks.
if (process.argv[1] && import.meta.url.endsWith(process.argv[1].replace(/\\/g, '/').split('/').pop()))
    process.exit(await main());
