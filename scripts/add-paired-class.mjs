// Adds the paired class to a Schema block whose properties do not all belong to the classes it
// names. A library page typed SoftwareSourceCode carries applicationCategory, which belongs to
// SoftwareApplication; a program page typed SoftwareApplication carries codeRepository and
// programmingLanguage, which belong to SoftwareSourceCode. Both were right about the thing and
// wrong about the class: a library is source and a thing you use, so the block should say both.
// Since 5fb88dd9 the interpreter reads every class a block names, so this is one word per block.
//
//   node scripts/add-paired-class.mjs                          report only, changes nothing
//   node scripts/add-paired-class.mjs --apply --comment="..."  rewrite the pages it can fix
//
// Needs AHAWIKI_API_KEY and PAIRED_PAGES_FILE (a tsv of "site<TAB>page"). Never write a key into
// this repository.
//
// It only ever adds, and only the paired class, and only when that makes every property in the
// block fit. A block it cannot settle that way is reported and left alone.
import fs from 'node:fs';
import path from 'node:path';
import {rootDir} from './lib/ahawiki.net.mjs';

const Paired = {
    SoftwareApplication: 'SoftwareSourceCode',
    WebApplication: 'SoftwareSourceCode',
    MobileApplication: 'SoftwareSourceCode',
    VideoGame: 'SoftwareSourceCode',
    SoftwareSourceCode: 'SoftwareApplication',
};

const SiteHost = {'1': 'aha00a.com', '2': 'ahawiki.net', '6': 'fuerinha.ahawiki.net'};

// InterpreterSchema.imageKeys renders these as pictures whatever the class, so they are a display
// convention rather than a claim about the type. logo's domain is Organization, Brand, Product and
// the like -- it fits no software class -- and it sits on twelve library pages. Twelve pages held
// back for a picture would be the wrong trade, so it does not weigh on the decision.
const DisplayOnly = new Set(['image', 'logo']);

export function loadVocabulary() {
    const file = path.join(rootDir, 'public/schema.org/26.0/schemaorg-current-https.jsonld');
    const graph = JSON.parse(fs.readFileSync(file, 'utf8')).graph;
    const byId = new Map(graph.map(node => [node.id, node]));
    const ids = value => [].concat(value || []).map(v => v.id || v);
    const ancestors = (id, seen = new Set()) => {
        if (seen.has(id)) return seen;
        seen.add(id);
        ids((byId.get(id) || {}).subClassOf).forEach(parent => ancestors(parent, seen));
        return seen;
    };
    return {
        isProperty: id => (byId.get(id) || {}).type === 'Property',
        // A property with no domain is not constrained; treat it as fitting.
        fits: (property, classes) => {
            const domain = ids((byId.get(property) || {}).domainIncludes);
            if (!domain.length) return true;
            const reach = new Set();
            classes.forEach(cls => ancestors(cls).forEach(a => reach.add(a)));
            return domain.some(d => reach.has(d));
        },
    };
}

const headerPattern = /^(\[\[\[#!\s*Schema)((?:\s+[A-Za-z0-9_]+)*)\s*$/i;

/** Decide, block by block, what the page's headers should become. */
export function planFor(content, vocabulary) {
    const lines = content.split('\n');
    const blocks = [];
    let current = null;
    lines.forEach((line, i) => {
        const header = line.match(headerPattern);
        if (header) {
            current = {headerIndex: i, prefix: header[1], classes: header[2].trim().split(/\s+/).filter(Boolean), fields: []};
            blocks.push(current);
        } else if (line.startsWith(']]]')) {
            current = null;
        } else if (current && line.trim() && !line.startsWith('#')) {
            current.fields.push({key: line.split('\t')[0], line, index: i});
        }
    });
    if (!blocks.length) return {kind: 'no-schema-block'};

    const changes = [];
    const unresolved = [];
    const duplicates = [];
    for (const block of blocks) {
        if (!block.classes.length) continue;
        const known = block.fields.filter(f => vocabulary.isProperty(f.key) && !DisplayOnly.has(f.key));
        const misfits = known.filter(f => !vocabulary.fits(f.key, block.classes));
        if (!misfits.length) continue;

        const candidates = block.classes.map(c => Paired[c]).filter(c => c && !block.classes.includes(c));
        const widened = [...block.classes, ...candidates];
        const still = known.filter(f => !vocabulary.fits(f.key, widened));
        if (!candidates.length || still.length) {
            unresolved.push({classes: block.classes, misfits: (still.length ? still : misfits).map(f => f.key)});
            continue;
        }

        // A page may already devote a block to the class we would add -- Graphviz and Homebrew
        // separate the program from its codebase deliberately -- and the offending line is then a
        // copy of what that block already states, identical down to the value. Widening here would
        // make the page declare the same application twice. The line is what is wrong, not the
        // class, so say so and let a person delete it.
        const copied = misfits.filter(f => blocks.some(other =>
            other !== block &&
            candidates.some(c => other.classes.includes(c)) &&
            other.fields.some(g => g.line === f.line)));
        if (copied.length === misfits.length) {
            duplicates.push({classes: block.classes, siblingClasses: candidates, lines: copied.map(f => ({index: f.index, line: f.line}))});
            continue;
        }

        changes.push({headerIndex: block.headerIndex, from: block.classes, to: widened, because: misfits.map(f => f.key)});
    }

    if (duplicates.length) return {kind: 'duplicate-of-sibling', duplicates, changes};
    if (unresolved.length) return {kind: 'unresolved', unresolved, changes};
    if (!changes.length) return {kind: 'already-fits'};

    const next = [...lines];
    for (const change of changes)
        next[change.headerIndex] = `${lines[change.headerIndex].match(headerPattern)[1]} ${change.to.join(' ')}`;
    return {kind: 'rewrite', changes, next: next.join('\n')};
}

async function main() {
    const apiKey = process.env.AHAWIKI_API_KEY;
    const apply = process.argv.includes('--apply');
    const comment = (process.argv.find(a => a.startsWith('--comment=')) || '').slice('--comment='.length);
    const pageList = process.env.PAIRED_PAGES_FILE;

    if (!apiKey) { console.error('AHAWIKI_API_KEY is not set.'); return 1; }
    if (apply && !comment) { console.error('--apply needs --comment="..."'); return 1; }
    if (!pageList) { console.error('PAIRED_PAGES_FILE is not set (tsv of "site<TAB>page").'); return 1; }

    const vocabulary = loadVocabulary();
    const rows = fs.readFileSync(pageList, 'utf8').trim().split('\n')
        .map(l => l.split('\t'))
        .map(([site, page]) => ({host: SiteHost[site.trim()], page: page.trim()}))
        .filter(r => r.host && r.page);

    async function api(host, method, apiPath, body) {
        const response = await fetch(`https://${host}${apiPath}`, {
            method,
            headers: {authorization: `Bearer ${apiKey}`, ...(body ? {'content-type': 'application/json'} : {})},
            ...(body ? {body: JSON.stringify(body)} : {}),
        });
        const text = await response.text();
        let json = null;
        try { json = JSON.parse(text); } catch { /* not json */ }
        return {ok: response.ok, status: response.status, text, json};
    }

    const buckets = {rewrite: [], 'duplicate-of-sibling': [], unresolved: [], 'already-fits': [], 'no-schema-block': [], failed: []};
    for (const {host, page} of rows) {
        const apiPath = `/api/v1/page/${encodeURIComponent(page)}`;
        const current = await api(host, 'GET', apiPath);
        if (!current.ok || !current.json) { buckets.failed.push(`${host} ${page}: read ${current.status}`); continue; }

        const plan = planFor(current.json.content, vocabulary);
        if (plan.kind === 'duplicate-of-sibling') {
            buckets['duplicate-of-sibling'].push(`${host} ${page}: a [${plan.duplicates[0].siblingClasses.join(' ')}] block already states ` +
                plan.duplicates.flatMap(d => d.lines.map(l => `line ${l.index}: ${l.line.replace(/\t/g, ' -> ')}`)).join('; '));
            continue;
        }
        if (plan.kind === 'unresolved') {
            buckets.unresolved.push(`${host} ${page}: ` + plan.unresolved.map(u => `[${u.classes.join(' ')}] cannot place ${u.misfits.join(',')}`).join('; '));
            continue;
        }
        if (plan.kind !== 'rewrite') { buckets[plan.kind].push(`${host} ${page}`); continue; }

        const summary = plan.changes.map(c => `[${c.from.join(' ')}] -> [${c.to.join(' ')}] for ${c.because.join(',')}`).join('; ');
        if (!apply) { buckets.rewrite.push(`${host} ${page}: ${summary}`); continue; }

        const saved = await api(host, 'POST', apiPath, {revision: current.json.revision, text: plan.next, comment, minorEdit: false});
        if (saved.ok) buckets.rewrite.push(`OK   ${host} ${page} r${current.json.revision} -> r${current.json.revision + 1}: ${summary}`);
        else buckets.failed.push(`${host} ${page}: save ${saved.status} ${saved.text.slice(0, 120)}`);
    }

    for (const [name, list] of Object.entries(buckets)) {
        console.log(`\n${name}: ${list.length}`);
        list.forEach(l => console.log('  ' + l));
    }
    if (!apply) console.log('\nReport only. Pass --apply --comment="..." to rewrite.');
    return buckets.failed.length ? 1 : 0;
}

if (process.argv[1] && import.meta.url.endsWith(process.argv[1].replace(/\\/g, '/').split('/').pop()))
    process.exit(await main());
