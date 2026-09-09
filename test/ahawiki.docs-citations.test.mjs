// The wiki pages cite code by name — classes, methods, files. A rename that does not reach
// the page leaves a document describing a codebase that no longer exists, which this
// repository treats as worse than no document at all. This finds those by checking that every
// cited name still appears somewhere in the tracked source.
//
// It reads names, not structure: it cannot tell that a method moved to another class, only
// that nothing is called that any more. That is the failure a rename actually produces.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { execFileSync } from 'node:child_process';
import { docsGitPath, manifestFileName, rootDir } from '../scripts/lib/ahawiki.net.mjs';

// Citations that name something real but outside this repository. Each needs a reason, so
// that the list stays a set of decisions rather than a place to silence failures.
const namedOutsideTheRepository = new Map([
    ['redis.conf', "Redis's own configuration file on the host, not part of this repository"],
    ['wiki.aha00a.com.conf', 'a per-environment Play config deployed to the server; it carries secrets and is not committed'],
]);

// Deliberately narrow. Prose, SQL fragments and config keys also live in backticks, and a
// shape that admits them would report noise until someone stopped reading the output.
//
// Config keys were surveyed on 2026-09-09 to see whether that was still true. A dotted
// lowercase name matched 35 things across the pages, and most were prose the shape could not
// tell from a key: `console.log`, `target.length`, `request.host`, `chat.id`. Of the ones the
// source did not hold, every real citation was a value a page names as legacy -- AccessControl
// quotes `permission.default.read` twice to say permission no longer reads it. So the check
// would report prose and history, and nothing that was actually stale.
const citationShapes = [
    {name: 'file', re: /^[A-Za-z0-9_.-]+\.(scala|jsx?|mjs|less|html|sql|conf)$/},
    {name: 'path', re: /^[A-Za-z0-9_./-]+\/[A-Za-z0-9_.-]+\.(scala|jsx?|mjs|less|html|sql|conf)$/},
    {name: 'member', re: /^[A-Z][A-Za-z0-9]*(\.[A-Za-z][A-Za-z0-9]*){1,3}$/},
];

const sourceExtensions = /\.(scala|java|jsx?|mjs|html|less|css|conf|sql|json|md|sh|xml|sbt)$/;

function trackedSourceFiles() {
    return execFileSync('git', ['ls-files'], {cwd: rootDir, encoding: 'utf8', maxBuffer: 64 * 1024 * 1024})
        .split('\n').map(line => line.trim()).filter(Boolean)
        .filter(file => !file.startsWith(`${docsGitPath}/`))
        .filter(file => sourceExtensions.test(file) || file === 'conf/routes');
}

function buildCorpus(files) {
    const contents = files.map(file => {
        try { return fs.readFileSync(path.join(rootDir, file), 'utf8'); } catch { return ''; }
    });
    // The paths themselves are part of the corpus, so a citation of a file by path matches
    // even when nothing quotes that path in its own text.
    return `${contents.join('\n')}\n${files.join('\n')}`;
}

/**
 * Only the final name is checked.
 *
 * `Page.dateTime` is a column the Scala spells as `dateTime`, and `WikiPermission.isWritable`
 * is called on an instance, so requiring the dotted pair verbatim flags nearly every true
 * citation. A missing final name is the thing that actually means something.
 */
function citationNeedle(raw, shape) {
    return shape === 'member' ? raw.split('.').pop() : raw;
}

test('every code name the wiki pages cite still exists in the source', () => {
    const corpus = buildCorpus(trackedSourceFiles());
    const docsDirectory = path.join(rootDir, ...docsGitPath.split('/'));
    const missing = [];

    for (const page of fs.readdirSync(docsDirectory)) {
        if (page === manifestFileName) continue;
        const text = fs.readFileSync(path.join(docsDirectory, page), 'utf8');
        const seen = new Set();

        for (const match of text.matchAll(/`([^`\n]{2,80})`/g)) {
            const raw = match[1].trim();
            if (seen.has(raw) || namedOutsideTheRepository.has(raw)) continue;
            seen.add(raw);

            const shape = citationShapes.find(candidate => candidate.re.test(raw));
            if (!shape) continue;
            if (!corpus.includes(citationNeedle(raw, shape.name))) missing.push(`${page}: \`${raw}\``);
        }
    }

    assert.deepEqual(missing, [], `Wiki pages cite names that no longer exist in the source:\n  ${missing.join('\n  ')}\n\n` +
        'Update the page, or — if the name is real but lives outside this repository — add it to ' +
        'namedOutsideTheRepository in this file with the reason.');
});

test('the outside-the-repository list stays a set of decisions', () => {
    for (const [name, reason] of namedOutsideTheRepository) {
        assert.ok(reason && reason.length > 20, `${name} needs a reason saying what it actually is`);
    }
});

// The pages also quote endpoints — `GET /api/Admin/Site/:seq/Admins`. The citation test above
// does not see those: its shapes are names, and a verb and a path are neither. conf/routes is
// the only thing that decides whether one exists, so a renamed route leaves the page describing
// something that answers 404, and nothing says so.

// A route quoted for what it once was, rather than for what it answers now. Same rule as the
// list above: each needs a reason, so this stays decisions rather than a way to silence a failure.
const routesQuotedAsHistory = new Map([
    ['POST /account/nickname/request',
        'TODO-User-Nickname-Change names the path it planned, in the paragraph explaining why the built one differs'],
]);

/** Turn a route into what it matches. A page quotes a value where the route holds a parameter. */
function routePattern(routePath) {
    const escape = segment => segment.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
    return new RegExp('^' + routePath.split('/').map(segment => {
        if (segment.startsWith('*')) return '.+';        // *nameEncoded -- swallows slashes
        if (segment.startsWith(':')) return '[^/]+';     // :seq
        return escape(segment);
    }).join('/') + '$');
}

function definedRoutes() {
    return fs.readFileSync(path.join(rootDir, 'conf', 'routes'), 'utf8').split('\n')
        .map(line => line.match(/^(GET|POST|PUT|DELETE|PATCH|HEAD)\s+(\S+)/))
        .filter(Boolean)
        .map(match => ({verb: match[1], re: routePattern(match[2])}));
}

test('every API route the wiki pages quote is still in conf/routes', () => {
    const routes = definedRoutes();
    const docsDirectory = path.join(rootDir, ...docsGitPath.split('/'));
    const missing = [];

    for (const page of fs.readdirSync(docsDirectory)) {
        if (page === manifestFileName) continue;
        const text = fs.readFileSync(path.join(docsDirectory, page), 'utf8');
        const seen = new Set();

        for (const match of text.matchAll(/`(GET|POST|PUT|DELETE|PATCH)\s+(\/[^`\n]+)`/g)) {
            const cited = `${match[1]} ${match[2].trim()}`;
            if (seen.has(cited) || routesQuotedAsHistory.has(cited)) continue;
            seen.add(cited);

            // A query string is the caller's, not the route's; conf/routes spells defaults instead.
            const citedPath = match[2].trim().replace(/\?.*$/, '').replace(/\/+$/, '') || '/';
            if (!routes.some(route => route.verb === match[1] && route.re.test(citedPath)))
                missing.push(`${page}: \`${cited}\``);
        }
    }

    assert.deepEqual(missing, [], `Wiki pages quote routes that conf/routes no longer defines:\n  ${missing.join('\n  ')}\n\n` +
        'Update the page, or — if the page quotes the route as history rather than as what it ' +
        'answers now — add it to routesQuotedAsHistory in this file with the reason.');
});

test('the route check can tell a real citation from a wrong one', () => {
    // Without this, the test above passes just as well when nothing matches at all.
    const routes = definedRoutes();
    assert.ok(routes.length > 50, 'conf/routes should parse into routes, not an empty list');

    const matches = (verb, citedPath) => routes.some(route => route.verb === verb && route.re.test(citedPath));

    assert.ok(matches('GET', '/api/Admin/Site/999/Admins'), 'a value where the route holds :seq');
    assert.ok(matches('GET', '/w/FrontPage'), 'a page name where the route holds *nameEncoded');
    assert.ok(!matches('POST', '/api/Admin/Site/999/Admins/1/2'), 'more segments than the route has');
    assert.ok(!matches('GET', '/api/Admin/Site'), 'a prefix of a route is not the route');

    for (const [route, reason] of routesQuotedAsHistory) {
        assert.ok(reason && reason.length > 20, `${route} needs a reason saying why it is quoted`);
    }
});
