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
