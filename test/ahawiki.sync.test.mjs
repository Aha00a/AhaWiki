// The wiki sync decides whether to overwrite a live page. Getting `diverged` wrong in the
// permissive direction silently destroys somebody's browser edit, and the trailing-newline
// comparison is the kind of thing that quietly starts reporting all 116 pages as drifted.
// Both run against this repository's real git history rather than a fixture, because the
// history is what the script actually reads.
import test from 'node:test';
import assert from 'node:assert/strict';
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import {
  classify,
  hashMatches,
  readCommittedPages,
  remoteIsOlderLocalVersion,
} from '../scripts/sync.ahawiki.net.mjs';
import { docsGitPath, fileNameCollisions, manifestFileName, rootDir } from '../scripts/lib/ahawiki.net.mjs';

const git = (args) => execFileSync('git', args, { cwd: rootDir, encoding: 'utf8', maxBuffer: 64 * 1024 * 1024 });
const remoteHash = (content) => `sha256:${createHash('sha256').update(content, 'utf8').digest('hex')}`;

// The wiki stores a trailing newline the local file does not have, so this is the spelling a
// page actually comes back as.
const asStoredOnWiki = (localContent) => `${localContent.replace(/\n+$/, '')}\n`;

const localPages = readCommittedPages();
const somePage = [...localPages.keys()].sort()[0];

/** A page with at least two distinct committed versions, so "older version" means something. */
function pageWithHistory() {
  for (const file of [...localPages.keys()].sort()) {
    const path = `${docsGitPath}/${file}`;
    const commits = git(['log', '--max-count=20', '--format=%H', '--', path]).split('\n').map((c) => c.trim()).filter(Boolean);
    const head = localPages.get(file);
    for (const commit of commits) {
      let older;
      try {
        older = git(['show', `${commit}:${path}`]);
      } catch {
        continue;
      }
      if (older !== head) return { file, head, older };
    }
  }
  return null;
}

test('hashMatches accepts the trailing newline the wiki adds', () => {
  const local = '= Page\n\nbody';
  assert.equal(hashMatches(local, remoteHash(asStoredOnWiki(local))), true);
  assert.equal(hashMatches(local, remoteHash(local)), true);
});

test('hashMatches strips the sha256: prefix rather than comparing the whole string', () => {
  const local = '= Page\n';
  const bare = remoteHash(local).replace(/^sha256:/, '');
  assert.equal(hashMatches(local, bare), true);
});

test('hashMatches rejects different content, and a missing hash', () => {
  assert.equal(hashMatches('= A\n', remoteHash('= B\n')), false);
  assert.equal(hashMatches('= A\n', ''), false);
  assert.equal(hashMatches('= A\n', null), false);
  assert.equal(hashMatches('= A\n', undefined), false);
});

test('readCommittedPages returns the page copies without the download manifest', () => {
  assert.ok(localPages.size > 0, 'expected committed pages under docs/ahawiki.net/');
  assert.equal(localPages.has(manifestFileName), false);
  assert.ok(localPages.get(somePage).length > 0);
});

test('a page whose remote content matches the committed file is in sync', () => {
  const remote = [{ name: somePage, revision: 1, contentHash: remoteHash(asStoredOnWiki(localPages.get(somePage))) }];
  const { inSync, localAhead, diverged } = classify(remote, localPages);
  assert.deepEqual(inSync.map((entry) => entry.name), [somePage]);
  assert.equal(localAhead.length, 0);
  assert.equal(diverged.length, 0);
});

test('a page whose remote content is an older committed version is local-ahead', (t) => {
  const found = pageWithHistory();
  if (!found) return t.skip('no page has two distinct committed versions yet');

  assert.equal(remoteIsOlderLocalVersion(found.file, remoteHash(asStoredOnWiki(found.older))), true);

  const remote = [{ name: found.file, revision: 1, contentHash: remoteHash(asStoredOnWiki(found.older)) }];
  const { localAhead, diverged } = classify(remote, localPages);
  assert.deepEqual(localAhead.map((entry) => entry.name), [found.file]);
  assert.equal(diverged.length, 0);
});

test('a page whose remote content was never committed here is diverged, not local-ahead', () => {
  const neverCommitted = `${localPages.get(somePage)}\n\n== edited in the browser at some point\n`;
  assert.equal(remoteIsOlderLocalVersion(somePage, remoteHash(neverCommitted)), false);

  const remote = [{ name: somePage, revision: 9, contentHash: remoteHash(neverCommitted) }];
  const { localAhead, diverged } = classify(remote, localPages);
  assert.equal(localAhead.length, 0, 'a browser edit must never be classified as safe to overwrite');
  assert.deepEqual(diverged.map((entry) => entry.name), [somePage]);
});

test('classify separates pages that exist on only one side', () => {
  const { remoteOnly, localOnly } = classify(
    [{ name: 'No Such Page', revision: 1, contentHash: remoteHash('anything') }],
    localPages,
  );
  assert.deepEqual(remoteOnly, ['No Such Page']);
  assert.equal(localOnly.includes(somePage), true, 'every unclaimed local page is reported');
  assert.equal(localOnly.length, localPages.size);
});

// The wiki can hold two names that this filesystem cannot keep apart. The download empties
// docs/ahawiki.net/ and writes every page concurrently, so before this the two raced for one
// file, the loser vanished, and the manifest still listed both as written -- which one lost
// depended on timing. It happened for real on 2026-09-09, right after a rename.
test('names that differ only by case are reported as one file', () => {
  assert.deepEqual(
    fileNameCollisions(['ToDo NewUserFlow', 'Dev Api', 'TODO NewUserFlow']),
    [['TODO NewUserFlow', 'ToDo NewUserFlow']],
  );
});

test('nothing collides in the ordinary case', () => {
  assert.deepEqual(fileNameCollisions(['Dev Api', 'Dev ApiKey', 'AhaMark']), []);
});

test('the collision is judged on the file name, not the page name', () => {
  // safeFileName percent-escapes what Windows rejects, so two page names can differ while their
  // files do not. Comparing page names would miss it.
  assert.deepEqual(fileNameCollisions(['A/B', 'a%2Fb']), [['A/B', 'a%2Fb']]);
});

test('of a colliding pair, the one the file holds still syncs and the other is stranded', () => {
  const mirrored = localPages.get(somePage);
  const remote = [
    { name: somePage, revision: 1, contentHash: remoteHash(asStoredOnWiki(mirrored)) },
    { name: somePage.toUpperCase(), revision: 1, contentHash: remoteHash('#!redirect somewhere\n') },
  ];
  const { inSync, unmirrorable, diverged, remoteOnly } = classify(remote, localPages);

  assert.deepEqual(inSync.map((entry) => entry.name), [somePage]);
  assert.deepEqual(unmirrorable, [somePage.toUpperCase()]);
  // Neither of the two states that would send someone to the download, which is what loses one.
  assert.equal(diverged.length, 0);
  assert.equal(remoteOnly.length, 0);
});
