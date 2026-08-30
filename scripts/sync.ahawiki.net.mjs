// Compares the committed page copies under docs/ahawiki.net/ against the live wiki, and
// uploads the ones where only the local side moved. This is the procedure AGENTS.md describes
// under "AhaWikiDoc Sync"; it lived only as prose, so every sync re-implemented it by hand and
// the sweep at the end — the part that catches a page stranded by an earlier missed sync — was
// the part most often skipped.
//
//   node scripts/sync.ahawiki.net.mjs                          report only, changes nothing
//   node scripts/sync.ahawiki.net.mjs --apply --comment="..."  upload the local-ahead pages
//   node scripts/sync.ahawiki.net.mjs --apply --comment="..." --only="Dev Testing"
//
// Needs AHAWIKI_API_KEY. Never write a key into this repository.
//
// The exported functions are what `test/ahawiki.sync.test.mjs` covers; the CLI below only runs
// when this file is the entry point.
import { createHash } from 'node:crypto';
import { execFileSync } from 'node:child_process';
import { pathToFileURL } from 'node:url';
import { baseUrl, docsGitPath, manifestFileName, rootDir, safeFileName } from './lib/ahawiki.net.mjs';

// How far back to look for the remote content among a file's older committed versions. Deep
// enough to recognise a page last synced many commits ago, bounded so a drifted page cannot
// walk the whole history.
export const historyDepth = 40;

function git(args) {
  return execFileSync('git', args, { cwd: rootDir, encoding: 'utf8', maxBuffer: 64 * 1024 * 1024 });
}

function sha256(text) {
  return createHash('sha256').update(text, 'utf8').digest('hex');
}

/**
 * Whether local file content and a remote contentHash are the same page.
 *
 * The wiki stores a trailing newline the local file does not have, so the raw hashes differ on
 * every page. The remote content itself is not in the page list — only its hash — so this
 * offers the plausible spellings rather than normalising both sides.
 */
export function hashMatches(localContent, remoteContentHash) {
  const remote = String(remoteContentHash ?? '').replace(/^sha256:/, '');
  if (remote === '') return false;
  const withoutTrailing = localContent.replace(/\n+$/, '');
  return [localContent, `${localContent}\n`, withoutTrailing, `${withoutTrailing}\n`]
    .some((candidate) => sha256(candidate) === remote);
}

/**
 * The committed content of every page copy, keyed by filename.
 *
 * Committed rather than working-tree: this tree is normalised to LF, but a file checked out
 * before that attribute existed keeps CRLF in the working copy, and hashing that reports drift
 * on a page nobody touched. It also keeps an unreviewed local edit from being uploaded.
 */
export function readCommittedPages() {
  const files = git(['ls-tree', '-r', 'HEAD', '--name-only', docsGitPath])
    .split('\n')
    .map((line) => line.trim())
    .filter(Boolean)
    .filter((file) => file !== `${docsGitPath}/${manifestFileName}`);

  return new Map(files.map((file) => [
    file.slice(docsGitPath.length + 1),
    git(['show', `HEAD:${file}`]),
  ]));
}

/**
 * Whether the remote holds some older committed version of this file.
 *
 * If it does, the local side is simply ahead and saving it loses nothing. If it holds content
 * that was never committed here, someone edited the page in the browser and the two sides have
 * to be merged by hand — AGENTS.md is explicit that neither side gets overwritten blindly.
 */
export function remoteIsOlderLocalVersion(file, remoteContentHash) {
  const path = `${docsGitPath}/${file}`;
  const commits = git(['log', `--max-count=${historyDepth}`, '--format=%H', '--', path])
    .split('\n')
    .map((line) => line.trim())
    .filter(Boolean);

  return commits.some((commit) => {
    try {
      return hashMatches(git(['show', `${commit}:${path}`]), remoteContentHash);
    } catch {
      return false; // The file did not exist at that commit.
    }
  });
}

/** Sorts every page into one of the five states a sync can find it in. */
export function classify(remotePages, localPages) {
  const inSync = [];
  const localAhead = [];
  const diverged = [];
  const remoteOnly = [];
  const claimed = new Set();

  for (const page of remotePages) {
    const file = safeFileName(page.name);
    const local = localPages.get(file);

    if (local === undefined) {
      remoteOnly.push(page.name);
      continue;
    }

    claimed.add(file);
    const entry = { name: page.name, file, revision: page.revision };

    if (hashMatches(local, page.contentHash)) {
      inSync.push(entry);
    } else if (remoteIsOlderLocalVersion(file, page.contentHash)) {
      localAhead.push(entry);
    } else {
      diverged.push(entry);
    }
  }

  const localOnly = [...localPages.keys()].filter((file) => !claimed.has(file));
  return { inSync, localAhead, diverged, localOnly, remoteOnly };
}

function argValue(argv, flag) {
  const found = argv.find((arg) => arg.startsWith(`${flag}=`));
  return found ? found.slice(flag.length + 1) : null;
}

function argValues(argv, flag) {
  const found = argv.filter((arg) => arg.startsWith(`${flag}=`)).map((arg) => arg.slice(flag.length + 1));
  return found.length > 0 ? found : null;
}

async function main(argv) {
  const apiKey = process.env.AHAWIKI_API_KEY;
  const apply = argv.includes('--apply');
  const minorEdit = argv.includes('--minor');
  const only = argValues(argv, '--only');
  const comment = argValue(argv, '--comment');

  if (!apiKey) {
    console.error('AHAWIKI_API_KEY is not set.');
    return 1;
  }

  if (apply && !comment) {
    console.error('--apply needs --comment="..." summarizing the actual content change.');
    return 1;
  }

  async function api(method, path, body) {
    const response = await fetch(`${baseUrl}${path}`, {
      method,
      headers: {
        authorization: `Bearer ${apiKey}`,
        ...(body ? { 'content-type': 'application/json' } : {}),
      },
      ...(body ? { body: JSON.stringify(body) } : {}),
    });

    const text = await response.text();
    let json = null;
    try {
      json = JSON.parse(text);
    } catch {
      // Left null; callers report `text` instead.
    }

    return { ok: response.ok, status: response.status, json, text };
  }

  const listed = await api('GET', '/api/v1/pages?limit=5000');
  if (!listed.ok) {
    console.error(`Page list failed: ${listed.status} ${listed.text.slice(0, 300)}`);
    return 1;
  }

  const remotePages = listed.json.pages;
  const localPages = readCommittedPages();
  const { inSync, localAhead, diverged, localOnly, remoteOnly } = classify(remotePages, localPages);

  const report = (label, names) =>
    console.log(`${label}: ${names.length}${names.length > 0 ? ` -> ${names.join(', ')}` : ''}`);

  console.log(`${remotePages.length} pages on the wiki, ${localPages.size} committed locally`);
  console.log(`in sync: ${inSync.length}`);
  report('local ahead', localAhead.map((entry) => entry.name));
  report('diverged (resolve by hand)', diverged.map((entry) => entry.name));
  report('local only (never uploaded)', localOnly);
  report('wiki only (never downloaded)', remoteOnly);

  if (diverged.length > 0) {
    console.log('\nA diverged page holds content that was never committed here — someone edited it');
    console.log('in the browser. Read the page, merge it into the local file, commit, then sync.');
  }

  if (!apply) {
    console.log('\nReport only. Pass --apply --comment="..." to upload the local-ahead pages.');
    return 0;
  }

  const targets = localAhead.filter((entry) => !only || only.includes(entry.name));
  if (only) {
    for (const name of only.filter((n) => !targets.some((entry) => entry.name === n))) {
      console.log(`\nSKIP ${name}: not local-ahead (in sync, diverged, or not a page).`);
    }
  }

  if (targets.length === 0) {
    console.log('\nNothing to upload.');
    return 0;
  }

  console.log('');
  let failures = 0;

  for (const target of targets) {
    const path = `/api/v1/page/${encodeURIComponent(target.name)}`;

    // Re-read immediately before saving: the revision goes back with the save, so a page
    // edited between the listing and now is rejected as a 409 rather than overwritten.
    const current = await api('GET', path);
    if (!current.ok) {
      console.log(`FAIL ${target.name}: read ${current.status} ${current.text.slice(0, 200)}`);
      failures += 1;
      continue;
    }

    const local = localPages.get(target.file);
    const saved = await api('POST', path, {
      revision: current.json.revision,
      text: local,
      comment,
      minorEdit,
    });

    if (!saved.ok) {
      console.log(`FAIL ${target.name}: save ${saved.status} ${saved.text.slice(0, 200)}`);
      failures += 1;
      continue;
    }

    const verified = await api('GET', path);
    if (!verified.ok || !hashMatches(local, verified.json.contentHash)) {
      console.log(`FAIL ${target.name}: saved but the wiki does not hold the local content`);
      failures += 1;
      continue;
    }

    console.log(`OK   ${target.name} r${current.json.revision} -> r${verified.json.revision}`);
  }

  if (failures > 0) {
    console.error(`\n${failures} page(s) failed.`);
    return 1;
  }

  return 0;
}

const invokedDirectly = process.argv[1] !== undefined
  && pathToFileURL(process.argv[1]).href === import.meta.url;

if (invokedDirectly) {
  process.exitCode = await main(process.argv.slice(2));
}
