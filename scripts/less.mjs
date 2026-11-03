// Recompiles the stylesheets the repository commits next to their Less sources, with the lessc
// that package.json pins, the way the Dev Less page records as the one command whose output
// matches the committed files byte for byte: `lessc --source-map X.less X.css`, run from
// app/assets with nothing else on the line. Other options change the source map, and then a file
// nobody touched shows up in the diff.
//
//   node scripts/less.mjs        # recompile every listed stylesheet in place
//
// less.less is left out on purpose: it holds remote `@import url()` lines for Google Fonts, and
// lessc bakes the day's answer into the output, so a recompile changes font URLs nobody touched.
// The owner's editor compiles that one on save.
//
// test/less.compiled.test.mjs compiles the same list into a temporary directory and fails when a
// committed .css or .css.map differs from what the .less makes. Until 2026-11-03 the only guard
// was a measurement written on the wiki page, and in the meantime kanban.css had lost its
// sourceMappingURL line and adjacent.css carried a rule at a different position than lessc puts
// it -- hand edits, each correct in effect, each a diff waiting for the next recompile.
import { spawnSync } from 'node:child_process';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const rootDir = path.resolve(path.dirname(fileURLToPath(import.meta.url)), '..');
export const assetsDir = path.join(rootDir, 'app', 'assets');

/** The stylesheets whose output is deterministic: every `*.less` not named `_*` or `less.less`. */
export const stylesheets = ['adjacent', 'blame', 'diff', 'edit', 'gantt', 'interpreter', 'kanban', 'search', 'wiki'];

export const remoteImportOnly = { 'less': 'remote @import url() of Google Fonts; lessc bakes the response of the day into the output' };

const lessc = path.join(rootDir, 'node_modules', 'less', 'bin', 'lessc');

/**
 * Compiles `<name>.less` into `<name>.css` and `<name>.css.map` in `dir`, which holds the Less
 * sources: the map's `sources` are written relative to the output, and they have to read
 * `wiki.less`, `_mixins.less`, as the committed maps do.
 */
export function compile(name, dir) {
  const result = spawnSync(process.execPath, [lessc, '--source-map', `${name}.less`, `${name}.css`], { cwd: dir, encoding: 'utf8' });
  if (result.error) throw result.error;
  // lessc 4 prints deprecation warnings for syntax the sources still use; only the exit code says whether it compiled.
  if (result.status !== 0) throw new Error(`lessc failed on ${name}.less (exit ${result.status}):\n${result.stderr}`);
}

if (process.argv[1] && path.resolve(process.argv[1]) === fileURLToPath(import.meta.url)) {
  for (const name of stylesheets) {
    compile(name, assetsDir);
    console.log(`${name}.less -> ${name}.css`);
  }
}
