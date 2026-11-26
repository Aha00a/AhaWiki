// What a browser harness under test/manual/ has printed, read as a verdict. harnesses.mjs opens
// the pages and waits; this is the reading, kept apart so it can be tested without a browser.
//
// The harnesses were written one at a time, each for a person to read, and they do not print
// alike. What they share is a line a check that starts with PASS or FAIL. The rest differs:
//
//   paper-contract, word-break, graph-hud   ... then «PASS — 17 checks» or «FAIL — 2 of 17»
//   editor-keys, editor-hotkeys             ... then «PASS — <what held>» or «FAIL — 1 of 3»
//   editor-grid                             «OK   name» for a check that held, then «PASS» or «FAIL — 1»
//   slide-layout                            ... then «ALL PASSED» or «2 FAILED»
//   admin-vendor, vendor-assets             ... then «ALL PASS» or «SOME FAIL»
//   diff-long-lines, slide-notes, table-data-bars   the checks and no last line
//   graph-glow                              one line, «PASS 300 frames, 0 exceptions, ...», and two of numbers
//
// So the reading is by the lines, not by the last one: a harness passes when at least one check
// held and none failed. A last line that counts is kept as the harness's own summary.

const held = (line) => /^(PASS|OK)\b/.test(line);
const failed = (line) => /^FAIL\b/.test(line);
// A last line that speaks for the whole page rather than for one check.
const sumsUp = (line) => /^(PASS|FAIL)( —|$)/.test(line) || /^(ALL PASS(ED)?|SOME FAIL|\d+ FAILED)$/.test(line);

/**
 * `text` is what the page's result element holds. The answer:
 *
 *   state     'pass', 'fail', or 'silent' when it has printed no check yet -- «running…», or nothing
 *   checks    how many checks it printed
 *   failures  the lines of the checks that failed, each with the indented lines under it
 *   summary   the harness's own last line when it has one, or the count
 *   summed    whether it had one: a page that has summed up has said it is done
 */
export function verdictOf(text) {
  const lines = (text || '').split('\n').map((line) => line.trimEnd()).filter((line) => line.trim());
  const last = lines.length ? lines[lines.length - 1].trim() : '';
  const own = sumsUp(last) ? last : '';
  const body = own ? lines.slice(0, -1) : lines;

  const failures = [];
  let checks = 0;
  let inFailure = false;
  for (const line of body) {
    const indented = /^\s/.test(line);
    const trimmed = line.trim();
    if (!indented && (held(trimmed) || failed(trimmed))) checks++;
    if (!indented) inFailure = failed(trimmed);
    if (inFailure) failures.push(indented ? '  ' + trimmed : trimmed);
  }

  if (checks === 0 && !own) return { state: 'silent', checks, failures, summary: last, summed: false };
  const state = failures.length || /^(FAIL|SOME FAIL|\d+ FAILED)/.test(own) ? 'fail' : 'pass';
  return { state, checks, failures, summary: own || `${checks} check${checks === 1 ? '' : 's'}`, summed: !!own };
}
