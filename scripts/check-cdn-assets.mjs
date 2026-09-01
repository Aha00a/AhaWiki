// Fetches every external asset the templates point at and reports the ones that do not
// answer 200.
//
// A stylesheet that 404s is silent. `mantine-datatable@9.2.2/styles.css` was wrong — the
// package ships it under `dist/` — and the admin UI rendered every table unstyled for as long
// as that link existed, drawing the "no records" overlay across real rows because the rule
// that hides it was in the file that never arrived. Nothing failed; it just looked slightly
// wrong, which is the kind of thing that survives for months.
//
// This is not part of `npm test`: it needs the network, and a version bump is when to run it.
import fs from "node:fs";
import path from "node:path";
import { rootDir } from "./lib/ahawiki.net.mjs";

const searchDirs = ["app/views", "public/js", "app/assets"];
const concurrency = 8;

function filesUnder(dir) {
  const absolute = path.join(rootDir, dir);
  if (!fs.existsSync(absolute)) return [];
  return fs.readdirSync(absolute, { withFileTypes: true }).flatMap((entry) => {
    const relative = `${dir}/${entry.name}`;
    return entry.isDirectory() ? filesUnder(relative) : [relative];
  });
}

/**
 * Every external URL a browser would be told to fetch.
 *
 * `href`/`src` covers stylesheets and scripts; the bare-quoted form catches importmap values,
 * which are plain JSON strings rather than attributes. Twirl writes a literal `@` as `@@`, so
 * that is undone before asking for the URL.
 */
function urlsIn(text) {
  const found = [
    ...[...text.matchAll(/(?:href|src)="(https?:\/\/[^"]+)"/g)].map((m) => m[1]),
    ...[...text.matchAll(/"(https?:\/\/[^"\s]+)"/g)].map((m) => m[1]),
  ];
  return found
    .map((url) => url.replaceAll("@@", "@"))
    // Templates also write URLs the server fills in — `https://${host}/w/$name`, schema.org
    // ids, a search template carrying `{search_term_string}`. Those are not assets and there
    // is nothing to fetch, so anything still holding a placeholder is not ours to check.
    .filter((url) => !/[${}]/.test(url));
}

const byUrl = new Map();
for (const file of searchDirs.flatMap(filesUnder)) {
  const text = fs.readFileSync(path.join(rootDir, file), "utf8");
  for (const url of urlsIn(text)) {
    if (!byUrl.has(url)) byUrl.set(url, file);
  }
}

const urls = [...byUrl.keys()].sort();
console.log(`${urls.length} external assets referenced by ${searchDirs.join(", ")}`);

async function statusOf(url) {
  try {
    // Some CDNs answer HEAD differently from GET, and GET is what a browser does.
    const response = await fetch(url, { redirect: "follow" });
    return response.status;
  } catch (error) {
    return `error: ${error.message}`;
  }
}

const failures = [];
const queue = [...urls];
await Promise.all(Array.from({ length: Math.min(concurrency, queue.length) }, async () => {
  while (queue.length > 0) {
    const url = queue.shift();
    const status = await statusOf(url);
    if (status !== 200) failures.push({ url, status, file: byUrl.get(url) });
  }
}));

if (failures.length === 0) {
  console.log("all 200");
  process.exit(0);
}

console.error(`\n${failures.length} asset(s) did not answer 200:`);
for (const failure of failures.sort((a, b) => a.url.localeCompare(b.url))) {
  console.error(`  ${failure.status}  ${failure.url}`);
  console.error(`        referenced by ${failure.file}`);
}
process.exit(1);
