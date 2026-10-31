// Copies the third-party files the wiki's pages load -- jQuery and its plugins, MathJax, d3,
// echarts, mermaid, CodeMirror, diff2html, Font Awesome -- from node_modules into
// public/vendor/, where the templates name them through PublicAsset.url.
//
//   npm run vendor
//
// Until 2026-10-31 every page loaded these from cdnjs, jsdelivr and unpkg, and MathJax's
// combined configuration pulled one more file from cdn.mathjax.org on every view. The admin
// screen had the same shape with esm.sh and stopped for a day when esm.sh ran out of disk
// (scripts/README.md, "Front-end admin build"); the pages were one CDN incident away from the
// same. Now the only addresses off this host are Google's services (Maps, AdSense, Fonts,
// reCAPTCHA on the edit page) and profile pictures.
//
// The manifest below is the one list. The versions come from package.json; a version is raised
// by changing it there and running this again. The output directory is emptied first, so a
// file that leaves the manifest leaves the tree, and CI rebuilds it and fails on a difference
// (.github/workflows/ci.yml).
//
// This is a copy, not a bundle: these are classic scripts loaded by <script src>, each with
// its own globals, and MathJax loads its extensions and fonts at run time, which only works
// when those trees are kept as they ship. The admin UI's packages are ESM and are bundled
// instead (scripts/admin.mjs --vendor).
import { build } from "esbuild";
import { createHash } from "node:crypto";
import { cpSync, existsSync, mkdirSync, readFileSync, rmSync, statSync, writeFileSync } from "node:fs";
import path from "node:path";

const outRoot = "public/vendor";

/**
 * One entry per package: `from` is a file or directory under node_modules/<pkg>/, `to` is the
 * path under public/vendor/. A directory is copied whole. `minify` runs esbuild over a file
 * that ships unminified. `url` + `sha256` fetches a file the registry does not have, and
 * refuses it unless the digest matches.
 */
const manifest = [
  // Every page. The CSS names ../webfonts/, so the fonts go beside it under the same root.
  { pkg: "@fortawesome/fontawesome-free", from: "css/all.min.css", to: "font-awesome/css/all.min.css" },
  { pkg: "@fortawesome/fontawesome-free", from: "webfonts", to: "font-awesome/webfonts" },
  { pkg: "jquery", from: "dist/jquery.min.js", to: "jquery/jquery.min.js" },
  // jquery.form 3.51 is on cdnjs and not on npm (npm has 3.50.0, then 4.x). The template
  // carried this digest for it since the SRI attributes were added, so the file is the same
  // bytes the pages have always run. 4.x is the one with a maintained release; moving to it
  // is a separate change to the three ajaxForm callers.
  { url: "https://cdnjs.cloudflare.com/ajax/libs/jquery.form/3.51/jquery.form.min.js", sha256: "jkaBMXLp+mraE29Q0r6gtTniSfPhS1N0R7YcQwdt7nQ=", to: "jquery-form/jquery.form.min.js" },
  { pkg: "jquery-validation", from: "dist/jquery.validate.min.js", to: "jquery-validation/jquery.validate.min.js" },
  { pkg: "devbridge-autocomplete", from: "dist/jquery.autocomplete.min.js", to: "devbridge-autocomplete/jquery.autocomplete.min.js" },
  { pkg: "tablesorter", from: "dist/js/jquery.tablesorter.min.js", to: "tablesorter/jquery.tablesorter.min.js" },
  { pkg: "tablesorter", from: "dist/js/jquery.tablesorter.widgets.min.js", to: "tablesorter/jquery.tablesorter.widgets.min.js" },
  { pkg: "mark.js", from: "dist/jquery.mark.min.js", to: "mark.js/jquery.mark.min.js" },
  { pkg: "d3", from: "dist/d3.min.js", to: "d3/d3.min.js" },
  { pkg: "sortablejs", from: "Sortable.min.js", to: "sortablejs/Sortable.min.js" },
  { pkg: "echarts", from: "dist/echarts.min.js", to: "echarts/echarts.min.js" },
  { pkg: "dayjs", from: "dayjs.min.js", to: "dayjs/dayjs.min.js" },
  { pkg: "dayjs", from: "plugin/duration.js", to: "dayjs/plugin/duration.js" },
  { pkg: "dayjs", from: "plugin/relativeTime.js", to: "dayjs/plugin/relativeTime.js" },
  { pkg: "mermaid", from: "dist/mermaid.min.js", to: "mermaid/mermaid.min.js" },
  // tex-chtml.js holds the TeX input and the HTML output. What it can fetch at run time is
  // beside it, and the fonts are their own package. Not copied: sre/, the speech engine, which
  // it would otherwise take from jsdelivr and which the template switches off, and the SVG
  // fonts and outputs, which nothing asks for.
  { pkg: "mathjax", from: "tex-chtml.js", to: "mathjax/tex-chtml.js" },
  { pkg: "mathjax", from: "input/tex/extensions", to: "mathjax/input/tex/extensions" },
  { pkg: "mathjax", from: "a11y", to: "mathjax/a11y" },
  { pkg: "mathjax", from: "ui", to: "mathjax/ui" },
  { pkg: "@mathjax/mathjax-newcm-font", from: "chtml/woff2", to: "mathjax-newcm-font/chtml/woff2" },
  { pkg: "@mathjax/mathjax-newcm-font", from: "chtml/dynamic", to: "mathjax-newcm-font/chtml/dynamic" },
  // The edit page.
  { pkg: "codemirror", from: "lib/codemirror.css", to: "codemirror/codemirror.css" },
  { pkg: "codemirror", from: "lib/codemirror.js", to: "codemirror/codemirror.min.js", minify: true },
  // The diff page.
  { pkg: "diff2html", from: "bundles/css/diff2html.min.css", to: "diff2html/diff2html.min.css" },
  { pkg: "diff2html", from: "bundles/js/diff2html-ui.min.js", to: "diff2html/diff2html-ui.min.js" },
];

function versionOf(pkg) {
  return JSON.parse(readFileSync(path.join("node_modules", pkg, "package.json"), "utf8")).version;
}

async function fetchChecked(url, sha256) {
  const response = await fetch(url);
  if (!response.ok) throw new Error(`${url}: HTTP ${response.status}`);
  const bytes = Buffer.from(await response.arrayBuffer());
  const digest = createHash("sha256").update(bytes).digest("base64");
  if (digest !== sha256) throw new Error(`${url}: sha256 ${digest} does not match the manifest's ${sha256}`);
  return bytes;
}

rmSync(outRoot, { recursive: true, force: true });
const versions = new Map();
for (const entry of manifest) {
  const dest = path.join(outRoot, entry.to);
  mkdirSync(path.dirname(dest), { recursive: true });
  if (entry.url) {
    writeFileSync(dest, await fetchChecked(entry.url, entry.sha256));
    versions.set(entry.url, "(cdnjs, digest-checked)");
    continue;
  }
  const source = path.join("node_modules", entry.pkg, entry.from);
  if (!existsSync(source)) throw new Error(`${source} is not there -- run npm install, or the package moved the file`);
  if (entry.minify) {
    await build({ entryPoints: [source], outfile: dest, minify: true, logLevel: "silent" });
  } else {
    cpSync(source, dest, { recursive: statSync(source).isDirectory() });
  }
  versions.set(entry.pkg, versionOf(entry.pkg));
}

// What was copied, in the tree itself, so a reader of public/vendor/ sees the versions
// without opening package.json.
writeFileSync(path.join(outRoot, "VERSIONS"), [...versions].map(([k, v]) => `${k} ${v}`).join("\n") + "\n");
for (const [k, v] of versions) console.log(`${k} ${v}`);
console.log(`Wrote ${outRoot}/ (${manifest.length} entries)`);
