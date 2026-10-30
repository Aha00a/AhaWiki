// Builds the admin UI: the app bundle, and separately the vendor bundle it imports from.
//
//   node scripts/admin.mjs            app bundle   app/assets/js/admin.jsx -> public/js/babel/admin.js
//   node scripts/admin.mjs --watch    the same, rebuilt on every change
//   node scripts/admin.mjs --vendor   vendor bundle node_modules -> public/js/babel/vendor/
//
// The app bundle leaves the packages in `vendorModules` as bare imports. The admin template's
// import map points each of them at a file under public/js/babel/vendor/, which the --vendor
// build writes from node_modules: one entry file per package, and shared chunks between them so
// React is loaded once however many packages use it.
//
// Until 2026-10-31 the import map pointed at esm.sh, which builds `?deps=` addresses on request.
// That day esm.sh ran out of disk and answered 503 for four of them, and the admin screen of
// every site stayed on its loading message while nothing of ours had changed. The vendor bundle
// is the cure: the packages are served from the same place as the app.
//
// They are two bundles rather than one because both are committed. The app bundle changes with
// every admin change and is ~140KB; the vendor bundle changes only when a version is raised and
// is ten times that. One bundle would put the large part into the history with every small
// change.
import { build, context } from "esbuild";
import { access, cp, mkdir, readdir, rm } from "node:fs/promises";
import path from "node:path";

const appInput = "app/assets/js/admin.jsx";
const appOutput = "public/js/babel/admin.js";
const vendorOutDir = "public/js/babel/vendor";

/**
 * Bare specifier -> file name under public/js/babel/vendor/.
 *
 * One list for both bundles: the app bundle marks these external, and the vendor build makes
 * exactly these. A package added to one and not the other is an import the browser cannot
 * resolve, and that shows up as a blank admin page rather than a build error.
 */
const vendorModules = {
  "react": "react.js",
  "react-dom/client": "react-dom-client.js",
  "@mantine/core": "mantine-core.js",
  "mantine-datatable": "mantine-datatable.js",
  "recharts": "recharts.js",
  "react-router-dom": "react-router-dom.js",
  "dayjs": "dayjs.js",
};

/** Stylesheets the packages ship, copied next to the vendor scripts so the template can name them. */
const vendorStylesheets = {
  "node_modules/@mantine/core/styles.css": "mantine-core.css",
  // mantine-datatable ships its stylesheet under dist/; the root path 404s. See the template.
  "node_modules/mantine-datatable/dist/styles.css": "mantine-datatable.css",
};

const appConfig = {
  entryPoints: [appInput],
  outfile: appOutput,
  bundle: true,
  format: "esm",
  platform: "browser",
  external: Object.keys(vendorModules),
  minify: false,
  sourcemap: false,
  logLevel: "info",
};

/**
 * The source of one vendor entry: every export of the package by name, and a default.
 *
 * The names are read by importing the package here in Node. They have to be spelled out:
 * `export * from "react"` compiles, but React is CommonJS, so esbuild can only re-export it at
 * run time, and a browser resolving `import {createContext} from "react"` against that file
 * finds no such static export and stops before the first render. Writing the names makes them
 * static. The default covers the CommonJS packages (`react`, `dayjs`) that the app imports as
 * one object; for the ESM packages it is the namespace, which nothing imports.
 */
async function vendorEntrySource(specifier) {
  const module = await import(specifier);
  const names = Object.keys(module).filter((name) => name !== "default" && /^[A-Za-z_$][\w$]*$/.test(name));
  return [
    `import * as m from ${JSON.stringify(specifier)};`,
    `export const {${names.join(", ")}} = m;`,
    `export default m.default === undefined ? m : m.default;`,
  ].join("\n");
}

const virtualPrefix = "vendor-entry:";

/** Serves the generated entry sources to esbuild, so nothing has to be written to disk or committed. */
const vendorEntryPlugin = {
  name: "vendor-entry",
  setup(pluginBuild) {
    pluginBuild.onResolve({ filter: new RegExp(`^${virtualPrefix}`) }, (args) => ({ path: args.path, namespace: "vendor-entry" }));
    pluginBuild.onLoad({ filter: /.*/, namespace: "vendor-entry" }, async (args) => ({
      contents: await vendorEntrySource(args.path.slice(virtualPrefix.length)),
      resolveDir: process.cwd(),
      loader: "js",
    }));
  },
};

async function buildVendor() {
  await rm(vendorOutDir, { recursive: true, force: true });
  await mkdir(vendorOutDir, { recursive: true });
  await build({
    entryPoints: Object.fromEntries(
      Object.entries(vendorModules).map(([specifier, file]) => [file.replace(/\.js$/, ""), virtualPrefix + specifier]),
    ),
    plugins: [vendorEntryPlugin],
    outdir: vendorOutDir,
    bundle: true,
    splitting: true,
    format: "esm",
    platform: "browser",
    // The chunk name carries its content hash, so a rebuilt chunk is a new address and the
    // hour-long cache on /public/ cannot serve a stale one. The entry files keep fixed names
    // and are addressed through PublicAsset.url, which puts the digest in the query string.
    chunkNames: "chunk-[hash]",
    minify: true,
    sourcemap: false,
    define: { "process.env.NODE_ENV": '"production"' },
    logLevel: "info",
  });
  for (const [from, to] of Object.entries(vendorStylesheets)) {
    await cp(from, path.join(vendorOutDir, to));
  }
  const files = await readdir(vendorOutDir);
  console.log(`Built ${vendorOutDir}/ (${files.length} files) from node_modules`);
}

if (process.argv.includes("--vendor")) {
  await buildVendor();
} else if (process.argv.includes("--watch")) {
  const ctx = await context(appConfig);
  await ctx.watch();
  console.log(`Watching ${appInput} -> ${appOutput}`);
} else {
  await build(appConfig);
  await access(appOutput);
  console.log(`Built ${appOutput} from ${appInput}`);
}
