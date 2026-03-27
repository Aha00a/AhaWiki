import { context } from "esbuild";

const inputPath = "app/assets/js/admin.jsx";
const outputPath = "public/js/babel/admin.js";

const ctx = await context({
  entryPoints: [inputPath],
  outfile: outputPath,
  bundle: true,
  format: "esm",
  platform: "browser",
  external: ["react", "react-dom/client", "@mantine/core"],
  minify: false,
  sourcemap: false,
  logLevel: "info",
});

await ctx.watch();
console.log(`Watching ${inputPath} -> ${outputPath}`);
