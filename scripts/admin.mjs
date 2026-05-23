import { build, context } from "esbuild";
import { access } from "node:fs/promises";

const inputPath = "app/assets/js/admin.jsx";
const outputPath = "public/js/babel/admin.js";

const isWatch = process.argv.includes("--watch");

const config = {
  entryPoints: [inputPath],
  outfile: outputPath,
  bundle: true,
  format: "esm",
  platform: "browser",
  external: ["react", "react-dom/client", "@mantine/core", "mantine-datatable", "recharts", "react-router-dom", "dayjs"],
  minify: false,
  sourcemap: false,
  logLevel: "info",
};

if (isWatch) {
  const ctx = await context(config);
  await ctx.watch();
  console.log(`Watching ${inputPath} -> ${outputPath}`);
} else {
  await build(config);
  await access(outputPath);
  console.log(`Built ${outputPath} from ${inputPath}`);
}
