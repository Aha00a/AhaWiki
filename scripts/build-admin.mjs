import { build } from "esbuild";
import { access } from "node:fs/promises";

const inputPath = "app/assets/js/admin.jsx";
const outputPath = "public/js/babel/admin.js";

await build({
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

await access(outputPath);
console.log(`Built ${outputPath} from ${inputPath}`);
