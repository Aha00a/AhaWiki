const inputPath = "app/assets/js/admin.jsx";
const outputPath = "public/js/babel/admin.js";

const result = await Bun.build({
  entrypoints: [inputPath],
  outdir: "public/js/babel",
  naming: "admin.js",
  target: "browser",
  format: "esm",
  external: ["react", "react-dom/client", "@mantine/core"],
  minify: false,
  sourcemap: "none",
});

if (!result.success) {
  for (const log of result.logs) {
    console.error(log);
  }
  process.exit(1);
}

if (!(await Bun.file(outputPath).exists())) {
  console.error(`Build succeeded but output file was not written: ${outputPath}`);
  process.exit(1);
}

console.log(`Built ${outputPath} from ${inputPath}`);
