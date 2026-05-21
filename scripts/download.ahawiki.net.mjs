import { mkdir, readdir, rm, writeFile } from "node:fs/promises";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const rootDir = path.resolve(__dirname, "..");

const baseUrl = "https://ahawiki.net";
const pageListUrl = `${baseUrl}/w/PageList`;
const outputDir = path.join(rootDir, "docs", "ahawiki.net");
const concurrency = Number.parseInt(process.env.AHAWIKI_DOWNLOAD_CONCURRENCY ?? "6", 10);
const cleanOutputDir = process.argv.includes("--clean");

function decodeHtml(text) {
  return text
    .replace(/&amp;/g, "&")
    .replace(/&lt;/g, "<")
    .replace(/&gt;/g, ">")
    .replace(/&quot;/g, "\"")
    .replace(/&#39;/g, "'");
}

function pageNameFromHref(href) {
  const rawPath = decodeHtml(href).split("?")[0];
  const encodedName = rawPath.replace(/^\/w\//, "");
  return decodeURIComponent(encodedName);
}

function safeFileName(pageName) {
  return pageName.replace(/[<>:"/\\|?*\x00-\x1F]/g, (char) => {
    return `%${char.codePointAt(0).toString(16).toUpperCase().padStart(2, "0")}`;
  });
}

function rawUrlFromHref(href) {
  const url = new URL(decodeHtml(href), baseUrl);
  url.search = "";
  url.searchParams.set("action", "raw");
  return url.toString();
}

async function fetchText(url, options = {}) {
  const response = await fetch(url, {
    headers: {
      "user-agent": "AhaWiki raw text downloader",
    },
  });

  if (!response.ok && !options.allowNotOk) {
    throw new Error(`GET ${url} failed: ${response.status} ${response.statusText}`);
  }

  return response.text();
}

function extractPages(pageListHtml) {
  const tableMatch = pageListHtml.match(
    /<div class="Macro BlockMacro PageList">([\s\S]*?)<\/table>/,
  );

  if (!tableMatch) {
    throw new Error("Could not find the PageList table in the downloaded HTML.");
  }

  const pages = [];
  const seen = new Set();
  const linkPattern = /<td><strong><a href="([^"]+)">([\s\S]*?)<\/a><\/strong><\/td>/g;

  for (const match of tableMatch[1].matchAll(linkPattern)) {
    const href = decodeHtml(match[1]);
    const name = pageNameFromHref(href);

    if (seen.has(name)) {
      continue;
    }

    seen.add(name);
    pages.push({
      name,
      href,
      rawUrl: rawUrlFromHref(href),
      fileName: safeFileName(name),
    });
  }

  if (pages.length === 0) {
    throw new Error("No pages were found in the PageList table.");
  }

  return pages;
}

async function mapWithConcurrency(items, limit, mapper) {
  const results = new Array(items.length);
  let nextIndex = 0;

  async function worker() {
    while (nextIndex < items.length) {
      const currentIndex = nextIndex;
      nextIndex += 1;
      results[currentIndex] = await mapper(items[currentIndex], currentIndex);
    }
  }

  const workerCount = Math.max(1, Math.min(limit, items.length));
  await Promise.all(Array.from({ length: workerCount }, worker));
  return results;
}

async function cleanDirectory(dir) {
  await mkdir(dir, { recursive: true });

  for (const entry of await readdir(dir, { withFileTypes: true })) {
    await rm(path.join(dir, entry.name), { recursive: true, force: true });
  }
}

async function main() {
  if (Number.isNaN(concurrency) || concurrency < 1) {
    throw new Error("AHAWIKI_DOWNLOAD_CONCURRENCY must be a positive integer.");
  }

  const pageListHtml = await fetchText(pageListUrl, { allowNotOk: true });
  const pages = extractPages(pageListHtml);

  if (cleanOutputDir) {
    await cleanDirectory(outputDir);
  } else {
    await mkdir(outputDir, { recursive: true });
  }

  console.log(`Found ${pages.length} pages from ${pageListUrl}`);
  console.log(`Writing raw text files to ${path.relative(rootDir, outputDir)}`);

  const downloaded = await mapWithConcurrency(pages, concurrency, async (page, index) => {
    const rawText = await fetchText(page.rawUrl);
    const filePath = path.join(outputDir, page.fileName);
    await writeFile(filePath, rawText, "utf8");
    console.log(`[${index + 1}/${pages.length}] ${page.name}`);

    return {
      name: page.name,
      rawUrl: page.rawUrl,
      fileName: page.fileName,
      bytes: Buffer.byteLength(rawText, "utf8"),
    };
  });

  await writeFile(
    path.join(outputDir, "manifest.json"),
    `${JSON.stringify(
      {
        source: pageListUrl,
        downloadedAt: new Date().toISOString(),
        count: downloaded.length,
        pages: downloaded,
      },
      null,
      2,
    )}\n`,
    "utf8",
  );

  console.log(`Downloaded ${downloaded.length} pages.`);
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});
