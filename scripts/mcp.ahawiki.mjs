// An MCP server that lets an agent search, read and (if allowed) write an AhaWiki through its API.
//
//   AHAWIKI_BASE_URL=https://<wiki host>  AHAWIKI_API_KEY=<key>  node scripts/mcp.ahawiki.mjs
//
// Where the wiki is comes from the environment, because this repository is public and names no
// host. The key is the user's own API key (Dev ApiKey): the agent sees what that user can see, and
// writes only what that user may write. `AHAWIKI_MCP_WRITE=1` is what adds the write tool; without
// it the agent has no way to change a page, whatever the key could do.
//
// Why this exists and what it leaves out is in the wiki page Dev Mcp.
import readline from "node:readline";
import { pathToFileURL } from "node:url";
import { createServer } from "./lib/mcp.mjs";

const encode = (name) => encodeURIComponent(name);

/** The wiki's tools, over `call(method, path, body) -> parsed JSON`. Exported so a test can feed it a fake. */
export function wikiTools({ call, write }) {
  const tools = [
    {
      name: "ahawiki_search",
      description: "Search page names and text. Returns up to `limit` pages, newest first, each with the words around the first hit. Use it to find which page to read.",
      inputSchema: {
        type: "object",
        properties: {
          q: { type: "string", description: "Text to look for, in a page's name or body." },
          limit: { type: "number", description: "How many pages at most (default 20, cut at 100)." },
        },
        required: ["q"],
      },
      async run({ q, limit }) {
        const found = await call("GET", `/api/v1/search?q=${encodeURIComponent(q ?? "")}&limit=${limit ?? 20}`);
        if (found.pages.length === 0) return `No page matches "${found.q}".`;
        const lines = found.pages.map((page) => `- ${page.name} (${page.dateTime})\n  ${page.snippet}`);
        return `${lines.join("\n")}${found.truncated ? "\n(more pages match than were returned)" : ""}`;
      },
    },
    {
      name: "ahawiki_read_page",
      description: "Read one page's source (AhaMark markup) and its current revision. The revision is what ahawiki_save_page needs.",
      inputSchema: {
        type: "object",
        properties: { name: { type: "string", description: "The page's exact name." } },
        required: ["name"],
      },
      async run({ name }) {
        const page = await call("GET", `/api/v1/page/${encode(name)}`);
        return `revision: ${page.revision}\n\n${page.content}`;
      },
    },
    {
      name: "ahawiki_list_pages",
      description: "List pages whose name starts with a prefix, newest first, with revision and size. Not a search: it matches names from the start.",
      inputSchema: {
        type: "object",
        properties: {
          prefix: { type: "string", description: "Names must start with this. Empty lists everything." },
          limit: { type: "number", description: "How many at most (default 50)." },
        },
      },
      async run({ prefix, limit }) {
        const listed = await call("GET", `/api/v1/pages?prefix=${encodeURIComponent(prefix ?? "")}&limit=${limit ?? 50}`);
        return listed.pages.map((page) => `- ${page.name} (r${page.revision}, ${page.size} chars, ${page.dateTime})`).join("\n") || "No such pages.";
      },
    },
    {
      name: "ahawiki_recent_changes",
      description: "What was edited lately, newest first. Optionally only pages under a name prefix.",
      inputSchema: {
        type: "object",
        properties: {
          prefix: { type: "string", description: "Only pages whose name starts with this." },
          limit: { type: "number", description: "How many at most (default 30)." },
        },
      },
      async run({ prefix, limit }) {
        const changed = await call("GET", `/api/v1/changes?prefix=${encodeURIComponent(prefix ?? "")}&limit=${limit ?? 30}`);
        return changed.changes.map((row) => `- ${row.name} r${row.revision} ${row.dateTime}${row.comment ? ` — ${row.comment}` : ""}`).join("\n") || "No changes.";
      },
    },
  ];

  if (write) {
    tools.push({
      name: "ahawiki_save_page",
      description: "Replace a page's whole text. `revision` must be the one ahawiki_read_page returned (0 for a page that does not exist yet); if the page has changed since, the save is refused and nothing is overwritten — read it again and merge. Say in `comment` what actually changed.",
      inputSchema: {
        type: "object",
        properties: {
          name: { type: "string" },
          text: { type: "string", description: "The whole new source of the page." },
          revision: { type: "number", description: "The revision you read; 0 to create." },
          comment: { type: "string", description: "What changed, for the page history." },
          minorEdit: { type: "boolean", description: "Default true: an agent's edit is not news in the recent-changes list." },
        },
        required: ["name", "text", "revision", "comment"],
      },
      async run({ name, text, revision, comment, minorEdit }) {
        if (!Number.isInteger(revision)) throw new Error("revision is required: read the page first, and pass the revision it reported (0 to create).");
        if (!comment || !String(comment).trim()) throw new Error("comment is required: say what changed.");
        const saved = await call("POST", `/api/v1/page/${encode(name)}`, { revision, text, comment, minorEdit: minorEdit ?? true });
        return `Saved ${saved.name} as revision ${saved.revision}.`;
      },
    });
  }

  return tools;
}

/** `call` for a real wiki: bearer key, JSON in and out, a readable error for anything but 2xx. */
export function wikiCaller({ baseUrl, apiKey, fetchImpl = fetch }) {
  return async function call(method, path, body) {
    const response = await fetchImpl(`${baseUrl}${path}`, {
      method,
      headers: { authorization: `Bearer ${apiKey}`, ...(body ? { "content-type": "application/json" } : {}) },
      ...(body ? { body: JSON.stringify(body) } : {}),
    });
    const text = await response.text();
    let json = null;
    try { json = JSON.parse(text); } catch { /* answered with something that is not JSON */ }
    if (!response.ok) {
      throw new Error(`${response.status} ${json?.error ?? json?.message ?? text.slice(0, 200)}`);
    }
    return json;
  };
}

function main() {
  const baseUrl = (process.env.AHAWIKI_BASE_URL ?? "").replace(/\/+$/, "");
  const apiKey = process.env.AHAWIKI_API_KEY;
  if (!baseUrl || !apiKey) {
    console.error("AHAWIKI_BASE_URL and AHAWIKI_API_KEY are required.");
    process.exit(1);
  }
  const write = process.env.AHAWIKI_MCP_WRITE === "1";
  const server = createServer({
    name: "ahawiki",
    version: "1.0.0",
    instructions: `Tools for one AhaWiki${write ? " (writing is enabled)" : " (read only)"}. Search to find a page, read it for its source, and write only after reading it.`,
    tools: wikiTools({ call: wikiCaller({ baseUrl, apiKey }), write }),
  });

  const lines = readline.createInterface({ input: process.stdin });
  lines.on("line", async (line) => {
    if (!line.trim()) return;
    let message;
    try {
      message = JSON.parse(line);
    } catch {
      process.stdout.write(`${JSON.stringify({ jsonrpc: "2.0", id: null, error: { code: -32700, message: "Parse error" } })}\n`);
      return;
    }
    const answer = await server.handle(message);
    if (answer) process.stdout.write(`${JSON.stringify(answer)}\n`);
  });
}

if (import.meta.url === pathToFileURL(process.argv[1] ?? "").href) main();
