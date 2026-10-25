// A Model Context Protocol server's message handling, with nothing about the wiki in it.
//
// MCP over stdio is newline-delimited JSON-RPC 2.0. This is the part that does not change with
// what the tools do: answer `initialize`, list the tools, run one, and turn a failure into the
// shape a client can show. It is a function from a message to its answer so that it can be tested
// without a pipe, and so the wiki's tools (scripts/mcp.ahawiki.mjs) are only the tools.
//
// Written by hand rather than with the SDK: the protocol needs four methods here, and an
// dependency in a repository whose only dependency is its bundler is a bigger thing than 80 lines.
//
// A tool is {name, description, inputSchema, run(args) -> string}. A tool that throws is answered
// with `isError: true` and the message, not with a protocol error: the call reached the tool, and
// the model should read what went wrong and try again; a protocol error is for a client that sent
// something this server cannot make sense of.

const PROTOCOL_VERSION = "2025-03-26";

const METHOD_NOT_FOUND = -32601;
const INVALID_PARAMS = -32602;

export function createServer({ name, version, instructions, tools }) {
  const byName = new Map(tools.map((tool) => [tool.name, tool]));

  const result = (id, value) => ({ jsonrpc: "2.0", id, result: value });
  const failure = (id, code, message) => ({ jsonrpc: "2.0", id, error: { code, message } });

  /** The answer to one message, or null when none is owed — a notification has no id. */
  async function handle(message) {
    const { id, method, params } = message;
    const isNotification = id === undefined || id === null;

    switch (method) {
      case "initialize":
        return result(id, {
          // The client states the version it wants. Answering with the one this was written
          // against, and letting the client decide whether it can speak it, is what the spec says.
          protocolVersion: PROTOCOL_VERSION,
          capabilities: { tools: {} },
          serverInfo: { name, version },
          ...(instructions ? { instructions } : {}),
        });

      case "ping":
        return result(id, {});

      case "tools/list":
        return result(id, {
          tools: tools.map(({ name: toolName, description, inputSchema }) => ({ name: toolName, description, inputSchema })),
        });

      case "tools/call": {
        const tool = byName.get(params?.name);
        if (!tool) return failure(id, INVALID_PARAMS, `Unknown tool: ${params?.name}`);
        try {
          const text = await tool.run(params.arguments ?? {});
          return result(id, { content: [{ type: "text", text }] });
        } catch (error) {
          return result(id, { content: [{ type: "text", text: String(error.message ?? error) }], isError: true });
        }
      }

      default:
        // `notifications/initialized` and the like: nothing to do, nothing to say.
        if (isNotification) return null;
        return failure(id, METHOD_NOT_FOUND, `Method not found: ${method}`);
    }
  }

  return { handle };
}
