import React, { useEffect, useMemo, useState } from "react";
import { createRoot } from "react-dom/client";
import htm from "https://esm.sh/htm@3.1.1";
import {
  MantineProvider,
  AppShell,
  Badge,
  Card,
  Group,
  Loader,
  NavLink,
  Paper,
  Stack,
  Table,
  Text,
  Title,
} from "@mantine/core";

const html = htm.bind(React.createElement);
const LOG_PREFIX = "[AdminUI]";

function logInfo(...args) {
  console.log(LOG_PREFIX, ...args);
}

function logError(...args) {
  console.error(LOG_PREFIX, ...args);
}

async function fetchJson(url) {
  logInfo("fetch:start", url);
  const response = await fetch(url, { credentials: "same-origin" });
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}`);
  }
  const data = await response.json();
  logInfo("fetch:success", url, { count: Array.isArray(data) ? data.length : undefined });
  return data;
}

function useAdminData(page) {
  const [loading, setLoading] = useState(true);
  const [sites, setSites] = useState([]);
  const [users, setUsers] = useState([]);
  const [error, setError] = useState("");

  useEffect(() => {
    let mounted = true;
    const load = async () => {
      setLoading(true);
      setError("");
      try {
        if (page === "sites") {
          const data = await fetchJson("/api/admin/sites");
          if (mounted) {
            setSites(data);
            setUsers([]);
          }
          return;
        }

        if (page === "users") {
          const data = await fetchJson("/api/admin/site-users");
          if (mounted) {
            setUsers(data);
            setSites([]);
          }
          return;
        }

        const [siteData, userData] = await Promise.all([
          fetchJson("/api/admin/sites"),
          fetchJson("/api/admin/site-users"),
        ]);

        if (mounted) {
          setSites(siteData);
          setUsers(userData);
        }
      } catch (caughtError) {
        logError("data:load:error", caughtError);
        if (mounted) {
          setError(caughtError.message || String(caughtError));
        }
      } finally {
        if (mounted) {
          setLoading(false);
        }
      }
    };

    load();
    return () => {
      mounted = false;
    };
  }, [page]);

  return { loading, sites, users, error };
}

function makeTable(headers, rows) {
  return html`
    <${Table} striped highlightOnHover withTableBorder withColumnBorders>
      <${Table.Thead}>
        <${Table.Tr}>
          ${headers.map((header) => html`<${Table.Th} key=${header}>${header}<//>`)}
        <//>
      <//>
      <${Table.Tbody}>
        ${rows.map(
          (columns, rowIndex) => html`
            <${Table.Tr} key=${`row-${rowIndex}`}>
              ${columns.map(
                (column, colIndex) =>
                  html`<${Table.Td} key=${`col-${rowIndex}-${colIndex}`}>${String(column ?? "")}<//>`,
              )}
            <//>
          `,
        )}
      <//>
    <//>
  `;
}

function Navigation({ activePage }) {
  const links = useMemo(
    () => [
      { href: "/", label: "Home", key: "home" },
      { href: "/admin", label: "Dashboard", key: "dashboard" },
      { href: "/admin/sites", label: "All Sites", key: "sites" },
      { href: "/admin/site-users", label: "Site Users", key: "users" },
    ],
    [],
  );

  return html`
    <${Stack} gap=${4}>
      ${links.map(
        (link) =>
          html`<${NavLink}
            key=${link.key}
            href=${link.href}
            label=${link.label}
            active=${activePage === link.key}
            variant="light"
          />`,
      )}
    <//>
  `;
}

function AdminContent({ page }) {
  const { loading, sites, users, error } = useAdminData(page);

  if (loading) {
    return html`
      <${Paper} p="lg" withBorder radius="md">
        <${Group} gap="sm">
          <${Loader} size="sm" />
          <${Text}>데이터를 불러오는 중...<//>
        <//>
      <//>
    `;
  }

  if (error) {
    return html`
      <${Paper} p="lg" withBorder radius="md">
        <${Text} c="red" fw=${600}>클라이언트 렌더링 오류: ${error}<//>
      <//>
    `;
  }

  if (page === "sites") {
    return html`
      <${Card} withBorder radius="md" padding="lg">
        <${Title} order=${3} mb="md">All Sites<//>
        ${makeTable(
          ["Seq", "Name"],
          sites.map((site) => [site.seq, site.name]),
        )}
      <//>
    `;
  }

  if (page === "users") {
    return html`
      <${Card} withBorder radius="md" padding="lg">
        <${Title} order=${3} mb="md">Site Users (current host)<//>
        ${makeTable(
          ["User", "Email", "Nickname", "Created"],
          users.map((user) => [user.user, user.email, user.nickname, user.created]),
        )}
      <//>
    `;
  }

  return html`
    <${Stack} gap="lg">
      <${Card} withBorder radius="md" padding="lg">
        <${Title} order=${3} mb="md">Sites<//>
        ${makeTable(
          ["Seq", "Name"],
          sites.map((site) => [site.seq, site.name]),
        )}
      <//>
      <${Card} withBorder radius="md" padding="lg">
        <${Title} order=${3} mb="md">Site Users (current host)<//>
        ${makeTable(
          ["User", "Email", "Nickname", "Created"],
          users.map((user) => [user.user, user.email, user.nickname, user.created]),
        )}
      <//>
    <//>
  `;
}

function AdminApp({ page }) {
  return html`
    <${MantineProvider} defaultColorScheme="light">
      <${AppShell}
        padding="md"
        navbar=${{
          width: 240,
          breakpoint: "sm",
        }}
      >
        <${AppShell.Navbar} p="md">
          <${Navigation} activePage=${page} />
        <//>
        <${AppShell.Main}>
          <${Stack} gap="md">
            <${Group} justify="space-between" align="center">
              <${Title} order=${2}>Admin<//>
              <${Badge} variant="light" color="blue" size="lg">Mantine Enabled<//>
            <//>
            <${AdminContent} page=${page} />
          <//>
        <//>
      <//>
    <//>
  `;
}

function pageLoad() {
  logInfo("pageLoad:start", window.location.pathname);

  const rootElement = document.getElementById("main");
  if (!rootElement) {
    logError("pageLoad:no-root", "#main not found");
    return;
  }

  const page = rootElement.dataset.adminPage || "dashboard";
  logInfo("pageLoad:render", { page });

  const root = createRoot(rootElement);
  root.render(html`<${AdminApp} page=${page} />`);
}

window.addEventListener("error", (event) => {
  logError("window:error", event.message, event.error);
});

window.addEventListener("unhandledrejection", (event) => {
  logError("window:unhandledrejection", event.reason);
});

window.addEventListener("DOMContentLoaded", pageLoad);
