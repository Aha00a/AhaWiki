import React, { useEffect, useMemo, useState } from "react";
import { createRoot } from "react-dom/client";
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
  const head = React.createElement(
    Table.Thead,
    null,
    React.createElement(
      Table.Tr,
      null,
      ...headers.map((header) => React.createElement(Table.Th, { key: header }, header)),
    ),
  );

  const bodyRows = rows.map((columns, rowIndex) =>
    React.createElement(
      Table.Tr,
      { key: `row-${rowIndex}` },
      ...columns.map((column, colIndex) =>
        React.createElement(Table.Td, { key: `col-${rowIndex}-${colIndex}` }, String(column ?? "")),
      ),
    ),
  );

  const body = React.createElement(Table.Tbody, null, ...bodyRows);

  return React.createElement(
    Table,
    { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true },
    head,
    body,
  );
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

  return React.createElement(
    Stack,
    { gap: 4 },
    ...links.map((link) =>
      React.createElement(NavLink, {
        key: link.key,
        href: link.href,
        label: link.label,
        active: activePage === link.key,
        variant: "light",
      }),
    ),
  );
}

function AdminContent({ page }) {
  const { loading, sites, users, error } = useAdminData(page);

  if (loading) {
    return React.createElement(
      Paper,
      { p: "lg", withBorder: true, radius: "md" },
      React.createElement(Group, { gap: "sm" }, React.createElement(Loader, { size: "sm" }), React.createElement(Text, null, "데이터를 불러오는 중...")),
    );
  }

  if (error) {
    return React.createElement(
      Paper,
      { p: "lg", withBorder: true, radius: "md" },
      React.createElement(Text, { c: "red", fw: 600 }, `클라이언트 렌더링 오류: ${error}`),
    );
  }

  if (page === "sites") {
    return React.createElement(
      Card,
      { withBorder: true, radius: "md", padding: "lg" },
      React.createElement(Title, { order: 3, mb: "md" }, "All Sites"),
      makeTable(
        ["Seq", "Name"],
        sites.map((site) => [site.seq, site.name]),
      ),
    );
  }

  if (page === "users") {
    return React.createElement(
      Card,
      { withBorder: true, radius: "md", padding: "lg" },
      React.createElement(Title, { order: 3, mb: "md" }, "Site Users (current host)"),
      makeTable(
        ["User", "Email", "Nickname", "Created"],
        users.map((user) => [user.user, user.email, user.nickname, user.created]),
      ),
    );
  }

  return React.createElement(
    Stack,
    { gap: "lg" },
    React.createElement(
      Card,
      { withBorder: true, radius: "md", padding: "lg" },
      React.createElement(Title, { order: 3, mb: "md" }, "Sites"),
      makeTable(
        ["Seq", "Name"],
        sites.map((site) => [site.seq, site.name]),
      ),
    ),
    React.createElement(
      Card,
      { withBorder: true, radius: "md", padding: "lg" },
      React.createElement(Title, { order: 3, mb: "md" }, "Site Users (current host)"),
      makeTable(
        ["User", "Email", "Nickname", "Created"],
        users.map((user) => [user.user, user.email, user.nickname, user.created]),
      ),
    ),
  );
}

function AdminApp({ page }) {
  return React.createElement(
    MantineProvider,
    { defaultColorScheme: "light" },
    React.createElement(
      AppShell,
      {
        padding: "md",
        navbar: {
          width: 240,
          breakpoint: "sm",
        },
      },
      React.createElement(
        AppShell.Navbar,
        { p: "md" },
        React.createElement(Navigation, { activePage: page }),
      ),
      React.createElement(
        AppShell.Main,
        null,
        React.createElement(
          Stack,
          { gap: "md" },
          React.createElement(
            Group,
            { justify: "space-between", align: "center" },
            React.createElement(Title, { order: 2 }, "Admin"),
            React.createElement(Badge, { variant: "light", color: "blue", size: "lg" }, "Mantine Enabled"),
          ),
          React.createElement(AdminContent, { page }),
        ),
      ),
    ),
  );
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
  root.render(React.createElement(AdminApp, { page }));
}

window.addEventListener("error", (event) => {
  logError("window:error", event.message, event.error);
});

window.addEventListener("unhandledrejection", (event) => {
  logError("window:unhandledrejection", event.reason);
});

window.addEventListener("DOMContentLoaded", pageLoad);
