// app/assets/js/admin.jsx
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
  Title
} from "@mantine/core";
import { jsxDEV } from "react/jsx-dev-runtime";
var LOG_PREFIX = "[AdminUI]";
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
          fetchJson("/api/admin/site-users")
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
  return /* @__PURE__ */ jsxDEV(Table, {
    striped: true,
    highlightOnHover: true,
    withTableBorder: true,
    withColumnBorders: true,
    children: [
      /* @__PURE__ */ jsxDEV(Table.Thead, {
        children: /* @__PURE__ */ jsxDEV(Table.Tr, {
          children: headers.map((header) => /* @__PURE__ */ jsxDEV(Table.Th, {
            children: header
          }, header, false, undefined, this))
        }, undefined, false, undefined, this)
      }, undefined, false, undefined, this),
      /* @__PURE__ */ jsxDEV(Table.Tbody, {
        children: rows.map((columns, rowIndex) => /* @__PURE__ */ jsxDEV(Table.Tr, {
          children: columns.map((column, colIndex) => /* @__PURE__ */ jsxDEV(Table.Td, {
            children: String(column ?? "")
          }, `col-${rowIndex}-${colIndex}`, false, undefined, this))
        }, `row-${rowIndex}`, false, undefined, this))
      }, undefined, false, undefined, this)
    ]
  }, undefined, true, undefined, this);
}
function Navigation({ activePage }) {
  const links = useMemo(() => [
    { href: "/", label: "Home", key: "home" },
    { href: "/admin", label: "Dashboard", key: "dashboard" },
    { href: "/admin/sites", label: "All Sites", key: "sites" },
    { href: "/admin/site-users", label: "Site Users", key: "users" }
  ], []);
  return /* @__PURE__ */ jsxDEV(Stack, {
    gap: 4,
    children: links.map((link) => /* @__PURE__ */ jsxDEV(NavLink, {
      href: link.href,
      label: link.label,
      active: activePage === link.key,
      variant: "light"
    }, link.key, false, undefined, this))
  }, undefined, false, undefined, this);
}
function AdminContent({ page }) {
  const { loading, sites, users, error } = useAdminData(page);
  if (loading) {
    return /* @__PURE__ */ jsxDEV(Paper, {
      p: "lg",
      withBorder: true,
      radius: "md",
      children: /* @__PURE__ */ jsxDEV(Group, {
        gap: "sm",
        children: [
          /* @__PURE__ */ jsxDEV(Loader, {
            size: "sm"
          }, undefined, false, undefined, this),
          /* @__PURE__ */ jsxDEV(Text, {
            children: "데이터를 불러오는 중..."
          }, undefined, false, undefined, this)
        ]
      }, undefined, true, undefined, this)
    }, undefined, false, undefined, this);
  }
  if (error) {
    return /* @__PURE__ */ jsxDEV(Paper, {
      p: "lg",
      withBorder: true,
      radius: "md",
      children: /* @__PURE__ */ jsxDEV(Text, {
        c: "red",
        fw: 600,
        children: [
          "클라이언트 렌더링 오류: ",
          error
        ]
      }, undefined, true, undefined, this)
    }, undefined, false, undefined, this);
  }
  if (page === "sites") {
    return /* @__PURE__ */ jsxDEV(Card, {
      withBorder: true,
      radius: "md",
      padding: "lg",
      children: [
        /* @__PURE__ */ jsxDEV(Title, {
          order: 3,
          mb: "md",
          children: "All Sites"
        }, undefined, false, undefined, this),
        makeTable(["Seq", "Name"], sites.map((site) => [site.seq, site.name]))
      ]
    }, undefined, true, undefined, this);
  }
  if (page === "users") {
    return /* @__PURE__ */ jsxDEV(Card, {
      withBorder: true,
      radius: "md",
      padding: "lg",
      children: [
        /* @__PURE__ */ jsxDEV(Title, {
          order: 3,
          mb: "md",
          children: "Site Users (current host)"
        }, undefined, false, undefined, this),
        makeTable(["User", "Email", "Nickname", "Created"], users.map((user) => [user.user, user.email, user.nickname, user.created]))
      ]
    }, undefined, true, undefined, this);
  }
  return /* @__PURE__ */ jsxDEV(Stack, {
    gap: "lg",
    children: [
      /* @__PURE__ */ jsxDEV(Card, {
        withBorder: true,
        radius: "md",
        padding: "lg",
        children: [
          /* @__PURE__ */ jsxDEV(Title, {
            order: 3,
            mb: "md",
            children: "Sites"
          }, undefined, false, undefined, this),
          makeTable(["Seq", "Name"], sites.map((site) => [site.seq, site.name]))
        ]
      }, undefined, true, undefined, this),
      /* @__PURE__ */ jsxDEV(Card, {
        withBorder: true,
        radius: "md",
        padding: "lg",
        children: [
          /* @__PURE__ */ jsxDEV(Title, {
            order: 3,
            mb: "md",
            children: "Site Users (current host)"
          }, undefined, false, undefined, this),
          makeTable(["User", "Email", "Nickname", "Created"], users.map((user) => [user.user, user.email, user.nickname, user.created]))
        ]
      }, undefined, true, undefined, this)
    ]
  }, undefined, true, undefined, this);
}
function AdminApp({ page }) {
  return /* @__PURE__ */ jsxDEV(MantineProvider, {
    defaultColorScheme: "light",
    children: /* @__PURE__ */ jsxDEV(AppShell, {
      padding: "md",
      navbar: {
        width: 240,
        breakpoint: "sm"
      },
      children: [
        /* @__PURE__ */ jsxDEV(AppShell.Navbar, {
          p: "md",
          children: /* @__PURE__ */ jsxDEV(Navigation, {
            activePage: page
          }, undefined, false, undefined, this)
        }, undefined, false, undefined, this),
        /* @__PURE__ */ jsxDEV(AppShell.Main, {
          children: /* @__PURE__ */ jsxDEV(Stack, {
            gap: "md",
            children: [
              /* @__PURE__ */ jsxDEV(Group, {
                justify: "space-between",
                align: "center",
                children: [
                  /* @__PURE__ */ jsxDEV(Title, {
                    order: 2,
                    children: "Admin"
                  }, undefined, false, undefined, this),
                  /* @__PURE__ */ jsxDEV(Badge, {
                    variant: "light",
                    color: "blue",
                    size: "lg",
                    children: "Mantine Enabled"
                  }, undefined, false, undefined, this)
                ]
              }, undefined, true, undefined, this),
              /* @__PURE__ */ jsxDEV(AdminContent, {
                page
              }, undefined, false, undefined, this)
            ]
          }, undefined, true, undefined, this)
        }, undefined, false, undefined, this)
      ]
    }, undefined, true, undefined, this)
  }, undefined, false, undefined, this);
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
  root.render(/* @__PURE__ */ jsxDEV(AdminApp, {
    page
  }, undefined, false, undefined, this));
}
window.addEventListener("error", (event) => {
  logError("window:error", event.message, event.error);
});
window.addEventListener("unhandledrejection", (event) => {
  logError("window:unhandledrejection", event.reason);
});
window.addEventListener("DOMContentLoaded", pageLoad);
