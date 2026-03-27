// app/assets/js/admin.jsx
import React, { useCallback, useEffect, useMemo, useState } from "react";
import { createRoot } from "react-dom/client";
import {
  MantineProvider,
  AppShell,
  Badge,
  Button,
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
var LOG_PREFIX = "[AdminUI]";
function logInfo(...args) {
  console.log(LOG_PREFIX, ...args);
}
function logError(...args) {
  console.error(LOG_PREFIX, ...args);
}
function routeToPage(pathname) {
  if (pathname === "/admin/sites") {
    return "sites";
  }
  if (pathname === "/admin/site-users") {
    return "users";
  }
  return "dashboard";
}
async function fetchJson(url) {
  logInfo("fetch:start", url);
  const response = await fetch(url, { credentials: "same-origin" });
  if (!response.ok) {
    throw new Error(`HTTP ${response.status}`);
  }
  const data = await response.json();
  logInfo("fetch:success", url, { count: Array.isArray(data) ? data.length : void 0 });
  return data;
}
function useAdminData(page) {
  const [loading, setLoading] = useState(true);
  const [sites, setSites] = useState([]);
  const [users, setUsers] = useState([]);
  const [schedulers, setSchedulers] = useState([]);
  const [runningSchedulerName, setRunningSchedulerName] = useState("");
  const [error, setError] = useState("");

  const reloadSchedulers = useCallback(async () => {
    const data = await fetchJson("/api/admin/schedulers");
    setSchedulers(data);
  }, []);

  const runScheduler = useCallback(async (name) => {
    setRunningSchedulerName(name);
    setError("");
    try {
      await fetchJson(`/api/admin/schedulers/run/${encodeURIComponent(name)}`);
      await reloadSchedulers();
    } catch (caughtError) {
      logError("scheduler:run:error", name, caughtError);
      setError(caughtError.message || String(caughtError));
    } finally {
      setRunningSchedulerName("");
    }
  }, [reloadSchedulers]);

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
            setSchedulers([]);
          }
          return;
        }
        if (page === "users") {
          const data = await fetchJson("/api/admin/site-users");
          if (mounted) {
            setUsers(data);
            setSites([]);
            setSchedulers([]);
          }
          return;
        }

        const [siteData, userData, schedulerData] = await Promise.all([
          fetchJson("/api/admin/sites"),
          fetchJson("/api/admin/site-users"),
          fetchJson("/api/admin/schedulers")
        ]);
        if (mounted) {
          setSites(siteData);
          setUsers(userData);
          setSchedulers(schedulerData);
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

  return { loading, sites, users, schedulers, runningSchedulerName, runScheduler, reloadSchedulers, error };
}
function makeTable(headers, rows) {
  return /* @__PURE__ */ React.createElement(Table, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true }, /* @__PURE__ */ React.createElement(Table.Thead, null, /* @__PURE__ */ React.createElement(Table.Tr, null, headers.map((header) => /* @__PURE__ */ React.createElement(Table.Th, { key: header }, header)))), /* @__PURE__ */ React.createElement(Table.Tbody, null, rows.map((columns, rowIndex) => /* @__PURE__ */ React.createElement(Table.Tr, { key: `row-${rowIndex}` }, columns.map((column, colIndex) => /* @__PURE__ */ React.createElement(Table.Td, { key: `col-${rowIndex}-${colIndex}` }, String(column ?? "")))))));
}
function Navigation({ activePage, onNavigate }) {
  const links = useMemo(
    () => [
      { href: "/", label: "Home", key: "home" },
      { href: "/admin", label: "Dashboard", key: "dashboard" },
      { href: "/admin/sites", label: "All Sites", key: "sites" },
      { href: "/admin/site-users", label: "Site Users", key: "users" }
    ],
    []
  );
  return /* @__PURE__ */ React.createElement(Stack, { gap: 4 }, links.map((link) => /* @__PURE__ */ React.createElement(
    NavLink,
    {
      key: link.key,
      href: link.href,
      label: link.label,
      active: activePage === link.key,
      variant: "light",
      onClick: (event) => {
        if (link.key === "home") {
          return;
        }
        event.preventDefault();
        onNavigate(link.href);
      }
    }
  )));
}
function SchedulerControls({ schedulers, runningSchedulerName, onRun, onRefresh }) {
  return /* @__PURE__ */ React.createElement(Stack, { gap: "sm" },
    /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "center" },
      /* @__PURE__ */ React.createElement(Title, { order: 4 }, "Schedulers"),
      /* @__PURE__ */ React.createElement(Button, { size: "xs", variant: "light", onClick: onRefresh }, "Refresh")
    ),
    ...schedulers.map((scheduler) => /* @__PURE__ */ React.createElement(Group, { key: scheduler.name, justify: "space-between", align: "center" },
      /* @__PURE__ */ React.createElement(Stack, { gap: 2 },
        /* @__PURE__ */ React.createElement(Text, { fw: 600 }, scheduler.name),
        /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, `interval ${scheduler.minSeconds}s ~ ${scheduler.maxSeconds}s | next ${scheduler.nextDelaySeconds ?? "-"}s | last ${scheduler.lastResult ?? "-"}`)
      ),
      /* @__PURE__ */ React.createElement(Button, {
        size: "xs",
        loading: runningSchedulerName === scheduler.name,
        disabled: scheduler.running,
        onClick: () => onRun(scheduler.name)
      }, scheduler.running ? "Running..." : "Run now")
    ))
  );
}
function AdminContent({ page }) {
  const { loading, sites, users, schedulers, runningSchedulerName, runScheduler, reloadSchedulers, error } = useAdminData(page);
  if (loading) {
    return /* @__PURE__ */ React.createElement(Paper, { p: "xl", withBorder: true, radius: "md", shadow: "xs" }, /* @__PURE__ */ React.createElement(Stack, { align: "center", gap: "xs", py: "xl" }, /* @__PURE__ */ React.createElement(Loader, { size: "lg", color: "blue", type: "dots" }), /* @__PURE__ */ React.createElement(Title, { order: 4, c: "dark" }, "Admin 데이터를 준비하고 있어요"), /* @__PURE__ */ React.createElement(Text, { c: "dimmed", size: "sm" }, "페이지가 곧 표시됩니다. 잠시만 기다려 주세요.")));
  }
  if (error) {
    return /* @__PURE__ */ React.createElement(Paper, { p: "lg", withBorder: true, radius: "md" }, /* @__PURE__ */ React.createElement(Text, { c: "red", fw: 600 }, "\uD074\uB77C\uC774\uC5B8\uD2B8 \uB80C\uB354\uB9C1 \uC624\uB958: ", error));
  }
  if (page === "sites") {
    return /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Title, { order: 3, mb: "md" }, "All Sites"), makeTable(
      ["Seq", "Name"],
      sites.map((site) => [site.seq, site.name])
    ));
  }
  if (page === "users") {
    return /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Title, { order: 3, mb: "md" }, "Site Users (current host)"), makeTable(
      ["User", "Email", "Nickname", "Created"],
      users.map((user) => [user.user, user.email, user.nickname, user.created])
    ));
  }
  return /* @__PURE__ */ React.createElement(Stack, { gap: "lg" }, /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Title, { order: 3, mb: "md" }, "Sites"), makeTable(
    ["Seq", "Name"],
    sites.map((site) => [site.seq, site.name])
  )), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Title, { order: 3, mb: "md" }, "Site Users (current host)"), makeTable(
    ["User", "Email", "Nickname", "Created"],
    users.map((user) => [user.user, user.email, user.nickname, user.created])
  )), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" },
    /* @__PURE__ */ React.createElement(SchedulerControls, {
      schedulers,
      runningSchedulerName,
      onRun: runScheduler,
      onRefresh: reloadSchedulers
    })
  ));
}
function AdminApp({ initialPage }) {
  const [page, setPage] = useState(initialPage);
  useEffect(() => {
    const onPopState = () => {
      setPage(routeToPage(window.location.pathname));
    };
    window.addEventListener("popstate", onPopState);
    return () => {
      window.removeEventListener("popstate", onPopState);
    };
  }, []);
  const onNavigate = useCallback(
    (href) => {
      if (window.location.pathname !== href) {
        window.history.pushState({}, "", href);
      }
      setPage(routeToPage(href));
    },
    []
  );
  return /* @__PURE__ */ React.createElement(MantineProvider, { defaultColorScheme: "light" }, /* @__PURE__ */ React.createElement(
    AppShell,
    {
      padding: "md",
      navbar: {
        width: 240,
        breakpoint: "sm"
      }
    },
    /* @__PURE__ */ React.createElement(AppShell.Navbar, { p: "md" }, /* @__PURE__ */ React.createElement(Navigation, { activePage: page, onNavigate })),
    /* @__PURE__ */ React.createElement(AppShell.Main, null, /* @__PURE__ */ React.createElement(Stack, { gap: "md" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "center" }, /* @__PURE__ */ React.createElement(Title, { order: 2 }, "Admin"), /* @__PURE__ */ React.createElement(Badge, { variant: "light", color: "blue", size: "lg" }, "Mantine Enabled")), /* @__PURE__ */ React.createElement(AdminContent, { page })))
  ));
}
function pageLoad() {
  logInfo("pageLoad:start", window.location.pathname);
  const rootElement = document.getElementById("main");
  if (!rootElement) {
    logError("pageLoad:no-root", "#main not found");
    return;
  }
  const page = routeToPage(window.location.pathname);
  logInfo("pageLoad:render", { page });
  const root = createRoot(rootElement);
  root.render(/* @__PURE__ */ React.createElement(AdminApp, { initialPage: page }));
}
window.addEventListener("error", (event) => {
  logError("window:error", event.message, event.error);
});
window.addEventListener("unhandledrejection", (event) => {
  logError("window:unhandledrejection", event.reason);
});
window.addEventListener("DOMContentLoaded", pageLoad);
