// app/assets/js/admin.jsx
import React, { useCallback, useEffect, useMemo, useState } from "react";
import { createRoot } from "react-dom/client";
import {
  MantineProvider,
  AppShell,
  Badge,
  Button,
  Card,
  Divider,
  Group,
  Loader,
  NavLink,
  Paper,
  Progress,
  SimpleGrid,
  Stack,
  Table,
  Text,
  ThemeIcon,
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
  const [dailyStats, setDailyStats] = useState({
    userCreated: [],
    siteUserCreated: [],
    pageCreated: [],
    pageEdited: []
  });
  const [runningSchedulerName, setRunningSchedulerName] = useState("");
  const [error, setError] = useState("");
  const loadDashboard = useCallback(async () => {
    const [siteData, userData, schedulerData, dailyStatsData] = await Promise.all([
      fetchJson("/api/admin/sites"),
      fetchJson("/api/admin/site-users"),
      fetchJson("/api/admin/schedulers"),
      fetchJson("/api/admin/daily-stats")
    ]);
    setSites(siteData);
    setUsers(userData);
    setSchedulers(schedulerData);
    setDailyStats({
      userCreated: dailyStatsData?.userCreated ?? [],
      siteUserCreated: dailyStatsData?.siteUserCreated ?? [],
      pageCreated: dailyStatsData?.pageCreated ?? [],
      pageEdited: dailyStatsData?.pageEdited ?? []
    });
  }, []);
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
            setDailyStats({
              userCreated: [],
              siteUserCreated: [],
              pageCreated: [],
              pageEdited: []
            });
          }
          return;
        }
        if (page === "users") {
          const data = await fetchJson("/api/admin/site-users");
          if (mounted) {
            setUsers(data);
            setSites([]);
            setSchedulers([]);
            setDailyStats({
              userCreated: [],
              siteUserCreated: [],
              pageCreated: [],
              pageEdited: []
            });
          }
          return;
        }
        if (mounted) {
          await loadDashboard();
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
  }, [page, loadDashboard]);
  return { loading, sites, users, schedulers, dailyStats, runningSchedulerName, runScheduler, reloadSchedulers, error };
}
function makeTable(headers, rows) {
  return /* @__PURE__ */ React.createElement(Table, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true, stickyHeader: true, stickyHeaderOffset: 0 }, /* @__PURE__ */ React.createElement(Table.Thead, null, /* @__PURE__ */ React.createElement(Table.Tr, null, headers.map((header) => /* @__PURE__ */ React.createElement(Table.Th, { key: header }, header)))), /* @__PURE__ */ React.createElement(Table.Tbody, null, rows.map((columns, rowIndex) => /* @__PURE__ */ React.createElement(Table.Tr, { key: `row-${rowIndex}` }, columns.map((column, colIndex) => /* @__PURE__ */ React.createElement(Table.Td, { key: `col-${rowIndex}-${colIndex}` }, String(column ?? "")))))));
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
  return /* @__PURE__ */ React.createElement(Stack, { gap: 8 }, /* @__PURE__ */ React.createElement(Text, { size: "xs", tt: "uppercase", fw: 700, c: "dimmed", px: 8 }, "Admin Navigation"), links.map((link) => /* @__PURE__ */ React.createElement(
    NavLink,
    {
      key: link.key,
      href: link.href,
      label: link.label,
      active: activePage === link.key,
      variant: activePage === link.key ? "filled" : "light",
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
function SchedulerTable({ schedulers, runningSchedulerName, onRun, onRefresh }) {
  return /* @__PURE__ */ React.createElement(Stack, { gap: "sm" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between" }, /* @__PURE__ */ React.createElement(Title, { order: 4 }, "Schedulers"), /* @__PURE__ */ React.createElement(Button, { size: "xs", variant: "light", onClick: onRefresh }, "Refresh")), /* @__PURE__ */ React.createElement(
    Progress,
    {
      size: "sm",
      value: schedulers.length === 0 ? 0 : schedulers.filter((scheduler) => scheduler.running).length / schedulers.length * 100,
      color: "blue",
      radius: "xl"
    }
  ), /* @__PURE__ */ React.createElement(Table, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true }, /* @__PURE__ */ React.createElement(Table.Thead, null, /* @__PURE__ */ React.createElement(Table.Tr, null, /* @__PURE__ */ React.createElement(Table.Th, null, "Name"), /* @__PURE__ */ React.createElement(Table.Th, null, "Interval"), /* @__PURE__ */ React.createElement(Table.Th, null, "Next Delay(s)"), /* @__PURE__ */ React.createElement(Table.Th, null, "Last Started"), /* @__PURE__ */ React.createElement(Table.Th, null, "Last Finished"), /* @__PURE__ */ React.createElement(Table.Th, null, "Result"), /* @__PURE__ */ React.createElement(Table.Th, null, "Run Count"), /* @__PURE__ */ React.createElement(Table.Th, null, "Action"))), /* @__PURE__ */ React.createElement(Table.Tbody, null, schedulers.map((scheduler) => /* @__PURE__ */ React.createElement(Table.Tr, { key: scheduler.name }, /* @__PURE__ */ React.createElement(Table.Td, null, scheduler.name), /* @__PURE__ */ React.createElement(Table.Td, null, `${scheduler.minSeconds}s ~ ${scheduler.maxSeconds}s`), /* @__PURE__ */ React.createElement(Table.Td, null, scheduler.nextDelaySeconds ?? "-"), /* @__PURE__ */ React.createElement(Table.Td, null, scheduler.lastStartedAt ?? "-"), /* @__PURE__ */ React.createElement(Table.Td, null, scheduler.lastFinishedAt ?? "-"), /* @__PURE__ */ React.createElement(Table.Td, null, scheduler.lastResult ?? "-"), /* @__PURE__ */ React.createElement(Table.Td, null, scheduler.runCount ?? 0), /* @__PURE__ */ React.createElement(Table.Td, null, /* @__PURE__ */ React.createElement(
    Button,
    {
      size: "xs",
      variant: "filled",
      loading: runningSchedulerName === scheduler.name,
      disabled: scheduler.running,
      onClick: () => onRun(scheduler.name)
    },
    scheduler.running ? "Running..." : "Run now"
  )))))));
}
function DailyStatTable({ title, description, rows, badgeColor }) {
  return /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, title), /* @__PURE__ */ React.createElement(Badge, { color: badgeColor, variant: "light" }, rows.length, " days")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, description), makeTable(
    ["Date", "Count"],
    rows.map((row) => [row.ymd, row.count])
  ));
}
function AdminContent({ page }) {
  const { loading, sites, users, schedulers, dailyStats, runningSchedulerName, runScheduler, reloadSchedulers, error } = useAdminData(page);
  if (loading) {
    return /* @__PURE__ */ React.createElement(Paper, { p: "xl", withBorder: true, radius: "md", shadow: "xs" }, /* @__PURE__ */ React.createElement(Stack, { align: "center", gap: "xs", py: "xl" }, /* @__PURE__ */ React.createElement(Loader, { size: "lg", color: "blue", type: "dots" }), /* @__PURE__ */ React.createElement(Title, { order: 4, c: "dark" }, "Admin \uB370\uC774\uD130\uB97C \uC900\uBE44\uD558\uACE0 \uC788\uC5B4\uC694"), /* @__PURE__ */ React.createElement(Text, { c: "dimmed", size: "sm" }, "\uD398\uC774\uC9C0\uAC00 \uACE7 \uD45C\uC2DC\uB429\uB2C8\uB2E4. \uC7A0\uC2DC\uB9CC \uAE30\uB2E4\uB824 \uC8FC\uC138\uC694.")));
  }
  if (error) {
    return /* @__PURE__ */ React.createElement(Paper, { p: "lg", withBorder: true, radius: "md" }, /* @__PURE__ */ React.createElement(Text, { c: "red", fw: 600 }, "\uD074\uB77C\uC774\uC5B8\uD2B8 \uB80C\uB354\uB9C1 \uC624\uB958: ", error));
  }
  if (page === "sites") {
    return /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "All Sites"), /* @__PURE__ */ React.createElement(Badge, { color: "indigo", variant: "light" }, sites.length, " sites")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uC804\uCCB4 \uC0AC\uC774\uD2B8 \uBAA9\uB85D\uACFC \uD568\uAED8 \uB3C4\uBA54\uC778, \uC0AC\uC6A9\uC790 \uC218, \uD398\uC774\uC9C0 \uC218\uB97C \uD655\uC778\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."), /* @__PURE__ */ React.createElement(Divider, { mb: "md" }), /* @__PURE__ */ React.createElement(Title, { order: 6, c: "dimmed", mb: "sm" }, "All Sites"), makeTable(
      ["Seq", "Name", "Domains", "Users", "Pages"],
      sites.map((site) => [
        site.seq,
        site.name,
        (site.domains ?? []).join(", "),
        site.userCount ?? 0,
        site.pageCount ?? 0
      ])
    ));
  }
  if (page === "users") {
    return /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "Site Users"), /* @__PURE__ */ React.createElement(Badge, { color: "teal", variant: "light" }, users.length, " users")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uD604\uC7AC \uD638\uC2A4\uD2B8 \uAE30\uC900 \uC0AC\uC6A9\uC790 \uBAA9\uB85D\uC785\uB2C8\uB2E4."), /* @__PURE__ */ React.createElement(Divider, { mb: "md" }), makeTable(
      ["User", "Email", "Nickname", "Created"],
      users.map((user) => [user.user, user.email, user.nickname, user.created])
    ));
  }
  return /* @__PURE__ */ React.createElement(Stack, { gap: "lg" }, /* @__PURE__ */ React.createElement(SimpleGrid, { cols: { base: 1, sm: 3 }, spacing: "md" }, /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "flex-start" }, /* @__PURE__ */ React.createElement(Stack, { gap: 2 }, /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "Sites"), /* @__PURE__ */ React.createElement(Title, { order: 2 }, sites.length)), /* @__PURE__ */ React.createElement(ThemeIcon, { color: "indigo", variant: "light", radius: "xl" }, "S"))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "flex-start" }, /* @__PURE__ */ React.createElement(Stack, { gap: 2 }, /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "Users"), /* @__PURE__ */ React.createElement(Title, { order: 2 }, users.length)), /* @__PURE__ */ React.createElement(ThemeIcon, { color: "teal", variant: "light", radius: "xl" }, "U"))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "flex-start" }, /* @__PURE__ */ React.createElement(Stack, { gap: 2 }, /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "Schedulers"), /* @__PURE__ */ React.createElement(Title, { order: 2 }, schedulers.length)), /* @__PURE__ */ React.createElement(ThemeIcon, { color: "blue", variant: "light", radius: "xl" }, "R")))), /* @__PURE__ */ React.createElement(SimpleGrid, { cols: { base: 1, sm: 2, lg: 4 }, spacing: "md" }, /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "New Users (30d)"), /* @__PURE__ */ React.createElement(Title, { order: 3 }, dailyStats.userCreated.reduce((sum, item) => sum + (item.count ?? 0), 0))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "Site User Joins (30d)"), /* @__PURE__ */ React.createElement(Title, { order: 3 }, dailyStats.siteUserCreated.reduce((sum, item) => sum + (item.count ?? 0), 0))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "New Pages (30d)"), /* @__PURE__ */ React.createElement(Title, { order: 3 }, dailyStats.pageCreated.reduce((sum, item) => sum + (item.count ?? 0), 0))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "Page Edits (30d)"), /* @__PURE__ */ React.createElement(Title, { order: 3 }, dailyStats.pageEdited.reduce((sum, item) => sum + (item.count ?? 0), 0)))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "Sites"), /* @__PURE__ */ React.createElement(Badge, { color: "indigo", variant: "light" }, sites.length)), makeTable(
    ["Seq", "Name", "Domains", "Users", "Pages"],
    sites.map((site) => [
      site.seq,
      site.name,
      (site.domains ?? []).join(", "),
      site.userCount ?? 0,
      site.pageCount ?? 0
    ])
  )), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "Site Users (current host)"), /* @__PURE__ */ React.createElement(Badge, { color: "teal", variant: "light" }, users.length)), makeTable(
    ["User", "Email", "Nickname", "Created"],
    users.map((user) => [user.user, user.email, user.nickname, user.created])
  )), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(
    SchedulerTable,
    {
      schedulers,
      runningSchedulerName,
      onRun: runScheduler,
      onRefresh: reloadSchedulers
    }
  )), /* @__PURE__ */ React.createElement(
    DailyStatTable,
    {
      title: "Daily New Users",
      description: "\uCD5C\uADFC 30\uC77C \uAE30\uC900 \uC804\uCCB4 \uC0AC\uC6A9\uC790 \uC2E0\uADDC \uC0DD\uC131 \uC218\uC785\uB2C8\uB2E4.",
      rows: dailyStats.userCreated,
      badgeColor: "blue"
    }
  ), /* @__PURE__ */ React.createElement(
    DailyStatTable,
    {
      title: "Daily Site User Joins",
      description: "\uCD5C\uADFC 30\uC77C \uAE30\uC900 \uC0AC\uC774\uD2B8 \uAC00\uC785(UserSite) \uC218\uC785\uB2C8\uB2E4.",
      rows: dailyStats.siteUserCreated,
      badgeColor: "teal"
    }
  ), /* @__PURE__ */ React.createElement(
    DailyStatTable,
    {
      title: "Daily New Pages",
      description: "\uCD5C\uADFC 30\uC77C \uAE30\uC900 revision=1 \uD398\uC774\uC9C0 \uC0DD\uC131 \uC218\uC785\uB2C8\uB2E4.",
      rows: dailyStats.pageCreated,
      badgeColor: "indigo"
    }
  ), /* @__PURE__ */ React.createElement(
    DailyStatTable,
    {
      title: "Daily Page Edits",
      description: "\uCD5C\uADFC 30\uC77C \uAE30\uC900 \uD398\uC774\uC9C0 \uC804\uCCB4 \uC218\uC815(\uBAA8\uB4E0 \uB9AC\uBE44\uC804) \uC218\uC785\uB2C8\uB2E4.",
      rows: dailyStats.pageEdited,
      badgeColor: "grape"
    }
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
  return /* @__PURE__ */ React.createElement(
    MantineProvider,
    {
      defaultColorScheme: "light",
      theme: {
        primaryColor: "indigo",
        defaultRadius: "md"
      }
    },
    /* @__PURE__ */ React.createElement(
      AppShell,
      {
        padding: "md",
        navbar: {
          width: 240,
          breakpoint: "sm"
        }
      },
      /* @__PURE__ */ React.createElement(AppShell.Navbar, { p: "md" }, /* @__PURE__ */ React.createElement(Stack, { mb: "md", gap: 4 }, /* @__PURE__ */ React.createElement(Text, { fw: 700, size: "lg" }, "AhaWiki"), /* @__PURE__ */ React.createElement(Text, { size: "xs", c: "dimmed" }, "\uAD00\uB9AC\uC790 \uCF58\uC194")), /* @__PURE__ */ React.createElement(Navigation, { activePage: page, onNavigate })),
      /* @__PURE__ */ React.createElement(AppShell.Main, null, /* @__PURE__ */ React.createElement(Stack, { gap: "md" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "center" }, /* @__PURE__ */ React.createElement(Stack, { gap: 2 }, /* @__PURE__ */ React.createElement(Title, { order: 2 }, "Admin Dashboard"), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "\uC6B4\uC601 \uD604\uD669\uC744 \uD55C\uB208\uC5D0 \uD655\uC778\uD558\uACE0 \uC989\uC2DC \uC791\uC5C5\uD558\uC138\uC694.")), /* @__PURE__ */ React.createElement(Badge, { variant: "light", color: "indigo", size: "lg" }, "Live")), /* @__PURE__ */ React.createElement(AdminContent, { page })))
    )
  );
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
