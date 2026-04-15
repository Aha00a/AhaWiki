// app/assets/js/admin.jsx
import React, { useCallback, useEffect, useMemo, useState } from "react";
import { createRoot } from "react-dom/client";
import {
  MantineProvider,
  Anchor,
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
  TextInput,
  ThemeIcon,
  Title
} from "@mantine/core";
import {
  Area,
  AreaChart,
  CartesianGrid,
  Legend,
  Line,
  LineChart,
  ResponsiveContainer,
  Tooltip,
  XAxis,
  YAxis
} from "recharts";
var LOG_PREFIX = "[AdminUI]";
function logInfo(...args) {
  console.log(LOG_PREFIX, ...args);
}
function logError(...args) {
  console.error(LOG_PREFIX, ...args);
}
function routeToPage(pathname) {
  if (pathname === "/Admin/User/UserViewHistory") {
    return "user-views";
  }
  if (pathname === "/Admin/Site") {
    return "sites";
  }
  if (pathname === "/Admin/SiteUser") {
    return "users";
  }
  if (pathname === "/Admin/User") {
    return "all-users";
  }
  if (pathname === "/Admin/Operation") {
    return "operations";
  }
  if (pathname === "/Admin/RecentChange") {
    return "recent-changes";
  }
  if (pathname === "/Admin/Sites") {
    return "sites";
  }
  if (pathname === "/Admin/SiteUsers") {
    return "users";
  }
  if (pathname === "/Admin/AllUsers") {
    return "all-users";
  }
  if (pathname === "/Admin/UserViews") {
    return "user-views";
  }
  if (pathname === "/Admin/Operations") {
    return "operations";
  }
  if (pathname === "/Admin/RecentChanges") {
    return "recent-changes";
  }
  return "dashboard";
}
function parseUserSeqFromPathname(pathname) {
  if (pathname !== "/Admin/User/UserViewHistory") {
    return 0;
  }
  const params = new URLSearchParams(window.location.search);
  const userSeqBySeq = Number.parseInt(params.get("seq") ?? "", 10);
  if (Number.isFinite(userSeqBySeq) && userSeqBySeq > 0) {
    return userSeqBySeq;
  }
  const userSeqByLegacyQuery = Number.parseInt(params.get("userSeq") ?? "", 10);
  return Number.isFinite(userSeqByLegacyQuery) && userSeqByLegacyQuery > 0 ? userSeqByLegacyQuery : 0;
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
async function fetchCsrfToken() {
  const response = await fetch("/api/csrf", { credentials: "same-origin" });
  if (!response.ok) {
    throw new Error(`CSRF HTTP ${response.status}`);
  }
  const token = await response.json();
  return token?.value ?? "";
}
function useAdminData(page) {
  const [loading, setLoading] = useState(true);
  const [sites, setSites] = useState([]);
  const [users, setUsers] = useState([]);
  const [allUsers, setAllUsers] = useState([]);
  const [schedulers, setSchedulers] = useState([]);
  const [dailyStats, setDailyStats] = useState({
    userCreated: [],
    siteUserCreated: [],
    pageCreated: [],
    pageEdited: []
  });
  const [recentChanges, setRecentChanges] = useState([]);
  const [topViewedPages, setTopViewedPages] = useState([]);
  const [userViewHistories, setUserViewHistories] = useState([]);
  const [loadingUserViewHistories, setLoadingUserViewHistories] = useState(false);
  const [runningSchedulerName, setRunningSchedulerName] = useState("");
  const [clearingSiteSeq, setClearingSiteSeq] = useState(0);
  const [error, setError] = useState("");
  const loadRecentChanges = useCallback(async (n = 50) => {
    const data = await fetchJson(`/api/Admin/RecentChanges?n=${encodeURIComponent(n)}`);
    setRecentChanges(data);
  }, []);
  const loadUserViewHistories = useCallback(async (userSeq, n = 200) => {
    setLoadingUserViewHistories(true);
    try {
      const clampedN = Math.min(1e3, Math.max(1, Number.parseInt(String(n), 10) || 200));
      const data = await fetchJson(
        `/api/Admin/UserViews?userSeq=${encodeURIComponent(userSeq)}&n=${encodeURIComponent(clampedN)}`
      );
      setUserViewHistories(data);
    } finally {
      setLoadingUserViewHistories(false);
    }
  }, []);
  const loadDashboard = useCallback(async () => {
    const [siteData, userData, allUserData, schedulerData, dailyStatsData, recentChangesData, topViewedPagesData] = await Promise.all([
      fetchJson("/api/Admin/Sites"),
      fetchJson("/api/Admin/SiteUsers"),
      fetchJson("/api/Admin/Users"),
      fetchJson("/api/Admin/Schedulers"),
      fetchJson("/api/Admin/DailyStats"),
      fetchJson("/api/Admin/RecentChanges?n=30"),
      fetchJson("/api/Admin/TopViewedPages?n=20")
    ]);
    setSites(siteData);
    setUsers(userData);
    setAllUsers(allUserData);
    setSchedulers(schedulerData);
    setRecentChanges(recentChangesData);
    setTopViewedPages(topViewedPagesData);
    setDailyStats({
      userCreated: dailyStatsData?.userCreated ?? [],
      siteUserCreated: dailyStatsData?.siteUserCreated ?? [],
      pageCreated: dailyStatsData?.pageCreated ?? [],
      pageEdited: dailyStatsData?.pageEdited ?? []
    });
  }, []);
  const reloadSchedulers = useCallback(async () => {
    const data = await fetchJson("/api/Admin/Schedulers");
    setSchedulers(data);
  }, []);
  const runScheduler = useCallback(async (name) => {
    setRunningSchedulerName(name);
    setError("");
    try {
      await fetchJson(`/api/Admin/Schedulers/Run/${encodeURIComponent(name)}`);
      await reloadSchedulers();
    } catch (caughtError) {
      logError("scheduler:run:error", name, caughtError);
      setError(caughtError.message || String(caughtError));
    } finally {
      setRunningSchedulerName("");
    }
  }, [reloadSchedulers]);
  const clearSiteCache = useCallback(async (siteSeq) => {
    setClearingSiteSeq(siteSeq);
    setError("");
    try {
      const csrfToken = await fetchCsrfToken();
      const response = await fetch(`/api/cache/${siteSeq}`, {
        method: "DELETE",
        credentials: "same-origin",
        headers: {
          "X-CSRF-Token": csrfToken
        }
      });
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}`);
      }
    } catch (caughtError) {
      logError("cache:clear:error", siteSeq, caughtError);
      setError(caughtError.message || String(caughtError));
    } finally {
      setClearingSiteSeq(0);
    }
  }, []);
  useEffect(() => {
    let mounted = true;
    const load = async () => {
      setLoading(true);
      setError("");
      try {
        if (page === "sites") {
          const data = await fetchJson("/api/Admin/Sites");
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
            setTopViewedPages([]);
          }
          return;
        }
        if (page === "users") {
          const data = await fetchJson("/api/Admin/SiteUsers");
          if (mounted) {
            setUsers(data);
            setSites([]);
            setAllUsers([]);
            setSchedulers([]);
            setDailyStats({
              userCreated: [],
              siteUserCreated: [],
              pageCreated: [],
              pageEdited: []
            });
            setTopViewedPages([]);
          }
          return;
        }
        if (page === "all-users") {
          const data = await fetchJson("/api/Admin/Users");
          if (mounted) {
            setAllUsers(data);
            setUsers([]);
            setSites([]);
            setSchedulers([]);
            setDailyStats({
              userCreated: [],
              siteUserCreated: [],
              pageCreated: [],
              pageEdited: []
            });
            setTopViewedPages([]);
          }
          return;
        }
        if (page === "user-views") {
          const userSeq = parseUserSeqFromPathname(window.location.pathname);
          if (Number.isFinite(userSeq) && userSeq > 0) {
            await loadUserViewHistories(userSeq, 200);
          } else {
            setUserViewHistories([]);
          }
          if (mounted) {
            setUsers([]);
            setSites([]);
            setAllUsers([]);
            setSchedulers([]);
            setDailyStats({
              userCreated: [],
              siteUserCreated: [],
              pageCreated: [],
              pageEdited: []
            });
            setTopViewedPages([]);
          }
          return;
        }
        if (page === "recent-changes") {
          await loadRecentChanges(50);
          if (mounted) {
            setSites([]);
            setUsers([]);
            setAllUsers([]);
            setSchedulers([]);
            setDailyStats({
              userCreated: [],
              siteUserCreated: [],
              pageCreated: [],
              pageEdited: []
            });
            setTopViewedPages([]);
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
  }, [page, loadDashboard, loadRecentChanges, loadUserViewHistories]);
  return {
    loading,
    sites,
    users,
    allUsers,
    schedulers,
    dailyStats,
    recentChanges,
    topViewedPages,
    userViewHistories,
    loadingUserViewHistories,
    loadUserViewHistories,
    loadRecentChanges,
    runningSchedulerName,
    runScheduler,
    reloadSchedulers,
    clearSiteCache,
    clearingSiteSeq,
    error
  };
}
function makeTable(headers, rows) {
  return /* @__PURE__ */ React.createElement(Table, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true, stickyHeader: true, stickyHeaderOffset: 0 }, /* @__PURE__ */ React.createElement(Table.Thead, null, /* @__PURE__ */ React.createElement(Table.Tr, null, headers.map((header) => /* @__PURE__ */ React.createElement(Table.Th, { key: header }, header)))), /* @__PURE__ */ React.createElement(Table.Tbody, null, rows.map((columns, rowIndex) => /* @__PURE__ */ React.createElement(Table.Tr, { key: `row-${rowIndex}` }, columns.map((column, colIndex) => /* @__PURE__ */ React.createElement(Table.Td, { key: `col-${rowIndex}-${colIndex}` }, column ?? ""))))));
}
function Navigation({ activePage, onNavigate }) {
  const links = useMemo(
    () => [
      { href: "/", label: "Home", key: "home" },
      { href: "/Admin", label: "\uD575\uC2EC \uB300\uC2DC\uBCF4\uB4DC", key: "dashboard" },
      { href: "/Admin/RecentChange", label: "\uCD5C\uADFC \uBCC0\uACBD", key: "recent-changes" },
      { href: "/Admin/User", label: "\uC0AC\uC6A9\uC790 \uAD00\uB9AC", key: "all-users" },
      { href: "/Admin/Operation", label: "\uC6B4\uC601 \uC791\uC5C5", key: "operations" }
    ],
    []
  );
  return /* @__PURE__ */ React.createElement(Stack, { gap: 8 }, /* @__PURE__ */ React.createElement(Text, { size: "xs", tt: "uppercase", fw: 700, c: "dimmed", px: 8 }, "Admin Navigation"), links.map((link) => /* @__PURE__ */ React.createElement(
    NavLink,
    {
      key: link.key,
      href: link.href,
      label: link.label,
      active: activePage === link.key || activePage === "user-views" && link.key === "all-users",
      variant: activePage === link.key || activePage === "user-views" && link.key === "all-users" ? "filled" : "light",
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
function normalizeDailyRows(rows) {
  return [...rows].map((row) => ({
    ymd: row.ymd,
    count: Number(row.count ?? 0)
  })).sort((left, right) => left.ymd > right.ymd ? 1 : -1);
}
function Sparkline({ rows, color }) {
  const data = normalizeDailyRows(rows).slice(-30);
  if (data.length === 0) {
    return /* @__PURE__ */ React.createElement(Text, { size: "xs", c: "dimmed" }, "No data");
  }
  const latest = data[data.length - 1]?.count ?? 0;
  const previous = data[data.length - 2]?.count ?? latest;
  const delta = latest - previous;
  const deltaColor = delta >= 0 ? "teal" : "red";
  return /* @__PURE__ */ React.createElement(Stack, { gap: 4 }, /* @__PURE__ */ React.createElement("div", { style: { width: "100%", height: 72 }, role: "img", "aria-label": "trend sparkline" }, /* @__PURE__ */ React.createElement(ResponsiveContainer, { width: "100%", height: "100%" }, /* @__PURE__ */ React.createElement(AreaChart, { data, margin: { top: 4, right: 0, left: 0, bottom: 0 } }, /* @__PURE__ */ React.createElement("defs", null, /* @__PURE__ */ React.createElement("linearGradient", { id: `sparklineGradient-${color}`, x1: "0", y1: "0", x2: "0", y2: "1" }, /* @__PURE__ */ React.createElement("stop", { offset: "0%", stopColor: `var(--mantine-color-${color}-4)`, stopOpacity: 0.35 }), /* @__PURE__ */ React.createElement("stop", { offset: "100%", stopColor: `var(--mantine-color-${color}-1)`, stopOpacity: 0.1 }))), /* @__PURE__ */ React.createElement(
    Tooltip,
    {
      cursor: false,
      labelFormatter: (value) => `Date: ${value}`,
      formatter: (value) => [value, "Count"]
    }
  ), /* @__PURE__ */ React.createElement(
    Area,
    {
      type: "monotone",
      dataKey: "count",
      stroke: `var(--mantine-color-${color}-6)`,
      strokeWidth: 2,
      fill: `url(#sparklineGradient-${color})`,
      dot: false,
      activeDot: { r: 3 },
      isAnimationActive: false
    }
  )))), /* @__PURE__ */ React.createElement(Group, { justify: "space-between" }, /* @__PURE__ */ React.createElement(Text, { size: "xs", c: "dimmed" }, "\uCD5C\uADFC 30\uC77C"), /* @__PURE__ */ React.createElement(Badge, { color: deltaColor, variant: "light", size: "xs" }, delta >= 0 ? "+" : "", delta, " vs yesterday")));
}
function StatTrendCard({ title, total, rows, color }) {
  return /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React.createElement(Stack, { gap: 8 }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "flex-start" }, /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, title), /* @__PURE__ */ React.createElement(Badge, { color, variant: "light" }, "30d")), /* @__PURE__ */ React.createElement(Title, { order: 3 }, total), /* @__PURE__ */ React.createElement(Sparkline, { rows, color })));
}
function MultiTrendChart({ series }) {
  const dateSet = /* @__PURE__ */ new Set();
  series.forEach((line) => {
    normalizeDailyRows(line.rows).forEach((row) => {
      dateSet.add(row.ymd);
    });
  });
  const dates = [...dateSet].sort().slice(-30);
  if (dates.length === 0) {
    return /* @__PURE__ */ React.createElement(Text, { c: "dimmed", size: "sm" }, "\uCC28\uD2B8 \uB370\uC774\uD130\uAC00 \uC5C6\uC2B5\uB2C8\uB2E4.");
  }
  const colorByName = {};
  const chartDataByDate = new Map(dates.map((date) => [date, { date }]));
  series.forEach((line) => {
    colorByName[line.name] = line.color;
    const indexed = new Map(normalizeDailyRows(line.rows).map((row) => [row.ymd, row.count]));
    dates.forEach((date) => {
      chartDataByDate.get(date)[line.name] = indexed.get(date) ?? 0;
    });
  });
  const chartData = dates.map((date) => chartDataByDate.get(date));
  const xAxisTickFormatter = (value) => value.slice(5);
  return /* @__PURE__ */ React.createElement(Stack, { gap: 8 }, /* @__PURE__ */ React.createElement("div", { style: { width: "100%", height: 280 }, role: "img", "aria-label": "daily trends chart" }, /* @__PURE__ */ React.createElement(ResponsiveContainer, { width: "100%", height: "100%" }, /* @__PURE__ */ React.createElement(LineChart, { data: chartData, margin: { top: 8, right: 12, bottom: 8, left: 0 } }, /* @__PURE__ */ React.createElement(CartesianGrid, { stroke: "var(--mantine-color-gray-2)", strokeDasharray: "3 3" }), /* @__PURE__ */ React.createElement(XAxis, { dataKey: "date", tickFormatter: xAxisTickFormatter, tick: { fontSize: 12 } }), /* @__PURE__ */ React.createElement(YAxis, { allowDecimals: false, tick: { fontSize: 12 } }), /* @__PURE__ */ React.createElement(Tooltip, { labelFormatter: (value) => `Date: ${value}` }), /* @__PURE__ */ React.createElement(Legend, { verticalAlign: "top", height: 30 }), series.map((line) => /* @__PURE__ */ React.createElement(
    Line,
    {
      key: line.name,
      type: "monotone",
      dataKey: line.name,
      stroke: `var(--mantine-color-${colorByName[line.name]}-6)`,
      strokeWidth: 2.5,
      dot: false,
      activeDot: { r: 4 },
      isAnimationActive: false
    }
  ))))), /* @__PURE__ */ React.createElement(Group, { gap: 8 }, series.map((line) => /* @__PURE__ */ React.createElement(Badge, { key: line.name, color: line.color, variant: "light" }, line.name))));
}
function AdminContent({ page, onNavigate }) {
  const {
    loading,
    sites,
    users,
    allUsers,
    schedulers,
    dailyStats,
    recentChanges,
    topViewedPages,
    userViewHistories,
    loadingUserViewHistories,
    loadUserViewHistories,
    loadRecentChanges,
    runningSchedulerName,
    runScheduler,
    reloadSchedulers,
    clearSiteCache,
    clearingSiteSeq,
    error
  } = useAdminData(page);
  const [recentChangeLimitInput, setRecentChangeLimitInput] = useState("50");
  const selectedUserSeq = useMemo(() => {
    const userSeqByPath = parseUserSeqFromPathname(window.location.pathname);
    if (userSeqByPath > 0) {
      return userSeqByPath;
    }
    const params = new URLSearchParams(window.location.search);
    const userSeqByQuery = Number.parseInt(params.get("userSeq") ?? "", 10);
    return Number.isFinite(userSeqByQuery) && userSeqByQuery > 0 ? userSeqByQuery : 0;
  }, [page]);
  const selectedAllUser = useMemo(
    () => allUsers.find((user) => user.seq === selectedUserSeq) ?? null,
    [allUsers, selectedUserSeq]
  );
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
  if (page === "all-users") {
    return /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "All Users"), /* @__PURE__ */ React.createElement(Badge, { color: "blue", variant: "light" }, allUsers.length, " users")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uC804\uCCB4 \uC0AC\uC774\uD2B8 \uAE30\uC900 \uC0AC\uC6A9\uC790 \uBAA9\uB85D\uC774\uBA70, \uCD5C\uADFC \uBC29\uBB38\uC21C\uC73C\uB85C \uC815\uB82C\uB429\uB2C8\uB2E4."), /* @__PURE__ */ React.createElement(Divider, { mb: "md" }), /* @__PURE__ */ React.createElement(Table, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true }, /* @__PURE__ */ React.createElement(Table.Thead, null, /* @__PURE__ */ React.createElement(Table.Tr, null, /* @__PURE__ */ React.createElement(Table.Th, null, "Seq"), /* @__PURE__ */ React.createElement(Table.Th, null, "Email"), /* @__PURE__ */ React.createElement(Table.Th, null, "Nickname"), /* @__PURE__ */ React.createElement(Table.Th, null, "Created"), /* @__PURE__ */ React.createElement(Table.Th, null, "Updated"), /* @__PURE__ */ React.createElement(Table.Th, null, "Sites"), /* @__PURE__ */ React.createElement(Table.Th, null, "Visits"), /* @__PURE__ */ React.createElement(Table.Th, null, "Last Viewed"), /* @__PURE__ */ React.createElement(Table.Th, null, "Action"))), /* @__PURE__ */ React.createElement(Table.Tbody, null, allUsers.map((user) => /* @__PURE__ */ React.createElement(Table.Tr, { key: user.seq }, /* @__PURE__ */ React.createElement(Table.Td, null, user.seq), /* @__PURE__ */ React.createElement(Table.Td, null, user.email), /* @__PURE__ */ React.createElement(Table.Td, null, user.nickname), /* @__PURE__ */ React.createElement(Table.Td, null, user.created), /* @__PURE__ */ React.createElement(Table.Td, null, user.updated), /* @__PURE__ */ React.createElement(Table.Td, null, user.siteCount ?? 0), /* @__PURE__ */ React.createElement(Table.Td, null, user.visitCount ?? 0), /* @__PURE__ */ React.createElement(Table.Td, null, user.lastViewed ?? "-"), /* @__PURE__ */ React.createElement(Table.Td, null, /* @__PURE__ */ React.createElement(
      Button,
      {
        size: "xs",
        variant: "light",
        onClick: () => onNavigate(`/Admin/User/UserViewHistory?seq=${encodeURIComponent(user.seq)}`)
      },
      "\uC5F4\uB78C \uC774\uB825"
    )))))));
  }
  if (page === "user-views") {
    return /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "User View Histories"), /* @__PURE__ */ React.createElement(Badge, { color: "cyan", variant: "light" }, userViewHistories.length, " rows")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uC120\uD0DD\uD55C \uC0AC\uC6A9\uC790\uC758 \uD398\uC774\uC9C0 \uC5F4\uB78C \uC774\uB825\uC785\uB2C8\uB2E4. Site \uBC0F Page \uB9C1\uD06C\uB85C \uC9C1\uC811 \uC774\uB3D9\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."), /* @__PURE__ */ React.createElement(Group, { mb: "md", justify: "space-between" }, /* @__PURE__ */ React.createElement(Button, { variant: "light", size: "xs", onClick: () => onNavigate("/Admin/User") }, "\u2190 User"), selectedAllUser ? /* @__PURE__ */ React.createElement(Text, { size: "sm" }, "\uC0AC\uC6A9\uC790: ", /* @__PURE__ */ React.createElement("b", null, selectedAllUser.nickname), " (", selectedAllUser.email, ")") : /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "seq\uB97C \uC9C0\uC815\uD574 \uC8FC\uC138\uC694. (/Admin/User/UserViewHistory?seq=\uC22B\uC790)")), loadingUserViewHistories ? /* @__PURE__ */ React.createElement(Group, null, /* @__PURE__ */ React.createElement(Loader, { size: "sm" }), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "\uC5F4\uB78C \uC774\uB825\uC744 \uBD88\uB7EC\uC624\uB294 \uC911\uC785\uB2C8\uB2E4...")) : makeTable(
      ["When", "Site", "Page", "History Seq"],
      userViewHistories.map((history) => {
        const siteUrl = history.siteDomain ? `https://${history.siteDomain}` : "";
        const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(history.pageName)}` : "";
        return [
          history.viewedAt,
          siteUrl ? /* @__PURE__ */ React.createElement(Anchor, { href: siteUrl, target: "_blank" }, history.siteName, " (#", history.site, ")") : `${history.siteName} (#${history.site})`,
          pageUrl ? /* @__PURE__ */ React.createElement(Anchor, { href: pageUrl, target: "_blank" }, history.pageName) : history.pageName,
          history.seq
        ];
      })
    ));
  }
  if (page === "operations") {
    return /* @__PURE__ */ React.createElement(Stack, { gap: "lg" }, /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "Site Cache Operations"), /* @__PURE__ */ React.createElement(Badge, { color: "orange", variant: "light" }, "Careful")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uC0AC\uC774\uD2B8\uBCC4 \uCE90\uC2DC\uB97C \uC989\uC2DC \uBE44\uC6CC\uC11C \uB3C4\uBA54\uC778/\uD398\uC774\uC9C0/\uD5E4\uB354 \uCE90\uC2DC\uB97C \uAC15\uC81C\uB85C \uAC31\uC2E0\uD569\uB2C8\uB2E4."), /* @__PURE__ */ React.createElement(Divider, { mb: "md" }), /* @__PURE__ */ React.createElement(Table, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true }, /* @__PURE__ */ React.createElement(Table.Thead, null, /* @__PURE__ */ React.createElement(Table.Tr, null, /* @__PURE__ */ React.createElement(Table.Th, null, "Seq"), /* @__PURE__ */ React.createElement(Table.Th, null, "Site"), /* @__PURE__ */ React.createElement(Table.Th, null, "Domains"), /* @__PURE__ */ React.createElement(Table.Th, null, "Action"))), /* @__PURE__ */ React.createElement(Table.Tbody, null, sites.map((site) => /* @__PURE__ */ React.createElement(Table.Tr, { key: site.seq }, /* @__PURE__ */ React.createElement(Table.Td, null, site.seq), /* @__PURE__ */ React.createElement(Table.Td, null, site.name), /* @__PURE__ */ React.createElement(Table.Td, null, (site.domains ?? []).join(", ") || "-"), /* @__PURE__ */ React.createElement(Table.Td, null, /* @__PURE__ */ React.createElement(
      Button,
      {
        color: "orange",
        variant: "filled",
        size: "xs",
        loading: clearingSiteSeq === site.seq,
        onClick: () => clearSiteCache(site.seq)
      },
      "Clear cache"
    ))))))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(
      SchedulerTable,
      {
        schedulers,
        runningSchedulerName,
        onRun: runScheduler,
        onRefresh: reloadSchedulers
      }
    )));
  }
  if (page === "recent-changes") {
    return /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "Recent Changes (All Sites)"), /* @__PURE__ */ React.createElement(Badge, { color: "violet", variant: "light" }, recentChanges.length, " rows")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uC0AC\uC774\uD2B8 \uC804\uCCB4 \uCD5C\uADFC \uBCC0\uACBD \uAE30\uB85D\uC744 n\uAC1C \uB2E8\uC704\uB85C \uC870\uD68C\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."), /* @__PURE__ */ React.createElement(Group, { align: "flex-end", mb: "md" }, /* @__PURE__ */ React.createElement(
      TextInput,
      {
        label: "\uC870\uD68C \uAC1C\uC218 n",
        value: recentChangeLimitInput,
        onChange: (event) => setRecentChangeLimitInput(event.currentTarget.value),
        placeholder: "1 ~ 500"
      }
    ), /* @__PURE__ */ React.createElement(
      Button,
      {
        variant: "filled",
        onClick: () => {
          const parsed = Number.parseInt(recentChangeLimitInput, 10);
          const n = Number.isFinite(parsed) ? Math.min(500, Math.max(1, parsed)) : 50;
          setRecentChangeLimitInput(String(n));
          loadRecentChanges(n);
        }
      },
      "\uC870\uD68C"
    )), makeTable(
      ["When", "Site", "Page", "Revision", "Editor", "Comment", "IP"],
      recentChanges.map((row) => [
        row.dateTime,
        `${row.siteName} (#${row.siteSeq})`,
        row.name,
        row.revision,
        row.nickname ?? "-",
        row.comment || "-",
        row.remoteAddress
      ])
    ));
  }
  return /* @__PURE__ */ React.createElement(Stack, { gap: "lg" }, /* @__PURE__ */ React.createElement(SimpleGrid, { cols: { base: 1, sm: 2, lg: 4 }, spacing: "md" }, /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "flex-start" }, /* @__PURE__ */ React.createElement(Stack, { gap: 2 }, /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "Sites"), /* @__PURE__ */ React.createElement(Title, { order: 2 }, sites.length)), /* @__PURE__ */ React.createElement(ThemeIcon, { color: "indigo", variant: "light", radius: "xl" }, "S"))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "flex-start" }, /* @__PURE__ */ React.createElement(Stack, { gap: 2 }, /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "All Users"), /* @__PURE__ */ React.createElement(Title, { order: 2 }, allUsers.length)), /* @__PURE__ */ React.createElement(ThemeIcon, { color: "teal", variant: "light", radius: "xl" }, "U"))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "flex-start" }, /* @__PURE__ */ React.createElement(Stack, { gap: 2 }, /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "30\uC77C \uBB38\uC11C \uC218\uC815"), /* @__PURE__ */ React.createElement(Title, { order: 2 }, dailyStats.pageEdited.reduce((sum, item) => sum + (item.count ?? 0), 0))), /* @__PURE__ */ React.createElement(ThemeIcon, { color: "grape", variant: "light", radius: "xl" }, "E"))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "flex-start" }, /* @__PURE__ */ React.createElement(Stack, { gap: 2 }, /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "Running Schedulers"), /* @__PURE__ */ React.createElement(Title, { order: 2 }, schedulers.filter((scheduler) => scheduler.running).length, "/", schedulers.length)), /* @__PURE__ */ React.createElement(ThemeIcon, { color: "blue", variant: "light", radius: "xl" }, "R")))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "\uBE60\uB978 \uC774\uB3D9"), /* @__PURE__ */ React.createElement(Badge, { color: "indigo", variant: "light" }, "Quick Access")), /* @__PURE__ */ React.createElement(SimpleGrid, { cols: { base: 1, sm: 3 }, spacing: "sm" }, /* @__PURE__ */ React.createElement(Button, { variant: "light", onClick: () => onNavigate("/Admin/RecentChange") }, "\uCD5C\uADFC \uBCC0\uACBD \uBCF4\uAE30"), /* @__PURE__ */ React.createElement(Button, { variant: "light", onClick: () => onNavigate("/Admin/User") }, "\uC0AC\uC6A9\uC790 \uBAA9\uB85D \uBCF4\uAE30"), /* @__PURE__ */ React.createElement(Button, { variant: "light", onClick: () => onNavigate("/Admin/Operation") }, "\uC6B4\uC601 \uC791\uC5C5 \uC5F4\uAE30"))), /* @__PURE__ */ React.createElement(SimpleGrid, { cols: { base: 1, sm: 2, lg: 4 }, spacing: "md" }, /* @__PURE__ */ React.createElement(
    StatTrendCard,
    {
      title: "New Users",
      color: "blue",
      rows: dailyStats.userCreated,
      total: dailyStats.userCreated.reduce((sum, item) => sum + (item.count ?? 0), 0)
    }
  ), /* @__PURE__ */ React.createElement(
    StatTrendCard,
    {
      title: "Site User Joins",
      color: "teal",
      rows: dailyStats.siteUserCreated,
      total: dailyStats.siteUserCreated.reduce((sum, item) => sum + (item.count ?? 0), 0)
    }
  ), /* @__PURE__ */ React.createElement(
    StatTrendCard,
    {
      title: "New Pages",
      color: "indigo",
      rows: dailyStats.pageCreated,
      total: dailyStats.pageCreated.reduce((sum, item) => sum + (item.count ?? 0), 0)
    }
  ), /* @__PURE__ */ React.createElement(
    StatTrendCard,
    {
      title: "Page Edits",
      color: "grape",
      rows: dailyStats.pageEdited,
      total: dailyStats.pageEdited.reduce((sum, item) => sum + (item.count ?? 0), 0)
    }
  )), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "30\uC77C \uC6B4\uC601 \uCD94\uC774 \uCC28\uD2B8"), /* @__PURE__ */ React.createElement(Badge, { color: "blue", variant: "light" }, "Chart")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uC2E0\uADDC \uC0AC\uC6A9\uC790, \uC0AC\uC774\uD2B8 \uAC00\uC785, \uBB38\uC11C \uC0DD\uC131/\uC218\uC815 \uC9C0\uD45C\uB97C \uD558\uB098\uC758 \uC2DC\uACC4\uC5F4 \uCC28\uD2B8\uB85C \uBE44\uAD50\uD569\uB2C8\uB2E4."), /* @__PURE__ */ React.createElement(
    MultiTrendChart,
    {
      series: [
        { name: "New Users", color: "blue", rows: dailyStats.userCreated },
        { name: "Site User Joins", color: "teal", rows: dailyStats.siteUserCreated },
        { name: "New Pages", color: "indigo", rows: dailyStats.pageCreated },
        { name: "Page Edits", color: "grape", rows: dailyStats.pageEdited }
      ]
    }
  )), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "Most Viewed Pages"), /* @__PURE__ */ React.createElement(Badge, { color: "pink", variant: "light" }, Math.min(topViewedPages.length, 10))), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uB85C\uADF8\uC778 \uC0AC\uC6A9\uC790 \uAE30\uC900 \uD398\uC774\uC9C0 \uC870\uD68C \uB204\uC801 \uC0C1\uC704 \uBB38\uC11C\uC785\uB2C8\uB2E4. \uD575\uC2EC 10\uAC1C\uB9CC \uD45C\uC2DC\uD569\uB2C8\uB2E4."), makeTable(
    ["Rank", "Site", "Page", "Views", "Last Viewed"],
    topViewedPages.slice(0, 10).map((row, index) => {
      const siteUrl = row.siteDomain ? `https://${row.siteDomain}` : "";
      const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(row.pageName)}` : "";
      return [
        index + 1,
        siteUrl ? /* @__PURE__ */ React.createElement(Anchor, { href: siteUrl, target: "_blank" }, row.siteName, " (#", row.siteSeq, ")") : `${row.siteName} (#${row.siteSeq})`,
        pageUrl ? /* @__PURE__ */ React.createElement(Anchor, { href: pageUrl, target: "_blank" }, row.pageName) : row.pageName,
        row.viewCount,
        row.lastViewedAt
      ];
    })
  )), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "Recent Changes (All Sites)"), /* @__PURE__ */ React.createElement(Badge, { color: "violet", variant: "light" }, recentChanges.length)), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uC804\uCCB4 \uC0AC\uC774\uD2B8 \uAE30\uC900 \uCD5C\uADFC \uBCC0\uACBD 30\uAC1C\uC785\uB2C8\uB2E4. \uB354 \uB9CE\uC774 \uBCF4\uB824\uBA74 \uC67C\uCABD \uBA54\uB274 Recent Changes\uB97C \uC0AC\uC6A9\uD558\uC138\uC694."), makeTable(
    ["When", "Site", "Page", "Revision", "Editor", "Comment"],
    recentChanges.slice(0, 10).map((row) => [
      row.dateTime,
      `${row.siteName} (#${row.siteSeq})`,
      row.name,
      row.revision,
      row.nickname ?? "-",
      row.comment || "-"
    ])
  )));
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
      const currentPathWithSearch = `${window.location.pathname}${window.location.search}`;
      if (currentPathWithSearch !== href) {
        window.history.pushState({}, "", href);
      }
      setPage(routeToPage(new URL(href, window.location.origin).pathname));
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
      /* @__PURE__ */ React.createElement(AppShell.Main, null, /* @__PURE__ */ React.createElement(Stack, { gap: "md" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "center" }, /* @__PURE__ */ React.createElement(Stack, { gap: 2 }, /* @__PURE__ */ React.createElement(Title, { order: 2 }, "Admin Dashboard"), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "\uC6B4\uC601 \uD604\uD669\uC744 \uD55C\uB208\uC5D0 \uD655\uC778\uD558\uACE0 \uC989\uC2DC \uC791\uC5C5\uD558\uC138\uC694.")), /* @__PURE__ */ React.createElement(Badge, { variant: "light", color: "indigo", size: "lg" }, "Live")), /* @__PURE__ */ React.createElement(AdminContent, { page, onNavigate })))
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
