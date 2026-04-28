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
  ColorInput,
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
  if (/^\/Admin\/Site\/\d+$/.test(pathname)) {
    return "site-detail";
  }
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
function parseSiteSeqFromPathname(pathname) {
  const matched = pathname.match(/^\/Admin\/Site\/(\d+)$/);
  if (!matched) {
    return "";
  }
  const siteSeq = Number.parseInt(matched[1], 10);
  return Number.isFinite(siteSeq) && siteSeq > 0 ? String(siteSeq) : "";
}
function pageTitleByKey(page) {
  if (page === "recent-changes") {
    return "RecentChanges";
  }
  if (page === "all-users" || page === "user-views") {
    return "User";
  }
  if (page === "site-detail" || page === "sites" || page === "users") {
    return "Site";
  }
  if (page === "operations") {
    return "Operation";
  }
  return "Dashboard";
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
  return {
    name: token?.name ?? "csrfToken",
    value: token?.value ?? ""
  };
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
  const [siteFaviconUrl, setSiteFaviconUrl] = useState("/public/favicon.png");
  const [siteFaviconObjectKey, setSiteFaviconObjectKey] = useState("");
  const [uploadingFavicon, setUploadingFavicon] = useState(false);
  const [deletingFavicon, setDeletingFavicon] = useState(false);
  const [siteTheme, setSiteTheme] = useState({
    headerBackgroundColor: "",
    headerForegroundColor: "",
    footerBackgroundColor: "",
    footerForegroundColor: ""
  });
  const [savingSiteTheme, setSavingSiteTheme] = useState(false);
  const [calculatingSiteSeq, setCalculatingSiteSeq] = useState(0);
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
          "Csrf-Token": csrfToken.value,
          "X-CSRF-Token": csrfToken.value
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
  const loadSiteFavicon = useCallback(async (siteSeq) => {
    if (!siteSeq) {
      setSiteFaviconUrl("/public/favicon.png");
      setSiteFaviconObjectKey("");
      return;
    }
    try {
      const data = await fetchJson(`/api/Admin/Favicon?siteSeq=${encodeURIComponent(siteSeq)}`);
      setSiteFaviconUrl(data?.faviconUrl || "/public/favicon.png");
      setSiteFaviconObjectKey(data?.objectKey || "");
    } catch (caughtError) {
      logError("favicon:load:error", caughtError);
      setError(caughtError.message || String(caughtError));
    }
  }, []);
  const uploadSiteFavicon = useCallback(async (file, siteSeq) => {
    if (!file || !siteSeq) {
      return;
    }
    setUploadingFavicon(true);
    setError("");
    try {
      const csrfToken = await fetchCsrfToken();
      const formData = new FormData();
      formData.append("file", file);
      formData.append("siteSeq", String(siteSeq));
      formData.append(csrfToken.name, csrfToken.value);
      formData.append("csrfToken", csrfToken.value);
      const response = await fetch("/api/Admin/Favicon", {
        method: "POST",
        credentials: "same-origin",
        headers: {
          "Csrf-Token": csrfToken.value,
          "X-CSRF-Token": csrfToken.value
        },
        body: formData
      });
      if (!response.ok) {
        const payload = await response.json().catch(() => null);
        throw new Error(payload?.error || `HTTP ${response.status}`);
      }
      const data = await response.json();
      setSiteFaviconUrl(data?.faviconUrl || "/public/favicon.png");
      setSiteFaviconObjectKey(data?.objectKey || "");
    } catch (caughtError) {
      logError("favicon:upload:error", caughtError);
      setError(caughtError.message || String(caughtError));
    } finally {
      setUploadingFavicon(false);
    }
  }, []);
  const resetSiteFavicon = useCallback(async (siteSeq) => {
    if (!siteSeq) {
      return;
    }
    setDeletingFavicon(true);
    setError("");
    try {
      const csrfToken = await fetchCsrfToken();
      const response = await fetch(`/api/Admin/Favicon?siteSeq=${encodeURIComponent(siteSeq)}`, {
        method: "DELETE",
        credentials: "same-origin",
        headers: {
          "Csrf-Token": csrfToken.value,
          "X-CSRF-Token": csrfToken.value
        }
      });
      if (!response.ok) {
        const payload = await response.json().catch(() => null);
        throw new Error(payload?.error || `HTTP ${response.status}`);
      }
      setSiteFaviconUrl("/public/favicon.png");
      setSiteFaviconObjectKey("");
    } catch (caughtError) {
      logError("favicon:delete:error", caughtError);
      setError(caughtError.message || String(caughtError));
    } finally {
      setDeletingFavicon(false);
    }
  }, []);
  const loadSiteTheme = useCallback(async (siteSeq) => {
    if (!siteSeq) {
      setSiteTheme({
        headerBackgroundColor: "",
        headerForegroundColor: "",
        footerBackgroundColor: "",
        footerForegroundColor: ""
      });
      return;
    }
    try {
      const data = await fetchJson(`/api/Admin/SiteTheme?siteSeq=${encodeURIComponent(siteSeq)}`);
      setSiteTheme({
        headerBackgroundColor: data?.headerBackgroundColor ?? "",
        headerForegroundColor: data?.headerForegroundColor ?? "",
        footerBackgroundColor: data?.footerBackgroundColor ?? "",
        footerForegroundColor: data?.footerForegroundColor ?? ""
      });
    } catch (caughtError) {
      logError("site-theme:load:error", caughtError);
      setError(caughtError.message || String(caughtError));
    }
  }, []);
  const saveSiteTheme = useCallback(async (siteSeq, nextTheme) => {
    if (!siteSeq) {
      return;
    }
    setSavingSiteTheme(true);
    setError("");
    try {
      const csrfToken = await fetchCsrfToken();
      const payload = new URLSearchParams();
      payload.set("siteSeq", String(siteSeq));
      payload.set("headerBackgroundColor", nextTheme?.headerBackgroundColor ?? "");
      payload.set("headerForegroundColor", nextTheme?.headerForegroundColor ?? "");
      payload.set("footerBackgroundColor", nextTheme?.footerBackgroundColor ?? "");
      payload.set("footerForegroundColor", nextTheme?.footerForegroundColor ?? "");
      payload.set(csrfToken.name, csrfToken.value);
      payload.set("csrfToken", csrfToken.value);
      const response = await fetch("/api/Admin/SiteTheme", {
        method: "PUT",
        credentials: "same-origin",
        headers: {
          "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8",
          "Csrf-Token": csrfToken.value,
          "X-CSRF-Token": csrfToken.value
        },
        body: payload.toString()
      });
      if (!response.ok) {
        const payloadJson = await response.json().catch(() => null);
        throw new Error(payloadJson?.error || `HTTP ${response.status}`);
      }
      const data = await response.json();
      setSiteTheme({
        headerBackgroundColor: data?.headerBackgroundColor ?? "",
        headerForegroundColor: data?.headerForegroundColor ?? "",
        footerBackgroundColor: data?.footerBackgroundColor ?? "",
        footerForegroundColor: data?.footerForegroundColor ?? ""
      });
    } catch (caughtError) {
      logError("site-theme:save:error", caughtError);
      setError(caughtError.message || String(caughtError));
    } finally {
      setSavingSiteTheme(false);
    }
  }, []);
  const loadAdminSitePageNames = useCallback(async (siteSeq) => {
    if (!siteSeq) {
      return [];
    }
    const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/PageNames`);
    return Array.isArray(data) ? data : [];
  }, []);
  const runSiteCalculate = useCallback(async (siteSeq, pageName = "") => {
    if (!siteSeq) {
      return null;
    }
    setCalculatingSiteSeq(Number.parseInt(String(siteSeq), 10) || 0);
    setError("");
    try {
      const csrfToken = await fetchCsrfToken();
      const suffix = pageName?.trim() ? `?pageName=${encodeURIComponent(pageName.trim())}` : "";
      const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Calculate${suffix}`, {
        method: "POST",
        credentials: "same-origin",
        headers: {
          "Csrf-Token": csrfToken.value,
          "X-CSRF-Token": csrfToken.value,
          "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8"
        },
        body: `${encodeURIComponent(csrfToken.name)}=${encodeURIComponent(csrfToken.value)}&csrfToken=${encodeURIComponent(csrfToken.value)}`
      });
      if (!response.ok) {
        const payloadJson = await response.json().catch(() => null);
        throw new Error(payloadJson?.error || `HTTP ${response.status}`);
      }
      return await response.json();
    } catch (caughtError) {
      logError("site:calculate:error", siteSeq, pageName, caughtError);
      setError(caughtError.message || String(caughtError));
      return null;
    } finally {
      setCalculatingSiteSeq(0);
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
    siteFaviconUrl,
    siteFaviconObjectKey,
    loadSiteFavicon,
    uploadSiteFavicon,
    uploadingFavicon,
    resetSiteFavicon,
    deletingFavicon,
    siteTheme,
    setSiteTheme,
    loadSiteTheme,
    saveSiteTheme,
    savingSiteTheme,
    loadAdminSitePageNames,
    runSiteCalculate,
    calculatingSiteSeq,
    error
  };
}
function makeTable(headers, rows) {
  return /* @__PURE__ */ React.createElement(Table, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true, stickyHeader: true, stickyHeaderOffset: 0 }, /* @__PURE__ */ React.createElement(Table.Thead, null, /* @__PURE__ */ React.createElement(Table.Tr, null, headers.map((header) => /* @__PURE__ */ React.createElement(Table.Th, { key: header }, header)))), /* @__PURE__ */ React.createElement(Table.Tbody, null, rows.map((columns, rowIndex) => /* @__PURE__ */ React.createElement(Table.Tr, { key: `row-${rowIndex}` }, columns.map((column, colIndex) => /* @__PURE__ */ React.createElement(Table.Td, { key: `col-${rowIndex}-${colIndex}` }, column ?? ""))))));
}
function Navigation({ activePage, onNavigate }) {
  const [siteLinks, setSiteLinks] = useState([]);
  const currentSiteSeq = parseSiteSeqFromPathname(window.location.pathname);
  useEffect(() => {
    let mounted = true;
    fetchJson("/api/Admin/Sites").then((siteData) => {
      if (!mounted) {
        return;
      }
      setSiteLinks(Array.isArray(siteData) ? siteData : []);
    }).catch((caughtError) => {
      logError("navigation:sites:error", caughtError);
    });
    return () => {
      mounted = false;
    };
  }, []);
  const links = useMemo(
    () => [
      { href: "/", label: "\uC704\uD0A4\uB85C \uB3CC\uC544\uAC00\uAE30", key: "home" },
      { href: "/Admin", label: "Dashboard", key: "dashboard" },
      { href: "/Admin/RecentChange", label: "RecentChanges", key: "recent-changes" },
      { href: "/Admin/User", label: "User", key: "all-users" },
      { href: "/Admin/Site", label: "Site", key: "sites" },
      { href: "/Admin/Operation", label: "Operation", key: "operations" }
    ],
    []
  );
  return /* @__PURE__ */ React.createElement(Stack, { gap: 8 }, /* @__PURE__ */ React.createElement(Text, { size: "xs", tt: "uppercase", fw: 700, c: "dimmed", px: 8 }, "Admin Navigation"), links.map((link) => /* @__PURE__ */ React.createElement(React.Fragment, { key: link.key }, /* @__PURE__ */ React.createElement(
    NavLink,
    {
      href: link.href,
      label: link.label,
      active: activePage === link.key || activePage === "user-views" && link.key === "all-users" || activePage === "site-detail" && link.key === "sites",
      variant: activePage === link.key || activePage === "user-views" && link.key === "all-users" || activePage === "site-detail" && link.key === "sites" ? "filled" : "light",
      onClick: (event) => {
        if (link.key === "home") {
          return;
        }
        event.preventDefault();
        onNavigate(link.href);
      }
    }
  ), link.key === "sites" && (activePage === "sites" || activePage === "site-detail") && /* @__PURE__ */ React.createElement(Stack, { gap: 2, ml: 8 }, siteLinks.map((site) => /* @__PURE__ */ React.createElement(
    NavLink,
    {
      key: `site-${site.seq}`,
      href: `/Admin/Site/${site.seq}`,
      label: `${site.name} (#${site.seq})`,
      active: currentSiteSeq === String(site.seq),
      variant: currentSiteSeq === String(site.seq) ? "subtle" : "light",
      onClick: (event) => {
        event.preventDefault();
        onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}`);
      }
    }
  ))))));
}
function SchedulerTable({ schedulers, runningSchedulerName, onRun, onRefresh }) {
  const schedulersWithoutCalculate = schedulers.filter((scheduler) => scheduler.name !== "Calculate");
  return /* @__PURE__ */ React.createElement(Stack, { gap: "sm" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between" }, /* @__PURE__ */ React.createElement(Title, { order: 4 }, "Schedulers"), /* @__PURE__ */ React.createElement(Button, { size: "xs", variant: "light", onClick: onRefresh }, "Refresh")), /* @__PURE__ */ React.createElement(Text, { size: "xs", c: "dimmed" }, "Calculate\uB294 \uC0AC\uC774\uD2B8 \uC0C1\uC138(/Admin/Site/:seq)\uC5D0\uC11C \uC2E4\uD589\uD558\uC138\uC694."), /* @__PURE__ */ React.createElement(
    Progress,
    {
      size: "sm",
      value: schedulersWithoutCalculate.length === 0 ? 0 : schedulersWithoutCalculate.filter((scheduler) => scheduler.running).length / schedulersWithoutCalculate.length * 100,
      color: "blue",
      radius: "xl"
    }
  ), /* @__PURE__ */ React.createElement(Table, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true }, /* @__PURE__ */ React.createElement(Table.Thead, null, /* @__PURE__ */ React.createElement(Table.Tr, null, /* @__PURE__ */ React.createElement(Table.Th, null, "Name"), /* @__PURE__ */ React.createElement(Table.Th, null, "Interval"), /* @__PURE__ */ React.createElement(Table.Th, null, "Next Delay(s)"), /* @__PURE__ */ React.createElement(Table.Th, null, "Last Started"), /* @__PURE__ */ React.createElement(Table.Th, null, "Last Finished"), /* @__PURE__ */ React.createElement(Table.Th, null, "Result"), /* @__PURE__ */ React.createElement(Table.Th, null, "Run Count"), /* @__PURE__ */ React.createElement(Table.Th, null, "Action"))), /* @__PURE__ */ React.createElement(Table.Tbody, null, schedulersWithoutCalculate.map((scheduler) => /* @__PURE__ */ React.createElement(Table.Tr, { key: scheduler.name }, /* @__PURE__ */ React.createElement(Table.Td, null, scheduler.name), /* @__PURE__ */ React.createElement(Table.Td, null, `${scheduler.minSeconds}s ~ ${scheduler.maxSeconds}s`), /* @__PURE__ */ React.createElement(Table.Td, null, scheduler.nextDelaySeconds ?? "-"), /* @__PURE__ */ React.createElement(Table.Td, null, scheduler.lastStartedAt ?? "-"), /* @__PURE__ */ React.createElement(Table.Td, null, scheduler.lastFinishedAt ?? "-"), /* @__PURE__ */ React.createElement(Table.Td, null, scheduler.lastResult ?? "-"), /* @__PURE__ */ React.createElement(Table.Td, null, scheduler.runCount ?? 0), /* @__PURE__ */ React.createElement(Table.Td, null, /* @__PURE__ */ React.createElement(
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
function AdminContent({ page, onNavigate, pathname, search }) {
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
    siteFaviconUrl,
    siteFaviconObjectKey,
    loadSiteFavicon,
    uploadSiteFavicon,
    uploadingFavicon,
    resetSiteFavicon,
    deletingFavicon,
    siteTheme,
    setSiteTheme,
    loadSiteTheme,
    saveSiteTheme,
    savingSiteTheme,
    loadAdminSitePageNames,
    runSiteCalculate,
    calculatingSiteSeq,
    error
  } = useAdminData(page);
  const [recentChangeLimitInput, setRecentChangeLimitInput] = useState("50");
  const [faviconFile, setFaviconFile] = useState(null);
  const [selectedSiteSeq, setSelectedSiteSeq] = useState("");
  const [sitePageNames, setSitePageNames] = useState([]);
  const [selectedCalculatePageName, setSelectedCalculatePageName] = useState("");
  const [siteCalculateMessage, setSiteCalculateMessage] = useState("");
  const selectedSite = useMemo(
    () => sites.find((site) => String(site.seq) === selectedSiteSeq) ?? null,
    [sites, selectedSiteSeq]
  );
  const selectedUserSeq = useMemo(() => {
    const userSeqByPath = parseUserSeqFromPathname(pathname);
    if (userSeqByPath > 0) {
      return userSeqByPath;
    }
    const params = new URLSearchParams(search);
    const userSeqByQuery = Number.parseInt(params.get("userSeq") ?? "", 10);
    return Number.isFinite(userSeqByQuery) && userSeqByQuery > 0 ? userSeqByQuery : 0;
  }, [page, pathname, search]);
  const selectedAllUser = useMemo(
    () => allUsers.find((user) => user.seq === selectedUserSeq) ?? null,
    [allUsers, selectedUserSeq]
  );
  useEffect(() => {
    if (page !== "site-detail") {
      return;
    }
    const siteSeqByPath = parseSiteSeqFromPathname(pathname);
    if (siteSeqByPath && selectedSiteSeq !== siteSeqByPath) {
      setSelectedSiteSeq(siteSeqByPath);
      setSelectedCalculatePageName("");
      setSiteCalculateMessage("");
    }
  }, [page, pathname, selectedSiteSeq]);
  useEffect(() => {
    if (page === "site-detail" && selectedSiteSeq) {
      loadSiteFavicon(selectedSiteSeq);
      loadSiteTheme(selectedSiteSeq);
      loadAdminSitePageNames(selectedSiteSeq).then((pageNames) => {
        setSitePageNames(pageNames);
      }).catch((caughtError) => {
        logError("site:pageNames:error", selectedSiteSeq, caughtError);
      });
    }
  }, [page, selectedSiteSeq, loadSiteFavicon, loadSiteTheme, loadAdminSitePageNames]);
  if (loading) {
    return /* @__PURE__ */ React.createElement(Paper, { p: "xl", withBorder: true, radius: "md", shadow: "xs" }, /* @__PURE__ */ React.createElement(Stack, { align: "center", gap: "xs", py: "xl" }, /* @__PURE__ */ React.createElement(Loader, { size: "lg", color: "blue", type: "dots" }), /* @__PURE__ */ React.createElement(Title, { order: 4, c: "dark" }, "Admin \uB370\uC774\uD130\uB97C \uC900\uBE44\uD558\uACE0 \uC788\uC5B4\uC694"), /* @__PURE__ */ React.createElement(Text, { c: "dimmed", size: "sm" }, "\uD398\uC774\uC9C0\uAC00 \uACE7 \uD45C\uC2DC\uB429\uB2C8\uB2E4. \uC7A0\uC2DC\uB9CC \uAE30\uB2E4\uB824 \uC8FC\uC138\uC694.")));
  }
  if (error) {
    return /* @__PURE__ */ React.createElement(Paper, { p: "lg", withBorder: true, radius: "md" }, /* @__PURE__ */ React.createElement(Text, { c: "red", fw: 600 }, "\uD074\uB77C\uC774\uC5B8\uD2B8 \uB80C\uB354\uB9C1 \uC624\uB958: ", error));
  }
  if (page === "sites") {
    return /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "Site List"), /* @__PURE__ */ React.createElement(Badge, { color: "indigo", variant: "light" }, sites.length, " sites")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uC804\uCCB4 \uC0AC\uC774\uD2B8 \uBAA9\uB85D\uC785\uB2C8\uB2E4. \uC0C1\uC138 \uC124\uC815\uC740 \uAC01 \uC0AC\uC774\uD2B8\uC758 \uAD00\uB9AC \uBC84\uD2BC\uC73C\uB85C \uC774\uB3D9\uD558\uC138\uC694."), /* @__PURE__ */ React.createElement(Divider, { mb: "md" }), /* @__PURE__ */ React.createElement(Table, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true }, /* @__PURE__ */ React.createElement(Table.Thead, null, /* @__PURE__ */ React.createElement(Table.Tr, null, /* @__PURE__ */ React.createElement(Table.Th, null, "Seq"), /* @__PURE__ */ React.createElement(Table.Th, null, "Name"), /* @__PURE__ */ React.createElement(Table.Th, null, "Domains"), /* @__PURE__ */ React.createElement(Table.Th, null, "Users"), /* @__PURE__ */ React.createElement(Table.Th, null, "Pages"), /* @__PURE__ */ React.createElement(Table.Th, null, "Action"))), /* @__PURE__ */ React.createElement(Table.Tbody, null, sites.map((site) => /* @__PURE__ */ React.createElement(Table.Tr, { key: site.seq }, /* @__PURE__ */ React.createElement(Table.Td, null, site.seq), /* @__PURE__ */ React.createElement(Table.Td, null, site.name), /* @__PURE__ */ React.createElement(Table.Td, null, (site.domains ?? []).join(", ") || "-"), /* @__PURE__ */ React.createElement(Table.Td, null, site.userCount ?? 0), /* @__PURE__ */ React.createElement(Table.Td, null, site.pageCount ?? 0), /* @__PURE__ */ React.createElement(Table.Td, null, /* @__PURE__ */ React.createElement(
      Button,
      {
        size: "xs",
        variant: "light",
        onClick: () => onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}`)
      },
      "\uAD00\uB9AC"
    )))))));
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
  if (page === "site-detail") {
    return /* @__PURE__ */ React.createElement(Stack, { gap: "lg" }, /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "xs" }, /* @__PURE__ */ React.createElement(Title, { order: 4 }, "\uC0AC\uC774\uD2B8 \uC0C1\uC138"), /* @__PURE__ */ React.createElement(Badge, { color: "blue", variant: "light" }, "Site Detail")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "/Admin/Site/", `{seq}`, " \uACBD\uB85C\uB85C \uC811\uADFC\uD55C \uC0AC\uC774\uD2B8\uC758 favicon/\uD14C\uB9C8\uB97C \uC124\uC815\uD569\uB2C8\uB2E4."), /* @__PURE__ */ React.createElement(SimpleGrid, { cols: { base: 1, lg: 2 }, spacing: "md" }, /* @__PURE__ */ React.createElement(Paper, { withBorder: true, radius: "md", p: "sm" }, /* @__PURE__ */ React.createElement(Button, { variant: "light", size: "xs", onClick: () => onNavigate("/Admin/Site") }, "\u2190 \uC0AC\uC774\uD2B8 \uBAA9\uB85D")), /* @__PURE__ */ React.createElement(Paper, { withBorder: true, radius: "md", p: "sm" }, selectedSite ? /* @__PURE__ */ React.createElement(Stack, { gap: 4 }, /* @__PURE__ */ React.createElement(Text, { size: "xs", c: "dimmed" }, "\uC120\uD0DD\uB41C \uC0AC\uC774\uD2B8"), /* @__PURE__ */ React.createElement(Text, { fw: 700 }, selectedSite.name, " (#", selectedSite.seq, ")"), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "\uB3C4\uBA54\uC778: ", (selectedSite.domains ?? []).join(", ") || "-")) : /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed" }, "\uC720\uD6A8\uD55C \uC0AC\uC774\uD2B8 seq\uAC00 \uD544\uC694\uD569\uB2C8\uB2E4. (/Admin/Site/\uC22B\uC790)")))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "Site Cache Operation"), /* @__PURE__ */ React.createElement(Badge, { color: "orange", variant: "light" }, "Current Site")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uD604\uC7AC \uBCF4\uACE0 \uC788\uB294 \uC0AC\uC774\uD2B8\uC758 \uCE90\uC2DC\uB97C \uC989\uC2DC \uCD08\uAE30\uD654\uD569\uB2C8\uB2E4. \uB3C4\uBA54\uC778/\uD398\uC774\uC9C0/\uD5E4\uB354 \uCE90\uC2DC\uAC00 \uAC15\uC81C\uB85C \uAC31\uC2E0\uB429\uB2C8\uB2E4."), /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "center" }, /* @__PURE__ */ React.createElement(Stack, { gap: 2 }, /* @__PURE__ */ React.createElement(Text, { size: "xs", c: "dimmed" }, "\uB300\uC0C1 \uC0AC\uC774\uD2B8"), /* @__PURE__ */ React.createElement(Text, { fw: 700 }, selectedSite ? `${selectedSite.name} (#${selectedSite.seq})` : "\uC120\uD0DD\uB41C \uC0AC\uC774\uD2B8 \uC5C6\uC74C")), /* @__PURE__ */ React.createElement(
      Button,
      {
        color: "orange",
        variant: "filled",
        disabled: !selectedSite,
        loading: selectedSite ? clearingSiteSeq === selectedSite.seq : false,
        onClick: () => {
          if (!selectedSite) {
            return;
          }
          clearSiteCache(selectedSite.seq);
        }
      },
      "Clear current site cache"
    ))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "Site Calculate Operation"), /* @__PURE__ */ React.createElement(Badge, { color: "teal", variant: "light" }, "Per Site")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uD604\uC7AC \uC0AC\uC774\uD2B8\uC5D0\uC11C 1\uAC1C \uD398\uC774\uC9C0\uB9CC Calculate \uD050\uC5D0 \uB123\uC2B5\uB2C8\uB2E4. \uD398\uC774\uC9C0\uB97C \uBE44\uC6CC\uB450\uBA74 \uB79C\uB364 1\uAC1C, \uC785\uB825\uD558\uBA74 \uD574\uB2F9 \uD398\uC774\uC9C0\uB97C Calculate\uD569\uB2C8\uB2E4."), /* @__PURE__ */ React.createElement(Stack, { gap: "sm" }, /* @__PURE__ */ React.createElement(
      TextInput,
      {
        label: "Calculate\uD560 \uD398\uC774\uC9C0 (\uC790\uB3D9\uC644\uC131)",
        placeholder: "\uBE44\uC6CC\uB450\uBA74 \uB79C\uB364 1\uAC1C",
        value: selectedCalculatePageName,
        onChange: (event) => setSelectedCalculatePageName(event.currentTarget.value),
        list: "site-calculate-page-name-list",
        disabled: !selectedSiteSeq
      }
    ), /* @__PURE__ */ React.createElement("datalist", { id: "site-calculate-page-name-list" }, sitePageNames.map((pageName) => /* @__PURE__ */ React.createElement("option", { key: `calculate-page-${pageName}`, value: pageName }))), /* @__PURE__ */ React.createElement(Group, null, /* @__PURE__ */ React.createElement(
      Button,
      {
        variant: "filled",
        color: "teal",
        disabled: !selectedSiteSeq,
        loading: selectedSite ? calculatingSiteSeq === selectedSite.seq : false,
        onClick: async () => {
          if (!selectedSiteSeq) {
            return;
          }
          const response = await runSiteCalculate(selectedSiteSeq, selectedCalculatePageName);
          if (!response) {
            return;
          }
          const modeLabel = response?.source === "selected" ? "\uC120\uD0DD \uD398\uC774\uC9C0" : "\uB79C\uB364 \uD398\uC774\uC9C0";
          setSiteCalculateMessage(`${modeLabel}: ${response?.pageName ?? "-"} (queued)`);
        }
      },
      "Calculate 1 page"
    ), /* @__PURE__ */ React.createElement(
      Button,
      {
        variant: "light",
        disabled: !selectedSiteSeq,
        onClick: async () => {
          if (!selectedSiteSeq) {
            return;
          }
          const pageNames = await loadAdminSitePageNames(selectedSiteSeq);
          setSitePageNames(pageNames);
        }
      },
      "\uD398\uC774\uC9C0 \uBAA9\uB85D \uC0C8\uB85C\uACE0\uCE68"
    )), siteCalculateMessage ? /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "teal" }, siteCalculateMessage) : null, /* @__PURE__ */ React.createElement(Text, { size: "xs", c: "dimmed" }, "\uD398\uC774\uC9C0 \uC774\uB984 \uBAA9\uB85D\uC740 \uC0AC\uC774\uD2B8 \uCE90\uC2DC\uB97C \uC0AC\uC6A9\uD569\uB2C8\uB2E4. (count: ", sitePageNames.length, ")"))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "Site Favicon"), /* @__PURE__ */ React.createElement(Badge, { color: "blue", variant: "light" }, "Current Site")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uC120\uD0DD\uD55C \uC0AC\uC774\uD2B8\uC758 favicon\uC744 \uAD00\uB9AC\uC790 \uC5C5\uB85C\uB4DC\uB85C \uAD50\uCCB4\uD569\uB2C8\uB2E4. \uC5C5\uB85C\uB4DC \uD6C4 \uBC14\uB85C \uBC18\uC601\uB429\uB2C8\uB2E4."), /* @__PURE__ */ React.createElement(Group, { align: "flex-start", grow: true, mb: "md" }, /* @__PURE__ */ React.createElement(Stack, { gap: 6 }, /* @__PURE__ */ React.createElement(Text, { size: "sm", fw: 600 }, "\uD604\uC7AC favicon"), /* @__PURE__ */ React.createElement(
      "img",
      {
        src: siteFaviconUrl,
        alt: "Current favicon",
        style: { width: 32, height: 32, borderRadius: 6, border: "1px solid #e5e7eb" }
      }
    ), /* @__PURE__ */ React.createElement(Text, { size: "xs", c: "dimmed", style: { wordBreak: "break-all" } }, siteFaviconObjectKey || "/public/favicon.png"), /* @__PURE__ */ React.createElement(Anchor, { size: "xs", href: siteFaviconUrl, target: "_blank", rel: "noopener" }, "\uC0C8 \uD0ED\uC73C\uB85C \uBCF4\uAE30")), /* @__PURE__ */ React.createElement(Stack, { gap: 8 }, /* @__PURE__ */ React.createElement(Text, { size: "sm", fw: 600 }, "\uC0C8 favicon \uC5C5\uB85C\uB4DC"), /* @__PURE__ */ React.createElement(
      "input",
      {
        type: "file",
        accept: "image/*,.ico",
        onChange: (event) => {
          const selected = event.currentTarget.files?.[0] ?? null;
          setFaviconFile(selected);
        }
      }
    ), /* @__PURE__ */ React.createElement(Group, null, /* @__PURE__ */ React.createElement(
      Button,
      {
        variant: "filled",
        loading: uploadingFavicon,
        disabled: !faviconFile || !selectedSiteSeq,
        onClick: async () => {
          await uploadSiteFavicon(faviconFile, selectedSiteSeq);
          await loadSiteFavicon(selectedSiteSeq);
        }
      },
      "Upload favicon"
    ), /* @__PURE__ */ React.createElement(Button, { variant: "light", disabled: !selectedSiteSeq, onClick: () => loadSiteFavicon(selectedSiteSeq) }, "Refresh"), /* @__PURE__ */ React.createElement(
      Button,
      {
        color: "red",
        variant: "light",
        loading: deletingFavicon,
        disabled: !selectedSiteSeq,
        onClick: async () => {
          await resetSiteFavicon(selectedSiteSeq);
          await loadSiteFavicon(selectedSiteSeq);
          setFaviconFile(null);
        }
      },
      "Reset to default"
    )), /* @__PURE__ */ React.createElement(Text, { size: "xs", c: "dimmed" }, "\uAD8C\uC7A5: 32x32 \uB610\uB294 48x48 PNG/ICO")))), /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React.createElement(Title, { order: 3 }, "Site Header/Footer Theme"), /* @__PURE__ */ React.createElement(Badge, { color: "grape", variant: "light" }, "Per Site")), /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", mb: "md" }, "\uC0AC\uC774\uD2B8\uBCC4 \uD5E4\uB354/\uD478\uD130 \uBC30\uACBD\uC0C9\xB7\uC804\uACBD\uC0C9\uC744 16\uC9C4\uC218(#RGB, #RRGGBB, #RRGGBBAA)\uB85C \uC9C0\uC815\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uBE44\uC6CC\uB450\uBA74 \uAE30\uBCF8 \uC2A4\uD0C0\uC77C\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."), /* @__PURE__ */ React.createElement(SimpleGrid, { cols: { base: 1, lg: 2 }, spacing: "md" }, /* @__PURE__ */ React.createElement(SimpleGrid, { cols: { base: 1, sm: 2 }, spacing: "md" }, /* @__PURE__ */ React.createElement(
      ColorInput,
      {
        label: "Header \uBC30\uACBD\uC0C9",
        placeholder: "#FFFFFF",
        format: "hexa",
        value: siteTheme.headerBackgroundColor,
        onChange: (value) => setSiteTheme((prev) => ({ ...prev, headerBackgroundColor: value })),
        swatches: ["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#6c5ce7", "#0b7285"],
        withEyeDropper: false,
        clearable: true
      }
    ), /* @__PURE__ */ React.createElement(
      ColorInput,
      {
        label: "Header \uC804\uACBD\uC0C9",
        placeholder: "#111111",
        format: "hexa",
        value: siteTheme.headerForegroundColor,
        onChange: (value) => setSiteTheme((prev) => ({ ...prev, headerForegroundColor: value })),
        swatches: ["#111111", "#212529", "#495057", "#ffffff", "#f1f3f5", "#ffd43b"],
        withEyeDropper: false,
        clearable: true
      }
    ), /* @__PURE__ */ React.createElement(
      ColorInput,
      {
        label: "Footer \uBC30\uACBD\uC0C9",
        placeholder: "#FFFFFF",
        format: "hexa",
        value: siteTheme.footerBackgroundColor,
        onChange: (value) => setSiteTheme((prev) => ({ ...prev, footerBackgroundColor: value })),
        swatches: ["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#2b8a3e", "#862e9c"],
        withEyeDropper: false,
        clearable: true
      }
    ), /* @__PURE__ */ React.createElement(
      ColorInput,
      {
        label: "Footer \uC804\uACBD\uC0C9",
        placeholder: "#111111",
        format: "hexa",
        value: siteTheme.footerForegroundColor,
        onChange: (value) => setSiteTheme((prev) => ({ ...prev, footerForegroundColor: value })),
        swatches: ["#111111", "#212529", "#495057", "#ffffff", "#f1f3f5", "#ff922b"],
        withEyeDropper: false,
        clearable: true
      }
    )), /* @__PURE__ */ React.createElement(Paper, { withBorder: true, radius: "md", p: "md", style: { overflow: "hidden" } }, /* @__PURE__ */ React.createElement(Text, { size: "sm", fw: 600, mb: 8 }, "\uBBF8\uB9AC\uBCF4\uAE30"), /* @__PURE__ */ React.createElement(Stack, { gap: 0, style: { borderRadius: 10, overflow: "hidden", border: "1px solid #e9ecef" } }, /* @__PURE__ */ React.createElement(
      "div",
      {
        style: {
          backgroundColor: siteTheme.headerBackgroundColor || "#f8f9fa",
          color: siteTheme.headerForegroundColor || "#111111",
          padding: "12px 14px",
          fontWeight: 600
        }
      },
      "Header Preview"
    ), /* @__PURE__ */ React.createElement("div", { style: { padding: "16px 14px", backgroundColor: "#ffffff", color: "#495057" } }, "\uCF58\uD150\uCE20 \uC601\uC5ED (\uACE0\uC815 \uBBF8\uB9AC\uBCF4\uAE30)"), /* @__PURE__ */ React.createElement(
      "div",
      {
        style: {
          backgroundColor: siteTheme.footerBackgroundColor || "#f8f9fa",
          color: siteTheme.footerForegroundColor || "#111111",
          padding: "12px 14px",
          fontWeight: 600
        }
      },
      "Footer Preview"
    )))), /* @__PURE__ */ React.createElement(Group, { mt: "md" }, /* @__PURE__ */ React.createElement(
      Button,
      {
        variant: "filled",
        color: "grape",
        loading: savingSiteTheme,
        disabled: !selectedSiteSeq,
        onClick: async () => {
          await saveSiteTheme(selectedSiteSeq, siteTheme);
          await loadSiteTheme(selectedSiteSeq);
        }
      },
      "Save theme"
    ), /* @__PURE__ */ React.createElement(Button, { variant: "light", disabled: !selectedSiteSeq, onClick: () => loadSiteTheme(selectedSiteSeq) }, "Refresh"), /* @__PURE__ */ React.createElement(
      Button,
      {
        color: "gray",
        variant: "light",
        disabled: !selectedSiteSeq,
        onClick: async () => {
          const emptyTheme = {
            headerBackgroundColor: "",
            headerForegroundColor: "",
            footerBackgroundColor: "",
            footerForegroundColor: ""
          };
          setSiteTheme(emptyTheme);
          await saveSiteTheme(selectedSiteSeq, emptyTheme);
          await loadSiteTheme(selectedSiteSeq);
        }
      },
      "Reset"
    ))));
  }
  if (page === "operations") {
    return /* @__PURE__ */ React.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React.createElement(
      SchedulerTable,
      {
        schedulers,
        runningSchedulerName,
        onRun: runScheduler,
        onRefresh: reloadSchedulers
      }
    ));
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
  const [pathname, setPathname] = useState(window.location.pathname);
  const [search, setSearch] = useState(window.location.search);
  const pageTitle = pageTitleByKey(page);
  useEffect(() => {
    const onPopState = () => {
      setPathname(window.location.pathname);
      setSearch(window.location.search);
      setPage(routeToPage(window.location.pathname));
    };
    window.addEventListener("popstate", onPopState);
    return () => {
      window.removeEventListener("popstate", onPopState);
    };
  }, []);
  const onNavigate = useCallback(
    (href) => {
      const nextUrl = new URL(href, window.location.origin);
      const currentPathWithSearch = `${window.location.pathname}${window.location.search}`;
      const nextPathWithSearch = `${nextUrl.pathname}${nextUrl.search}`;
      if (currentPathWithSearch !== nextPathWithSearch) {
        window.history.pushState({}, "", href);
      }
      setPathname(nextUrl.pathname);
      setSearch(nextUrl.search);
      setPage(routeToPage(nextUrl.pathname));
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
      /* @__PURE__ */ React.createElement(AppShell.Navbar, { p: "md" }, /* @__PURE__ */ React.createElement(Stack, { mb: "md", gap: 4 }, /* @__PURE__ */ React.createElement(Text, { fw: 700, size: "lg" }, "AhaWiki Admin")), /* @__PURE__ */ React.createElement(Navigation, { activePage: page, onNavigate })),
      /* @__PURE__ */ React.createElement(AppShell.Main, null, /* @__PURE__ */ React.createElement(Stack, { gap: "md" }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", align: "center" }, /* @__PURE__ */ React.createElement(Stack, { gap: 2 }, /* @__PURE__ */ React.createElement(Title, { order: 2 }, pageTitle)), /* @__PURE__ */ React.createElement(Badge, { variant: "light", color: "indigo", size: "lg" }, "Live")), /* @__PURE__ */ React.createElement(AdminContent, { page, onNavigate, pathname, search })))
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
