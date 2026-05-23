// app/assets/js/admin.jsx
import React23 from "react";
import { createRoot } from "react-dom/client";
import { createBrowserRouter, RouterProvider } from "react-router-dom";
import { MantineProvider } from "@mantine/core";

// app/assets/js/admin/layout/AdminLayout.jsx
import React3, { useEffect as useEffect2, useState as useState2 } from "react";
import { Outlet, useNavigate as useNavigate2 } from "react-router-dom";
import { AppShell, Badge as Badge2, Group as Group2, Stack as Stack2, Text as Text2, Title } from "@mantine/core";

// app/assets/js/admin/navigation.jsx
import React, { useEffect, useMemo, useState } from "react";
import { useLocation, useNavigate } from "react-router-dom";
import { Badge, Divider, Group, NavLink, Paper, Skeleton, Stack, Text } from "@mantine/core";

// app/assets/js/admin/api.js
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
  if (!response.ok) throw new Error(`HTTP ${response.status}`);
  const data = await response.json();
  logInfo("fetch:success", url, { count: Array.isArray(data) ? data.length : void 0 });
  return data;
}
async function fetchCsrfToken() {
  const response = await fetch("/api/csrf", { credentials: "same-origin" });
  if (!response.ok) throw new Error(`CSRF HTTP ${response.status}`);
  const token = await response.json();
  return { name: token?.name ?? "csrfToken", value: token?.value ?? "" };
}

// app/assets/js/admin/navigation.jsx
function parseSiteSeqFromPathname(pathname) {
  const matched = pathname.match(/^\/Admin\/(?:Site\/)?(\d+)(?:\/(?:Detail|Config|Cache|Permission|AccessLog|Admins))?$/);
  if (!matched) return "";
  const siteSeq = Number.parseInt(matched[1], 10);
  return Number.isFinite(siteSeq) && siteSeq > 0 ? String(siteSeq) : "";
}
function SiteNavItem({ site, currentPathname, currentSiteSeq, isAdmin }) {
  const navigate = useNavigate();
  const siteSeq = String(site.seq);
  const isCurrentSite = siteSeq === String(currentSiteSeq);
  const isActiveSite = parseSiteSeqFromPathname(currentPathname) === siteSeq;
  const navTo = (href) => (event) => {
    event.preventDefault();
    navigate(href);
  };
  return /* @__PURE__ */ React.createElement(NavLink, { href: `/Admin/Site/${site.seq}`, label: `${site.name} (#${site.seq})`, leftSection: /* @__PURE__ */ React.createElement("i", { className: "fas fa-globe-asia", "aria-hidden": "true" }), active: isActiveSite, opened: isCurrentSite || isActiveSite, variant: isActiveSite ? "filled" : "light", onClick: navTo(`/Admin/Site/${site.seq}`) }, /* @__PURE__ */ React.createElement(NavLink, { href: `/Admin/Site/${site.seq}/Detail`, label: "Meta\uBAA9\uB85D", leftSection: /* @__PURE__ */ React.createElement("i", { className: "fas fa-list", "aria-hidden": "true" }), active: currentPathname === `/Admin/Site/${site.seq}/Detail`, variant: currentPathname === `/Admin/Site/${site.seq}/Detail` ? "filled" : "subtle", onClick: navTo(`/Admin/Site/${site.seq}/Detail`) }), /* @__PURE__ */ React.createElement(NavLink, { href: `/Admin/Site/${site.seq}/Config`, label: "Config", leftSection: /* @__PURE__ */ React.createElement("i", { className: "fas fa-sliders-h", "aria-hidden": "true" }), active: currentPathname === `/Admin/Site/${site.seq}/Config`, variant: currentPathname === `/Admin/Site/${site.seq}/Config` ? "filled" : "subtle", onClick: navTo(`/Admin/Site/${site.seq}/Config`) }), /* @__PURE__ */ React.createElement(NavLink, { href: `/Admin/Site/${site.seq}/Cache`, label: "Cache", leftSection: /* @__PURE__ */ React.createElement("i", { className: "fas fa-database", "aria-hidden": "true" }), active: currentPathname === `/Admin/Site/${site.seq}/Cache`, variant: currentPathname === `/Admin/Site/${site.seq}/Cache` ? "filled" : "subtle", onClick: navTo(`/Admin/Site/${site.seq}/Cache`) }), /* @__PURE__ */ React.createElement(NavLink, { href: `/Admin/Site/${site.seq}/Permission`, label: "Permission", leftSection: /* @__PURE__ */ React.createElement("i", { className: "fas fa-key", "aria-hidden": "true" }), active: currentPathname === `/Admin/Site/${site.seq}/Permission`, variant: currentPathname === `/Admin/Site/${site.seq}/Permission` ? "filled" : "subtle", onClick: navTo(`/Admin/Site/${site.seq}/Permission`) }), isAdmin && /* @__PURE__ */ React.createElement(NavLink, { href: `/Admin/${site.seq}/AccessLog`, label: "AccessLog", leftSection: /* @__PURE__ */ React.createElement("i", { className: "fas fa-network-wired", "aria-hidden": "true" }), active: currentPathname === `/Admin/${site.seq}/AccessLog`, variant: currentPathname === `/Admin/${site.seq}/AccessLog` ? "filled" : "subtle", onClick: navTo(`/Admin/${site.seq}/AccessLog`) }), isAdmin && /* @__PURE__ */ React.createElement(NavLink, { href: `/Admin/Site/${site.seq}/Admins`, label: "SiteAdmins", leftSection: /* @__PURE__ */ React.createElement("i", { className: "fas fa-user-shield", "aria-hidden": "true" }), active: currentPathname === `/Admin/Site/${site.seq}/Admins`, variant: currentPathname === `/Admin/Site/${site.seq}/Admins` ? "filled" : "subtle", onClick: navTo(`/Admin/Site/${site.seq}/Admins`) }));
}
function Navigation({ me, onNavigate }) {
  const [siteLinks, setSiteLinks] = useState([]);
  const location = useLocation();
  const navigate = useNavigate();
  const currentPathname = location.pathname;
  useEffect(() => {
    if (!me?.loggedIn) return;
    let mounted = true;
    fetchJson("/api/Admin/Sites").then((data) => {
      if (mounted) setSiteLinks(Array.isArray(data) ? data : []);
    }).catch((err) => console.error("[AdminUI] navigation:sites:error", err));
    return () => {
      mounted = false;
    };
  }, [me]);
  const globalLinks = useMemo(() => [
    { href: "/Admin", label: "Dashboard", key: "dashboard", iconClassName: "fas fa-chart-line" },
    { href: "/Admin/User", label: "User", key: "all-users", iconClassName: "fas fa-users" },
    { href: "/Admin/RecentChange", label: "RecentChanges", key: "recent-changes", iconClassName: "fas fa-history" },
    { href: "/Admin/AccessLog", label: "AccessLog", key: "access-logs", iconClassName: "fas fa-network-wired" },
    { href: "/Admin/CrawlerCache", label: "Crawler Cache", key: "crawler-cache", iconClassName: "fas fa-spider" },
    { href: "/Admin/S3", label: "S3 Browser", key: "s3-browser", iconClassName: "fas fa-folder-open" }
  ], []);
  if (!me) {
    return /* @__PURE__ */ React.createElement(Stack, { gap: 8 }, /* @__PURE__ */ React.createElement(Paper, { withBorder: true, radius: "md", p: 8 }, /* @__PURE__ */ React.createElement(Skeleton, { height: 12, mb: 8, radius: "sm" }), [1, 2, 3].map((i) => /* @__PURE__ */ React.createElement(Skeleton, { key: i, height: 32, mb: 4, radius: "sm" }))));
  }
  const isAdmin = me?.isAdmin ?? false;
  const currentSiteSeq = me?.currentSiteSeq ?? "";
  return /* @__PURE__ */ React.createElement(Stack, { gap: 8 }, isAdmin && /* @__PURE__ */ React.createElement(Paper, { withBorder: true, radius: "md", p: 8 }, /* @__PURE__ */ React.createElement(Text, { size: "xs", c: "dimmed", fw: 700, tt: "uppercase", mb: 6 }, "\uC804\uCCB4 \uAD00\uB9AC"), /* @__PURE__ */ React.createElement(Stack, { gap: 4 }, globalLinks.map((link) => {
    const isActive = currentPathname === link.href || link.key === "all-users" && currentPathname.startsWith("/Admin/User");
    return /* @__PURE__ */ React.createElement(NavLink, { key: link.key, href: link.href, label: link.label, leftSection: /* @__PURE__ */ React.createElement("i", { className: link.iconClassName, "aria-hidden": "true" }), active: isActive, variant: isActive ? "filled" : "light", onClick: (e) => {
      e.preventDefault();
      navigate(link.href);
    } });
  }))), /* @__PURE__ */ React.createElement(Paper, { withBorder: true, radius: "md", p: 8 }, /* @__PURE__ */ React.createElement(Group, { justify: "space-between", mb: 6 }, /* @__PURE__ */ React.createElement(Text, { size: "xs", c: "dimmed", fw: 700, tt: "uppercase" }, "Sites"), isAdmin && /* @__PURE__ */ React.createElement(Badge, { color: "indigo", variant: "light", size: "sm" }, siteLinks.length)), /* @__PURE__ */ React.createElement(Stack, { gap: 2 }, siteLinks.map((site) => /* @__PURE__ */ React.createElement(SiteNavItem, { key: `site-${site.seq}`, site, currentPathname, currentSiteSeq, isAdmin })), siteLinks.length === 0 && /* @__PURE__ */ React.createElement(Text, { size: "sm", c: "dimmed", px: "sm", py: 6 }, "\uB4F1\uB85D\uB41C Site \uAC00 \uC5C6\uC2B5\uB2C8\uB2E4."))), /* @__PURE__ */ React.createElement(Divider, { my: 6, label: "\uBC14\uB85C\uAC00\uAE30", labelPosition: "center" }), /* @__PURE__ */ React.createElement(Paper, { withBorder: true, radius: "md", p: 6 }, /* @__PURE__ */ React.createElement(NavLink, { href: "/", label: "\uC704\uD0A4\uB85C \uB3CC\uC544\uAC00\uAE30", description: "\uAD00\uB9AC\uC790 \uC601\uC5ED\uC744 \uB098\uAC00 \uBA54\uC778 \uC704\uD0A4\uB85C \uC774\uB3D9", leftSection: /* @__PURE__ */ React.createElement("i", { className: "fas fa-arrow-left", "aria-hidden": "true" }), rightSection: /* @__PURE__ */ React.createElement("i", { className: "fas fa-external-link-alt", "aria-hidden": "true" }), color: "gray", variant: "subtle" })));
}

// app/assets/js/admin/context/AdminContext.jsx
import React2, { createContext, useContext } from "react";
var AdminContext = createContext(null);
function useAdminContext() {
  return useContext(AdminContext);
}

// app/assets/js/admin/layout/AdminLayout.jsx
function pageTitleFromPath(pathname) {
  if (/^\/Admin\/Site\/\d+\/Permission/.test(pathname)) return "Permission";
  if (/^\/Admin\/Site\/\d+\/Config/.test(pathname)) return "Config";
  if (/^\/Admin\/Site\/\d+\/Cache/.test(pathname)) return "Cache";
  if (/^\/Admin\/Site\/\d+\/Admins/.test(pathname)) return "SiteAdmins";
  if (/^\/Admin\/Site\/\d+\/Detail/.test(pathname)) return "Meta\uBAA9\uB85D";
  if (/^\/Admin\/Site\/\d+/.test(pathname)) return "Dashboard";
  if (/^\/Admin\/Site/.test(pathname)) return "Sites";
  if (/\/UserViewHistory/.test(pathname) || /\/UserViews/.test(pathname)) return "User";
  if (/\/User/.test(pathname) || /\/AllUsers/.test(pathname)) return "User";
  if (/\/AccessLog/.test(pathname)) return "AccessLog";
  if (/\/RecentChange/.test(pathname)) return "RecentChanges";
  if (/\/S3/.test(pathname)) return "S3 Browser";
  if (/\/CrawlerCache/.test(pathname)) return "Crawler Cache";
  return "Dashboard";
}
function AdminLayout() {
  const [me, setMe] = useState2(null);
  const navigate = useNavigate2();
  const pathname = window.location.pathname;
  const pageTitle = pageTitleFromPath(pathname);
  useEffect2(() => {
    fetchJson("/api/me").then(setMe).catch((err) => {
      logError("app:me:error", err);
      setMe({ loggedIn: false });
    });
  }, []);
  return /* @__PURE__ */ React3.createElement(AdminContext.Provider, { value: { me } }, /* @__PURE__ */ React3.createElement(AppShell, { padding: "md", navbar: { width: 240, breakpoint: "sm" } }, /* @__PURE__ */ React3.createElement(AppShell.Navbar, { p: "md", style: { overflowY: "auto" } }, /* @__PURE__ */ React3.createElement(Stack2, { mb: "md", gap: 4 }, /* @__PURE__ */ React3.createElement(Text2, { fw: 700, size: "lg" }, "AhaWiki Admin")), /* @__PURE__ */ React3.createElement(Navigation, { me, onNavigate: (href) => navigate(href) })), /* @__PURE__ */ React3.createElement(AppShell.Main, null, /* @__PURE__ */ React3.createElement(Stack2, { gap: "md" }, /* @__PURE__ */ React3.createElement(Group2, { justify: "space-between", align: "center" }, /* @__PURE__ */ React3.createElement(Title, { order: 2 }, pageTitle), /* @__PURE__ */ React3.createElement(Badge2, { variant: "light", color: "indigo", size: "lg" }, "Live")), /* @__PURE__ */ React3.createElement(Outlet, null)))));
}

// app/assets/js/admin/layout/SiteLayout.jsx
import React4 from "react";
import { Outlet as Outlet2, useParams } from "react-router-dom";

// app/assets/js/admin/hooks/useSiteData.js
import { useCallback, useEffect as useEffect3, useState as useState3 } from "react";
function useSiteData(siteSeq) {
  const [sites, setSites] = useState3([]);
  const [sitePageNames, setSitePageNames] = useState3([]);
  const [savingSiteMeta, setSavingSiteMeta] = useState3(false);
  const [error, setError] = useState3("");
  useEffect3(() => {
    fetchJson("/api/Admin/Sites").then(setSites).catch((err) => logError("site-data:sites:error", err));
  }, []);
  useEffect3(() => {
    if (!siteSeq) return;
    fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/PageNames`).then((data) => setSitePageNames(Array.isArray(data) ? data : [])).catch((err) => logError("site-data:page-names:error", err));
  }, [siteSeq]);
  const site = sites.find((s) => String(s.seq) === String(siteSeq)) ?? null;
  const saveSiteMeta = useCallback(async (nextMeta) => {
    if (!siteSeq) return null;
    setSavingSiteMeta(true);
    setError("");
    try {
      const csrfToken = await fetchCsrfToken();
      const payload = new URLSearchParams();
      payload.set("abbr", nextMeta?.abbr ?? "");
      payload.set("mainDomain", nextMeta?.mainDomain ?? "");
      payload.set("publicListedOrder", nextMeta?.publicListedOrder ?? "");
      payload.set(csrfToken.name, csrfToken.value);
      const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}`, {
        method: "PUT",
        credentials: "same-origin",
        headers: { "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8", "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value },
        body: payload.toString()
      });
      if (!response.ok) {
        const payloadJson = await response.json().catch(() => null);
        throw new Error(payloadJson?.error || `HTTP ${response.status}`);
      }
      const updated = await response.json();
      setSites((prev) => prev.map((s) => s.seq === updated.seq ? { ...s, ...updated } : s));
      return updated;
    } catch (err) {
      logError("site-meta:save:error", err);
      setError(err.message || String(err));
      return null;
    } finally {
      setSavingSiteMeta(false);
    }
  }, [siteSeq]);
  const refreshPageNames = useCallback(async () => {
    if (!siteSeq) return [];
    const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/PageNames`);
    const names = Array.isArray(data) ? data : [];
    setSitePageNames(names);
    return names;
  }, [siteSeq]);
  return { site, sites, sitePageNames, saveSiteMeta, savingSiteMeta, refreshPageNames, error };
}

// app/assets/js/admin/layout/SiteLayout.jsx
function SiteLayout() {
  const { siteSeq } = useParams();
  const { me } = useAdminContext();
  const { site, sitePageNames, saveSiteMeta, savingSiteMeta, refreshPageNames, error } = useSiteData(siteSeq);
  return /* @__PURE__ */ React4.createElement(Outlet2, { context: { site, siteSeq, sitePageNames, me, refreshPageNames, saveSiteMeta, savingSiteMeta, error } });
}

// app/assets/js/admin/pages/DashboardPage.jsx
import React8, { useEffect as useEffect4, useMemo as useMemo2 } from "react";
import { useNavigate as useNavigate3 } from "react-router-dom";
import { Anchor, Badge as Badge5, Button as Button2, Card as Card3, Group as Group5, SimpleGrid, Stack as Stack4, Text as Text5, ThemeIcon, Title as Title4 } from "@mantine/core";

// app/assets/js/admin/hooks/useDashboardData.js
import { useCallback as useCallback2, useState as useState4 } from "react";
function useDashboardData() {
  const [loading, setLoading] = useState4(true);
  const [error, setError] = useState4("");
  const [sites, setSites] = useState4([]);
  const [allUsers, setAllUsers] = useState4([]);
  const [dailyStats, setDailyStats] = useState4({ userCreated: [], pageCreated: [], pageEdited: [] });
  const [recentChanges, setRecentChanges] = useState4([]);
  const [topViewedPages, setTopViewedPages] = useState4([]);
  const [memoryCacheStats, setMemoryCacheStats] = useState4([]);
  const load = useCallback2(async () => {
    setLoading(true);
    setError("");
    try {
      const [siteData, allUserData, dailyStatsData, recentChangesData, topViewedPagesData] = await Promise.all([
        fetchJson("/api/Admin/Sites"),
        fetchJson("/api/Admin/Users"),
        fetchJson("/api/Admin/DailyStats"),
        fetchJson("/api/Admin/RecentChanges?n=30"),
        fetchJson("/api/Admin/TopViewedPages?n=30")
      ]);
      const allUserRows = Array.isArray(allUserData?.array) ? allUserData.array : Array.isArray(allUserData) ? allUserData : [];
      setSites(siteData);
      setAllUsers(allUserRows);
      setRecentChanges(recentChangesData);
      setTopViewedPages(topViewedPagesData);
      setDailyStats({
        userCreated: dailyStatsData?.userCreated ?? [],
        pageCreated: dailyStatsData?.pageCreated ?? [],
        pageEdited: dailyStatsData?.pageEdited ?? []
      });
    } catch (err) {
      logError("dashboard:load:error", err);
      setError(err.message || String(err));
    } finally {
      setLoading(false);
    }
  }, []);
  const loadMemoryCacheStats = useCallback2(async () => {
    try {
      const data = await fetchJson("/api/Admin/MemoryCacheStats");
      setMemoryCacheStats(Array.isArray(data) ? data : []);
    } catch (err) {
      logError("memory-cache:load:error", err);
    }
  }, []);
  return { loading, error, sites, allUsers, dailyStats, recentChanges, topViewedPages, memoryCacheStats, load, loadMemoryCacheStats };
}

// app/assets/js/site/siteWidgets.jsx
import React5 from "react";
import { Badge as Badge3, Button, Card, Divider as Divider2, Group as Group3, Table, Text as Text3, Title as Title2 } from "@mantine/core";
function formatPublicListedOrder(value) {
  return value === null || value === void 0 || value === "" ? "-" : String(value);
}
function SiteListCard({ sites, onNavigate }) {
  return /* @__PURE__ */ React5.createElement(Card, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React5.createElement(Group3, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React5.createElement(Title2, { order: 3 }, "Site List"), /* @__PURE__ */ React5.createElement(Badge3, { color: "indigo", variant: "light" }, sites.length, " sites")), /* @__PURE__ */ React5.createElement(Text3, { size: "sm", c: "dimmed", mb: "md" }, "\uC804\uCCB4 \uC0AC\uC774\uD2B8 \uBAA9\uB85D\uC785\uB2C8\uB2E4. \uC0C1\uC138 \uC124\uC815\uC740 \uAC01 \uC0AC\uC774\uD2B8\uC758 \uAD00\uB9AC \uBC84\uD2BC\uC73C\uB85C \uC774\uB3D9\uD558\uC138\uC694."), /* @__PURE__ */ React5.createElement(Divider2, { mb: "md" }), /* @__PURE__ */ React5.createElement(Table, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true }, /* @__PURE__ */ React5.createElement(Table.Thead, null, /* @__PURE__ */ React5.createElement(Table.Tr, null, /* @__PURE__ */ React5.createElement(Table.Th, null, "Seq"), /* @__PURE__ */ React5.createElement(Table.Th, null, "Name"), /* @__PURE__ */ React5.createElement(Table.Th, null, "Abbr"), /* @__PURE__ */ React5.createElement(Table.Th, null, "Main Domain"), /* @__PURE__ */ React5.createElement(Table.Th, null, "Public Listed Order"), /* @__PURE__ */ React5.createElement(Table.Th, null, "Domains"), /* @__PURE__ */ React5.createElement(Table.Th, null, "Pages"), /* @__PURE__ */ React5.createElement(Table.Th, null, "Action"))), /* @__PURE__ */ React5.createElement(Table.Tbody, null, sites.map((site) => /* @__PURE__ */ React5.createElement(Table.Tr, { key: site.seq }, /* @__PURE__ */ React5.createElement(Table.Td, null, site.seq), /* @__PURE__ */ React5.createElement(Table.Td, null, site.name), /* @__PURE__ */ React5.createElement(Table.Td, null, site.abbr ?? "-"), /* @__PURE__ */ React5.createElement(Table.Td, null, site.mainDomain ?? "-"), /* @__PURE__ */ React5.createElement(Table.Td, null, formatPublicListedOrder(site.publicListedOrder)), /* @__PURE__ */ React5.createElement(Table.Td, null, (site.domains ?? []).join(", ") || "-"), /* @__PURE__ */ React5.createElement(Table.Td, null, site.pageCount ?? 0), /* @__PURE__ */ React5.createElement(Table.Td, null, /* @__PURE__ */ React5.createElement(Button, { size: "xs", variant: "light", onClick: () => onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}`) }, "\uAD00\uB9AC")))))));
}

// app/assets/js/admin/mainWidgets.jsx
import React7 from "react";
import { Area, AreaChart, CartesianGrid, Legend, Line, LineChart, ResponsiveContainer, Tooltip, XAxis, YAxis } from "recharts";
import { Badge as Badge4, Card as Card2, Group as Group4, Progress, Stack as Stack3, Text as Text4, Title as Title3 } from "@mantine/core";

// app/assets/js/component/commonWidgets.jsx
import React6 from "react";
import { Table as Table2 } from "@mantine/core";
function IconChevronUp({ size = 14 }) {
  return /* @__PURE__ */ React6.createElement("svg", { width: size, height: size, viewBox: "0 0 24 24", fill: "none", stroke: "currentColor", strokeWidth: "2", strokeLinecap: "round", strokeLinejoin: "round", "aria-hidden": "true" }, /* @__PURE__ */ React6.createElement("path", { d: "M18 15l-6-6-6 6" }));
}
function IconSelector({ size = 14 }) {
  return /* @__PURE__ */ React6.createElement("svg", { width: size, height: size, viewBox: "0 0 24 24", fill: "none", stroke: "currentColor", strokeWidth: "2", strokeLinecap: "round", strokeLinejoin: "round", "aria-hidden": "true" }, /* @__PURE__ */ React6.createElement("path", { d: "M8 9l4-4 4 4" }), /* @__PURE__ */ React6.createElement("path", { d: "M16 15l-4 4-4-4" }));
}
function makeTable(headers, rows) {
  return /* @__PURE__ */ React6.createElement(Table2, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true, stickyHeader: true, stickyHeaderOffset: 0 }, /* @__PURE__ */ React6.createElement(Table2.Thead, null, /* @__PURE__ */ React6.createElement(Table2.Tr, null, headers.map((header) => /* @__PURE__ */ React6.createElement(Table2.Th, { key: header }, header)))), /* @__PURE__ */ React6.createElement(Table2.Tbody, null, rows.map((columns, rowIndex) => /* @__PURE__ */ React6.createElement(Table2.Tr, { key: `row-${rowIndex}` }, columns.map((column, colIndex) => /* @__PURE__ */ React6.createElement(Table2.Td, { key: `col-${rowIndex}-${colIndex}` }, column ?? ""))))));
}

// app/assets/js/admin/mainWidgets.jsx
function normalizeDailyRows(rows) {
  return [...rows].map((row) => ({ ymd: row.ymd, count: Number(row.count ?? 0) })).sort((left, right) => left.ymd > right.ymd ? 1 : -1);
}
function Sparkline({ rows, color }) {
  const data = normalizeDailyRows(rows).slice(-30);
  if (data.length === 0) return /* @__PURE__ */ React7.createElement(Text4, { size: "xs", c: "dimmed" }, "No data");
  const latest = data[data.length - 1]?.count ?? 0;
  const previous = data[data.length - 2]?.count ?? latest;
  const delta = latest - previous;
  return /* @__PURE__ */ React7.createElement(Stack3, { gap: 4 }, /* @__PURE__ */ React7.createElement("div", { style: { width: "100%", height: 72 }, role: "img", "aria-label": "trend sparkline" }, /* @__PURE__ */ React7.createElement(ResponsiveContainer, { width: "100%", height: "100%" }, /* @__PURE__ */ React7.createElement(AreaChart, { data, margin: { top: 4, right: 0, left: 0, bottom: 0 } }, /* @__PURE__ */ React7.createElement("defs", null, /* @__PURE__ */ React7.createElement("linearGradient", { id: `sparklineGradient-${color}`, x1: "0", y1: "0", x2: "0", y2: "1" }, /* @__PURE__ */ React7.createElement("stop", { offset: "0%", stopColor: `var(--mantine-color-${color}-4)`, stopOpacity: 0.35 }), /* @__PURE__ */ React7.createElement("stop", { offset: "100%", stopColor: `var(--mantine-color-${color}-1)`, stopOpacity: 0.1 }))), /* @__PURE__ */ React7.createElement(Tooltip, { cursor: false, labelFormatter: (value) => `Date: ${value}`, formatter: (value) => [value, "Count"] }), /* @__PURE__ */ React7.createElement(Area, { type: "monotone", dataKey: "count", stroke: `var(--mantine-color-${color}-6)`, strokeWidth: 2, fill: `url(#sparklineGradient-${color})`, dot: false, activeDot: { r: 3 }, isAnimationActive: false })))), /* @__PURE__ */ React7.createElement(Group4, { justify: "space-between" }, /* @__PURE__ */ React7.createElement(Text4, { size: "xs", c: "dimmed" }, "\uCD5C\uADFC 30\uC77C"), /* @__PURE__ */ React7.createElement(Badge4, { color: delta >= 0 ? "teal" : "red", variant: "light", size: "xs" }, delta >= 0 ? "+" : "", delta, " vs yesterday")));
}
function StatTrendCard({ title, total, rows, color }) {
  return /* @__PURE__ */ React7.createElement(Card2, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React7.createElement(Stack3, { gap: 8 }, /* @__PURE__ */ React7.createElement(Group4, { justify: "space-between", align: "flex-start" }, /* @__PURE__ */ React7.createElement(Text4, { size: "sm", c: "dimmed" }, title), /* @__PURE__ */ React7.createElement(Badge4, { color, variant: "light" }, "30d")), /* @__PURE__ */ React7.createElement(Title3, { order: 3 }, total), /* @__PURE__ */ React7.createElement(Sparkline, { rows, color })));
}
function MultiTrendChart({ series }) {
  const dateSet = /* @__PURE__ */ new Set();
  series.forEach((line) => normalizeDailyRows(line.rows).forEach((row) => dateSet.add(row.ymd)));
  const dates = [...dateSet].sort().slice(-30);
  if (dates.length === 0) return /* @__PURE__ */ React7.createElement(Text4, { c: "dimmed", size: "sm" }, "\uCC28\uD2B8 \uB370\uC774\uD130\uAC00 \uC5C6\uC2B5\uB2C8\uB2E4.");
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
  return /* @__PURE__ */ React7.createElement(Stack3, { gap: 8 }, /* @__PURE__ */ React7.createElement("div", { style: { width: "100%", height: 280 }, role: "img", "aria-label": "daily trends chart" }, /* @__PURE__ */ React7.createElement(ResponsiveContainer, { width: "100%", height: "100%" }, /* @__PURE__ */ React7.createElement(LineChart, { data: chartData, margin: { top: 8, right: 12, bottom: 8, left: 0 } }, /* @__PURE__ */ React7.createElement(CartesianGrid, { stroke: "var(--mantine-color-gray-2)", strokeDasharray: "3 3" }), /* @__PURE__ */ React7.createElement(XAxis, { dataKey: "date", tickFormatter: (value) => value.slice(5), tick: { fontSize: 12 } }), /* @__PURE__ */ React7.createElement(YAxis, { allowDecimals: false, tick: { fontSize: 12 } }), /* @__PURE__ */ React7.createElement(Tooltip, { labelFormatter: (value) => `Date: ${value}` }), /* @__PURE__ */ React7.createElement(Legend, { verticalAlign: "top", height: 30 }), series.map((line) => /* @__PURE__ */ React7.createElement(Line, { key: line.name, type: "monotone", dataKey: line.name, stroke: `var(--mantine-color-${colorByName[line.name]}-6)`, strokeWidth: 2.5, dot: false, activeDot: { r: 4 }, isAnimationActive: false }))))), /* @__PURE__ */ React7.createElement(Group4, { gap: 8 }, series.map((line) => /* @__PURE__ */ React7.createElement(Badge4, { key: line.name, color: line.color, variant: "light" }, line.name))));
}

// app/assets/js/admin/utils.js
import dayjs from "dayjs";

// app/assets/js/admin/constants.js
var CRAWLER_CACHE_PAGE_SIZE = 20;
var ADMIN_PAGE_META_PAGE_SIZE = 20;
var ACCESS_LOG_PAGE_SIZE = 20;
var PERMISSION_TARGET_TYPE_OPTIONS = ["All", "Exact", "StartsWith", "EndsWith", "RegularExpression"];
var PERMISSION_ACTOR_TYPE_OPTIONS = ["All", "Login", "Exact", "Domain"];
var PERMISSION_ACTION_DEFINITIONS = [
  { value: "None", action: 0 },
  { value: "Read", action: 1 },
  { value: "Edit", action: 2 },
  { value: "Create", action: 4 },
  { value: "Upload", action: 8 },
  { value: "Delete", action: 16 },
  { value: "Admin", action: 255 }
];

// app/assets/js/admin/utils.js
function toPascalCase(value) {
  if (!value) return "";
  return String(value).replace(/(^|[-_\s]+)([a-z0-9])/g, (_match, _separator, character) => character.toUpperCase());
}
var PERMISSION_ACTION_OPTIONS = PERMISSION_ACTION_DEFINITIONS.map(({ value, action }) => ({
  value,
  label: `${action} - ${toPascalCase(value)}`
}));
function formatPermissionAction(actionName, action) {
  const definition = PERMISSION_ACTION_DEFINITIONS.find((item) => item.value === actionName || item.action === action);
  const resolvedActionName = actionName || definition?.value || String(action ?? "");
  const resolvedAction = action ?? definition?.action;
  const label = toPascalCase(resolvedActionName);
  return resolvedAction === void 0 || resolvedAction === null || resolvedAction === "" ? label : `${resolvedAction} - ${label}`;
}
function compareValuesForSort(leftValue, rightValue, direction) {
  const directionMultiplier = direction === "desc" ? -1 : 1;
  const normalizedLeft = leftValue ?? "";
  const normalizedRight = rightValue ?? "";
  if (typeof normalizedLeft === "number" && typeof normalizedRight === "number") {
    return (normalizedLeft - normalizedRight) * directionMultiplier;
  }
  return String(normalizedLeft).localeCompare(String(normalizedRight), void 0, { numeric: true, sensitivity: "base" }) * directionMultiplier;
}
function formatDateTimeInClientTimezone(value) {
  if (!value) return "-";
  const parsed = dayjs(value);
  if (!parsed.isValid()) return value;
  return parsed.format("YYYY-MM-DDTHH:mm:ssZ");
}
function formatCrawlerStatus(value) {
  if (!value) return "-";
  if (typeof value === "string") return value;
  if (typeof value === "object") {
    const keys = Object.keys(value);
    if (keys.length > 0) return keys.join(", ");
  }
  return String(value);
}
function resolveSiteUrl(row, siteDomainBySeq) {
  const domainFromRow = typeof row.siteDomain === "string" ? row.siteDomain.trim() : "";
  const domainFromSites = (siteDomainBySeq?.get(row.siteSeq) ?? "").trim();
  const domain = domainFromRow || domainFromSites;
  return domain ? `https://${domain}` : "";
}

// app/assets/js/admin/pages/DashboardPage.jsx
function DashboardPage() {
  const { me } = useAdminContext();
  const navigate = useNavigate3();
  const { loading, error, sites, allUsers, dailyStats, recentChanges, topViewedPages, memoryCacheStats, load, loadMemoryCacheStats } = useDashboardData();
  useEffect4(() => {
    if (me === null) return;
    if (me?.isAdmin) {
      load();
      loadMemoryCacheStats();
    } else {
      load();
    }
  }, [me]);
  const siteDomainBySeq = useMemo2(() => new Map(sites.map((s) => [s.seq, (s.domains ?? []).find((d) => !!d) ?? ""])), [sites]);
  if (loading) return null;
  if (!me?.isAdmin) {
    return /* @__PURE__ */ React8.createElement(Stack4, { gap: "md" }, /* @__PURE__ */ React8.createElement(Card3, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React8.createElement(Title4, { order: 3, mb: "xs" }, "\uB0B4 \uB2F4\uB2F9 \uC0AC\uC774\uD2B8"), /* @__PURE__ */ React8.createElement(Text5, { size: "sm", c: "dimmed", mb: "md" }, "SiteAdmin\uC73C\uB85C \uB4F1\uB85D\uB41C \uC0AC\uC774\uD2B8 \uBAA9\uB85D\uC785\uB2C8\uB2E4."), /* @__PURE__ */ React8.createElement(SimpleGrid, { cols: { base: 1, sm: 2 }, spacing: "md" }, sites.map((site) => /* @__PURE__ */ React8.createElement(SiteListCard, { key: site.seq, sites: [site], onNavigate: (href) => navigate(href) }))), sites.length === 0 && /* @__PURE__ */ React8.createElement(Text5, { c: "dimmed", size: "sm" }, "\uB2F4\uB2F9 \uC0AC\uC774\uD2B8\uAC00 \uC5C6\uC2B5\uB2C8\uB2E4.")));
  }
  return /* @__PURE__ */ React8.createElement(Stack4, { gap: "lg" }, /* @__PURE__ */ React8.createElement(SimpleGrid, { cols: { base: 1, sm: 2, lg: 3 }, spacing: "md" }, /* @__PURE__ */ React8.createElement(Card3, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React8.createElement(Group5, { justify: "space-between", align: "flex-start" }, /* @__PURE__ */ React8.createElement(Stack4, { gap: 2 }, /* @__PURE__ */ React8.createElement(Text5, { size: "sm", c: "dimmed" }, "Sites"), /* @__PURE__ */ React8.createElement(Title4, { order: 2 }, sites.length)), /* @__PURE__ */ React8.createElement(ThemeIcon, { color: "indigo", variant: "light", radius: "xl" }, "S"))), /* @__PURE__ */ React8.createElement(Card3, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React8.createElement(Group5, { justify: "space-between", align: "flex-start" }, /* @__PURE__ */ React8.createElement(Stack4, { gap: 2 }, /* @__PURE__ */ React8.createElement(Text5, { size: "sm", c: "dimmed" }, "All Users"), /* @__PURE__ */ React8.createElement(Title4, { order: 2 }, allUsers.length)), /* @__PURE__ */ React8.createElement(ThemeIcon, { color: "teal", variant: "light", radius: "xl" }, "U"))), /* @__PURE__ */ React8.createElement(Card3, { withBorder: true, radius: "md", padding: "md" }, /* @__PURE__ */ React8.createElement(Group5, { justify: "space-between", align: "flex-start" }, /* @__PURE__ */ React8.createElement(Stack4, { gap: 2 }, /* @__PURE__ */ React8.createElement(Text5, { size: "sm", c: "dimmed" }, "30\uC77C \uBB38\uC11C \uC218\uC815"), /* @__PURE__ */ React8.createElement(Title4, { order: 2 }, dailyStats.pageEdited.reduce((sum, item) => sum + (item.count ?? 0), 0))), /* @__PURE__ */ React8.createElement(ThemeIcon, { color: "grape", variant: "light", radius: "xl" }, "E")))), /* @__PURE__ */ React8.createElement(Card3, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React8.createElement(Group5, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React8.createElement(Title4, { order: 3 }, "\uBE60\uB978 \uC774\uB3D9"), /* @__PURE__ */ React8.createElement(Badge5, { color: "indigo", variant: "light" }, "Quick Access")), /* @__PURE__ */ React8.createElement(SimpleGrid, { cols: { base: 1, sm: 3 }, spacing: "sm" }, /* @__PURE__ */ React8.createElement(Button2, { variant: "light", onClick: () => navigate("/Admin/RecentChange") }, "\uCD5C\uADFC \uBCC0\uACBD \uBCF4\uAE30"), /* @__PURE__ */ React8.createElement(Button2, { variant: "light", onClick: () => navigate("/Admin/User") }, "\uC0AC\uC6A9\uC790 \uBAA9\uB85D \uBCF4\uAE30"))), /* @__PURE__ */ React8.createElement(SimpleGrid, { cols: { base: 1, sm: 2, lg: 3 }, spacing: "md" }, /* @__PURE__ */ React8.createElement(StatTrendCard, { title: "New Users", color: "blue", rows: dailyStats.userCreated, total: dailyStats.userCreated.reduce((sum, item) => sum + (item.count ?? 0), 0) }), /* @__PURE__ */ React8.createElement(StatTrendCard, { title: "New Pages", color: "indigo", rows: dailyStats.pageCreated, total: dailyStats.pageCreated.reduce((sum, item) => sum + (item.count ?? 0), 0) }), /* @__PURE__ */ React8.createElement(StatTrendCard, { title: "Page Edits", color: "grape", rows: dailyStats.pageEdited, total: dailyStats.pageEdited.reduce((sum, item) => sum + (item.count ?? 0), 0) })), /* @__PURE__ */ React8.createElement(Card3, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React8.createElement(Group5, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React8.createElement(Title4, { order: 3 }, "30\uC77C \uC6B4\uC601 \uCD94\uC774 \uCC28\uD2B8"), /* @__PURE__ */ React8.createElement(Badge5, { color: "blue", variant: "light" }, "Chart")), /* @__PURE__ */ React8.createElement(MultiTrendChart, { series: [{ name: "New Users", color: "blue", rows: dailyStats.userCreated }, { name: "New Pages", color: "indigo", rows: dailyStats.pageCreated }, { name: "Page Edits", color: "grape", rows: dailyStats.pageEdited }] })), /* @__PURE__ */ React8.createElement(Card3, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React8.createElement(Group5, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React8.createElement(Title4, { order: 3 }, "Most Viewed Pages"), /* @__PURE__ */ React8.createElement(Badge5, { color: "pink", variant: "light" }, Math.min(topViewedPages.length, 30))), makeTable(["Rank", "Site", "Page", "Views", "Last Viewed"], topViewedPages.slice(0, 30).map((row, index) => {
    const siteUrl = resolveSiteUrl(row, siteDomainBySeq);
    const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(row.pageName)}` : "";
    return [index + 1, siteUrl ? /* @__PURE__ */ React8.createElement(Anchor, { href: siteUrl, target: "_blank" }, row.siteName, " (#", row.siteSeq, ")") : `${row.siteName} (#${row.siteSeq})`, pageUrl ? /* @__PURE__ */ React8.createElement(Anchor, { href: pageUrl, target: "_blank" }, row.pageName) : row.pageName, row.viewCount, row.lastViewedAt];
  }))), /* @__PURE__ */ React8.createElement(Card3, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React8.createElement(Group5, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React8.createElement(Title4, { order: 3 }, "Recent Changes (All Sites)"), /* @__PURE__ */ React8.createElement(Badge5, { color: "violet", variant: "light" }, recentChanges.length)), makeTable(["When", "Site", "Page", "Revision", "Editor", "Comment"], recentChanges.map((row) => {
    const siteUrl = resolveSiteUrl(row, siteDomainBySeq);
    const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(row.name)}` : "";
    const revisionUrl = pageUrl ? `${pageUrl}?rev=${row.revision}` : "";
    return [row.dateTime, siteUrl ? /* @__PURE__ */ React8.createElement(Anchor, { href: siteUrl, target: "_blank" }, row.siteName, " (#", row.siteSeq, ")") : `${row.siteName} (#${row.siteSeq})`, pageUrl ? /* @__PURE__ */ React8.createElement(Anchor, { href: pageUrl, target: "_blank" }, row.name) : row.name, revisionUrl ? /* @__PURE__ */ React8.createElement(Anchor, { href: revisionUrl, target: "_blank" }, row.revision) : row.revision, row.nickname ?? "-", row.comment || "-"];
  }))));
}

// app/assets/js/admin/pages/SitesPage.jsx
import React9, { useEffect as useEffect5, useState as useState5 } from "react";
import { useNavigate as useNavigate4 } from "react-router-dom";
function SitesPage() {
  const navigate = useNavigate4();
  const [sites, setSites] = useState5([]);
  useEffect5(() => {
    fetchJson("/api/Admin/Sites").then(setSites).catch((err) => logError("sites:load:error", err));
  }, []);
  return /* @__PURE__ */ React9.createElement(SiteListCard, { sites, onNavigate: (href) => navigate(href) });
}

// app/assets/js/admin/pages/AllUsersPage.jsx
import React11, { useEffect as useEffect6, useState as useState7 } from "react";
import { useNavigate as useNavigate5 } from "react-router-dom";
import { Badge as Badge6, Button as Button3, Card as Card4, Divider as Divider3, Group as Group7, Text as Text7, TextInput, Title as Title5 } from "@mantine/core";
import { DataTable } from "mantine-datatable";

// app/assets/js/admin/hooks/useAllUsersData.js
import { useCallback as useCallback3, useState as useState6 } from "react";
function useAllUsersData() {
  const [loading, setLoading] = useState6(true);
  const [error, setError] = useState6("");
  const [allUsers, setAllUsers] = useState6([]);
  const [allUserCount, setAllUserCount] = useState6(0);
  const loadAllUsers = useCallback3(async ({ page = 1, pageSize = ACCESS_LOG_PAGE_SIZE, search = "", sortBy = "seq", sortOrder = "desc" } = {}) => {
    setLoading(true);
    setError("");
    try {
      const params = new URLSearchParams({ page: String(page), pageSize: String(pageSize), search, sortBy, sortOrder });
      const data = await fetchJson(`/api/Admin/Users?${params.toString()}`);
      const rows = Array.isArray(data?.array) ? data.array : Array.isArray(data) ? data : [];
      setAllUsers(rows);
      setAllUserCount(Number(data?.count ?? rows.length));
    } catch (err) {
      logError("all-users:load:error", err);
      setError(err.message || String(err));
    } finally {
      setLoading(false);
    }
  }, []);
  return { loading, error, allUsers, allUserCount, loadAllUsers };
}

// app/assets/js/admin/components/UserProfileCell.jsx
import React10 from "react";
import { Avatar, Group as Group6, Stack as Stack5, Text as Text6 } from "@mantine/core";
function UserProfileCell({ user }) {
  const fallbackText = user?.nickname?.[0] || user?.email?.[0] || "?";
  return /* @__PURE__ */ React10.createElement(Group6, { gap: 8, wrap: "nowrap" }, /* @__PURE__ */ React10.createElement(Avatar, { src: user?.profileImageUrl || null, radius: "xl", size: "sm" }, fallbackText), /* @__PURE__ */ React10.createElement(Stack5, { gap: 0 }, /* @__PURE__ */ React10.createElement(Text6, { size: "sm", fw: 600 }, user?.nickname || "-"), /* @__PURE__ */ React10.createElement(Text6, { size: "xs", c: "dimmed" }, user?.email || "-")));
}

// app/assets/js/admin/pages/AllUsersPage.jsx
function AllUsersPage() {
  const navigate = useNavigate5();
  const { loading, allUsers, allUserCount, loadAllUsers } = useAllUsersData();
  const [page, setPage] = useState7(1);
  const [searchInput, setSearchInput] = useState7("");
  const [sortBy, setSortBy] = useState7("seq");
  const [sortOrder, setSortOrder] = useState7("desc");
  const totalPages = Math.max(1, Math.ceil(allUserCount / ACCESS_LOG_PAGE_SIZE));
  useEffect6(() => {
    loadAllUsers();
  }, []);
  return /* @__PURE__ */ React11.createElement(Card4, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React11.createElement(Group7, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React11.createElement(Title5, { order: 3 }, "All Users"), /* @__PURE__ */ React11.createElement(Badge6, { color: "blue", variant: "light" }, allUserCount, " users")), /* @__PURE__ */ React11.createElement(Text7, { size: "sm", c: "dimmed", mb: "md" }, "\uC804\uCCB4 \uC0AC\uC774\uD2B8 \uAE30\uC900 \uC0AC\uC6A9\uC790 \uBAA9\uB85D\uC774\uBA70, \uCD5C\uADFC \uBC29\uBB38\uC21C\uC73C\uB85C \uC815\uB82C\uB429\uB2C8\uB2E4."), /* @__PURE__ */ React11.createElement(Divider3, { mb: "md" }), /* @__PURE__ */ React11.createElement(Group7, { mb: "sm", align: "end" }, /* @__PURE__ */ React11.createElement(TextInput, { label: "search", value: searchInput, onChange: (e) => setSearchInput(e.currentTarget.value), placeholder: "email, nickname, seq" }), /* @__PURE__ */ React11.createElement(Button3, { variant: "light", onClick: () => {
    setPage(1);
    loadAllUsers({ page: 1, pageSize: ACCESS_LOG_PAGE_SIZE, search: searchInput, sortBy, sortOrder });
  } }, "\uAC80\uC0C9")), /* @__PURE__ */ React11.createElement(
    DataTable,
    {
      sortIcons: { sorted: /* @__PURE__ */ React11.createElement(IconChevronUp, { size: 14 }), unsorted: /* @__PURE__ */ React11.createElement(IconSelector, { size: 14 }) },
      withTableBorder: true,
      striped: true,
      highlightOnHover: true,
      records: allUsers,
      columns: [
        { accessor: "seq", title: "Seq", sortable: true },
        { accessor: "profile", title: "Profile", render: (row) => /* @__PURE__ */ React11.createElement(UserProfileCell, { user: row }) },
        { accessor: "email", title: "Email", sortable: true },
        { accessor: "nickname", title: "Nickname", sortable: true },
        { accessor: "created", title: "Created", sortable: true },
        { accessor: "updated", title: "Updated", sortable: true },
        { accessor: "visitCount", title: "Visits", sortable: true },
        { accessor: "lastViewed", title: "Last Viewed", sortable: true, render: (row) => row.lastViewed ?? "-" },
        { accessor: "action", title: "Action", render: (row) => /* @__PURE__ */ React11.createElement(Button3, { size: "xs", variant: "light", onClick: () => navigate(`/Admin/User/UserViewHistory?seq=${encodeURIComponent(row.seq)}`) }, "\uC5F4\uB78C \uC774\uB825") }
      ],
      sortStatus: { columnAccessor: sortBy, direction: sortOrder },
      onSortStatusChange: (next) => {
        const dir = next.direction ?? "desc";
        setPage(1);
        setSortBy(next.columnAccessor);
        setSortOrder(dir);
        loadAllUsers({ page: 1, pageSize: ACCESS_LOG_PAGE_SIZE, search: searchInput, sortBy: next.columnAccessor, sortOrder: dir });
      },
      totalRecords: allUserCount,
      recordsPerPage: ACCESS_LOG_PAGE_SIZE,
      page,
      onPageChange: (nextPage) => {
        setPage(nextPage);
        loadAllUsers({ page: nextPage, pageSize: ACCESS_LOG_PAGE_SIZE, search: searchInput, sortBy, sortOrder });
      },
      paginationText: ({ from, to, totalRecords }) => `${from}-${to} / ${totalRecords}`,
      minHeight: 380
    }
  ), /* @__PURE__ */ React11.createElement(Text7, { size: "xs", c: "dimmed", mt: "xs" }, "Page ", page, " / ", totalPages));
}

// app/assets/js/admin/pages/UserViewsPage.jsx
import React12, { useEffect as useEffect7, useMemo as useMemo3 } from "react";
import { useNavigate as useNavigate6, useSearchParams } from "react-router-dom";
import { Anchor as Anchor2, Badge as Badge7, Button as Button4, Card as Card5, Group as Group8, Loader, Text as Text8, Title as Title6 } from "@mantine/core";

// app/assets/js/admin/hooks/useUserViewsData.js
import { useCallback as useCallback4, useState as useState8 } from "react";
function useUserViewsData() {
  const [loading, setLoading] = useState8(false);
  const [userViewHistories, setUserViewHistories] = useState8([]);
  const loadUserViewHistories = useCallback4(async (userSeq, n = 200) => {
    setLoading(true);
    try {
      const clampedN = Math.min(1e3, Math.max(1, Number.parseInt(String(n), 10) || 200));
      const data = await fetchJson(`/api/Admin/UserViews?userSeq=${encodeURIComponent(userSeq)}&n=${encodeURIComponent(clampedN)}`);
      setUserViewHistories(data);
    } catch (err) {
      logError("user-views:load:error", err);
    } finally {
      setLoading(false);
    }
  }, []);
  return { loading, userViewHistories, loadUserViewHistories };
}

// app/assets/js/admin/pages/UserViewsPage.jsx
import { useState as useState9 } from "react";
function UserViewsPage() {
  const navigate = useNavigate6();
  const [searchParams] = useSearchParams();
  const { loading, userViewHistories, loadUserViewHistories } = useUserViewsData();
  const [allUsers, setAllUsers] = useState9([]);
  const userSeq = useMemo3(() => {
    const bySeq = Number.parseInt(searchParams.get("seq") ?? "", 10);
    if (Number.isFinite(bySeq) && bySeq > 0) return bySeq;
    const byLegacy = Number.parseInt(searchParams.get("userSeq") ?? "", 10);
    return Number.isFinite(byLegacy) && byLegacy > 0 ? byLegacy : 0;
  }, [searchParams]);
  const selectedUser = useMemo3(() => allUsers.find((u) => u.seq === userSeq) ?? null, [allUsers, userSeq]);
  useEffect7(() => {
    fetchJson("/api/Admin/Users").then((data) => {
      const rows = Array.isArray(data?.array) ? data.array : Array.isArray(data) ? data : [];
      setAllUsers(rows);
    }).catch((err) => logError("user-views:users:error", err));
  }, []);
  useEffect7(() => {
    if (userSeq > 0) loadUserViewHistories(userSeq, 200);
  }, [userSeq]);
  return /* @__PURE__ */ React12.createElement(Card5, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React12.createElement(Group8, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React12.createElement(Title6, { order: 3 }, "User View Histories"), /* @__PURE__ */ React12.createElement(Badge7, { color: "cyan", variant: "light" }, userViewHistories.length, " rows")), /* @__PURE__ */ React12.createElement(Text8, { size: "sm", c: "dimmed", mb: "md" }, "\uC120\uD0DD\uD55C \uC0AC\uC6A9\uC790\uC758 \uD398\uC774\uC9C0 \uC5F4\uB78C \uC774\uB825\uC785\uB2C8\uB2E4."), /* @__PURE__ */ React12.createElement(Group8, { mb: "md", justify: "space-between" }, /* @__PURE__ */ React12.createElement(Button4, { variant: "light", size: "xs", onClick: () => navigate("/Admin/User") }, "\u2190 User"), selectedUser ? /* @__PURE__ */ React12.createElement(Group8, { gap: 8 }, /* @__PURE__ */ React12.createElement(Text8, { size: "sm" }, "\uC0AC\uC6A9\uC790:"), /* @__PURE__ */ React12.createElement(UserProfileCell, { user: selectedUser })) : /* @__PURE__ */ React12.createElement(Text8, { size: "sm", c: "dimmed" }, "seq\uB97C \uC9C0\uC815\uD574 \uC8FC\uC138\uC694. (/Admin/User/UserViewHistory?seq=\uC22B\uC790)")), loading ? /* @__PURE__ */ React12.createElement(Group8, null, /* @__PURE__ */ React12.createElement(Loader, { size: "sm" }), /* @__PURE__ */ React12.createElement(Text8, { size: "sm", c: "dimmed" }, "\uC5F4\uB78C \uC774\uB825\uC744 \uBD88\uB7EC\uC624\uB294 \uC911\uC785\uB2C8\uB2E4...")) : makeTable(
    ["When", "Site", "Page", "History Seq"],
    userViewHistories.map((history) => {
      const siteUrl = history.siteDomain ? `https://${history.siteDomain}` : "";
      const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(history.pageName)}` : "";
      return [history.viewedAt, siteUrl ? /* @__PURE__ */ React12.createElement(Anchor2, { href: siteUrl, target: "_blank" }, history.siteName, " (#", history.site, ")") : `${history.siteName} (#${history.site})`, pageUrl ? /* @__PURE__ */ React12.createElement(Anchor2, { href: pageUrl, target: "_blank" }, history.pageName) : history.pageName, history.seq];
    })
  ));
}

// app/assets/js/admin/pages/SiteDashboardPage.jsx
import React13 from "react";
import { useOutletContext } from "react-router-dom";
import { Badge as Badge8, Card as Card6, Group as Group9, Stack as Stack6, Text as Text9, Title as Title7 } from "@mantine/core";
function SiteDashboardPage() {
  const { site, sitePageNames } = useOutletContext();
  return /* @__PURE__ */ React13.createElement(Card6, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React13.createElement(Group9, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React13.createElement(Title7, { order: 3 }, "Dashboard"), /* @__PURE__ */ React13.createElement(Badge8, { color: "indigo", variant: "light" }, "Site")), /* @__PURE__ */ React13.createElement(Stack6, { gap: "sm" }, /* @__PURE__ */ React13.createElement(Group9, null, /* @__PURE__ */ React13.createElement(Text9, { size: "sm", c: "dimmed", w: 120 }, "\uC0AC\uC774\uD2B8 \uC774\uB984"), /* @__PURE__ */ React13.createElement(Text9, { fw: 500 }, site?.name ?? "-")), /* @__PURE__ */ React13.createElement(Group9, null, /* @__PURE__ */ React13.createElement(Text9, { size: "sm", c: "dimmed", w: 120 }, "\uB3C4\uBA54\uC778"), /* @__PURE__ */ React13.createElement(Text9, { fw: 500 }, (site?.domains ?? []).join(", ") || "-")), /* @__PURE__ */ React13.createElement(Group9, null, /* @__PURE__ */ React13.createElement(Text9, { size: "sm", c: "dimmed", w: 120 }, "\uD398\uC774\uC9C0 \uC218"), /* @__PURE__ */ React13.createElement(Text9, { fw: 500 }, sitePageNames ? sitePageNames.length : "-"))));
}

// app/assets/js/admin/pages/SiteDetailPage.jsx
import React14, { useEffect as useEffect8, useState as useState11 } from "react";
import { useOutletContext as useOutletContext2 } from "react-router-dom";
import { Badge as Badge9, Button as Button5, Card as Card7, Group as Group10, Text as Text10, Title as Title8 } from "@mantine/core";
import { DataTable as DataTable2 } from "mantine-datatable";

// app/assets/js/admin/hooks/useAdminPageMetaData.js
import { useCallback as useCallback5, useState as useState10 } from "react";
function useAdminPageMetaData(siteSeq) {
  const [rows, setRows] = useState10([]);
  const [count, setCount] = useState10(0);
  const load = useCallback5(async ({ page = 1, pageSize = ADMIN_PAGE_META_PAGE_SIZE, search = "", sortBy = "dateUpdated", sortOrder = "desc" } = {}) => {
    if (!siteSeq) {
      setRows([]);
      setCount(0);
      return;
    }
    try {
      const params = new URLSearchParams({ page: String(page), pageSize: String(pageSize), search, sortBy, sortOrder });
      const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/PageMetaList?${params.toString()}`);
      const result = Array.isArray(data?.array) ? data.array : Array.isArray(data) ? data : [];
      setRows(result);
      setCount(Number(data?.count ?? result.length));
    } catch (err) {
      logError("page-meta:load:error", err);
    }
  }, [siteSeq]);
  return { rows, count, load };
}

// app/assets/js/admin/pages/SiteDetailPage.jsx
import { Anchor as Anchor3, Image } from "@mantine/core";
function SiteDetailPage() {
  const { site, siteSeq, sitePageNames, refreshPageNames } = useOutletContext2();
  const { rows, count, load } = useAdminPageMetaData(siteSeq);
  const [metaPage, setMetaPage] = useState11(1);
  const [searchInput, setSearchInput] = useState11("");
  const [search, setSearch] = useState11("");
  const [sortBy, setSortBy] = useState11("dateUpdated");
  const [sortOrder, setSortOrder] = useState11("desc");
  const [calculating, setCalculating] = useState11(false);
  const [calculateMessage, setCalculateMessage] = useState11("");
  const totalPages = Math.max(1, Math.ceil(count / ADMIN_PAGE_META_PAGE_SIZE));
  useEffect8(() => {
    if (siteSeq) load({ siteSeq, page: 1, pageSize: ADMIN_PAGE_META_PAGE_SIZE, search: "", sortBy: "dateUpdated", sortOrder: "desc" });
  }, [siteSeq]);
  const runCalculate = async (pageName) => {
    if (!siteSeq) return;
    setCalculating(true);
    try {
      const csrfToken = await fetchCsrfToken();
      const suffix = pageName?.trim() ? `?pageName=${encodeURIComponent(pageName.trim())}` : "";
      const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Calculate${suffix}`, { method: "POST", credentials: "same-origin", headers: { "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value, "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8" }, body: `${encodeURIComponent(csrfToken.name)}=${encodeURIComponent(csrfToken.value)}` });
      if (!response.ok) {
        const p = await response.json().catch(() => null);
        throw new Error(p?.error || `HTTP ${response.status}`);
      }
      return await response.json();
    } catch (err) {
      logError("site:calculate:error", err);
      return null;
    } finally {
      setCalculating(false);
    }
  };
  return /* @__PURE__ */ React14.createElement(Card7, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React14.createElement(Group10, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React14.createElement(Title8, { order: 3 }, "Admin Page \uBAA9\uB85D"), /* @__PURE__ */ React14.createElement(Badge9, { color: "indigo", variant: "light" }, count, " rows")), /* @__PURE__ */ React14.createElement(Group10, { mb: "md" }, /* @__PURE__ */ React14.createElement("input", { type: "text", value: searchInput, onChange: (e) => setSearchInput(e.target.value), placeholder: "page name, image", style: { border: "1px solid #ced4da", borderRadius: 6, padding: "6px 10px" } }), /* @__PURE__ */ React14.createElement(Button5, { mt: 22, variant: "filled", onClick: () => {
    setMetaPage(1);
    setSearch(searchInput);
    load({ siteSeq, page: 1, pageSize: ADMIN_PAGE_META_PAGE_SIZE, search: searchInput, sortBy, sortOrder });
  } }, "\uAC80\uC0C9")), /* @__PURE__ */ React14.createElement(
    DataTable2,
    {
      withTableBorder: true,
      borderRadius: "md",
      striped: true,
      highlightOnHover: true,
      records: rows,
      columns: [
        { accessor: "name", title: "Page", sortable: true },
        { accessor: "revision", title: "Revision", sortable: true },
        { accessor: "image", title: "Image", sortable: true, render: (row) => row.image ? /* @__PURE__ */ React14.createElement(Anchor3, { href: row.image, target: "_blank", rel: "noopener" }, /* @__PURE__ */ React14.createElement(Image, { src: row.image, alt: `${row.name} image`, h: 44, w: 72, fit: "cover", radius: "sm", fallbackSrc: "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw==" })) : "-" },
        { accessor: "dateUpdated", title: "Date Updated", sortable: true, render: (row) => formatDateTimeInClientTimezone(row.dateUpdated) },
        { accessor: "size", title: "Size", sortable: true },
        { accessor: "actions", title: "Actions", render: (row) => /* @__PURE__ */ React14.createElement(Button5, { size: "xs", variant: "light", color: "teal", disabled: !siteSeq, loading: calculating, onClick: async () => {
          const res = await runCalculate(row.name);
          if (res) setCalculateMessage(`\uC120\uD0DD \uD398\uC774\uC9C0: ${res?.pageName ?? row.name} (queued)`);
        } }, "\uC7AC\uACC4\uC0B0") }
      ],
      sortStatus: { columnAccessor: sortBy, direction: sortOrder },
      onSortStatusChange: (next) => {
        const dir = next.direction ?? "desc";
        setMetaPage(1);
        setSortBy(next.columnAccessor);
        setSortOrder(dir);
        load({ siteSeq, page: 1, pageSize: ADMIN_PAGE_META_PAGE_SIZE, search, sortBy: next.columnAccessor, sortOrder: dir });
      },
      totalRecords: count,
      recordsPerPage: ADMIN_PAGE_META_PAGE_SIZE,
      page: metaPage,
      onPageChange: (nextPage) => {
        setMetaPage(nextPage);
        load({ siteSeq, page: nextPage, pageSize: ADMIN_PAGE_META_PAGE_SIZE, search, sortBy, sortOrder });
      },
      paginationText: ({ from, to, totalRecords }) => `${from}-${to} / ${totalRecords}`,
      minHeight: 320
    }
  ), calculateMessage ? /* @__PURE__ */ React14.createElement(Text10, { size: "sm", c: "teal", mt: "xs" }, calculateMessage) : null, /* @__PURE__ */ React14.createElement(Text10, { size: "xs", c: "dimmed", mt: "xs" }, "Page ", metaPage, " / ", totalPages));
}

// app/assets/js/admin/pages/SiteConfigPage.jsx
import React15, { useEffect as useEffect9, useState as useState13 } from "react";
import { useNavigate as useNavigate7, useOutletContext as useOutletContext3 } from "react-router-dom";
import { Anchor as Anchor4, Badge as Badge10, Button as Button6, Card as Card8, ColorInput, Group as Group11, Paper as Paper2, SimpleGrid as SimpleGrid2, Stack as Stack7, Text as Text11, TextInput as TextInput2, Title as Title9 } from "@mantine/core";

// app/assets/js/admin/hooks/useSiteConfigData.js
import { useCallback as useCallback6, useState as useState12 } from "react";
var DEFAULT_THEME = { headerBackgroundColor: "", headerForegroundColor: "", bodyBackgroundColor: "", bodyForegroundColor: "", footerBackgroundColor: "", footerForegroundColor: "" };
function useSiteConfigData(siteSeq) {
  const [faviconUrl, setFaviconUrl] = useState12("/public/favicon.png");
  const [faviconObjectKey, setFaviconObjectKey] = useState12("");
  const [uploadingFavicon, setUploadingFavicon] = useState12(false);
  const [deletingFavicon, setDeletingFavicon] = useState12(false);
  const [siteTheme, setSiteTheme] = useState12(DEFAULT_THEME);
  const [savingTheme, setSavingTheme] = useState12(false);
  const [error, setError] = useState12("");
  const loadFavicon = useCallback6(async () => {
    if (!siteSeq) {
      setFaviconUrl("/public/favicon.png");
      setFaviconObjectKey("");
      return;
    }
    try {
      const data = await fetchJson(`/api/Admin/Favicon?siteSeq=${encodeURIComponent(siteSeq)}`);
      setFaviconUrl(data?.faviconUrl || "/public/favicon.png");
      setFaviconObjectKey(data?.objectKey || "");
    } catch (err) {
      logError("favicon:load:error", err);
      setError(err.message);
    }
  }, [siteSeq]);
  const uploadFavicon = useCallback6(async (file) => {
    if (!file || !siteSeq) return;
    setUploadingFavicon(true);
    setError("");
    try {
      const csrfToken = await fetchCsrfToken();
      const formData = new FormData();
      formData.append("file", file);
      formData.append("siteSeq", String(siteSeq));
      formData.append(csrfToken.name, csrfToken.value);
      const response = await fetch("/api/Admin/Favicon", { method: "POST", credentials: "same-origin", headers: { "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value }, body: formData });
      if (!response.ok) {
        const p = await response.json().catch(() => null);
        throw new Error(p?.error || `HTTP ${response.status}`);
      }
      const data = await response.json();
      setFaviconUrl(data?.faviconUrl || "/public/favicon.png");
      setFaviconObjectKey(data?.objectKey || "");
    } catch (err) {
      logError("favicon:upload:error", err);
      setError(err.message);
    } finally {
      setUploadingFavicon(false);
    }
  }, [siteSeq]);
  const resetFavicon = useCallback6(async () => {
    if (!siteSeq) return;
    setDeletingFavicon(true);
    setError("");
    try {
      const csrfToken = await fetchCsrfToken();
      const response = await fetch(`/api/Admin/Favicon?siteSeq=${encodeURIComponent(siteSeq)}`, { method: "DELETE", credentials: "same-origin", headers: { "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value } });
      if (!response.ok) {
        const p = await response.json().catch(() => null);
        throw new Error(p?.error || `HTTP ${response.status}`);
      }
      setFaviconUrl("/public/favicon.png");
      setFaviconObjectKey("");
    } catch (err) {
      logError("favicon:delete:error", err);
      setError(err.message);
    } finally {
      setDeletingFavicon(false);
    }
  }, [siteSeq]);
  const loadTheme = useCallback6(async () => {
    if (!siteSeq) {
      setSiteTheme(DEFAULT_THEME);
      return;
    }
    try {
      const data = await fetchJson(`/api/Admin/SiteTheme?siteSeq=${encodeURIComponent(siteSeq)}`);
      setSiteTheme({ headerBackgroundColor: data?.headerBackgroundColor ?? "", headerForegroundColor: data?.headerForegroundColor ?? "", bodyBackgroundColor: data?.bodyBackgroundColor ?? "", bodyForegroundColor: data?.bodyForegroundColor ?? "", footerBackgroundColor: data?.footerBackgroundColor ?? "", footerForegroundColor: data?.footerForegroundColor ?? "" });
    } catch (err) {
      logError("site-theme:load:error", err);
      setError(err.message);
    }
  }, [siteSeq]);
  const saveTheme = useCallback6(async (theme) => {
    if (!siteSeq) return;
    setSavingTheme(true);
    setError("");
    try {
      const csrfToken = await fetchCsrfToken();
      const payload = new URLSearchParams();
      payload.set("siteSeq", String(siteSeq));
      Object.entries(theme).forEach(([k, v]) => payload.set(k, v ?? ""));
      payload.set(csrfToken.name, csrfToken.value);
      const response = await fetch("/api/Admin/SiteTheme", { method: "PUT", credentials: "same-origin", headers: { "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8", "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value }, body: payload.toString() });
      if (!response.ok) {
        const p = await response.json().catch(() => null);
        throw new Error(p?.error || `HTTP ${response.status}`);
      }
      const data = await response.json();
      setSiteTheme({ headerBackgroundColor: data?.headerBackgroundColor ?? "", headerForegroundColor: data?.headerForegroundColor ?? "", bodyBackgroundColor: data?.bodyBackgroundColor ?? "", bodyForegroundColor: data?.bodyForegroundColor ?? "", footerBackgroundColor: data?.footerBackgroundColor ?? "", footerForegroundColor: data?.footerForegroundColor ?? "" });
    } catch (err) {
      logError("site-theme:save:error", err);
      setError(err.message);
    } finally {
      setSavingTheme(false);
    }
  }, [siteSeq]);
  return { faviconUrl, faviconObjectKey, uploadingFavicon, deletingFavicon, siteTheme, setSiteTheme, savingTheme, error, loadFavicon, uploadFavicon, resetFavicon, loadTheme, saveTheme };
}

// app/assets/js/admin/pages/SiteConfigPage.jsx
function SiteConfigPage() {
  const { site, siteSeq, saveSiteMeta, savingSiteMeta, error: siteMetaError } = useOutletContext3();
  const navigate = useNavigate7();
  const [siteMetaForm, setSiteMetaForm] = useState13({ abbr: "", mainDomain: "", publicListedOrder: "" });
  useEffect9(() => {
    setSiteMetaForm({
      abbr: site?.abbr ?? "",
      mainDomain: site?.mainDomain ?? "",
      publicListedOrder: site?.publicListedOrder == null ? "" : String(site.publicListedOrder)
    });
  }, [site]);
  const { faviconUrl, faviconObjectKey, uploadingFavicon, deletingFavicon, siteTheme, setSiteTheme, savingTheme, error, loadFavicon, uploadFavicon, resetFavicon, loadTheme, saveTheme } = useSiteConfigData(siteSeq);
  const [faviconFile, setFaviconFile] = useState13(null);
  useEffect9(() => {
    if (!siteSeq) return;
    loadFavicon();
    loadTheme();
  }, [siteSeq]);
  const selectedSiteDomainsText = (site?.domains ?? []).join(", ") || "-";
  return /* @__PURE__ */ React15.createElement(React15.Fragment, null, /* @__PURE__ */ React15.createElement(Card8, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React15.createElement(Group11, { justify: "space-between", mb: "xs" }, /* @__PURE__ */ React15.createElement(Title9, { order: 4 }, "\uC0AC\uC774\uD2B8 \uC0C1\uC138"), /* @__PURE__ */ React15.createElement(Badge10, { color: "blue", variant: "light" }, "Site Detail")), /* @__PURE__ */ React15.createElement(Stack7, { gap: "sm" }, /* @__PURE__ */ React15.createElement(Group11, { justify: "space-between", align: "flex-start", wrap: "wrap" }, /* @__PURE__ */ React15.createElement(Stack7, { gap: 2 }, /* @__PURE__ */ React15.createElement(Text11, { size: "xs", c: "dimmed" }, "\uC120\uD0DD\uB41C \uC0AC\uC774\uD2B8"), /* @__PURE__ */ React15.createElement(Text11, { fw: 700 }, site ? `${site.name} (#${site.seq})` : "\uC120\uD0DD\uB41C \uC0AC\uC774\uD2B8 \uC5C6\uC74C"), /* @__PURE__ */ React15.createElement(Text11, { size: "sm", c: "dimmed" }, "\uB3C4\uBA54\uC778: ", selectedSiteDomainsText)), /* @__PURE__ */ React15.createElement(Button6, { variant: "light", size: "xs", onClick: () => navigate("/Admin/Site") }, "\u2190 \uC0AC\uC774\uD2B8 \uBAA9\uB85D")), /* @__PURE__ */ React15.createElement(Group11, { align: "end", mt: "sm" }, /* @__PURE__ */ React15.createElement(TextInput2, { label: "Abbr", value: siteMetaForm.abbr, onChange: (e) => setSiteMetaForm({ ...siteMetaForm, abbr: e.currentTarget.value }), disabled: !site }), /* @__PURE__ */ React15.createElement(TextInput2, { label: "Main Domain", value: siteMetaForm.mainDomain, onChange: (e) => setSiteMetaForm({ ...siteMetaForm, mainDomain: e.currentTarget.value }), disabled: !site }), /* @__PURE__ */ React15.createElement(TextInput2, { label: "Public Listed Order", type: "number", min: "0", step: "0.01", placeholder: "empty means hidden", value: siteMetaForm.publicListedOrder, onChange: (e) => setSiteMetaForm({ ...siteMetaForm, publicListedOrder: e.currentTarget.value }), disabled: !site }), /* @__PURE__ */ React15.createElement(Button6, { variant: "light", disabled: !site || !siteMetaForm.abbr.trim(), loading: savingSiteMeta, onClick: () => saveSiteMeta(siteMetaForm) }, "Save site meta")), siteMetaError ? /* @__PURE__ */ React15.createElement(Text11, { size: "sm", c: "red" }, siteMetaError) : null)), /* @__PURE__ */ React15.createElement(SimpleGrid2, { cols: { base: 1, xl: 2 }, spacing: "lg" }, /* @__PURE__ */ React15.createElement(Card8, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React15.createElement(Group11, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React15.createElement(Title9, { order: 3 }, "Favicon"), /* @__PURE__ */ React15.createElement(Badge10, { color: "blue", variant: "light" }, "\uBE0C\uB79C\uB529")), /* @__PURE__ */ React15.createElement(Text11, { size: "sm", c: "dimmed", mb: "md" }, "\uC120\uD0DD\uD55C \uC0AC\uC774\uD2B8\uC758 favicon\uC744 \uAD00\uB9AC\uC790 \uC5C5\uB85C\uB4DC\uB85C \uAD50\uCCB4\uD569\uB2C8\uB2E4."), error ? /* @__PURE__ */ React15.createElement(Text11, { c: "red", size: "sm", mb: "md" }, error) : null, /* @__PURE__ */ React15.createElement(Group11, { align: "flex-start", grow: true, mb: "md" }, /* @__PURE__ */ React15.createElement(Stack7, { gap: 6 }, /* @__PURE__ */ React15.createElement(Text11, { size: "sm", fw: 600 }, "\uD604\uC7AC favicon"), /* @__PURE__ */ React15.createElement("img", { src: faviconUrl, alt: "Current favicon", style: { width: 32, height: 32, borderRadius: 6, border: "1px solid #e5e7eb" } }), /* @__PURE__ */ React15.createElement(Text11, { size: "xs", c: "dimmed", style: { wordBreak: "break-all" } }, faviconObjectKey || "/public/favicon.png"), /* @__PURE__ */ React15.createElement(Anchor4, { size: "xs", href: faviconUrl, target: "_blank", rel: "noopener" }, "\uC0C8 \uD0ED\uC73C\uB85C \uBCF4\uAE30")), /* @__PURE__ */ React15.createElement(Stack7, { gap: 8 }, /* @__PURE__ */ React15.createElement(Text11, { size: "sm", fw: 600 }, "\uC0C8 favicon \uC5C5\uB85C\uB4DC"), /* @__PURE__ */ React15.createElement("input", { type: "file", accept: "image/*,.ico", onChange: (e) => setFaviconFile(e.currentTarget.files?.[0] ?? null) }), /* @__PURE__ */ React15.createElement(Group11, null, /* @__PURE__ */ React15.createElement(Button6, { variant: "filled", loading: uploadingFavicon, disabled: !faviconFile || !siteSeq, onClick: async () => {
    await uploadFavicon(faviconFile);
    await loadFavicon();
  } }, "Upload favicon"), /* @__PURE__ */ React15.createElement(Button6, { variant: "light", disabled: !siteSeq, onClick: () => loadFavicon() }, "Refresh"), /* @__PURE__ */ React15.createElement(Button6, { color: "red", variant: "light", loading: deletingFavicon, disabled: !siteSeq, onClick: async () => {
    await resetFavicon();
    await loadFavicon();
    setFaviconFile(null);
  } }, "Reset to default")), /* @__PURE__ */ React15.createElement(Text11, { size: "xs", c: "dimmed" }, "\uAD8C\uC7A5: 32x32 \uB610\uB294 48x48 PNG/ICO"))))), /* @__PURE__ */ React15.createElement(Card8, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React15.createElement(Group11, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React15.createElement(Title9, { order: 3 }, "Theme"), /* @__PURE__ */ React15.createElement(Badge10, { color: "grape", variant: "light" }, "\uB514\uC790\uC778")), /* @__PURE__ */ React15.createElement(Text11, { size: "sm", c: "dimmed", mb: "md" }, "\uC0AC\uC774\uD2B8\uBCC4 \uD5E4\uB354/\uD478\uD130 \uBC30\uACBD\uC0C9\xB7\uC804\uACBD\uC0C9\uC744 16\uC9C4\uC218(#RGB, #RRGGBB, #RRGGBBAA)\uB85C \uC9C0\uC815\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."), /* @__PURE__ */ React15.createElement(SimpleGrid2, { cols: { base: 1, lg: 2 }, spacing: "md" }, /* @__PURE__ */ React15.createElement(SimpleGrid2, { cols: { base: 1, sm: 2 }, spacing: "md" }, [
    { key: "headerBackgroundColor", label: "Header \uBC30\uACBD\uC0C9", placeholder: "#EEEEEE", swatches: ["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#6c5ce7", "#0b7285"] },
    { key: "headerForegroundColor", label: "Header \uC804\uACBD\uC0C9", placeholder: "#000000", swatches: ["#111111", "#212529", "#495057", "#ffffff", "#f1f3f5", "#ffd43b"] },
    { key: "bodyBackgroundColor", label: "Body \uBC30\uACBD\uC0C9", placeholder: "#FFFFFF", swatches: ["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#f1f3f5", "#e9ecef"] },
    { key: "bodyForegroundColor", label: "Body \uC804\uACBD\uC0C9", placeholder: "#000000", swatches: ["#111111", "#212529", "#495057", "#000000", "#343a40", "#ffffff"] },
    { key: "footerBackgroundColor", label: "Footer \uBC30\uACBD\uC0C9", placeholder: "#EEEEEE", swatches: ["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#2b8a3e", "#862e9c"] },
    { key: "footerForegroundColor", label: "Footer \uC804\uACBD\uC0C9", placeholder: "#000000", swatches: ["#111111", "#212529", "#495057", "#ffffff", "#f1f3f5", "#ff922b"] }
  ].map(({ key, label, placeholder, swatches }) => /* @__PURE__ */ React15.createElement(ColorInput, { key, label, placeholder, format: "hexa", value: siteTheme[key], onChange: (value) => setSiteTheme((prev) => ({ ...prev, [key]: value })), swatches, withEyeDropper: false, clearable: true }))), /* @__PURE__ */ React15.createElement(Paper2, { withBorder: true, radius: "md", p: "md", style: { overflow: "hidden" } }, /* @__PURE__ */ React15.createElement(Text11, { size: "sm", fw: 600, mb: 8 }, "\uBBF8\uB9AC\uBCF4\uAE30"), /* @__PURE__ */ React15.createElement(Stack7, { gap: 0, style: { borderRadius: 10, overflow: "hidden", border: "1px solid #e9ecef" } }, /* @__PURE__ */ React15.createElement("div", { style: { backgroundColor: siteTheme.headerBackgroundColor || "#EEEEEE", color: siteTheme.headerForegroundColor || "#000000", padding: "12px 14px", fontWeight: 600 } }, "Header Preview"), /* @__PURE__ */ React15.createElement("div", { style: { padding: "16px 14px", backgroundColor: siteTheme.bodyBackgroundColor || "#FFFFFF", color: siteTheme.bodyForegroundColor || "#000000" } }, "\uCF58\uD150\uCE20 \uC601\uC5ED (\uACE0\uC815 \uBBF8\uB9AC\uBCF4\uAE30)"), /* @__PURE__ */ React15.createElement("div", { style: { backgroundColor: siteTheme.footerBackgroundColor || "#EEEEEE", color: siteTheme.footerForegroundColor || "#000000", padding: "12px 14px", fontWeight: 600 } }, "Footer Preview")))), /* @__PURE__ */ React15.createElement(Group11, { mt: "md" }, /* @__PURE__ */ React15.createElement(Button6, { variant: "filled", color: "grape", loading: savingTheme, disabled: !siteSeq, onClick: async () => {
    await saveTheme(siteTheme);
    await loadTheme();
  } }, "Save theme"), /* @__PURE__ */ React15.createElement(Button6, { variant: "light", disabled: !siteSeq, onClick: () => loadTheme() }, "Refresh"), /* @__PURE__ */ React15.createElement(Button6, { color: "gray", variant: "light", disabled: !siteSeq, onClick: async () => {
    const empty = { headerBackgroundColor: "", headerForegroundColor: "", bodyBackgroundColor: "", bodyForegroundColor: "", footerBackgroundColor: "", footerForegroundColor: "" };
    setSiteTheme(empty);
    await saveTheme(empty);
    await loadTheme();
  } }, "Reset"))));
}

// app/assets/js/admin/pages/SiteCachePage.jsx
import React16, { useEffect as useEffect10 } from "react";
import { useOutletContext as useOutletContext4 } from "react-router-dom";
import { Badge as Badge11, Button as Button7, Card as Card9, Group as Group12, Stack as Stack8, Table as Table3, Text as Text12, Title as Title10 } from "@mantine/core";

// app/assets/js/admin/hooks/useSiteCacheData.js
import { useCallback as useCallback7, useState as useState14 } from "react";
function useSiteCacheData(siteSeq) {
  const [clearing, setClearing] = useState14(false);
  const [memoryCacheStats, setMemoryCacheStats] = useState14([]);
  const [error, setError] = useState14("");
  const clearSiteCache = useCallback7(async () => {
    if (!siteSeq) return;
    setClearing(true);
    setError("");
    try {
      const csrfToken = await fetchCsrfToken();
      const response = await fetch(`/api/cache/${siteSeq}`, { method: "DELETE", credentials: "same-origin", headers: { "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value } });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
    } catch (err) {
      logError("cache:clear:error", siteSeq, err);
      setError(err.message || String(err));
    } finally {
      setClearing(false);
    }
  }, [siteSeq]);
  const loadMemoryCacheStats = useCallback7(async () => {
    try {
      const data = await fetchJson("/api/Admin/MemoryCacheStats");
      setMemoryCacheStats(Array.isArray(data) ? data : []);
    } catch (err) {
      logError("memory-cache:load:error", err);
    }
  }, []);
  return { clearing, memoryCacheStats, error, clearSiteCache, loadMemoryCacheStats };
}

// app/assets/js/admin/pages/SiteCachePage.jsx
function SiteCachePage() {
  const { site, siteSeq } = useOutletContext4();
  const { clearing, memoryCacheStats, error, clearSiteCache, loadMemoryCacheStats } = useSiteCacheData(siteSeq);
  useEffect10(() => {
    loadMemoryCacheStats();
  }, [siteSeq]);
  return /* @__PURE__ */ React16.createElement(React16.Fragment, null, /* @__PURE__ */ React16.createElement(Card9, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React16.createElement(Group12, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React16.createElement(Title10, { order: 3 }, "Operation \xB7 Site Cache Operation"), /* @__PURE__ */ React16.createElement(Badge11, { color: "orange", variant: "light" }, "\uC720\uC9C0\uBCF4\uC218")), /* @__PURE__ */ React16.createElement(Text12, { size: "sm", c: "dimmed", mb: "md" }, "\uD604\uC7AC \uBCF4\uACE0 \uC788\uB294 \uC0AC\uC774\uD2B8\uC758 \uCE90\uC2DC\uB97C \uC989\uC2DC \uCD08\uAE30\uD654\uD569\uB2C8\uB2E4."), error ? /* @__PURE__ */ React16.createElement(Text12, { c: "red", size: "sm", mb: "md" }, error) : null, /* @__PURE__ */ React16.createElement(Group12, { justify: "space-between", align: "center" }, /* @__PURE__ */ React16.createElement(Stack8, { gap: 2 }, /* @__PURE__ */ React16.createElement(Text12, { size: "xs", c: "dimmed" }, "\uB300\uC0C1 \uC0AC\uC774\uD2B8"), /* @__PURE__ */ React16.createElement(Text12, { fw: 700 }, site ? `${site.name} (#${site.seq})` : "\uC120\uD0DD\uB41C \uC0AC\uC774\uD2B8 \uC5C6\uC74C")), /* @__PURE__ */ React16.createElement(Button7, { color: "orange", variant: "filled", disabled: !site, loading: clearing, onClick: () => clearSiteCache() }, "Clear current site cache"))), /* @__PURE__ */ React16.createElement(Card9, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React16.createElement(Group12, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React16.createElement(Title10, { order: 3 }, "Memory Cache Status (All Instances)"), /* @__PURE__ */ React16.createElement(Button7, { variant: "light", onClick: loadMemoryCacheStats }, "Refresh")), /* @__PURE__ */ React16.createElement(Table3, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true }, /* @__PURE__ */ React16.createElement(Table3.Thead, null, /* @__PURE__ */ React16.createElement(Table3.Tr, null, /* @__PURE__ */ React16.createElement(Table3.Th, null, "Port"), /* @__PURE__ */ React16.createElement(Table3.Th, null, "Key Count"), /* @__PURE__ */ React16.createElement(Table3.Th, null, "Value Count"), /* @__PURE__ */ React16.createElement(Table3.Th, null, "Captured At"))), /* @__PURE__ */ React16.createElement(Table3.Tbody, null, memoryCacheStats.map((row) => /* @__PURE__ */ React16.createElement(Table3.Tr, { key: String(row.instancePort) }, /* @__PURE__ */ React16.createElement(Table3.Td, null, row.instancePort), /* @__PURE__ */ React16.createElement(Table3.Td, null, row.stats?.linksCacheKeyCount ?? 0), /* @__PURE__ */ React16.createElement(Table3.Td, null, row.stats?.linksCacheValueCount ?? 0), /* @__PURE__ */ React16.createElement(Table3.Td, null, row.stats?.capturedAtIso8601 ?? "-")))))));
}

// app/assets/js/admin/pages/SitePermissionPage.jsx
import React17, { useEffect as useEffect11, useMemo as useMemo4, useState as useState16 } from "react";
import { useOutletContext as useOutletContext5 } from "react-router-dom";
import { Autocomplete, Badge as Badge12, Button as Button8, Card as Card10, Group as Group13, Paper as Paper3, Select, SimpleGrid as SimpleGrid3, Text as Text13, TextInput as TextInput3, Title as Title11 } from "@mantine/core";
import { DataTable as DataTable3 } from "mantine-datatable";

// app/assets/js/admin/hooks/usePermissionData.js
import { useCallback as useCallback8, useState as useState15 } from "react";
function usePermissionData(siteSeq) {
  const [permissionRows, setPermissionRows] = useState15([]);
  const [permissionDiagnose, setPermissionDiagnose] = useState15(null);
  const [saving, setSaving] = useState15(false);
  const [deletingKey, setDeletingKey] = useState15("");
  const [error, setError] = useState15("");
  const loadPermissions = useCallback8(async () => {
    if (!siteSeq) {
      setPermissionRows([]);
      return;
    }
    try {
      const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Permissions`);
      setPermissionRows(Array.isArray(data?.permissions) ? data.permissions : []);
    } catch (err) {
      logError("permission:load:error", err);
    }
  }, [siteSeq]);
  const savePermission = useCallback8(async (permission) => {
    if (!siteSeq) return false;
    setSaving(true);
    try {
      const csrfToken = await fetchCsrfToken();
      const payload = new URLSearchParams();
      ["targetType", "target", "actorType", "actor", "action"].forEach((key) => payload.set(key, permission[key] ?? ""));
      payload.set(csrfToken.name, csrfToken.value);
      const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Permissions`, { method: "POST", credentials: "same-origin", headers: { "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8", "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value }, body: payload.toString() });
      if (!response.ok) {
        const p = await response.json().catch(() => null);
        throw new Error(p?.error || `HTTP ${response.status}`);
      }
      await loadPermissions();
      return true;
    } catch (err) {
      logError("permission:save:error", err);
      setError(err.message);
      return false;
    } finally {
      setSaving(false);
    }
  }, [siteSeq, loadPermissions]);
  const deletePermission = useCallback8(async (permission) => {
    if (!siteSeq || !permission) return;
    const key = `${permission.targetType}:${permission.target}:${permission.actorType}:${permission.actor}`;
    setDeletingKey(key);
    try {
      const csrfToken = await fetchCsrfToken();
      const params = new URLSearchParams({ targetType: permission.targetType ?? "", target: permission.target ?? "", actorType: permission.actorType ?? "", actor: permission.actor ?? "" });
      const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Permissions?${params.toString()}`, { method: "DELETE", credentials: "same-origin", headers: { "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value } });
      if (!response.ok) {
        const p = await response.json().catch(() => null);
        throw new Error(p?.error || `HTTP ${response.status}`);
      }
      await loadPermissions();
    } catch (err) {
      logError("permission:delete:error", err);
      setError(err.message);
    } finally {
      setDeletingKey("");
    }
  }, [siteSeq, loadPermissions]);
  const diagnosePermission = useCallback8(async (pageName, actor, action) => {
    if (!siteSeq) return;
    const params = new URLSearchParams({ pageName: pageName ?? "", actor: actor ?? "", action: action || "Read" });
    const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/PermissionDiagnose?${params.toString()}`);
    setPermissionDiagnose(data);
  }, [siteSeq]);
  return { permissionRows, permissionDiagnose, saving, deletingKey, error, loadPermissions, savePermission, deletePermission, diagnosePermission };
}

// app/assets/js/admin/pages/SitePermissionPage.jsx
function SitePermissionPage() {
  const { siteSeq, sitePageNames } = useOutletContext5();
  const { permissionRows, permissionDiagnose, saving, deletingKey, error, loadPermissions, savePermission, deletePermission, diagnosePermission } = usePermissionData(siteSeq);
  const [form, setForm] = useState16({ targetType: "All", target: "", actorType: "All", actor: "", action: "Read" });
  const [diagnoseForm, setDiagnoseForm] = useState16({ pageName: "", actor: "", action: "Read" });
  const [sortBy, setSortBy] = useState16("specificity");
  const [sortOrder, setSortOrder] = useState16("desc");
  useEffect11(() => {
    if (siteSeq) loadPermissions();
  }, [siteSeq]);
  const sortedRows = useMemo4(() => [...permissionRows].sort((a, b) => {
    const leftValue = sortBy === "actionLabel" ? formatPermissionAction(a.actionName, a.action) : a[sortBy];
    const rightValue = sortBy === "actionLabel" ? formatPermissionAction(b.actionName, b.action) : b[sortBy];
    return compareValuesForSort(leftValue, rightValue, sortOrder);
  }), [permissionRows, sortBy, sortOrder]);
  return /* @__PURE__ */ React17.createElement(Card10, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React17.createElement(Group13, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React17.createElement(Title11, { order: 3 }, "Permission"), /* @__PURE__ */ React17.createElement(Badge12, { color: "grape", variant: "light" }, permissionRows.length, " rows")), error ? /* @__PURE__ */ React17.createElement(Text13, { c: "red", size: "sm", mb: "sm" }, error) : null, /* @__PURE__ */ React17.createElement(SimpleGrid3, { cols: { base: 1, md: 5 }, spacing: "sm", mb: "sm" }, /* @__PURE__ */ React17.createElement(Select, { label: "targetType", data: PERMISSION_TARGET_TYPE_OPTIONS, value: form.targetType, onChange: (v) => setForm({ ...form, targetType: v ?? "All", target: v === "All" ? "" : form.target }) }), /* @__PURE__ */ React17.createElement(Autocomplete, { label: "target", data: sitePageNames, value: form.target, onChange: (v) => setForm({ ...form, target: v }), placeholder: "page name or prefix", disabled: form.targetType === "All" }), /* @__PURE__ */ React17.createElement(Select, { label: "actorType", data: PERMISSION_ACTOR_TYPE_OPTIONS, value: form.actorType, onChange: (v) => setForm({ ...form, actorType: v ?? "All", actor: v === "All" || v === "Login" ? "" : form.actor }) }), /* @__PURE__ */ React17.createElement(TextInput3, { label: "actor", value: form.actor, onChange: (e) => setForm({ ...form, actor: e.currentTarget.value }), placeholder: "email or @domain" }), /* @__PURE__ */ React17.createElement(Select, { label: "action", data: PERMISSION_ACTION_OPTIONS, value: form.action, onChange: (v) => setForm({ ...form, action: v ?? "Read" }) })), /* @__PURE__ */ React17.createElement(Group13, { mb: "md" }, /* @__PURE__ */ React17.createElement(Button8, { size: "xs", loading: saving, onClick: async () => {
    const saved = await savePermission(form);
    if (saved) setForm({ targetType: "All", target: "", actorType: "All", actor: "", action: "Read" });
  } }, "Save")), /* @__PURE__ */ React17.createElement(SimpleGrid3, { cols: { base: 1, md: 4 }, spacing: "sm", mb: "sm" }, /* @__PURE__ */ React17.createElement(Autocomplete, { label: "pageName", data: sitePageNames, value: diagnoseForm.pageName, onChange: (v) => setDiagnoseForm({ ...diagnoseForm, pageName: v }) }), /* @__PURE__ */ React17.createElement(TextInput3, { label: "actor", value: diagnoseForm.actor, onChange: (e) => setDiagnoseForm({ ...diagnoseForm, actor: e.currentTarget.value }), placeholder: "empty means anonymous" }), /* @__PURE__ */ React17.createElement(Select, { label: "action", data: PERMISSION_ACTION_OPTIONS, value: diagnoseForm.action, onChange: (v) => setDiagnoseForm({ ...diagnoseForm, action: v ?? "Read" }) }), /* @__PURE__ */ React17.createElement(Button8, { mt: 22, variant: "light", onClick: () => diagnosePermission(diagnoseForm.pageName, diagnoseForm.actor, diagnoseForm.action) }, "Diagnose")), permissionDiagnose ? /* @__PURE__ */ React17.createElement(Paper3, { withBorder: true, radius: "sm", p: "sm", mb: "md" }, /* @__PURE__ */ React17.createElement(Group13, { gap: "xs" }, /* @__PURE__ */ React17.createElement(Badge12, { color: permissionDiagnose.permitted ? "green" : "red", variant: "light" }, permissionDiagnose.permitted ? "allowed" : "denied"), /* @__PURE__ */ React17.createElement(Text13, { size: "sm" }, permissionDiagnose.matchedPermission ? `${permissionDiagnose.matchedPermission.targetType}/${permissionDiagnose.matchedPermission.actorType}/${formatPermissionAction(permissionDiagnose.matchedPermission.actionName, permissionDiagnose.matchedPermission.action)}` : "No matching row"))) : null, /* @__PURE__ */ React17.createElement(
    DataTable3,
    {
      withTableBorder: true,
      borderRadius: "md",
      striped: true,
      highlightOnHover: true,
      records: sortedRows,
      columns: [
        { accessor: "targetType", title: "Target Type", sortable: true },
        { accessor: "target", title: "Target", sortable: true, render: (row) => row.target || "*" },
        { accessor: "actorType", title: "Actor Type", sortable: true },
        { accessor: "actor", title: "Actor", sortable: true, render: (row) => row.actor || "*" },
        { accessor: "action", title: "Action", sortable: true, render: (row) => formatPermissionAction(row.actionName, row.action) },
        { accessor: "specificity", title: "Specificity", sortable: true },
        { accessor: "actions", title: "Actions", render: (row) => {
          const key = `${row.targetType}:${row.target}:${row.actorType}:${row.actor}`;
          return /* @__PURE__ */ React17.createElement(Group13, { gap: 6 }, /* @__PURE__ */ React17.createElement(Button8, { size: "xs", variant: "light", onClick: () => setForm({ targetType: row.targetType, target: row.target, actorType: row.actorType, actor: row.actor, action: row.actionName }) }, "Edit"), /* @__PURE__ */ React17.createElement(Button8, { size: "xs", variant: "light", color: "red", loading: deletingKey === key, onClick: () => deletePermission(row) }, "Delete"));
        } }
      ],
      sortStatus: { columnAccessor: sortBy, direction: sortOrder },
      onSortStatusChange: (next) => {
        setSortBy(next.columnAccessor);
        setSortOrder(next.direction ?? "asc");
      },
      minHeight: 220
    }
  ));
}

// app/assets/js/admin/pages/SiteAdminsPage.jsx
import React18, { useEffect as useEffect12, useState as useState18 } from "react";
import { useOutletContext as useOutletContext6, useNavigate as useNavigate8 } from "react-router-dom";
import { Badge as Badge13, Button as Button9, Card as Card11, Group as Group14, Stack as Stack9, Text as Text14, TextInput as TextInput4, Title as Title12 } from "@mantine/core";
import { DataTable as DataTable4 } from "mantine-datatable";

// app/assets/js/admin/hooks/useSiteAdminsData.js
import { useCallback as useCallback9, useState as useState17 } from "react";
function useSiteAdminsData(siteSeq) {
  const [siteAdmins, setSiteAdmins] = useState17([]);
  const [adding, setAdding] = useState17(false);
  const [deletingUserSeq, setDeletingUserSeq] = useState17(0);
  const [error, setError] = useState17("");
  const loadSiteAdmins = useCallback9(async () => {
    if (!siteSeq) {
      setSiteAdmins([]);
      return;
    }
    try {
      const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Admins`);
      setSiteAdmins(Array.isArray(data) ? data : []);
    } catch (err) {
      logError("site-admins:load:error", err);
    }
  }, [siteSeq]);
  const insertSiteAdmin = useCallback9(async (userSeq) => {
    if (!siteSeq || !userSeq) return false;
    setAdding(true);
    try {
      const csrfToken = await fetchCsrfToken();
      const payload = new URLSearchParams();
      payload.set("user", String(userSeq));
      payload.set(csrfToken.name, csrfToken.value);
      const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Admins`, { method: "POST", credentials: "same-origin", headers: { "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8", "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value }, body: payload.toString() });
      if (!response.ok) {
        const p = await response.json().catch(() => null);
        throw new Error(p?.error || `HTTP ${response.status}`);
      }
      await loadSiteAdmins();
      return true;
    } catch (err) {
      logError("site-admin:insert:error", err);
      setError(err.message);
      return false;
    } finally {
      setAdding(false);
    }
  }, [siteSeq, loadSiteAdmins]);
  const deleteSiteAdmin = useCallback9(async (userSeq) => {
    if (!siteSeq || !userSeq) return;
    setDeletingUserSeq(userSeq);
    try {
      const csrfToken = await fetchCsrfToken();
      const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Admins/${encodeURIComponent(userSeq)}`, { method: "DELETE", credentials: "same-origin", headers: { "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value } });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      await loadSiteAdmins();
    } catch (err) {
      logError("site-admin:delete:error", err);
      setError(err.message);
    } finally {
      setDeletingUserSeq(0);
    }
  }, [siteSeq, loadSiteAdmins]);
  return { siteAdmins, adding, deletingUserSeq, error, loadSiteAdmins, insertSiteAdmin, deleteSiteAdmin };
}

// app/assets/js/admin/pages/SiteAdminsPage.jsx
function SiteAdminsPage() {
  const { site, siteSeq } = useOutletContext6();
  const navigate = useNavigate8();
  const { siteAdmins, adding, deletingUserSeq, error, loadSiteAdmins, insertSiteAdmin, deleteSiteAdmin } = useSiteAdminsData(siteSeq);
  const [userSeqInput, setUserSeqInput] = useState18("");
  useEffect12(() => {
    if (siteSeq) loadSiteAdmins();
  }, [siteSeq]);
  return /* @__PURE__ */ React18.createElement(Card11, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React18.createElement(Group14, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React18.createElement(Title12, { order: 3 }, "Site Admins"), /* @__PURE__ */ React18.createElement(Badge13, { color: "orange", variant: "light" }, siteAdmins.length, " admins")), /* @__PURE__ */ React18.createElement(Text14, { size: "sm", c: "dimmed", mb: "md" }, "\uC774 \uC0AC\uC774\uD2B8\uC758 \uAD00\uB9AC\uC790 \uBAA9\uB85D\uC785\uB2C8\uB2E4. Site Admin\uC740 \uD574\uB2F9 \uC0AC\uC774\uD2B8\uC758 Permission, \uD398\uC774\uC9C0 \uB4F1\uC744 \uAD00\uB9AC\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."), error ? /* @__PURE__ */ React18.createElement(Text14, { c: "red", size: "sm", mb: "sm" }, error) : null, /* @__PURE__ */ React18.createElement(Group14, { mb: "md", align: "end" }, /* @__PURE__ */ React18.createElement(TextInput4, { label: "User Seq", placeholder: "\uCD94\uAC00\uD560 \uC720\uC800\uC758 seq (\uC22B\uC790)", value: userSeqInput, onChange: (e) => setUserSeqInput(e.currentTarget.value), type: "number", min: "1" }), /* @__PURE__ */ React18.createElement(Button9, { variant: "filled", loading: adding, disabled: !siteSeq || !userSeqInput.trim(), onClick: async () => {
    const userSeq = Number.parseInt(userSeqInput, 10);
    if (!Number.isFinite(userSeq) || userSeq <= 0) return;
    const ok = await insertSiteAdmin(userSeq);
    if (ok) setUserSeqInput("");
  } }, "\uCD94\uAC00")), /* @__PURE__ */ React18.createElement(
    DataTable4,
    {
      withTableBorder: true,
      borderRadius: "md",
      striped: true,
      highlightOnHover: true,
      records: siteAdmins,
      columns: [
        { accessor: "user", title: "User Seq" },
        { accessor: "dateInserted", title: "Date Inserted", render: (row) => formatDateTimeInClientTimezone(row.dateInserted) },
        { accessor: "actions", title: "Actions", render: (row) => /* @__PURE__ */ React18.createElement(Button9, { size: "xs", variant: "light", color: "red", loading: deletingUserSeq === row.user, onClick: () => deleteSiteAdmin(row.user) }, "\uC0AD\uC81C") }
      ],
      minHeight: 160
    }
  ));
}

// app/assets/js/admin/pages/RecentChangesPage.jsx
import React19, { useEffect as useEffect13, useMemo as useMemo5, useState as useState20 } from "react";
import { Anchor as Anchor5, Badge as Badge14, Button as Button10, Card as Card12, Group as Group15, Text as Text15, TextInput as TextInput5, Title as Title13 } from "@mantine/core";

// app/assets/js/admin/hooks/useRecentChangesData.js
import { useCallback as useCallback10, useState as useState19 } from "react";
function useRecentChangesData() {
  const [loading, setLoading] = useState19(true);
  const [recentChanges, setRecentChanges] = useState19([]);
  const [sites, setSites] = useState19([]);
  const loadRecentChanges = useCallback10(async (n = 50) => {
    setLoading(true);
    try {
      const data = await fetchJson(`/api/Admin/RecentChanges?n=${encodeURIComponent(n)}`);
      setRecentChanges(data);
    } catch (err) {
      logError("recent-changes:load:error", err);
    } finally {
      setLoading(false);
    }
  }, []);
  const loadSites = useCallback10(async () => {
    try {
      const data = await fetchJson("/api/Admin/Sites");
      setSites(Array.isArray(data) ? data : []);
    } catch (err) {
      logError("recent-changes:sites:load:error", err);
    }
  }, []);
  return { loading, recentChanges, sites, loadRecentChanges, loadSites };
}

// app/assets/js/admin/pages/RecentChangesPage.jsx
function RecentChangesPage() {
  const { loading, recentChanges, sites, loadRecentChanges, loadSites } = useRecentChangesData();
  const [limitInput, setLimitInput] = useState20("50");
  useEffect13(() => {
    loadSites();
    loadRecentChanges(50);
  }, []);
  const siteDomainBySeq = useMemo5(() => new Map(sites.map((s) => [s.seq, (s.domains ?? []).find((d) => !!d) ?? ""])), [sites]);
  return /* @__PURE__ */ React19.createElement(Card12, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React19.createElement(Group15, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React19.createElement(Title13, { order: 3 }, "Recent Changes (All Sites)"), /* @__PURE__ */ React19.createElement(Badge14, { color: "violet", variant: "light" }, recentChanges.length, " rows")), /* @__PURE__ */ React19.createElement(Text15, { size: "sm", c: "dimmed", mb: "md" }, "\uC0AC\uC774\uD2B8 \uC804\uCCB4 \uCD5C\uADFC \uBCC0\uACBD \uAE30\uB85D\uC744 n\uAC1C \uB2E8\uC704\uB85C \uC870\uD68C\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."), /* @__PURE__ */ React19.createElement(Group15, { align: "flex-end", mb: "md" }, /* @__PURE__ */ React19.createElement(TextInput5, { label: "\uC870\uD68C \uAC1C\uC218 n", value: limitInput, onChange: (e) => setLimitInput(e.currentTarget.value), placeholder: "1 ~ 500" }), /* @__PURE__ */ React19.createElement(Button10, { variant: "filled", onClick: () => {
    const parsed = Number.parseInt(limitInput, 10);
    const n = Number.isFinite(parsed) ? Math.min(500, Math.max(1, parsed)) : 50;
    setLimitInput(String(n));
    loadRecentChanges(n);
  } }, "\uC870\uD68C")), makeTable(["When", "Site", "Page", "Revision", "Editor", "Comment", "IP"], recentChanges.map((row) => {
    const siteUrl = resolveSiteUrl(row, siteDomainBySeq);
    const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(row.name)}` : "";
    const revisionUrl = pageUrl ? `${pageUrl}?rev=${row.revision}` : "";
    const editorUrl = row.nickname ? `/Admin/User?query=${encodeURIComponent(row.nickname)}` : "";
    return [row.dateTime, siteUrl ? /* @__PURE__ */ React19.createElement(Anchor5, { href: siteUrl, target: "_blank" }, row.siteName, " (#", row.siteSeq, ")") : `${row.siteName} (#${row.siteSeq})`, pageUrl ? /* @__PURE__ */ React19.createElement(Anchor5, { href: pageUrl, target: "_blank" }, row.name) : row.name, revisionUrl ? /* @__PURE__ */ React19.createElement(Anchor5, { href: revisionUrl, target: "_blank" }, row.revision) : row.revision, editorUrl ? /* @__PURE__ */ React19.createElement(Anchor5, { href: editorUrl }, row.nickname) : row.nickname ?? "-", row.comment || "-", row.remoteAddress];
  })));
}

// app/assets/js/admin/pages/AccessLogsPage.jsx
import React20, { useEffect as useEffect14, useMemo as useMemo6, useState as useState22 } from "react";
import { useParams as useParams2 } from "react-router-dom";
import { Anchor as Anchor6, Badge as Badge15, Button as Button11, Card as Card13, Group as Group16, Pagination, Text as Text16, TextInput as TextInput6, Title as Title14 } from "@mantine/core";
import { DataTable as DataTable5 } from "mantine-datatable";

// app/assets/js/admin/hooks/useAccessLogData.js
import { useCallback as useCallback11, useState as useState21 } from "react";
function useAccessLogData() {
  const [loading, setLoading] = useState21(true);
  const [error, setError] = useState21("");
  const [accessLogs, setAccessLogs] = useState21([]);
  const [accessLogCount, setAccessLogCount] = useState21(0);
  const [sites, setSites] = useState21([]);
  const loadAccessLogs = useCallback11(async ({ page = 1, pageSize = ACCESS_LOG_PAGE_SIZE, search = "", sortBy = "seq", sortOrder = "desc", siteSeq = "" } = {}) => {
    setLoading(true);
    setError("");
    try {
      const params = new URLSearchParams({ page: String(page), pageSize: String(pageSize), search, sortBy, sortOrder });
      if (siteSeq) params.set("siteSeq", String(siteSeq));
      const data = await fetchJson(`/api/Admin/AccessLogs?${params.toString()}`);
      const rows = Array.isArray(data?.array) ? data.array : Array.isArray(data) ? data : [];
      setAccessLogs(rows);
      setAccessLogCount(Number(data?.count ?? rows.length));
    } catch (err) {
      logError("access-logs:load:error", err);
      setError(err.message || String(err));
    } finally {
      setLoading(false);
    }
  }, []);
  const loadSites = useCallback11(async () => {
    try {
      const data = await fetchJson("/api/Admin/Sites");
      setSites(Array.isArray(data) ? data : []);
    } catch (err) {
      logError("access-logs:sites:load:error", err);
    }
  }, []);
  return { loading, error, accessLogs, accessLogCount, sites, loadAccessLogs, loadSites };
}

// app/assets/js/admin/pages/AccessLogsPage.jsx
function AccessLogsPage() {
  const params = useParams2();
  const siteSeq = params.siteSeq ?? "";
  const { loading, accessLogs, accessLogCount, sites, loadAccessLogs, loadSites } = useAccessLogData();
  const [page, setPage] = useState22(1);
  const [searchInput, setSearchInput] = useState22("");
  const [sortBy, setSortBy] = useState22("seq");
  const [sortOrder, setSortOrder] = useState22("desc");
  const totalPages = Math.max(1, Math.ceil(accessLogCount / ACCESS_LOG_PAGE_SIZE));
  const selectedSite = useMemo6(() => sites.find((s) => String(s.seq) === siteSeq) ?? null, [sites, siteSeq]);
  useEffect14(() => {
    loadSites();
    loadAccessLogs({ page: 1, pageSize: ACCESS_LOG_PAGE_SIZE, search: "", sortBy: "seq", sortOrder: "desc", siteSeq });
  }, [siteSeq]);
  return /* @__PURE__ */ React20.createElement(Card13, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React20.createElement(Group16, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React20.createElement(Title14, { order: 3 }, selectedSite ? `Access Logs (${selectedSite.name} #${selectedSite.seq})` : "Access Logs (All Sites)"), /* @__PURE__ */ React20.createElement(Badge15, { color: "cyan", variant: "light" }, accessLogs.length, " / ", accessLogCount, " rows")), /* @__PURE__ */ React20.createElement(Text16, { size: "sm", c: "dimmed", mb: "md" }, "\uAC80\uC0C9\uACFC \uD5E4\uB354 \uC815\uB82C, \uD398\uC774\uC9C0 \uC774\uB3D9\uC73C\uB85C \uC694\uCCAD \uB85C\uADF8\uB97C \uC870\uD68C\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4."), /* @__PURE__ */ React20.createElement(Group16, { align: "flex-end", mb: "md" }, /* @__PURE__ */ React20.createElement(TextInput6, { label: "search", value: searchInput, onChange: (e) => setSearchInput(e.currentTarget.value), placeholder: "site/method/uri/ip/user-agent" }), /* @__PURE__ */ React20.createElement(Button11, { variant: "filled", onClick: () => {
    setPage(1);
    loadAccessLogs({ page: 1, pageSize: ACCESS_LOG_PAGE_SIZE, search: searchInput, sortBy, sortOrder, siteSeq });
  } }, "\uC870\uD68C")), /* @__PURE__ */ React20.createElement(
    DataTable5,
    {
      sortIcons: { sorted: /* @__PURE__ */ React20.createElement(IconChevronUp, { size: 14 }), unsorted: /* @__PURE__ */ React20.createElement(IconSelector, { size: 14 }) },
      withTableBorder: true,
      borderRadius: "sm",
      striped: true,
      highlightOnHover: true,
      minHeight: 420,
      records: Array.isArray(accessLogs) ? accessLogs : [],
      idAccessor: "seq",
      columns: [
        { accessor: "seq", title: "seq", sortable: true },
        { accessor: "dateInserted", title: "DateInserted", sortable: true },
        { accessor: "siteName", title: "Site", render: (row) => `${row.siteName} (#${row.siteSeq})` },
        { accessor: "ipDenySeq", title: "IpDeny", render: (row) => row.ipDenySeq ?? "-" },
        { accessor: "userSeq", title: "User", render: (row) => row.userSeq ?? "-" },
        { accessor: "method", title: "Method", sortable: true },
        { accessor: "uri", title: "Request URL", sortable: true, render: (row) => {
          const href = `${row.scheme}://${row.host}${row.uri}`;
          return /* @__PURE__ */ React20.createElement(Anchor6, { href, target: "_blank", rel: "noopener noreferrer" }, href);
        } },
        { accessor: "status", title: "Status", sortable: true },
        { accessor: "remoteAddress", title: "IP" },
        { accessor: "durationMilli", title: "Duration(ms)", sortable: true },
        { accessor: "userAgent", title: "User-Agent" }
      ],
      sortStatus: { columnAccessor: sortBy, direction: sortOrder },
      onSortStatusChange: (next) => {
        const dir = next.direction ?? "desc";
        setPage(1);
        setSortBy(next.columnAccessor);
        setSortOrder(dir);
        loadAccessLogs({ page: 1, pageSize: ACCESS_LOG_PAGE_SIZE, search: searchInput, sortBy: next.columnAccessor, sortOrder: dir, siteSeq });
      }
    }
  ), /* @__PURE__ */ React20.createElement(Group16, { mt: "md", justify: "space-between" }, /* @__PURE__ */ React20.createElement(Text16, { size: "sm", c: "dimmed" }, "Page ", page, " / ", totalPages), /* @__PURE__ */ React20.createElement(Pagination, { value: page, onChange: (nextPage) => {
    setPage(nextPage);
    loadAccessLogs({ page: nextPage, pageSize: ACCESS_LOG_PAGE_SIZE, search: searchInput, sortBy, sortOrder, siteSeq });
  }, total: totalPages, siblings: 1, boundaries: 1, size: "sm" })));
}

// app/assets/js/admin/pages/S3BrowserPage.jsx
import React21, { useEffect as useEffect15 } from "react";
import { Badge as Badge16, Button as Button12, Card as Card14, Group as Group17, Table as Table4, Text as Text17, Title as Title15 } from "@mantine/core";

// app/assets/js/admin/hooks/useS3Data.js
import { useCallback as useCallback12, useState as useState23 } from "react";
function useS3Data() {
  const [loading, setLoading] = useState23(false);
  const [error, setError] = useState23("");
  const [s3Items, setS3Items] = useState23([]);
  const [selectedS3Keys, setSelectedS3Keys] = useState23([]);
  const [deletingS3, setDeletingS3] = useState23(false);
  const [expandedS3Nodes, setExpandedS3Nodes] = useState23({});
  const loadS3Objects = useCallback12(async () => {
    setLoading(true);
    setError("");
    try {
      const params = new URLSearchParams({ prefix: "", maxKeys: "5000", recursive: "true" });
      const data = await fetchJson(`/api/Admin/S3Objects?${params.toString()}`);
      setS3Items(Array.isArray(data?.items) ? data.items : []);
      setSelectedS3Keys([]);
      setExpandedS3Nodes({ "__root__": true });
    } catch (err) {
      logError("s3:load:error", err);
      setError(`S3 \uC870\uD68C \uC2E4\uD328: ${err.message}`);
    } finally {
      setLoading(false);
    }
  }, []);
  const toggleS3Node = useCallback12((key) => {
    setExpandedS3Nodes((prev) => ({ ...prev, [key]: !prev[key] }));
  }, []);
  const expandAllS3Nodes = useCallback12((items) => {
    const next = { "__root__": true };
    (Array.isArray(items) ? items : []).filter((item) => !item.isDirectory).forEach((item) => {
      const parts = String(item.key || "").split("/").filter(Boolean);
      for (let i = 1; i < parts.length; i += 1) {
        next[parts.slice(0, i).join("/")] = true;
      }
    });
    setExpandedS3Nodes(next);
  }, []);
  const deleteS3Selected = useCallback12(async (keys) => {
    if (keys.length === 0) return;
    setDeletingS3(true);
    try {
      const csrf = await fetchCsrfToken();
      const response = await fetch("/api/Admin/S3Objects", {
        method: "DELETE",
        credentials: "same-origin",
        headers: { "Content-Type": "application/json", "Csrf-Token": csrf.value, "X-CSRF-Token": csrf.value },
        body: JSON.stringify({ keys })
      });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      await loadS3Objects();
    } catch (err) {
      logError("s3:delete:error", err);
      setError(`S3 \uC0AD\uC81C \uC2E4\uD328: ${err.message}`);
    } finally {
      setDeletingS3(false);
    }
  }, [loadS3Objects]);
  const downloadS3Object = useCallback12(async (key) => {
    try {
      const params = new URLSearchParams({ key });
      const data = await fetchJson(`/api/Admin/S3DownloadUrl?${params.toString()}`);
      if (data?.url) window.open(data.url, "_blank", "noopener,noreferrer");
    } catch (err) {
      logError("s3:download:error", err);
      setError(`\uB2E4\uC6B4\uB85C\uB4DC URL \uC0DD\uC131 \uC2E4\uD328: ${err.message}`);
    }
  }, []);
  return { loading, error, s3Items, selectedS3Keys, setSelectedS3Keys, deletingS3, expandedS3Nodes, loadS3Objects, toggleS3Node, expandAllS3Nodes, deleteS3Selected, downloadS3Object };
}

// app/assets/js/admin/pages/S3BrowserPage.jsx
function S3BrowserPage() {
  const { loading, error, s3Items, selectedS3Keys, setSelectedS3Keys, deletingS3, expandedS3Nodes, loadS3Objects, toggleS3Node, expandAllS3Nodes, deleteS3Selected, downloadS3Object } = useS3Data();
  useEffect15(() => {
    loadS3Objects();
  }, []);
  const fileRows = Array.isArray(s3Items) ? s3Items.filter((item) => !item.isDirectory) : [];
  const root = { children: {} };
  fileRows.forEach((item) => {
    const parts = String(item.key || "").split("/").filter(Boolean);
    if (parts.length === 0) return;
    let node = root;
    parts.forEach((part, index) => {
      const currentPath = parts.slice(0, index + 1).join("/");
      if (!node.children[part]) node.children[part] = { name: part, path: currentPath, isFile: index === parts.length - 1, children: {}, meta: null };
      if (index === parts.length - 1) {
        node.children[part].isFile = true;
        node.children[part].meta = item;
      }
      node = node.children[part];
    });
  });
  const rows = [];
  const visit = (node, depth = 0) => {
    Object.values(node.children).sort((a, b) => {
      if (a.isFile === b.isFile) return a.name.localeCompare(b.name);
      return a.isFile ? 1 : -1;
    }).forEach((child) => {
      rows.push({ depth, node: child });
      if (!child.isFile && expandedS3Nodes[child.path]) visit(child, depth + 1);
    });
  };
  visit(root, 0);
  return /* @__PURE__ */ React21.createElement(Card14, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React21.createElement(Group17, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React21.createElement(Title15, { order: 3 }, "S3 \uD30C\uC77C \uBE0C\uB77C\uC6B0\uC800"), /* @__PURE__ */ React21.createElement(Badge16, { color: "red", variant: "light" }, "Admin Only")), error ? /* @__PURE__ */ React21.createElement(Text17, { c: "red", size: "sm", mb: "md" }, error) : null, /* @__PURE__ */ React21.createElement(Group17, { align: "flex-end", mb: "md" }, /* @__PURE__ */ React21.createElement(Button12, { loading, onClick: () => loadS3Objects(), leftSection: /* @__PURE__ */ React21.createElement("i", { className: "fas fa-sync-alt", "aria-hidden": "true" }) }, "\uC0C8\uB85C\uACE0\uCE68"), /* @__PURE__ */ React21.createElement(Button12, { variant: "light", onClick: () => expandAllS3Nodes(s3Items), leftSection: /* @__PURE__ */ React21.createElement("i", { className: "fas fa-angle-double-down", "aria-hidden": "true" }) }, "\uBAA8\uB450 \uD3BC\uCE58\uAE30"), /* @__PURE__ */ React21.createElement(Button12, { color: "red", variant: "light", disabled: selectedS3Keys.length === 0, loading: deletingS3, onClick: () => deleteS3Selected(selectedS3Keys), leftSection: /* @__PURE__ */ React21.createElement("i", { className: "fas fa-trash-alt", "aria-hidden": "true" }) }, "\uC120\uD0DD \uC0AD\uC81C (", selectedS3Keys.length, ")")), /* @__PURE__ */ React21.createElement(Table4, { striped: true, highlightOnHover: true, withTableBorder: true, withColumnBorders: true }, /* @__PURE__ */ React21.createElement(Table4.Thead, null, /* @__PURE__ */ React21.createElement(Table4.Tr, null, /* @__PURE__ */ React21.createElement(Table4.Th, null, /* @__PURE__ */ React21.createElement("input", { type: "checkbox", checked: selectedS3Keys.length > 0 && selectedS3Keys.length === fileRows.length, onChange: (e) => {
    setSelectedS3Keys(e.currentTarget.checked ? fileRows.map((item) => item.key) : []);
  } })), /* @__PURE__ */ React21.createElement(Table4.Th, null, "Key"), /* @__PURE__ */ React21.createElement(Table4.Th, { style: { textAlign: "right" } }, "Size(bytes)"), /* @__PURE__ */ React21.createElement(Table4.Th, { style: { textAlign: "center" } }, "Last Modified"), /* @__PURE__ */ React21.createElement(Table4.Th, { style: { textAlign: "center" } }, "Action"))), /* @__PURE__ */ React21.createElement(Table4.Tbody, null, rows.map(({ depth, node }) => {
    const key = node.path;
    const checked = selectedS3Keys.includes(key);
    return /* @__PURE__ */ React21.createElement(Table4.Tr, { key }, /* @__PURE__ */ React21.createElement(Table4.Td, null, /* @__PURE__ */ React21.createElement("input", { type: "checkbox", disabled: !node.isFile, checked, onChange: (e) => {
      if (e.currentTarget.checked) setSelectedS3Keys((prev) => [...prev, key]);
      else setSelectedS3Keys((prev) => prev.filter((k) => k !== key));
    } })), /* @__PURE__ */ React21.createElement(Table4.Td, null, /* @__PURE__ */ React21.createElement("div", { style: { display: "flex", alignItems: "center", gap: 8, paddingLeft: `${depth * 22}px` } }, !node.isFile ? /* @__PURE__ */ React21.createElement(Button12, { size: "compact-xs", variant: "subtle", onClick: () => toggleS3Node(node.path) }, /* @__PURE__ */ React21.createElement("i", { className: `fas ${expandedS3Nodes[node.path] ? "fa-chevron-down" : "fa-chevron-right"}`, "aria-hidden": "true" })) : /* @__PURE__ */ React21.createElement("span", { style: { display: "inline-block", width: 20 } }), /* @__PURE__ */ React21.createElement("span", null, /* @__PURE__ */ React21.createElement("i", { className: `fas ${node.isFile ? "fa-file-alt" : "fa-folder"}`, "aria-hidden": "true" })), /* @__PURE__ */ React21.createElement("span", null, node.name))), /* @__PURE__ */ React21.createElement(Table4.Td, { style: { textAlign: "right" } }, node.isFile ? Number(node.meta?.size ?? 0).toLocaleString() : "-"), /* @__PURE__ */ React21.createElement(Table4.Td, { style: { textAlign: "center" } }, node.isFile ? formatDateTimeInClientTimezone(node.meta?.lastModified) : "-"), /* @__PURE__ */ React21.createElement(Table4.Td, { style: { textAlign: "center" } }, node.isFile ? /* @__PURE__ */ React21.createElement(Button12, { size: "xs", variant: "light", onClick: () => downloadS3Object(key), leftSection: /* @__PURE__ */ React21.createElement("i", { className: "fas fa-download", "aria-hidden": "true" }) }, "\uB2E4\uC6B4\uB85C\uB4DC") : "-"));
  }))));
}

// app/assets/js/admin/pages/CrawlerCachePage.jsx
import React22, { useEffect as useEffect16, useState as useState25 } from "react";
import { Anchor as Anchor7, Badge as Badge17, Button as Button13, Card as Card15, Group as Group18, Image as Image2, Text as Text18, TextInput as TextInput7, Title as Title16 } from "@mantine/core";
import { DataTable as DataTable6 } from "mantine-datatable";

// app/assets/js/admin/hooks/useCrawlerCacheData.js
import { useCallback as useCallback13, useState as useState24 } from "react";
function useCrawlerCacheData() {
  const [loading, setLoading] = useState24(true);
  const [crawlerCaches, setCrawlerCaches] = useState24([]);
  const [crawlerCacheCount, setCrawlerCacheCount] = useState24(0);
  const [refreshingUrl, setRefreshingUrl] = useState24("");
  const [deletingUrl, setDeletingUrl] = useState24("");
  const loadCrawlerCaches = useCallback13(async ({ page = 1, pageSize = CRAWLER_CACHE_PAGE_SIZE, search = "", sortBy = "id", sortOrder = "desc" } = {}) => {
    setLoading(true);
    try {
      const params = new URLSearchParams({ page: String(page), pageSize: String(pageSize), search, sortBy, sortOrder });
      const data = await fetchJson(`/api/Admin/CrawlerCache?${params.toString()}`);
      const rows = Array.isArray(data?.array) ? data.array : Array.isArray(data) ? data : [];
      setCrawlerCaches(rows);
      setCrawlerCacheCount(Number(data?.count ?? rows.length));
    } catch (err) {
      logError("crawler-cache:load:error", err);
    } finally {
      setLoading(false);
    }
  }, []);
  const refreshCrawlerCache = useCallback13(async (url, currentParams) => {
    setRefreshingUrl(url);
    try {
      const csrfToken = await fetchCsrfToken();
      const payload = new URLSearchParams();
      payload.set("url", url);
      payload.set(csrfToken.name, csrfToken.value);
      const response = await fetch("/api/Admin/CrawlerCache/Refresh", {
        method: "POST",
        credentials: "same-origin",
        headers: { "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8", "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value },
        body: payload.toString()
      });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      await loadCrawlerCaches(currentParams);
    } finally {
      setRefreshingUrl("");
    }
  }, [loadCrawlerCaches]);
  const deleteCrawlerCache = useCallback13(async (url, currentParams) => {
    setDeletingUrl(url);
    try {
      const csrfToken = await fetchCsrfToken();
      const response = await fetch(`/api/Admin/CrawlerCache?url=${encodeURIComponent(url)}`, {
        method: "DELETE",
        credentials: "same-origin",
        headers: { "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value }
      });
      if (!response.ok) throw new Error(`HTTP ${response.status}`);
      await loadCrawlerCaches(currentParams);
    } finally {
      setDeletingUrl("");
    }
  }, [loadCrawlerCaches]);
  return { loading, crawlerCaches, crawlerCacheCount, refreshingUrl, deletingUrl, loadCrawlerCaches, refreshCrawlerCache, deleteCrawlerCache };
}

// app/assets/js/admin/pages/CrawlerCachePage.jsx
function CrawlerCachePage() {
  const { loading, crawlerCaches, crawlerCacheCount, refreshingUrl, deletingUrl, loadCrawlerCaches, refreshCrawlerCache, deleteCrawlerCache } = useCrawlerCacheData();
  const [page, setPage] = useState25(1);
  const [searchInput, setSearchInput] = useState25("");
  const [search, setSearch] = useState25("");
  const [sortBy, setSortBy] = useState25("id");
  const [sortOrder, setSortOrder] = useState25("desc");
  const totalPages = Math.max(1, Math.ceil(crawlerCacheCount / CRAWLER_CACHE_PAGE_SIZE));
  const normalizedPage = Math.min(page, totalPages);
  const currentParams = { page, pageSize: CRAWLER_CACHE_PAGE_SIZE, search, sortBy, sortOrder };
  useEffect16(() => {
    loadCrawlerCaches();
  }, []);
  return /* @__PURE__ */ React22.createElement(Card15, { withBorder: true, radius: "md", padding: "lg" }, /* @__PURE__ */ React22.createElement(Group18, { justify: "space-between", mb: "md" }, /* @__PURE__ */ React22.createElement(Title16, { order: 3 }, "Crawler Cache"), /* @__PURE__ */ React22.createElement(Badge17, { color: "lime", variant: "light" }, crawlerCacheCount, " rows")), /* @__PURE__ */ React22.createElement(Text18, { size: "sm", c: "dimmed", mb: "md" }, "URL\uBCC4 \uD06C\uB864\uB9C1 \uCE90\uC2DC \uC870\uD68C/\uC0AD\uC81C/\uAC15\uC81C\uAC31\uC2E0\uC744 \uC218\uD589\uD569\uB2C8\uB2E4."), /* @__PURE__ */ React22.createElement(Group18, { align: "flex-end", mb: "md" }, /* @__PURE__ */ React22.createElement(Button13, { variant: "filled", onClick: () => loadCrawlerCaches(currentParams) }, "\uC870\uD68C")), /* @__PURE__ */ React22.createElement(Group18, { mb: "sm", align: "end" }, /* @__PURE__ */ React22.createElement(TextInput7, { label: "search", value: searchInput, onChange: (e) => setSearchInput(e.currentTarget.value), placeholder: "url, title, image, description" }), /* @__PURE__ */ React22.createElement(Button13, { variant: "light", onClick: () => {
    setPage(1);
    setSearch(searchInput);
    loadCrawlerCaches({ page: 1, pageSize: CRAWLER_CACHE_PAGE_SIZE, search: searchInput, sortBy, sortOrder });
  } }, "\uAC80\uC0C9")), /* @__PURE__ */ React22.createElement(
    DataTable6,
    {
      sortIcons: { sorted: /* @__PURE__ */ React22.createElement(IconChevronUp, { size: 14 }), unsorted: /* @__PURE__ */ React22.createElement(IconSelector, { size: 14 }) },
      withTableBorder: true,
      striped: true,
      highlightOnHover: true,
      records: crawlerCaches,
      columns: [
        { accessor: "id", title: "ID", sortable: true },
        { accessor: "url", title: "URL", sortable: true, render: (row) => /* @__PURE__ */ React22.createElement(Text18, { size: "sm", style: { wordBreak: "break-all" } }, row.url) },
        { accessor: "status", title: "Status", sortable: true, render: (row) => formatCrawlerStatus(row.status) },
        { accessor: "title", title: "Title", sortable: true, render: (row) => row.title || "-" },
        { accessor: "image", title: "Image", sortable: true, render: (row) => {
          if (!row.image) return "-";
          return /* @__PURE__ */ React22.createElement(Anchor7, { href: row.image, target: "_blank", rel: "noreferrer" }, /* @__PURE__ */ React22.createElement(Image2, { src: row.image, alt: row.title || row.url || "crawler image", h: 44, w: 72, fit: "cover", radius: "sm", fallbackSrc: "data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw==" }));
        } },
        { accessor: "description", title: "Description", sortable: true, render: (row) => row.description || "-" },
        { accessor: "dateUpdated", title: "Updated", sortable: true, render: (row) => formatDateTimeInClientTimezone(row.dateUpdated) },
        { accessor: "action", title: "Action", render: (row) => /* @__PURE__ */ React22.createElement(Group18, { gap: 6 }, /* @__PURE__ */ React22.createElement(Button13, { size: "xs", variant: "light", loading: refreshingUrl === row.url, onClick: () => refreshCrawlerCache(row.url, currentParams) }, "\uAC31\uC2E0"), /* @__PURE__ */ React22.createElement(Button13, { size: "xs", color: "red", variant: "light", loading: deletingUrl === row.url, onClick: () => deleteCrawlerCache(row.url, currentParams) }, "\uC0AD\uC81C")) }
      ],
      sortStatus: { columnAccessor: sortBy, direction: sortOrder },
      onSortStatusChange: (next) => {
        const dir = next.direction ?? "desc";
        setPage(1);
        setSortBy(next.columnAccessor);
        setSortOrder(dir);
        loadCrawlerCaches({ page: 1, pageSize: CRAWLER_CACHE_PAGE_SIZE, search, sortBy: next.columnAccessor, sortOrder: dir });
      },
      totalRecords: crawlerCacheCount,
      recordsPerPage: CRAWLER_CACHE_PAGE_SIZE,
      page: normalizedPage,
      onPageChange: (nextPage) => {
        setPage(nextPage);
        loadCrawlerCaches({ page: nextPage, pageSize: CRAWLER_CACHE_PAGE_SIZE, search, sortBy, sortOrder });
      },
      paginationText: ({ from, to, totalRecords }) => `${from}-${to} / ${totalRecords}`,
      minHeight: 380
    }
  ));
}

// app/assets/js/admin.jsx
var router = createBrowserRouter([
  {
    path: "/Admin",
    element: /* @__PURE__ */ React23.createElement(AdminLayout, null),
    children: [
      { index: true, element: /* @__PURE__ */ React23.createElement(DashboardPage, null) },
      { path: "Site", element: /* @__PURE__ */ React23.createElement(SitesPage, null) },
      { path: "Sites", element: /* @__PURE__ */ React23.createElement(SitesPage, null) },
      {
        path: "Site/:siteSeq",
        element: /* @__PURE__ */ React23.createElement(SiteLayout, null),
        children: [
          { index: true, element: /* @__PURE__ */ React23.createElement(SiteDashboardPage, null) },
          { path: "Detail", element: /* @__PURE__ */ React23.createElement(SiteDetailPage, null) },
          { path: "Config", element: /* @__PURE__ */ React23.createElement(SiteConfigPage, null) },
          { path: "Cache", element: /* @__PURE__ */ React23.createElement(SiteCachePage, null) },
          { path: "Permission", element: /* @__PURE__ */ React23.createElement(SitePermissionPage, null) },
          { path: "Admins", element: /* @__PURE__ */ React23.createElement(SiteAdminsPage, null) }
        ]
      },
      { path: "User", element: /* @__PURE__ */ React23.createElement(AllUsersPage, null) },
      { path: "AllUsers", element: /* @__PURE__ */ React23.createElement(AllUsersPage, null) },
      { path: "User/UserViewHistory", element: /* @__PURE__ */ React23.createElement(UserViewsPage, null) },
      { path: "UserViews", element: /* @__PURE__ */ React23.createElement(UserViewsPage, null) },
      { path: "AccessLog", element: /* @__PURE__ */ React23.createElement(AccessLogsPage, null) },
      { path: "AccessLogs", element: /* @__PURE__ */ React23.createElement(AccessLogsPage, null) },
      { path: ":siteSeq/AccessLog", element: /* @__PURE__ */ React23.createElement(AccessLogsPage, null) },
      { path: "RecentChange", element: /* @__PURE__ */ React23.createElement(RecentChangesPage, null) },
      { path: "RecentChanges", element: /* @__PURE__ */ React23.createElement(RecentChangesPage, null) },
      { path: "S3", element: /* @__PURE__ */ React23.createElement(S3BrowserPage, null) },
      { path: "S3Browser", element: /* @__PURE__ */ React23.createElement(S3BrowserPage, null) },
      { path: "CrawlerCache", element: /* @__PURE__ */ React23.createElement(CrawlerCachePage, null) },
      { path: "CrawlerCaches", element: /* @__PURE__ */ React23.createElement(CrawlerCachePage, null) }
    ]
  }
]);
window.addEventListener("error", (event) => console.error("[AdminUI] window:error", event.message, event.error));
window.addEventListener("unhandledrejection", (event) => console.error("[AdminUI] window:unhandledrejection", event.reason));
window.addEventListener("DOMContentLoaded", () => {
  const rootElement = document.getElementById("main");
  if (!rootElement) {
    console.error("[AdminUI] #main not found");
    return;
  }
  createRoot(rootElement).render(
    /* @__PURE__ */ React23.createElement(MantineProvider, { defaultColorScheme: "light", theme: { primaryColor: "indigo", defaultRadius: "md" } }, /* @__PURE__ */ React23.createElement(RouterProvider, { router }))
  );
});
