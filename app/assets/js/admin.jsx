import React, {useCallback, useEffect, useMemo, useState} from "react";
import {createRoot} from "react-dom/client";
import dayjs from "https://cdn.jsdelivr.net/npm/dayjs@1.11.13/+esm";
import {
    MantineProvider,
    Anchor,
    AppShell,
    Avatar,
    Badge,
    Button,
    Card,
    ColorInput,
    Divider,
    Group,
    Image,
    Loader,
    Paper,
    Pagination,
    Progress,
    Select,
    SimpleGrid,
    Stack,
    Table,
    Text,
    TextInput,
    ThemeIcon,
    Title,
} from "@mantine/core";
import {DataTable} from "mantine-datatable";
import Navigation from "./admin/navigation";
import {IconChevronUp, IconSelector, makeTable} from "./component/commonWidgets";
import {DailyStatTable, MultiTrendChart, StatTrendCard} from "./admin/mainWidgets";
import {SiteListCard} from "./site/siteWidgets";


const LOG_PREFIX = "[AdminUI]";
const CRAWLER_CACHE_PAGE_SIZE = 20;
const ADMIN_PAGE_META_PAGE_SIZE = 20;
const PERMISSION_TARGET_TYPE_OPTIONS = ["All", "Exact", "StartsWith", "EndsWith"];
const PERMISSION_ACTOR_TYPE_OPTIONS = ["All", "Login", "Exact", "Domain"];
const PERMISSION_ACTION_OPTIONS = ["none", "read", "edit", "create", "upload", "delete", "admin"];

function logInfo(...args) {
    console.log(LOG_PREFIX, ...args);
}

function logError(...args) {
    console.error(LOG_PREFIX, ...args);
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

function routeToPage(pathname) {
    if (/^\/Admin\/\d+\/AccessLog$/.test(pathname)) {
        return "access-logs";
    }
    if (/^\/Admin\/Site\/\d+\/Config$/.test(pathname)) {
        return "site-config";
    }
    if (/^\/Admin\/Site\/\d+\/Cache$/.test(pathname)) {
        return "site-cache";
    }
    if (/^\/Admin\/Site\/\d+\/Permission$/.test(pathname)) {
        return "site-permission";
    }
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
    if (pathname === "/Admin/CrawlerCache") {
        return "crawler-cache";
    }
    if (pathname === "/Admin/S3") {
        return "s3-browser";
    }
    if (pathname === "/Admin/AccessLog") {
        return "access-logs";
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
    if (pathname === "/Admin/CrawlerCaches") {
        return "crawler-cache";
    }
    if (pathname === "/Admin/S3Browser") {
        return "s3-browser";
    }
    if (pathname === "/Admin/AccessLogs") {
        return "access-logs";
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
    const matched = pathname.match(/^\/Admin\/Site\/(\d+)(?:\/(?:Config|Cache|Permission))?$/);
    if (!matched) {
        return "";
    }
    const siteSeq = Number.parseInt(matched[1], 10);
    return Number.isFinite(siteSeq) && siteSeq > 0 ? String(siteSeq) : "";
}

function parseSiteSeqForAccessLogPathname(pathname) {
    const matched = pathname.match(/^\/Admin\/(\d+)\/AccessLog$/);
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
    if (page === "site-detail" || page === "site-config" || page === "site-cache" || page === "site-permission" || page === "sites" || page === "users") {
        return "Site";
    }
    if (page === "access-logs") {
        return "AccessLog";
    }
    if (page === "s3-browser") {
        return "S3 Browser";
    }
    if (page === "crawler-cache") {
        return "Crawler Cache";
    }
    return "Dashboard";
}

function UserProfileCell({user}) {
    const fallbackText = user?.nickname?.[0] || user?.email?.[0] || "?";
    return (
        <Group gap={8} wrap="nowrap">
            <Avatar src={user?.profileImageUrl || null} radius="xl" size="sm">{fallbackText}</Avatar>
            <Stack gap={0}>
                <Text size="sm" fw={600}>{user?.nickname || "-"}</Text>
                <Text size="xs" c="dimmed">{user?.email || "-"}</Text>
            </Stack>
        </Group>
    );
}

async function fetchJson(url) {
    logInfo("fetch:start", url);
    const response = await fetch(url, {credentials: "same-origin"});
    if (!response.ok) {
        throw new Error(`HTTP ${response.status}`);
    }
    const data = await response.json();
    logInfo("fetch:success", url, {count: Array.isArray(data) ? data.length : undefined});
    return data;
}

async function fetchCsrfToken() {
    const response = await fetch("/api/csrf", {credentials: "same-origin"});
    if (!response.ok) {
        throw new Error(`CSRF HTTP ${response.status}`);
    }
    const token = await response.json();
    return {
        name: token?.name ?? "csrfToken",
        value: token?.value ?? "",
    };
}

function useAdminData(page) {
    const [loading, setLoading] = useState(true);
    const [sites, setSites] = useState([]);
    const [users, setUsers] = useState([]);
    const [allUsers, setAllUsers] = useState([]);
    const [allUserCount, setAllUserCount] = useState(0);
    const [dailyStats, setDailyStats] = useState({
        userCreated: [],
        siteUserCreated: [],
        pageCreated: [],
        pageEdited: [],
    });
    const [recentChanges, setRecentChanges] = useState([]);
    const [accessLogs, setAccessLogs] = useState([]);
    const [accessLogCount, setAccessLogCount] = useState(0);
    const [topViewedPages, setTopViewedPages] = useState([]);
    const [userViewHistories, setUserViewHistories] = useState([]);
    const [loadingUserViewHistories, setLoadingUserViewHistories] = useState(false);
    const [clearingSiteSeq, setClearingSiteSeq] = useState(0);
    const [siteFaviconUrl, setSiteFaviconUrl] = useState("/public/favicon.png");
    const [siteFaviconObjectKey, setSiteFaviconObjectKey] = useState("");
    const [uploadingFavicon, setUploadingFavicon] = useState(false);
    const [deletingFavicon, setDeletingFavicon] = useState(false);
    const [siteTheme, setSiteTheme] = useState({
        headerBackgroundColor: "",
        headerForegroundColor: "",
        bodyBackgroundColor: "",
        bodyForegroundColor: "",
        footerBackgroundColor: "",
        footerForegroundColor: "",
    });
    const [savingSiteTheme, setSavingSiteTheme] = useState(false);
    const [calculatingSiteSeq, setCalculatingSiteSeq] = useState(0);
    const [memoryCacheStats, setMemoryCacheStats] = useState([]);
    const [error, setError] = useState("");
    const [s3Items, setS3Items] = useState([]);
    const [loadingS3, setLoadingS3] = useState(false);
    const [deletingS3, setDeletingS3] = useState(false);
    const [selectedS3Keys, setSelectedS3Keys] = useState([]);
    const [expandedS3Nodes, setExpandedS3Nodes] = useState({});
    const [crawlerCaches, setCrawlerCaches] = useState([]);
    const [crawlerCacheCount, setCrawlerCacheCount] = useState(0);
    const [refreshingCrawlerUrl, setRefreshingCrawlerUrl] = useState("");
    const [deletingCrawlerUrl, setDeletingCrawlerUrl] = useState("");
    const [crawlerSearchInput, setCrawlerSearchInput] = useState("");
    const [crawlerSearch, setCrawlerSearch] = useState("");
    const [crawlerPage, setCrawlerPage] = useState(1);
    const [crawlerSortBy, setCrawlerSortBy] = useState("id");
    const [crawlerSortOrder, setCrawlerSortOrder] = useState("desc");
    const [adminPageMetaRows, setAdminPageMetaRows] = useState([]);
    const [adminPageMetaCount, setAdminPageMetaCount] = useState(0);
    const [permissionRows, setPermissionRows] = useState([]);
    const [permissionDiagnose, setPermissionDiagnose] = useState(null);
    const [savingPermission, setSavingPermission] = useState(false);
    const [deletingPermissionKey, setDeletingPermissionKey] = useState("");
    const loadAdminPageMetaList = useCallback(async ({
        siteSeq,
        page = 1,
        pageSize = ADMIN_PAGE_META_PAGE_SIZE,
        search = "",
        sortBy = "dateUpdated",
        sortOrder = "desc",
    } = {}) => {
        if (!siteSeq) {
            setAdminPageMetaRows([]);
            setAdminPageMetaCount(0);
            return;
        }
        const params = new URLSearchParams({
            page: String(page),
            pageSize: String(pageSize),
            search,
            sortBy,
            sortOrder,
        });
        const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/PageMetaList?${params.toString()}`);
        const rows = Array.isArray(data?.array)
            ? data.array
            : (Array.isArray(data) ? data : []);
        setAdminPageMetaRows(rows);
        setAdminPageMetaCount(Number(data?.count ?? rows.length));
    }, []);
    const loadPermissions = useCallback(async (siteSeq) => {
        if (!siteSeq) {
            setPermissionRows([]);
            return;
        }
        const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Permissions`);
        setPermissionRows(Array.isArray(data?.permissions) ? data.permissions : []);
    }, []);
    const savePermission = useCallback(async (siteSeq, permission) => {
        if (!siteSeq) return false;
        setSavingPermission(true);
        try {
            const csrfToken = await fetchCsrfToken();
            const payload = new URLSearchParams();
            ["targetType", "target", "actorType", "actor", "action"].forEach((key) => payload.set(key, permission[key] ?? ""));
            payload.set(csrfToken.name, csrfToken.value);
            payload.set("csrfToken", csrfToken.value);
            const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Permissions`, {
                method: "POST",
                credentials: "same-origin",
                headers: {"Content-Type": "application/x-www-form-urlencoded; charset=UTF-8", "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value},
                body: payload.toString(),
            });
            if (!response.ok) {
                const payloadJson = await response.json().catch(() => null);
                throw new Error(payloadJson?.error || `HTTP ${response.status}`);
            }
            await loadPermissions(siteSeq);
            return true;
        } catch (caughtError) {
            logError("permission:save:error", caughtError);
            setError(caughtError.message || String(caughtError));
            return false;
        } finally {
            setSavingPermission(false);
        }
    }, [loadPermissions]);
    const deletePermission = useCallback(async (siteSeq, permission) => {
        if (!siteSeq || !permission) return;
        const key = `${permission.targetType}:${permission.target}:${permission.actorType}:${permission.actor}`;
        setDeletingPermissionKey(key);
        try {
            const csrfToken = await fetchCsrfToken();
            const params = new URLSearchParams({
                targetType: permission.targetType ?? "",
                target: permission.target ?? "",
                actorType: permission.actorType ?? "",
                actor: permission.actor ?? "",
            });
            const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Permissions?${params.toString()}`, {
                method: "DELETE",
                credentials: "same-origin",
                headers: {"Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value},
            });
            if (!response.ok) {
                const payloadJson = await response.json().catch(() => null);
                throw new Error(payloadJson?.error || `HTTP ${response.status}`);
            }
            await loadPermissions(siteSeq);
        } catch (caughtError) {
            logError("permission:delete:error", caughtError);
            setError(caughtError.message || String(caughtError));
        } finally {
            setDeletingPermissionKey("");
        }
    }, [loadPermissions]);
    const diagnosePermission = useCallback(async (siteSeq, pageName, actor, action) => {
        if (!siteSeq) return;
        const params = new URLSearchParams({
            pageName: pageName ?? "",
            actor: actor ?? "",
            action: action || "read",
        });
        const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/PermissionDiagnose?${params.toString()}`);
        setPermissionDiagnose(data);
    }, []);
    const loadAccessLogs = useCallback(async ({
        page = 1,
        pageSize = 20,
        search = "",
        sortBy = "seq",
        sortOrder = "desc",
        siteSeq = "",
    } = {}) => {
        const params = new URLSearchParams({
            page: String(page),
            pageSize: String(pageSize),
            search,
            sortBy,
            sortOrder,
        });
        if (siteSeq) {
            params.set("siteSeq", String(siteSeq));
        }
        const data = await fetchJson(`/api/Admin/AccessLogs?${params.toString()}`);
        const rows = Array.isArray(data?.array)
            ? data.array
            : (Array.isArray(data) ? data : []);
        setAccessLogs(rows);
        setAccessLogCount(Number(data?.count ?? rows.length));
    }, []);

    const loadS3Objects = useCallback(async () => {
        setLoadingS3(true);
        try {
            const params = new URLSearchParams({prefix: "", maxKeys: "5000", recursive: "true"});
            const data = await fetchJson(`/api/Admin/S3Objects?${params.toString()}`);
            setS3Items(Array.isArray(data?.items) ? data.items : []);
            setSelectedS3Keys([]);
            setExpandedS3Nodes({"__root__": true});
        } catch (caughtError) {
            logError("s3:load:error", caughtError);
            setError(`S3 조회 실패: ${caughtError.message}`);
        } finally {
            setLoadingS3(false);
        }
    }, []);

    const toggleS3Node = useCallback((key) => {
        setExpandedS3Nodes((prev) => ({...prev, [key]: !prev[key]}));
    }, []);
    const expandAllS3Nodes = useCallback(() => {
        const next = {"__root__": true};
        (Array.isArray(s3Items) ? s3Items : [])
            .filter((item) => !item.isDirectory)
            .forEach((item) => {
                const parts = String(item.key || "").split("/").filter(Boolean);
                for (let i = 1; i < parts.length; i += 1) {
                    next[parts.slice(0, i).join("/")] = true;
                }
            });
        setExpandedS3Nodes(next);
    }, [s3Items]);
    const deleteS3Selected = useCallback(async () => {
        if (selectedS3Keys.length === 0) return;
        setDeletingS3(true);
        try {
            const csrf = await fetchCsrfToken();
            const response = await fetch("/api/Admin/S3Objects", {
                method: "DELETE",
                credentials: "same-origin",
                headers: {"Content-Type": "application/json", "Csrf-Token": csrf.value, "X-CSRF-Token": csrf.value},
                body: JSON.stringify({keys: selectedS3Keys}),
            });
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            await loadS3Objects();
        } catch (caughtError) {
            logError("s3:delete:error", caughtError);
            setError(`S3 삭제 실패: ${caughtError.message}`);
        } finally {
            setDeletingS3(false);
        }
    }, [loadS3Objects, selectedS3Keys]);
    const downloadS3Object = useCallback(async (key) => {
        try {
            const params = new URLSearchParams({key});
            const data = await fetchJson(`/api/Admin/S3DownloadUrl?${params.toString()}`);
            if (data?.url) {
                window.open(data.url, "_blank", "noopener,noreferrer");
            }
        } catch (caughtError) {
            logError("s3:download:error", caughtError);
            setError(`다운로드 URL 생성 실패: ${caughtError.message}`);
        }
    }, []);
    const loadAllUsers = useCallback(async ({
        page = 1,
        pageSize = 20,
        search = "",
        sortBy = "seq",
        sortOrder = "desc",
    } = {}) => {
        const params = new URLSearchParams({
            page: String(page),
            pageSize: String(pageSize),
            search,
            sortBy,
            sortOrder,
        });
        const data = await fetchJson(`/api/Admin/Users?${params.toString()}`);
        const rows = Array.isArray(data?.array)
            ? data.array
            : (Array.isArray(data) ? data : []);
        setAllUsers(rows);
        setAllUserCount(Number(data?.count ?? rows.length));
    }, []);
    const loadCrawlerCaches = useCallback(async ({
        page = 1,
        pageSize = CRAWLER_CACHE_PAGE_SIZE,
        search = "",
        sortBy = "id",
        sortOrder = "desc",
    } = {}) => {
        const params = new URLSearchParams({
            page: String(page),
            pageSize: String(pageSize),
            search,
            sortBy,
            sortOrder,
        });
        const data = await fetchJson(`/api/Admin/CrawlerCache?${params.toString()}`);
        const rows = Array.isArray(data?.array)
            ? data.array
            : (Array.isArray(data) ? data : []);
        setCrawlerCaches(rows);
        setCrawlerCacheCount(Number(data?.count ?? rows.length));
    }, []);

    const refreshCrawlerCache = useCallback(async (url) => {
        setRefreshingCrawlerUrl(url);
        try {
            const csrfToken = await fetchCsrfToken();
            const payload = new URLSearchParams();
            payload.set("url", url);
            payload.set(csrfToken.name, csrfToken.value);
            payload.set("csrfToken", csrfToken.value);
            const response = await fetch("/api/Admin/CrawlerCache/Refresh", {
                method: "POST",
                credentials: "same-origin",
                headers: {"Content-Type": "application/x-www-form-urlencoded; charset=UTF-8", "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value},
                body: payload.toString(),
            });
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            await loadCrawlerCaches();
        } finally {
            setRefreshingCrawlerUrl("");
        }
    }, [loadCrawlerCaches]);

    const deleteCrawlerCache = useCallback(async (url) => {
        setDeletingCrawlerUrl(url);
        try {
            const csrfToken = await fetchCsrfToken();
            const response = await fetch(`/api/Admin/CrawlerCache?url=${encodeURIComponent(url)}`, {
                method: "DELETE",
                credentials: "same-origin",
                headers: {"Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value},
            });
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            await loadCrawlerCaches();
        } finally {
            setDeletingCrawlerUrl("");
        }
    }, [loadCrawlerCaches]);

    const loadRecentChanges = useCallback(async (n = 50) => {
        const data = await fetchJson(`/api/Admin/RecentChanges?n=${encodeURIComponent(n)}`);
        setRecentChanges(data);
    }, []);

    const loadUserViewHistories = useCallback(async (userSeq, n = 200) => {
        setLoadingUserViewHistories(true);
        try {
            const clampedN = Math.min(1000, Math.max(1, Number.parseInt(String(n), 10) || 200));
            const data = await fetchJson(
                `/api/Admin/UserViews?userSeq=${encodeURIComponent(userSeq)}&n=${encodeURIComponent(clampedN)}`,
            );
            setUserViewHistories(data);
        } finally {
            setLoadingUserViewHistories(false);
        }
    }, []);

    const loadDashboard = useCallback(async () => {
        const [siteData, userData, allUserData, dailyStatsData, recentChangesData, topViewedPagesData] = await Promise.all([
            fetchJson("/api/Admin/Sites"),
            fetchJson("/api/Admin/SiteUsers"),
            fetchJson("/api/Admin/Users"),
            fetchJson("/api/Admin/DailyStats"),
            fetchJson("/api/Admin/RecentChanges?n=30"),
            fetchJson("/api/Admin/TopViewedPages?n=30"),
        ]);
        const allUserRows = Array.isArray(allUserData?.array)
            ? allUserData.array
            : (Array.isArray(allUserData) ? allUserData : []);
        setSites(siteData);
        setUsers(userData);
        setAllUsers(allUserRows);
        setRecentChanges(recentChangesData);
        setTopViewedPages(topViewedPagesData);
        setDailyStats({
            userCreated: dailyStatsData?.userCreated ?? [],
            siteUserCreated: dailyStatsData?.siteUserCreated ?? [],
            pageCreated: dailyStatsData?.pageCreated ?? [],
            pageEdited: dailyStatsData?.pageEdited ?? [],
        });
    }, []);

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
                    "X-CSRF-Token": csrfToken.value,
                },
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
                    "X-CSRF-Token": csrfToken.value,
                },
                body: formData,
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
                    "X-CSRF-Token": csrfToken.value,
                },
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
                bodyBackgroundColor: "",
                bodyForegroundColor: "",
                footerBackgroundColor: "",
                footerForegroundColor: "",
            });
            return;
        }
        try {
            const data = await fetchJson(`/api/Admin/SiteTheme?siteSeq=${encodeURIComponent(siteSeq)}`);
            setSiteTheme({
                headerBackgroundColor: data?.headerBackgroundColor ?? "",
                headerForegroundColor: data?.headerForegroundColor ?? "",
                bodyBackgroundColor: data?.bodyBackgroundColor ?? "",
                bodyForegroundColor: data?.bodyForegroundColor ?? "",
                footerBackgroundColor: data?.footerBackgroundColor ?? "",
                footerForegroundColor: data?.footerForegroundColor ?? "",
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
            payload.set("bodyBackgroundColor", nextTheme?.bodyBackgroundColor ?? "");
            payload.set("bodyForegroundColor", nextTheme?.bodyForegroundColor ?? "");
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
                    "X-CSRF-Token": csrfToken.value,
                },
                body: payload.toString(),
            });
            if (!response.ok) {
                const payloadJson = await response.json().catch(() => null);
                throw new Error(payloadJson?.error || `HTTP ${response.status}`);
            }
            const data = await response.json();
            setSiteTheme({
                headerBackgroundColor: data?.headerBackgroundColor ?? "",
                headerForegroundColor: data?.headerForegroundColor ?? "",
                bodyBackgroundColor: data?.bodyBackgroundColor ?? "",
                bodyForegroundColor: data?.bodyForegroundColor ?? "",
                footerBackgroundColor: data?.footerBackgroundColor ?? "",
                footerForegroundColor: data?.footerForegroundColor ?? "",
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
            const suffix = pageName?.trim()
                ? `?pageName=${encodeURIComponent(pageName.trim())}`
                : "";
            const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Calculate${suffix}`, {
                method: "POST",
                credentials: "same-origin",
                headers: {
                    "Csrf-Token": csrfToken.value,
                    "X-CSRF-Token": csrfToken.value,
                    "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8",
                },
                body: `${encodeURIComponent(csrfToken.name)}=${encodeURIComponent(csrfToken.value)}&csrfToken=${encodeURIComponent(csrfToken.value)}`,
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

    const loadMemoryCacheStats = useCallback(async () => {
        try {
            const data = await fetchJson("/api/Admin/MemoryCacheStats");
            setMemoryCacheStats(Array.isArray(data) ? data : []);
        } catch (caughtError) {
            logError("memory-cache:load:error", caughtError);
            setMemoryCacheStats([]);
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
                        setDailyStats({
                            userCreated: [],
                            siteUserCreated: [],
                            pageCreated: [],
                            pageEdited: [],
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
                        setDailyStats({
                            userCreated: [],
                            siteUserCreated: [],
                            pageCreated: [],
                            pageEdited: [],
                        });
                        setTopViewedPages([]);
                    }
                    return;
                }

                if (page === "all-users") {
                    if (mounted) {
                        await loadAllUsers();
                        setUsers([]);
                        setSites([]);
                        setDailyStats({
                            userCreated: [],
                            siteUserCreated: [],
                            pageCreated: [],
                            pageEdited: [],
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
                        setDailyStats({
                            userCreated: [],
                            siteUserCreated: [],
                            pageCreated: [],
                            pageEdited: [],
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
                        setDailyStats({
                            userCreated: [],
                            siteUserCreated: [],
                            pageCreated: [],
                            pageEdited: [],
                        });
                        setTopViewedPages([]);
                    }
                    return;
                }
                if (page === "access-logs") {
                    if (mounted) {
                        setSites([]);
                        setUsers([]);
                        setAllUsers([]);
                        setDailyStats({
                            userCreated: [],
                            siteUserCreated: [],
                            pageCreated: [],
                            pageEdited: [],
                        });
                        setTopViewedPages([]);
                    }
                    return;
                }
                if (page === "s3-browser") {
                    if (mounted) {
                        setSites([]);
                        setUsers([]);
                        setAllUsers([]);
                        setDailyStats({userCreated: [], siteUserCreated: [], pageCreated: [], pageEdited: []});
                        setTopViewedPages([]);
                    }
                    await loadS3Objects();
                    return;
                }
                if (page === "crawler-cache") {
                    if (mounted) {
                        setSites([]);
                        setUsers([]);
                        setAllUsers([]);
                        setDailyStats({userCreated: [], siteUserCreated: [], pageCreated: [], pageEdited: []});
                        setTopViewedPages([]);
                    }
                    await loadCrawlerCaches();
                    return;
                }

                if (mounted) {
                    await loadDashboard();
                    await loadMemoryCacheStats();
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
    }, [page, loadDashboard, loadRecentChanges, loadUserViewHistories, loadMemoryCacheStats, loadAccessLogs, loadAllUsers, loadS3Objects, loadCrawlerCaches]);

    return {
        loading,
        sites,
        users,
        allUsers,
        allUserCount,
        dailyStats,
        recentChanges,
        accessLogs,
        accessLogCount,
        topViewedPages,
        userViewHistories,
        loadingUserViewHistories,
        loadUserViewHistories,
        loadRecentChanges,
        loadAccessLogs,
        loadAllUsers,
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
        memoryCacheStats,
        loadMemoryCacheStats,
        s3Items,
        loadS3Objects,
        selectedS3Keys,
        setSelectedS3Keys,
        deleteS3Selected,
        downloadS3Object,
        loadingS3,
        deletingS3,
        expandedS3Nodes,
        toggleS3Node,
        expandAllS3Nodes,
        crawlerCaches,
        crawlerCacheCount,
        loadCrawlerCaches,
        refreshCrawlerCache,
        deleteCrawlerCache,
        refreshingCrawlerUrl,
        deletingCrawlerUrl,
        crawlerSearchInput,
        setCrawlerSearchInput,
        crawlerSearch,
        setCrawlerSearch,
        crawlerPage,
        setCrawlerPage,
        crawlerSortBy,
        setCrawlerSortBy,
        crawlerSortOrder,
        setCrawlerSortOrder,
        adminPageMetaRows,
        adminPageMetaCount,
        loadAdminPageMetaList,
        permissionRows,
        permissionDiagnose,
        loadPermissions,
        savePermission,
        deletePermission,
        diagnosePermission,
        savingPermission,
        deletingPermissionKey,
        error,
    };
}


function AdminContent({page, onNavigate, pathname, search}) {
    const ACCESS_LOG_PAGE_SIZE = 20;
    const {
        loading,
        sites,
        users,
        allUsers,
        allUserCount,
        dailyStats,
        recentChanges,
        accessLogs,
        accessLogCount,
        topViewedPages,
        userViewHistories,
        loadingUserViewHistories,
        loadUserViewHistories,
        loadRecentChanges,
        loadAccessLogs,
        loadAllUsers,
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
        memoryCacheStats,
        loadMemoryCacheStats,
        s3Items,
        loadS3Objects,
        selectedS3Keys,
        setSelectedS3Keys,
        deleteS3Selected,
        downloadS3Object,
        loadingS3,
        deletingS3,
        expandedS3Nodes,
        toggleS3Node,
        expandAllS3Nodes,
        crawlerCaches,
        crawlerCacheCount,
        loadCrawlerCaches,
        refreshCrawlerCache,
        deleteCrawlerCache,
        refreshingCrawlerUrl,
        deletingCrawlerUrl,
        crawlerSearchInput,
        setCrawlerSearchInput,
        crawlerSearch,
        setCrawlerSearch,
        crawlerPage,
        setCrawlerPage,
        crawlerSortBy,
        setCrawlerSortBy,
        crawlerSortOrder,
        setCrawlerSortOrder,
        adminPageMetaRows,
        adminPageMetaCount,
        loadAdminPageMetaList,
        permissionRows,
        permissionDiagnose,
        loadPermissions,
        savePermission,
        deletePermission,
        diagnosePermission,
        savingPermission,
        deletingPermissionKey,
        error,
    } = useAdminData(page);
    const crawlerTotalPages = Math.max(1, Math.ceil(crawlerCacheCount / CRAWLER_CACHE_PAGE_SIZE));
    const normalizedCrawlerPage = Math.min(crawlerPage, crawlerTotalPages);
    const [recentChangeLimitInput, setRecentChangeLimitInput] = useState("50");
    const [accessLogPage, setAccessLogPage] = useState(1);
    const [accessLogSearchInput, setAccessLogSearchInput] = useState("");
    const [accessLogSortBy, setAccessLogSortBy] = useState("seq");
    const [accessLogSortOrder, setAccessLogSortOrder] = useState("desc");
    const [allUserPage, setAllUserPage] = useState(1);
    const [allUserSearchInput, setAllUserSearchInput] = useState("");
    const [allUserSortBy, setAllUserSortBy] = useState("seq");
    const [allUserSortOrder, setAllUserSortOrder] = useState("desc");
    const [faviconFile, setFaviconFile] = useState(null);
    const [selectedSiteSeq, setSelectedSiteSeq] = useState("");
    const [sitePageNames, setSitePageNames] = useState([]);
    const [adminPageMetaPage, setAdminPageMetaPage] = useState(1);
    const [adminPageMetaSearchInput, setAdminPageMetaSearchInput] = useState("");
    const [adminPageMetaSearch, setAdminPageMetaSearch] = useState("");
    const [adminPageMetaSortBy, setAdminPageMetaSortBy] = useState("dateUpdated");
    const [adminPageMetaSortOrder, setAdminPageMetaSortOrder] = useState("desc");
    const [permissionForm, setPermissionForm] = useState({targetType: "All", target: "", actorType: "All", actor: "", action: "read"});
    const [permissionDiagnoseForm, setPermissionDiagnoseForm] = useState({pageName: "", actor: "", action: "read"});
    const [selectedCalculatePageName, setSelectedCalculatePageName] = useState("");
    const [siteCalculateMessage, setSiteCalculateMessage] = useState("");
    const selectedSite = useMemo(
        () => sites.find((site) => String(site.seq) === selectedSiteSeq) ?? null,
        [sites, selectedSiteSeq],
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
        [allUsers, selectedUserSeq],
    );
    const selectedSiteDomainsText = useMemo(
        () => (selectedSite?.domains ?? []).join(", ") || "-",
        [selectedSite],
    );
    const selectedAccessLogSiteSeq = useMemo(
        () => parseSiteSeqForAccessLogPathname(pathname),
        [pathname],
    );
    const selectedAccessLogSite = useMemo(
        () => sites.find((site) => String(site.seq) === selectedAccessLogSiteSeq) ?? null,
        [sites, selectedAccessLogSiteSeq],
    );
    const siteDomainBySeq = useMemo(
        () => new Map(sites.map((site) => [site.seq, (site.domains ?? []).find((domain) => !!domain) ?? ""])),
        [sites],
    );
    const resolveSiteUrl = useCallback((row) => {
        const domainFromRow = typeof row.siteDomain === "string" ? row.siteDomain.trim() : "";
        const domainFromSites = (siteDomainBySeq.get(row.siteSeq) ?? "").trim();
        const domain = domainFromRow || domainFromSites;
        return domain ? `https://${domain}` : "";
    }, [siteDomainBySeq]);
    const isSiteConfigPage = page === "site-config";
    const accessLogTotalPages = Math.max(1, Math.ceil(accessLogCount / ACCESS_LOG_PAGE_SIZE));
    const allUserTotalPages = Math.max(1, Math.ceil(allUserCount / ACCESS_LOG_PAGE_SIZE));
    const adminPageMetaTotalPages = Math.max(1, Math.ceil(adminPageMetaCount / ADMIN_PAGE_META_PAGE_SIZE));

    useEffect(() => {
        if (page !== "site-detail" && page !== "site-config" && page !== "site-cache" && page !== "site-permission") {
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
        if ((page === "site-detail" || page === "site-config" || page === "site-cache" || page === "site-permission") && selectedSiteSeq) {
            loadSiteFavicon(selectedSiteSeq);
            loadSiteTheme(selectedSiteSeq);
            loadAdminSitePageNames(selectedSiteSeq)
                .then((pageNames) => {
                    setSitePageNames(pageNames);
                })
                .catch((caughtError) => {
                    logError("site:pageNames:error", selectedSiteSeq, caughtError);
                });
            loadAdminPageMetaList({
                siteSeq: selectedSiteSeq,
                page: 1,
                pageSize: ADMIN_PAGE_META_PAGE_SIZE,
                search: "",
                sortBy: "dateUpdated",
                sortOrder: "desc",
            }).catch((caughtError) => {
                logError("site:pageMetaList:error", selectedSiteSeq, caughtError);
            });
            loadPermissions(selectedSiteSeq).catch((caughtError) => {
                logError("site:permissions:error", selectedSiteSeq, caughtError);
            });
        }
    }, [page, selectedSiteSeq, loadSiteFavicon, loadSiteTheme, loadAdminSitePageNames, loadAdminPageMetaList, loadPermissions]);

    useEffect(() => {
        if (page !== "access-logs") {
            return;
        }
        setAccessLogPage(1);
        loadAccessLogs({
            page: 1,
            pageSize: ACCESS_LOG_PAGE_SIZE,
            search: accessLogSearchInput,
            sortBy: accessLogSortBy,
            sortOrder: accessLogSortOrder,
            siteSeq: selectedAccessLogSiteSeq,
        });
    }, [page, selectedAccessLogSiteSeq]);

    if (loading) {
        return (
            <Paper p="xl" withBorder radius="md" shadow="xs">
                <Stack align="center" gap="xs" py="xl">
                    <Loader size="lg" color="blue" type="dots"/>
                    <Title order={4} c="dark">
                        Admin 데이터를 준비하고 있어요
                    </Title>
                    <Text c="dimmed" size="sm">
                        페이지가 곧 표시됩니다. 잠시만 기다려 주세요.
                    </Text>
                </Stack>
            </Paper>
        );
    }

    if (error) {
        return (
            <Paper p="lg" withBorder radius="md">
                <Text c="red" fw={600}>
                    클라이언트 렌더링 오류: {error}
                </Text>
            </Paper>
        );
    }

    if (page === "sites") {
        return <SiteListCard sites={sites} onNavigate={onNavigate}/>;
    }

    if (page === "users") {
        return (
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>Site Users</Title>
                    <Badge color="teal" variant="light">{users.length} users</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">
                    현재 호스트 기준 사용자 목록입니다.
                </Text>
                <Divider mb="md"/>
                {makeTable(
                    ["User", "Profile", "Created"],
                    users.map((user) => [user.user, <UserProfileCell user={user}/>, user.created]),
                )}
            </Card>
        );
    }

    if (page === "all-users") {
        return (
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>All Users</Title>
                    <Badge color="blue" variant="light">{allUserCount} users</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">
                    전체 사이트 기준 사용자 목록이며, 최근 방문순으로 정렬됩니다.
                </Text>
                <Divider mb="md"/>
                <Group mb="sm" align="end">
                    <TextInput label="search" value={allUserSearchInput} onChange={(event) => setAllUserSearchInput(event.currentTarget.value)} placeholder="email, nickname, seq"/>
                    <Button variant="light" onClick={() => {
                        setAllUserPage(1);
                        loadAllUsers({page: 1, pageSize: ACCESS_LOG_PAGE_SIZE, search: allUserSearchInput, sortBy: allUserSortBy, sortOrder: allUserSortOrder});
                    }}>검색</Button>
                </Group>
                <DataTable
                    sortIcons={{
                        sorted: <IconChevronUp size={14} />,
                        unsorted: <IconSelector size={14} />,
                    }}
                    withTableBorder
                    striped
                    highlightOnHover
                    records={allUsers}
                    columns={[
                        {accessor: "seq", title: "Seq", sortable: true},
                        {accessor: "profile", title: "Profile", render: (row) => <UserProfileCell user={row}/>},
                        {accessor: "email", title: "Email", sortable: true},
                        {accessor: "nickname", title: "Nickname", sortable: true},
                        {accessor: "created", title: "Created", sortable: true},
                        {accessor: "updated", title: "Updated", sortable: true},
                        {accessor: "siteCount", title: "Sites", sortable: true},
                        {accessor: "visitCount", title: "Visits", sortable: true},
                        {accessor: "lastViewed", title: "Last Viewed", sortable: true, render: (row) => row.lastViewed ?? "-"},
                        {accessor: "action", title: "Action", render: (row) => <Button size="xs" variant="light" onClick={() => onNavigate(`/Admin/User/UserViewHistory?seq=${encodeURIComponent(row.seq)}`)}>열람 이력</Button>},
                    ]}
                    sortStatus={{columnAccessor: allUserSortBy, direction: allUserSortOrder}}
                    onSortStatusChange={(nextSortStatus) => {
                        const nextDirection = nextSortStatus.direction ?? "desc";
                        setAllUserPage(1);
                        setAllUserSortBy(nextSortStatus.columnAccessor);
                        setAllUserSortOrder(nextDirection);
                        loadAllUsers({page: 1, pageSize: ACCESS_LOG_PAGE_SIZE, search: allUserSearchInput, sortBy: nextSortStatus.columnAccessor, sortOrder: nextDirection});
                    }}
                    totalRecords={allUserCount}
                    recordsPerPage={ACCESS_LOG_PAGE_SIZE}
                    page={allUserPage}
                    onPageChange={(nextPage) => {
                        setAllUserPage(nextPage);
                        loadAllUsers({page: nextPage, pageSize: ACCESS_LOG_PAGE_SIZE, search: allUserSearchInput, sortBy: allUserSortBy, sortOrder: allUserSortOrder});
                    }}
                    paginationText={({from, to, totalRecords}) => `${from}-${to} / ${totalRecords}`}
                    minHeight={380}
                />
                <Text size="xs" c="dimmed" mt="xs">Page {allUserPage} / {allUserTotalPages}</Text>
            </Card>
        );
    }

    if (page === "user-views") {
        return (
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>User View Histories</Title>
                    <Badge color="cyan" variant="light">{userViewHistories.length} rows</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">
                    선택한 사용자의 페이지 열람 이력입니다. Site 및 Page 링크로 직접 이동할 수 있습니다.
                </Text>
                <Group mb="md" justify="space-between">
                    <Button variant="light" size="xs" onClick={() => onNavigate("/Admin/User")}>
                        ← User
                    </Button>
                    {selectedAllUser ? (
                        <Group gap={8}>
                            <Text size="sm">사용자:</Text>
                            <UserProfileCell user={selectedAllUser}/>
                        </Group>
                    ) : (
                        <Text size="sm" c="dimmed">seq를 지정해 주세요. (/Admin/User/UserViewHistory?seq=숫자)</Text>
                    )}
                </Group>
                {loadingUserViewHistories ? (
                    <Group>
                        <Loader size="sm"/>
                        <Text size="sm" c="dimmed">열람 이력을 불러오는 중입니다...</Text>
                    </Group>
                ) : makeTable(
                    ["When", "Site", "Page", "History Seq"],
                    userViewHistories.map((history) => {
                        const siteUrl = history.siteDomain ? `https://${history.siteDomain}` : "";
                        const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(history.pageName)}` : "";
                        return [
                            history.viewedAt,
                            siteUrl ? <Anchor href={siteUrl} target="_blank">{history.siteName} (#{history.site})</Anchor> : `${history.siteName} (#${history.site})`,
                            pageUrl ? <Anchor href={pageUrl} target="_blank">{history.pageName}</Anchor> : history.pageName,
                            history.seq,
                        ];
                    }),
                )}
            </Card>
        );
    }

    if (page === "site-detail" || page === "site-config" || page === "site-cache" || page === "site-permission") {
        return (
            <Stack gap="lg">
                <Card withBorder radius="md" padding="lg">
                    <Group justify="space-between" mb="xs">
                        <Title order={4}>사이트 상세</Title>
                        <Badge color="blue" variant="light">Site Detail</Badge>
                    </Group>
                    <Text size="sm" c="dimmed" mb="md">
                        /Admin/Site/{`{seq}`} 경로로 접근한 사이트 상세 정보입니다.
                    </Text>
                    <Stack gap="sm">
                        <Group justify="space-between" align="flex-start" wrap="wrap">
                            <Stack gap={2}>
                                <Text size="xs" c="dimmed">선택된 사이트</Text>
                                <Text fw={700}>{selectedSite ? `${selectedSite.name} (#${selectedSite.seq})` : "선택된 사이트 없음"}</Text>
                                <Text size="sm" c="dimmed">도메인: {selectedSiteDomainsText}</Text>
                            </Stack>
                            <Button variant="light" size="xs" onClick={() => onNavigate("/Admin/Site")}>
                                ← 사이트 목록
                            </Button>
                        </Group>
                        <SimpleGrid cols={{base: 2, sm: 4}} spacing="sm">
                            <Paper withBorder radius="md" p="sm">
                                <Text size="xs" c="dimmed">Site Seq</Text>
                                <Text fw={700}>{selectedSite?.seq ?? "-"}</Text>
                            </Paper>
                            <Paper withBorder radius="md" p="sm">
                                <Text size="xs" c="dimmed">이름</Text>
                                <Text fw={700}>{selectedSite?.name ?? "-"}</Text>
                            </Paper>
                            <Paper withBorder radius="md" p="sm">
                                <Text size="xs" c="dimmed">도메인 수</Text>
                                <Text fw={700}>{selectedSite?.domains?.length ?? 0}</Text>
                            </Paper>
                            <Paper withBorder radius="md" p="sm">
                                <Text size="xs" c="dimmed">페이지 목록 캐시</Text>
                                <Text fw={700}>{sitePageNames.length.toLocaleString()}</Text>
                            </Paper>
                        </SimpleGrid>
                    </Stack>
                </Card>
                {page === "site-detail" ? (
                <Card withBorder radius="md" padding="lg">
                    <Group justify="space-between" mb="md">
                        <Title order={3}>Admin Page 목록</Title>
                        <Badge color="indigo" variant="light">{adminPageMetaCount} rows</Badge>
                    </Group>
                    <Group mb="md">
                        <TextInput label="search" value={adminPageMetaSearchInput} onChange={(event) => setAdminPageMetaSearchInput(event.currentTarget.value)} placeholder="page name, image"/>
                        <Button mt={22} variant="filled" onClick={() => {
                            setAdminPageMetaPage(1);
                            setAdminPageMetaSearch(adminPageMetaSearchInput);
                            loadAdminPageMetaList({siteSeq: selectedSiteSeq, page: 1, pageSize: ADMIN_PAGE_META_PAGE_SIZE, search: adminPageMetaSearchInput, sortBy: adminPageMetaSortBy, sortOrder: adminPageMetaSortOrder});
                        }}>검색</Button>
                    </Group>
                    <DataTable
                        withTableBorder
                        borderRadius="md"
                        striped
                        highlightOnHover
                        records={adminPageMetaRows}
                        columns={[
                            {accessor: "name", title: "Page", sortable: true},
                            {accessor: "revision", title: "Revision", sortable: true},
                            {
                                accessor: "image",
                                title: "Image",
                                render: (row) => row.image ? (
                                    <Anchor href={row.image} target="_blank" rel="noopener">
                                        <Image
                                            src={row.image}
                                            alt={`${row.name} image`}
                                            h={44}
                                            w={72}
                                            fit="cover"
                                            radius="sm"
                                            fallbackSrc="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw=="
                                        />
                                    </Anchor>
                                ) : "-",
                            },
                            {accessor: "dateUpdated", title: "Date Updated", sortable: true, render: (row) => formatDateTimeInClientTimezone(row.dateUpdated)},
                            {accessor: "datePageLastChanged", title: "Last Changed", sortable: true, render: (row) => formatDateTimeInClientTimezone(row.datePageLastChanged)},
                            {accessor: "size", title: "Size", sortable: true},
                            {
                                accessor: "actions",
                                title: "Actions",
                                render: (row) => (
                                    <Button
                                        size="xs"
                                        variant="light"
                                        color="teal"
                                        disabled={!selectedSiteSeq}
                                        loading={selectedSite ? calculatingSiteSeq === selectedSite.seq : false}
                                        onClick={async () => {
                                            if (!selectedSiteSeq) {
                                                return;
                                            }
                                            const response = await runSiteCalculate(selectedSiteSeq, row.name);
                                            if (!response) {
                                                return;
                                            }
                                            setSiteCalculateMessage(`선택 페이지: ${response?.pageName ?? row.name} (queued)`);
                                        }}
                                    >
                                        재계산
                                    </Button>
                                ),
                            },
                        ]}
                        sortStatus={{columnAccessor: adminPageMetaSortBy, direction: adminPageMetaSortOrder}}
                        onSortStatusChange={(nextSortStatus) => {
                            const nextDirection = nextSortStatus.direction ?? "desc";
                            setAdminPageMetaPage(1);
                            setAdminPageMetaSortBy(nextSortStatus.columnAccessor);
                            setAdminPageMetaSortOrder(nextDirection);
                            loadAdminPageMetaList({siteSeq: selectedSiteSeq, page: 1, pageSize: ADMIN_PAGE_META_PAGE_SIZE, search: adminPageMetaSearch, sortBy: nextSortStatus.columnAccessor, sortOrder: nextDirection});
                        }}
                        totalRecords={adminPageMetaCount}
                        recordsPerPage={ADMIN_PAGE_META_PAGE_SIZE}
                        page={adminPageMetaPage}
                        onPageChange={(nextPage) => {
                            setAdminPageMetaPage(nextPage);
                            loadAdminPageMetaList({siteSeq: selectedSiteSeq, page: nextPage, pageSize: ADMIN_PAGE_META_PAGE_SIZE, search: adminPageMetaSearch, sortBy: adminPageMetaSortBy, sortOrder: adminPageMetaSortOrder});
                        }}
                        paginationText={({from, to, totalRecords}) => `${from}-${to} / ${totalRecords}`}
                        minHeight={320}
                    />
                    <Text size="xs" c="dimmed" mt="xs">Page {adminPageMetaPage} / {adminPageMetaTotalPages}</Text>
                </Card>
                ) : null}
                {page === "site-permission" ? (
                <Card withBorder radius="md" padding="lg">
                    <Group justify="space-between" mb="md">
                        <Title order={3}>Permission</Title>
                        <Badge color="grape" variant="light">{permissionRows.length} rows</Badge>
                    </Group>
                    <SimpleGrid cols={{base: 1, md: 5}} spacing="sm" mb="sm">
                        <Select label="targetType" data={PERMISSION_TARGET_TYPE_OPTIONS} value={permissionForm.targetType} onChange={(value) => setPermissionForm({...permissionForm, targetType: value ?? "All", target: value === "All" ? "" : permissionForm.target})}/>
                        <TextInput label="target" value={permissionForm.target} onChange={(event) => setPermissionForm({...permissionForm, target: event.currentTarget.value})} placeholder="page name or prefix"/>
                        <Select label="actorType" data={PERMISSION_ACTOR_TYPE_OPTIONS} value={permissionForm.actorType} onChange={(value) => setPermissionForm({...permissionForm, actorType: value ?? "All", actor: value === "All" || value === "Login" ? "" : permissionForm.actor})}/>
                        <TextInput label="actor" value={permissionForm.actor} onChange={(event) => setPermissionForm({...permissionForm, actor: event.currentTarget.value})} placeholder="email or @domain"/>
                        <Select label="action" data={PERMISSION_ACTION_OPTIONS} value={permissionForm.action} onChange={(value) => setPermissionForm({...permissionForm, action: value ?? "read"})}/>
                    </SimpleGrid>
                    <Group mb="md">
                        <Button
                            size="xs"
                            loading={savingPermission}
                            onClick={async () => {
                                const saved = await savePermission(selectedSiteSeq, permissionForm);
                                if (saved) {
                                    setPermissionForm({targetType: "All", target: "", actorType: "All", actor: "", action: "read"});
                                }
                            }}
                        >
                            Save
                        </Button>
                    </Group>
                    <SimpleGrid cols={{base: 1, md: 4}} spacing="sm" mb="sm">
                        <TextInput label="pageName" value={permissionDiagnoseForm.pageName} onChange={(event) => setPermissionDiagnoseForm({...permissionDiagnoseForm, pageName: event.currentTarget.value})}/>
                        <TextInput label="actor" value={permissionDiagnoseForm.actor} onChange={(event) => setPermissionDiagnoseForm({...permissionDiagnoseForm, actor: event.currentTarget.value})} placeholder="empty means anonymous"/>
                        <Select label="action" data={PERMISSION_ACTION_OPTIONS} value={permissionDiagnoseForm.action} onChange={(value) => setPermissionDiagnoseForm({...permissionDiagnoseForm, action: value ?? "read"})}/>
                        <Button mt={22} variant="light" onClick={() => diagnosePermission(selectedSiteSeq, permissionDiagnoseForm.pageName, permissionDiagnoseForm.actor, permissionDiagnoseForm.action)}>
                            Diagnose
                        </Button>
                    </SimpleGrid>
                    {permissionDiagnose ? (
                        <Paper withBorder radius="sm" p="sm" mb="md">
                            <Group gap="xs">
                                <Badge color={permissionDiagnose.permitted ? "green" : "red"} variant="light">{permissionDiagnose.permitted ? "allowed" : "denied"}</Badge>
                                <Text size="sm">{permissionDiagnose.matchedPermission ? `${permissionDiagnose.matchedPermission.targetType}/${permissionDiagnose.matchedPermission.actorType}/${permissionDiagnose.matchedPermission.actionName}` : "No matching row"}</Text>
                            </Group>
                        </Paper>
                    ) : null}
                    <DataTable
                        withTableBorder
                        borderRadius="md"
                        striped
                        highlightOnHover
                        records={permissionRows}
                        columns={[
                            {accessor: "targetType", title: "Target Type"},
                            {accessor: "target", title: "Target", render: (row) => row.target || "*"},
                            {accessor: "actorType", title: "Actor Type"},
                            {accessor: "actor", title: "Actor", render: (row) => row.actor || "*"},
                            {accessor: "actionName", title: "Action", render: (row) => `${row.actionName} (${row.action})`},
                            {accessor: "specificity", title: "Specificity"},
                            {
                                accessor: "actions",
                                title: "Actions",
                                render: (row) => {
                                    const key = `${row.targetType}:${row.target}:${row.actorType}:${row.actor}`;
                                    return (
                                        <Group gap={6}>
                                            <Button size="xs" variant="light" onClick={() => setPermissionForm({targetType: row.targetType, target: row.target, actorType: row.actorType, actor: row.actor, action: row.actionName})}>
                                                Edit
                                            </Button>
                                            <Button size="xs" variant="light" color="red" loading={deletingPermissionKey === key} onClick={() => deletePermission(selectedSiteSeq, row)}>
                                                Delete
                                            </Button>
                                        </Group>
                                    );
                                },
                            },
                        ]}
                        minHeight={220}
                    />
                </Card>
                ) : null}
                {isSiteConfigPage ? (
                    <>
                        <SimpleGrid cols={{base: 1, xl: 2}} spacing="lg">
                    <Card withBorder radius="md" padding="lg">
                        <Group justify="space-between" mb="md">
                            <Title order={3}>Favicon</Title>
                            <Badge color="blue" variant="light">브랜딩</Badge>
                        </Group>
                        <Text size="sm" c="dimmed" mb="md">
                            선택한 사이트의 favicon을 관리자 업로드로 교체합니다. 업로드 후 바로 반영됩니다.
                        </Text>
                        <Group align="flex-start" grow mb="md">
                            <Stack gap={6}>
                                <Text size="sm" fw={600}>현재 favicon</Text>
                                <img
                                    src={siteFaviconUrl}
                                    alt="Current favicon"
                                    style={{width: 32, height: 32, borderRadius: 6, border: "1px solid #e5e7eb"}}
                                />
                                <Text size="xs" c="dimmed" style={{wordBreak: "break-all"}}>
                                    {siteFaviconObjectKey || "/public/favicon.png"}
                                </Text>
                                <Anchor size="xs" href={siteFaviconUrl} target="_blank" rel="noopener">새 탭으로 보기</Anchor>
                            </Stack>
                            <Stack gap={8}>
                                <Text size="sm" fw={600}>새 favicon 업로드</Text>
                                <input
                                    type="file"
                                    accept="image/*,.ico"
                                    onChange={(event) => {
                                        const selected = event.currentTarget.files?.[0] ?? null;
                                        setFaviconFile(selected);
                                    }}
                                />
                                <Group>
                                    <Button
                                        variant="filled"
                                        loading={uploadingFavicon}
                                        disabled={!faviconFile || !selectedSiteSeq}
                                        onClick={async () => {
                                            await uploadSiteFavicon(faviconFile, selectedSiteSeq);
                                            await loadSiteFavicon(selectedSiteSeq);
                                        }}
                                    >
                                        Upload favicon
                                    </Button>
                                    <Button variant="light" disabled={!selectedSiteSeq} onClick={() => loadSiteFavicon(selectedSiteSeq)}>
                                        Refresh
                                    </Button>
                                    <Button
                                        color="red"
                                        variant="light"
                                        loading={deletingFavicon}
                                        disabled={!selectedSiteSeq}
                                        onClick={async () => {
                                            await resetSiteFavicon(selectedSiteSeq);
                                            await loadSiteFavicon(selectedSiteSeq);
                                            setFaviconFile(null);
                                        }}
                                    >
                                        Reset to default
                                    </Button>
                                </Group>
                                <Text size="xs" c="dimmed">
                                    권장: 32x32 또는 48x48 PNG/ICO
                                </Text>
                            </Stack>
                        </Group>
                    </Card>
                    {!isSiteConfigPage ? (
                    <Card withBorder radius="md" padding="lg">
                        <Group justify="space-between" mb="md">
                            <Title order={3}>Operation · Site Calculate Operation</Title>
                            <Badge color="teal" variant="light">운영 작업</Badge>
                        </Group>
                        <Text size="sm" c="dimmed" mb="md">
                            현재 사이트에서 1개 페이지만 Calculate 큐에 넣습니다. 페이지를 비워두면 랜덤 1개, 입력하면 해당 페이지를 Calculate합니다.
                        </Text>
                        <Stack gap="sm">
                            <TextInput
                                label="Calculate할 페이지 (자동완성)"
                                placeholder="비워두면 랜덤 1개"
                                value={selectedCalculatePageName}
                                onChange={(event) => setSelectedCalculatePageName(event.currentTarget.value)}
                                list="site-calculate-page-name-list"
                                disabled={!selectedSiteSeq}
                            />
                            <datalist id="site-calculate-page-name-list">
                                {sitePageNames.map((pageName) => (
                                    <option key={`calculate-page-${pageName}`} value={pageName}/>
                                ))}
                            </datalist>
                            <Group>
                                <Button
                                    variant="filled"
                                    color="teal"
                                    disabled={!selectedSiteSeq}
                                    loading={selectedSite ? calculatingSiteSeq === selectedSite.seq : false}
                                    onClick={async () => {
                                        if (!selectedSiteSeq) {
                                            return;
                                        }
                                        const response = await runSiteCalculate(selectedSiteSeq, selectedCalculatePageName);
                                        if (!response) {
                                            return;
                                        }
                                        const modeLabel = response?.source === "selected" ? "선택 페이지" : "랜덤 페이지";
                                        setSiteCalculateMessage(`${modeLabel}: ${response?.pageName ?? "-"} (queued)`);
                                    }}
                                >
                                    Calculate 1 page
                                </Button>
                                <Button
                                    variant="light"
                                    disabled={!selectedSiteSeq}
                                    onClick={async () => {
                                        if (!selectedSiteSeq) {
                                            return;
                                        }
                                        const pageNames = await loadAdminSitePageNames(selectedSiteSeq);
                                        setSitePageNames(pageNames);
                                    }}
                                >
                                    페이지 목록 새로고침
                                </Button>
                            </Group>
                            {siteCalculateMessage ? (
                                <Text size="sm" c="teal">{siteCalculateMessage}</Text>
                            ) : null}
                            <Text size="xs" c="dimmed">
                                페이지 이름 목록은 사이트 캐시를 사용합니다. (count: {sitePageNames.length})
                            </Text>
                        </Stack>
                    </Card>
                    ) : null}
                </SimpleGrid>
                <Card withBorder radius="md" padding="lg">
                    <Group justify="space-between" mb="md">
                        <Title order={3}>Theme</Title>
                        <Badge color="grape" variant="light">디자인</Badge>
                    </Group>
                    <Text size="sm" c="dimmed" mb="md">
                        사이트별 헤더/푸터 배경색·전경색을 16진수(#RGB, #RRGGBB, #RRGGBBAA)로 지정할 수 있습니다. 비워두면 Header/Footer 기본값(배경 #EEEEEE, 전경 #000000), Body 기본값(배경 #FFFFFF, 전경 #000000)을 사용합니다.
                    </Text>
                    <SimpleGrid cols={{base: 1, lg: 2}} spacing="md">
                        <SimpleGrid cols={{base: 1, sm: 2}} spacing="md">
                            <ColorInput
                                label="Header 배경색"
                                placeholder="#EEEEEE"
                                format="hexa"
                                value={siteTheme.headerBackgroundColor}
                                onChange={(value) => setSiteTheme((prev) => ({...prev, headerBackgroundColor: value}))}
                                swatches={["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#6c5ce7", "#0b7285"]}
                                withEyeDropper={false}
                                clearable
                            />
                            <ColorInput
                                label="Header 전경색"
                                placeholder="#000000"
                                format="hexa"
                                value={siteTheme.headerForegroundColor}
                                onChange={(value) => setSiteTheme((prev) => ({...prev, headerForegroundColor: value}))}
                                swatches={["#111111", "#212529", "#495057", "#ffffff", "#f1f3f5", "#ffd43b"]}
                                withEyeDropper={false}
                                clearable
                            />
                            <ColorInput
                                label="Body 배경색"
                                placeholder="#FFFFFF"
                                format="hexa"
                                value={siteTheme.bodyBackgroundColor}
                                onChange={(value) => setSiteTheme((prev) => ({...prev, bodyBackgroundColor: value}))}
                                swatches={["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#f1f3f5", "#e9ecef"]}
                                withEyeDropper={false}
                                clearable
                            />
                            <ColorInput
                                label="Body 전경색"
                                placeholder="#000000"
                                format="hexa"
                                value={siteTheme.bodyForegroundColor}
                                onChange={(value) => setSiteTheme((prev) => ({...prev, bodyForegroundColor: value}))}
                                swatches={["#111111", "#212529", "#495057", "#000000", "#343a40", "#ffffff"]}
                                withEyeDropper={false}
                                clearable
                            />
                            <ColorInput
                                label="Footer 배경색"
                                placeholder="#EEEEEE"
                                format="hexa"
                                value={siteTheme.footerBackgroundColor}
                                onChange={(value) => setSiteTheme((prev) => ({...prev, footerBackgroundColor: value}))}
                                swatches={["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#2b8a3e", "#862e9c"]}
                                withEyeDropper={false}
                                clearable
                            />
                            <ColorInput
                                label="Footer 전경색"
                                placeholder="#000000"
                                format="hexa"
                                value={siteTheme.footerForegroundColor}
                                onChange={(value) => setSiteTheme((prev) => ({...prev, footerForegroundColor: value}))}
                                swatches={["#111111", "#212529", "#495057", "#ffffff", "#f1f3f5", "#ff922b"]}
                                withEyeDropper={false}
                                clearable
                            />
                        </SimpleGrid>
                        <Paper withBorder radius="md" p="md" style={{overflow: "hidden"}}>
                            <Text size="sm" fw={600} mb={8}>미리보기</Text>
                            <Stack gap={0} style={{borderRadius: 10, overflow: "hidden", border: "1px solid #e9ecef"}}>
                                <div
                                    style={{
                                        backgroundColor: siteTheme.headerBackgroundColor || "#EEEEEE",
                                        color: siteTheme.headerForegroundColor || "#000000",
                                        padding: "12px 14px",
                                        fontWeight: 600,
                                    }}
                                >
                                    Header Preview
                                </div>
                                <div style={{padding: "16px 14px", backgroundColor: siteTheme.bodyBackgroundColor || "#FFFFFF", color: siteTheme.bodyForegroundColor || "#000000"}}>
                                    콘텐츠 영역 (고정 미리보기)
                                </div>
                                <div
                                    style={{
                                        backgroundColor: siteTheme.footerBackgroundColor || "#EEEEEE",
                                        color: siteTheme.footerForegroundColor || "#000000",
                                        padding: "12px 14px",
                                        fontWeight: 600,
                                    }}
                                >
                                    Footer Preview
                                </div>
                            </Stack>
                        </Paper>
                    </SimpleGrid>
                    <Group mt="md">
                        <Button
                            variant="filled"
                            color="grape"
                            loading={savingSiteTheme}
                            disabled={!selectedSiteSeq}
                            onClick={async () => {
                                await saveSiteTheme(selectedSiteSeq, siteTheme);
                                await loadSiteTheme(selectedSiteSeq);
                            }}
                        >
                            Save theme
                        </Button>
                        <Button variant="light" disabled={!selectedSiteSeq} onClick={() => loadSiteTheme(selectedSiteSeq)}>
                            Refresh
                        </Button>
                        <Button
                            color="gray"
                            variant="light"
                            disabled={!selectedSiteSeq}
                            onClick={async () => {
                                const emptyTheme = {
                                    headerBackgroundColor: "",
                                    headerForegroundColor: "",
                                    bodyBackgroundColor: "",
                                    bodyForegroundColor: "",
                                    footerBackgroundColor: "",
                                    footerForegroundColor: "",
                                };
                                setSiteTheme(emptyTheme);
                                await saveSiteTheme(selectedSiteSeq, emptyTheme);
                                await loadSiteTheme(selectedSiteSeq);
                            }}
                        >
                            Reset
                        </Button>
                    </Group>
                </Card>
                    </>
                ) : null}
                {page === "site-cache" ? (
                    <>
                        <Card withBorder radius="md" padding="lg">
                            <Group justify="space-between" mb="md">
                                <Title order={3}>Operation · Site Cache Operation</Title>
                                <Badge color="orange" variant="light">유지보수</Badge>
                            </Group>
                            <Text size="sm" c="dimmed" mb="md">
                                현재 보고 있는 사이트의 캐시를 즉시 초기화합니다. 도메인/페이지/헤더 캐시가 강제로 갱신됩니다.
                            </Text>
                            <Group justify="space-between" align="center">
                                <Stack gap={2}>
                                    <Text size="xs" c="dimmed">대상 사이트</Text>
                                    <Text fw={700}>
                                        {selectedSite ? `${selectedSite.name} (#${selectedSite.seq})` : "선택된 사이트 없음"}
                                    </Text>
                                </Stack>
                                <Button
                                    color="orange"
                                    variant="filled"
                                    disabled={!selectedSite}
                                    loading={selectedSite ? clearingSiteSeq === selectedSite.seq : false}
                                    onClick={() => {
                                        if (!selectedSite) {
                                            return;
                                        }
                                        clearSiteCache(selectedSite.seq);
                                    }}
                                >
                                    Clear current site cache
                                </Button>
                            </Group>
                        </Card>
                        <Card withBorder radius="md" padding="lg">
                            <Group justify="space-between" mb="md">
                                <Title order={3}>Memory Cache Status (All Instances)</Title>
                                <Button variant="light" onClick={loadMemoryCacheStats}>Refresh</Button>
                            </Group>
                            <Table striped highlightOnHover withTableBorder withColumnBorders>
                                <Table.Thead>
                                    <Table.Tr>
                                        <Table.Th>Port</Table.Th>
                                        <Table.Th>Key Count</Table.Th>
                                        <Table.Th>Value Count</Table.Th>
                                        <Table.Th>Captured At</Table.Th>
                                    </Table.Tr>
                                </Table.Thead>
                                <Table.Tbody>
                                    {memoryCacheStats.map((row) => (
                                        <Table.Tr key={String(row.instancePort)}>
                                            <Table.Td>{row.instancePort}</Table.Td>
                                            <Table.Td>{row.stats?.linksCacheKeyCount ?? 0}</Table.Td>
                                            <Table.Td>{row.stats?.linksCacheValueCount ?? 0}</Table.Td>
                                            <Table.Td>{row.stats?.capturedAtIso8601 ?? "-"}</Table.Td>
                                        </Table.Tr>
                                    ))}
                                </Table.Tbody>
                            </Table>
                        </Card>
                    </>
                ) : null}
            </Stack>
        );
    }

    if (page === "s3-browser") {
        const fileRows = Array.isArray(s3Items) ? s3Items.filter((item) => !item.isDirectory) : [];
        const root = {children: {}};
        fileRows.forEach((item) => {
            const parts = String(item.key || "").split("/").filter(Boolean);
            if (parts.length === 0) return;
            let node = root;
            parts.forEach((part, index) => {
                const currentPath = parts.slice(0, index + 1).join("/");
                if (!node.children[part]) {
                    node.children[part] = {name: part, path: currentPath, isFile: index === parts.length - 1, children: {}, meta: null};
                }
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
                rows.push({depth, node: child});
                if (!child.isFile && expandedS3Nodes[child.path]) {
                    visit(child, depth + 1);
                }
            });
        };
        visit(root, 0);
        return <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>S3 파일 브라우저</Title>
                <Badge color="red" variant="light">Admin Only</Badge>
            </Group>
            <Group align="flex-end" mb="md">
                <Button loading={loadingS3} onClick={() => loadS3Objects()} leftSection={<i className="fas fa-sync-alt" aria-hidden="true"/>}>새로고침</Button>
                <Button variant="light" onClick={expandAllS3Nodes} leftSection={<i className="fas fa-angle-double-down" aria-hidden="true"/>}>모두 펼치기</Button>
                <Button color="red" variant="light" disabled={selectedS3Keys.length === 0} loading={deletingS3} onClick={deleteS3Selected} leftSection={<i className="fas fa-trash-alt" aria-hidden="true"/>}>선택 삭제 ({selectedS3Keys.length})</Button>
            </Group>
            <Table striped highlightOnHover withTableBorder withColumnBorders>
                <Table.Thead><Table.Tr><Table.Th><input type="checkbox" checked={selectedS3Keys.length > 0 && selectedS3Keys.length === fileRows.length} onChange={(event) => {
                    if (event.currentTarget.checked) {
                        setSelectedS3Keys(fileRows.map((item) => item.key));
                    } else {
                        setSelectedS3Keys([]);
                    }
                }}/></Table.Th><Table.Th>Key</Table.Th><Table.Th style={{textAlign: "right"}}>Size(bytes)</Table.Th><Table.Th style={{textAlign: "center"}}>Last Modified</Table.Th><Table.Th style={{textAlign: "center"}}>Action</Table.Th></Table.Tr></Table.Thead>
                <Table.Tbody>
                    {rows.map(({depth, node}) => {
                        const key = node.path;
                        const checked = selectedS3Keys.includes(key);
                        return <Table.Tr key={key}>
                            <Table.Td><input type="checkbox" disabled={!node.isFile} checked={checked} onChange={(event) => {
                                if (event.currentTarget.checked) setSelectedS3Keys((prev) => [...prev, key]);
                                else setSelectedS3Keys((prev) => prev.filter((selectedKey) => selectedKey !== key));
                            }}/></Table.Td>
                            <Table.Td>
                                <div style={{display: "flex", alignItems: "center", gap: 8, paddingLeft: `${depth * 22}px`}}>
                                    {!node.isFile ? <Button size="compact-xs" variant="subtle" onClick={() => toggleS3Node(node.path)}><i className={`fas ${expandedS3Nodes[node.path] ? "fa-chevron-down" : "fa-chevron-right"}`} aria-hidden="true"/></Button> : <span style={{display: "inline-block", width: 20}}/>}
                                    <span><i className={`fas ${node.isFile ? "fa-file-alt" : "fa-folder"}`} aria-hidden="true"/></span>
                                    <span>{node.name}</span>
                                </div>
                            </Table.Td>
                            <Table.Td style={{textAlign: "right"}}>{node.isFile ? Number(node.meta?.size ?? 0).toLocaleString() : "-"}</Table.Td><Table.Td style={{textAlign: "center"}}>{node.isFile ? formatDateTimeInClientTimezone(node.meta?.lastModified) : "-"}</Table.Td>
                            <Table.Td style={{textAlign: "center"}}>{node.isFile ? <Button size="xs" variant="light" onClick={() => downloadS3Object(key)} leftSection={<i className="fas fa-download" aria-hidden="true"/>}>다운로드</Button> : "-"}</Table.Td>
                        </Table.Tr>;
                    })}
                </Table.Tbody>
            </Table>
        </Card>;
    }
    if (page === "crawler-cache") {
        return <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>Crawler Cache</Title>
                <Badge color="lime" variant="light">{crawlerCacheCount} rows</Badge>
            </Group>
            <Text size="sm" c="dimmed" mb="md">URL별 크롤링 캐시 조회/삭제/강제갱신을 수행합니다.</Text>
            <Group align="flex-end" mb="md">
                <Button variant="filled" onClick={() => loadCrawlerCaches({page: crawlerPage, pageSize: CRAWLER_CACHE_PAGE_SIZE, search: crawlerSearch, sortBy: crawlerSortBy, sortOrder: crawlerSortOrder})}>조회</Button>
            </Group>
            <Group mb="sm" align="end">
                <TextInput label="search" value={crawlerSearchInput} onChange={(event) => setCrawlerSearchInput(event.currentTarget.value)} placeholder="url, title, image, description"/>
                <Button variant="light" onClick={() => {
                    setCrawlerPage(1);
                    setCrawlerSearch(crawlerSearchInput);
                    loadCrawlerCaches({page: 1, pageSize: CRAWLER_CACHE_PAGE_SIZE, search: crawlerSearchInput, sortBy: crawlerSortBy, sortOrder: crawlerSortOrder});
                }}>검색</Button>
            </Group>
            <DataTable
                sortIcons={{sorted: <IconChevronUp size={14} />, unsorted: <IconSelector size={14} />}}
                withTableBorder
                striped
                highlightOnHover
                records={crawlerCaches}
                columns={[
                    {accessor: "id", title: "ID", sortable: true, render: (row) => row.id},
                    {accessor: "url", title: "URL", sortable: true, render: (row) => <Text size="sm" style={{wordBreak: "break-all"}}>{row.url}</Text>},
                    {accessor: "status", title: "Status", sortable: true, render: (row) => formatCrawlerStatus(row.status)},
                    {accessor: "title", title: "Title", sortable: true, render: (row) => row.title || "-"},
                    {
                        accessor: "image",
                        title: "Image",
                        sortable: true,
                        render: (row) => {
                            if (!row.image) return "-";
                            return <Anchor href={row.image} target="_blank" rel="noreferrer">
                                <Image
                                    src={row.image}
                                    alt={row.title || row.url || "crawler image"}
                                    h={44}
                                    w={72}
                                    fit="cover"
                                    radius="sm"
                                    fallbackSrc="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw=="
                                />
                            </Anchor>;
                        },
                    },
                    {accessor: "description", title: "Description", sortable: true, render: (row) => row.description || "-"},
                    {accessor: "dateUpdated", title: "Updated", sortable: true, render: (row) => formatDateTimeInClientTimezone(row.dateUpdated)},
                    {accessor: "action", title: "Action", render: (row) => <Group gap={6}><Button size="xs" variant="light" loading={refreshingCrawlerUrl === row.url} onClick={() => refreshCrawlerCache(row.url)}>갱신</Button><Button size="xs" color="red" variant="light" loading={deletingCrawlerUrl === row.url} onClick={() => deleteCrawlerCache(row.url)}>삭제</Button></Group>},
                ]}
                sortStatus={{columnAccessor: crawlerSortBy, direction: crawlerSortOrder}}
                onSortStatusChange={(nextSortStatus) => {
                    setCrawlerPage(1);
                    setCrawlerSortBy(nextSortStatus.columnAccessor);
                    setCrawlerSortOrder(nextSortStatus.direction ?? "desc");
                    loadCrawlerCaches({
                        page: 1,
                        pageSize: CRAWLER_CACHE_PAGE_SIZE,
                        search: crawlerSearch,
                        sortBy: nextSortStatus.columnAccessor,
                        sortOrder: nextSortStatus.direction ?? "desc",
                    });
                }}
                totalRecords={crawlerCacheCount}
                recordsPerPage={CRAWLER_CACHE_PAGE_SIZE}
                page={normalizedCrawlerPage}
                onPageChange={(nextPage) => {
                    setCrawlerPage(nextPage);
                    loadCrawlerCaches({page: nextPage, pageSize: CRAWLER_CACHE_PAGE_SIZE, search: crawlerSearch, sortBy: crawlerSortBy, sortOrder: crawlerSortOrder});
                }}
                paginationText={({from, to, totalRecords}) => `${from}-${to} / ${totalRecords}`}
                minHeight={380}
            />
        </Card>;
    }

    if (page === "recent-changes") {
        return (
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>Recent Changes (All Sites)</Title>
                    <Badge color="violet" variant="light">{recentChanges.length} rows</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">
                    사이트 전체 최근 변경 기록을 n개 단위로 조회할 수 있습니다.
                </Text>
                <Group align="flex-end" mb="md">
                    <TextInput
                        label="조회 개수 n"
                        value={recentChangeLimitInput}
                        onChange={(event) => setRecentChangeLimitInput(event.currentTarget.value)}
                        placeholder="1 ~ 500"
                    />
                    <Button
                        variant="filled"
                        onClick={() => {
                            const parsed = Number.parseInt(recentChangeLimitInput, 10);
                            const n = Number.isFinite(parsed) ? Math.min(500, Math.max(1, parsed)) : 50;
                            setRecentChangeLimitInput(String(n));
                            loadRecentChanges(n);
                        }}
                    >
                        조회
                    </Button>
                </Group>
                {makeTable(
                    ["When", "Site", "Page", "Revision", "Editor", "Comment", "IP"],
                    recentChanges.map((row) => {
                        const siteUrl = resolveSiteUrl(row);
                        const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(row.name)}` : "";
                        const revisionUrl = pageUrl ? `${pageUrl}?rev=${row.revision}` : "";
                        const editorUrl = row.nickname ? `/Admin/User?query=${encodeURIComponent(row.nickname)}` : "";
                        return [
                            row.dateTime,
                            siteUrl ? <Anchor href={siteUrl} target="_blank">{row.siteName} (#{row.siteSeq})</Anchor> : `${row.siteName} (#${row.siteSeq})`,
                            pageUrl ? <Anchor href={pageUrl} target="_blank">{row.name}</Anchor> : row.name,
                            revisionUrl ? <Anchor href={revisionUrl} target="_blank">{row.revision}</Anchor> : row.revision,
                            editorUrl ? <Anchor href={editorUrl}>{row.nickname}</Anchor> : (row.nickname ?? "-"),
                            row.comment || "-",
                            row.remoteAddress,
                        ];
                    }),
                )}
            </Card>
        );
    }
    if (page === "access-logs") {
        return (
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>
                        {selectedAccessLogSite ? `Access Logs (${selectedAccessLogSite.name} #${selectedAccessLogSite.seq})` : "Access Logs (All Sites)"}
                    </Title>
                    <Badge color="cyan" variant="light">{accessLogs.length} / {accessLogCount} rows</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">검색과 헤더 정렬, 페이지 이동으로 요청 로그를 조회할 수 있습니다.</Text>
                <Group align="flex-end" mb="md">
                    <TextInput
                        label="search"
                        value={accessLogSearchInput}
                        onChange={(event) => setAccessLogSearchInput(event.currentTarget.value)}
                        placeholder="site/method/uri/ip/user-agent"
                    />
                    <Button
                        variant="filled"
                        onClick={() => {
                            setAccessLogPage(1);
                            loadAccessLogs({
                                page: 1,
                                pageSize: ACCESS_LOG_PAGE_SIZE,
                                search: accessLogSearchInput,
                                sortBy: accessLogSortBy,
                                sortOrder: accessLogSortOrder,
                                siteSeq: selectedAccessLogSiteSeq,
                            });
                        }}
                    >
                        조회
                    </Button>
                </Group>
                <DataTable
                    sortIcons={{
                        sorted: <IconChevronUp size={14} />,
                        unsorted: <IconSelector size={14} />,
                    }}
                    withTableBorder
                    borderRadius="sm"
                    striped
                    highlightOnHover
                    minHeight={420}
                    records={Array.isArray(accessLogs) ? accessLogs : []}
                    idAccessor="seq"
                    columns={[
                        {accessor: "seq", title: "seq", sortable: true},
                        {accessor: "dateInserted", title: "DateInserted", sortable: true},
                        {accessor: "siteName", title: "Site", render: (row) => `${row.siteName} (#${row.siteSeq})`},
                        {accessor: "ipDenySeq", title: "IpDeny", render: (row) => row.ipDenySeq ?? "-"},
                        {accessor: "userSeq", title: "User", render: (row) => row.userSeq ?? "-"},
                        {accessor: "method", title: "Method", sortable: true},
                        {
                            accessor: "uri",
                            title: "Request URL",
                            sortable: true,
                            render: (row) => {
                                const href = `${row.scheme}://${row.host}${row.uri}`;
                                return <Anchor href={href} target="_blank" rel="noopener noreferrer">{href}</Anchor>
                            },
                        },
                        {accessor: "status", title: "Status", sortable: true},
                        {accessor: "remoteAddress", title: "IP"},
                        {accessor: "durationMilli", title: "Duration(ms)", sortable: true},
                        {accessor: "userAgent", title: "User-Agent"},
                    ]}
                    sortStatus={{columnAccessor: accessLogSortBy, direction: accessLogSortOrder}}
                    onSortStatusChange={(nextSortStatus) => {
                        const nextDirection = nextSortStatus.direction ?? "desc";
                        setAccessLogPage(1);
                        setAccessLogSortBy(nextSortStatus.columnAccessor);
                        setAccessLogSortOrder(nextDirection);
                        loadAccessLogs({
                            page: 1,
                            pageSize: ACCESS_LOG_PAGE_SIZE,
                            search: accessLogSearchInput,
                            sortBy: nextSortStatus.columnAccessor,
                            sortOrder: nextDirection,
                            siteSeq: selectedAccessLogSiteSeq,
                        });
                    }}
                />
                <Group mt="md" justify="space-between">
                    <Text size="sm" c="dimmed">Page {accessLogPage} / {accessLogTotalPages}</Text>
                    <Pagination
                        value={accessLogPage}
                        onChange={(nextPage) => {
                            setAccessLogPage(nextPage);
                            loadAccessLogs({page: nextPage, pageSize: ACCESS_LOG_PAGE_SIZE, search: accessLogSearchInput, sortBy: accessLogSortBy, sortOrder: accessLogSortOrder, siteSeq: selectedAccessLogSiteSeq});
                        }}
                        total={accessLogTotalPages}
                        siblings={1}
                        boundaries={1}
                        size="sm"
                    />
                </Group>
            </Card>
        );
    }

    return (
        <Stack gap="lg">
            <SimpleGrid cols={{base: 1, sm: 2, lg: 4}} spacing="md">
                <Card withBorder radius="md" padding="md">
                    <Group justify="space-between" align="flex-start">
                        <Stack gap={2}>
                            <Text size="sm" c="dimmed">Sites</Text>
                            <Title order={2}>{sites.length}</Title>
                        </Stack>
                        <ThemeIcon color="indigo" variant="light" radius="xl">S</ThemeIcon>
                    </Group>
                </Card>
                <Card withBorder radius="md" padding="md">
                    <Group justify="space-between" align="flex-start">
                        <Stack gap={2}>
                            <Text size="sm" c="dimmed">All Users</Text>
                            <Title order={2}>{allUsers.length}</Title>
                        </Stack>
                        <ThemeIcon color="teal" variant="light" radius="xl">U</ThemeIcon>
                    </Group>
                </Card>
                <Card withBorder radius="md" padding="md">
                    <Group justify="space-between" align="flex-start">
                        <Stack gap={2}>
                            <Text size="sm" c="dimmed">30일 문서 수정</Text>
                            <Title order={2}>{dailyStats.pageEdited.reduce((sum, item) => sum + (item.count ?? 0), 0)}</Title>
                        </Stack>
                        <ThemeIcon color="grape" variant="light" radius="xl">E</ThemeIcon>
                    </Group>
                </Card>
            </SimpleGrid>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>빠른 이동</Title>
                    <Badge color="indigo" variant="light">Quick Access</Badge>
                </Group>
                <SimpleGrid cols={{base: 1, sm: 3}} spacing="sm">
                    <Button variant="light" onClick={() => onNavigate("/Admin/RecentChange")}>최근 변경 보기</Button>
                    <Button variant="light" onClick={() => onNavigate("/Admin/User")}>사용자 목록 보기</Button>
                </SimpleGrid>
            </Card>
            <SimpleGrid cols={{base: 1, sm: 2, lg: 4}} spacing="md">
                <StatTrendCard
                    title="New Users"
                    color="blue"
                    rows={dailyStats.userCreated}
                    total={dailyStats.userCreated.reduce((sum, item) => sum + (item.count ?? 0), 0)}
                />
                <StatTrendCard
                    title="Site User Joins"
                    color="teal"
                    rows={dailyStats.siteUserCreated}
                    total={dailyStats.siteUserCreated.reduce((sum, item) => sum + (item.count ?? 0), 0)}
                />
                <StatTrendCard
                    title="New Pages"
                    color="indigo"
                    rows={dailyStats.pageCreated}
                    total={dailyStats.pageCreated.reduce((sum, item) => sum + (item.count ?? 0), 0)}
                />
                <StatTrendCard
                    title="Page Edits"
                    color="grape"
                    rows={dailyStats.pageEdited}
                    total={dailyStats.pageEdited.reduce((sum, item) => sum + (item.count ?? 0), 0)}
                />
            </SimpleGrid>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>30일 운영 추이 차트</Title>
                    <Badge color="blue" variant="light">Chart</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">
                    신규 사용자, 사이트 가입, 문서 생성/수정 지표를 하나의 시계열 차트로 비교합니다.
                </Text>
                <MultiTrendChart
                    series={[
                        {name: "New Users", color: "blue", rows: dailyStats.userCreated},
                        {name: "Site User Joins", color: "teal", rows: dailyStats.siteUserCreated},
                        {name: "New Pages", color: "indigo", rows: dailyStats.pageCreated},
                        {name: "Page Edits", color: "grape", rows: dailyStats.pageEdited},
                    ]}
                />
            </Card>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>Most Viewed Pages</Title>
                    <Badge color="pink" variant="light">{Math.min(topViewedPages.length, 30)}</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">
                    로그인 여부와 상관없이 페이지 조회 누적 상위 문서 30개입니다.
                </Text>
                {makeTable(
                    ["Rank", "Site", "Page", "Views", "Last Viewed"],
                    topViewedPages.slice(0, 30).map((row, index) => {
                        const siteUrl = resolveSiteUrl(row);
                        const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(row.pageName)}` : "";
                        return [
                            index + 1,
                            siteUrl ? <Anchor href={siteUrl} target="_blank">{row.siteName} (#{row.siteSeq})</Anchor> : `${row.siteName} (#${row.siteSeq})`,
                            pageUrl ? <Anchor href={pageUrl} target="_blank">{row.pageName}</Anchor> : row.pageName,
                            row.viewCount,
                            row.lastViewedAt,
                        ];
                    }),
                )}
            </Card>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>Recent Changes (All Sites)</Title>
                    <Badge color="violet" variant="light">{recentChanges.length}</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">
                    전체 사이트 기준 최근 변경 30개입니다. 더 많이 보려면 왼쪽 메뉴 Recent Changes를 사용하세요.
                </Text>
                {makeTable(
                    ["When", "Site", "Page", "Revision", "Editor", "Comment"],
                    recentChanges.map((row) => {
                        const siteUrl = resolveSiteUrl(row);
                        const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(row.name)}` : "";
                        const revisionUrl = pageUrl ? `${pageUrl}?rev=${row.revision}` : "";
                        const editorUrl = row.nickname ? `/Admin/User?query=${encodeURIComponent(row.nickname)}` : "";
                        return [
                            row.dateTime,
                            siteUrl ? <Anchor href={siteUrl} target="_blank">{row.siteName} (#{row.siteSeq})</Anchor> : `${row.siteName} (#${row.siteSeq})`,
                            pageUrl ? <Anchor href={pageUrl} target="_blank">{row.name}</Anchor> : row.name,
                            revisionUrl ? <Anchor href={revisionUrl} target="_blank">{row.revision}</Anchor> : row.revision,
                            editorUrl ? <Anchor href={editorUrl}>{row.nickname}</Anchor> : (row.nickname ?? "-"),
                            row.comment || "-",
                        ];
                    }),
                )}
            </Card>
        </Stack>
    );
}

function AdminApp({initialPage}) {
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
        [],
    );

    return (
        <MantineProvider
            defaultColorScheme="light"
            theme={{
                primaryColor: "indigo",
                defaultRadius: "md",
            }}
        >
            <AppShell
                padding="md"
                navbar={{
                    width: 240,
                    breakpoint: "sm",
                }}
            >
                <AppShell.Navbar p="md" style={{overflowY: "auto"}}>

                    <Stack mb="md" gap={4}>
                        <Text fw={700} size="lg">AhaWiki Admin</Text>
                    </Stack>
                    <Navigation activePage={page} onNavigate={onNavigate}/>
                </AppShell.Navbar>
                <AppShell.Main>
                    <Stack gap="md">
                        <Group justify="space-between" align="center">
                            <Stack gap={2}>
                                <Title order={2}>{pageTitle}</Title>
                            </Stack>
                            <Badge variant="light" color="indigo" size="lg">
                                Live
                            </Badge>
                        </Group>
                        <AdminContent page={page} onNavigate={onNavigate} pathname={pathname} search={search}/>
                    </Stack>
                </AppShell.Main>
            </AppShell>
        </MantineProvider>
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
    logInfo("pageLoad:render", {page});

    const root = createRoot(rootElement);
    root.render(<AdminApp initialPage={page}/>);
}

window.addEventListener("error", (event) => {
    logError("window:error", event.message, event.error);
});

window.addEventListener("unhandledrejection", (event) => {
    logError("window:unhandledrejection", event.reason);
});

window.addEventListener("DOMContentLoaded", pageLoad);
