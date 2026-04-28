import React, {useCallback, useEffect, useMemo, useState} from "react";
import {createRoot} from "react-dom/client";
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
    Title,
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
    YAxis,
} from "recharts";

const LOG_PREFIX = "[AdminUI]";

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
    const [schedulers, setSchedulers] = useState([]);
    const [dailyStats, setDailyStats] = useState({
        userCreated: [],
        siteUserCreated: [],
        pageCreated: [],
        pageEdited: [],
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
        footerForegroundColor: "",
    });
    const [savingSiteTheme, setSavingSiteTheme] = useState(false);
    const [error, setError] = useState("");

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
        const [siteData, userData, allUserData, schedulerData, dailyStatsData, recentChangesData, topViewedPagesData] = await Promise.all([
            fetchJson("/api/Admin/Sites"),
            fetchJson("/api/Admin/SiteUsers"),
            fetchJson("/api/Admin/Users"),
            fetchJson("/api/Admin/Schedulers"),
            fetchJson("/api/Admin/DailyStats"),
            fetchJson("/api/Admin/RecentChanges?n=30"),
            fetchJson("/api/Admin/TopViewedPages?n=20"),
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
            pageEdited: dailyStatsData?.pageEdited ?? [],
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
                        setSchedulers([]);
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
                        setSchedulers([]);
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
                        setSchedulers([]);
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
        error,
    };
}

function makeTable(headers, rows) {
    return (
        <Table striped highlightOnHover withTableBorder withColumnBorders stickyHeader stickyHeaderOffset={0}>
            <Table.Thead>
                <Table.Tr>
                    {headers.map((header) => (
                        <Table.Th key={header}>{header}</Table.Th>
                    ))}
                </Table.Tr>
            </Table.Thead>
            <Table.Tbody>
                {rows.map((columns, rowIndex) => (
                    <Table.Tr key={`row-${rowIndex}`}>
                        {columns.map((column, colIndex) => (
                            <Table.Td key={`col-${rowIndex}-${colIndex}`}>{column ?? ""}</Table.Td>
                        ))}
                    </Table.Tr>
                ))}
            </Table.Tbody>
        </Table>
    );
}

function Navigation({activePage, onNavigate}) {
    const [siteLinks, setSiteLinks] = useState([]);
    const currentSiteSeq = parseSiteSeqFromPathname(window.location.pathname);

    useEffect(() => {
        let mounted = true;
        fetchJson("/api/Admin/Sites")
            .then((siteData) => {
                if (!mounted) {
                    return;
                }
                setSiteLinks(Array.isArray(siteData) ? siteData : []);
            })
            .catch((caughtError) => {
                logError("navigation:sites:error", caughtError);
            });

        return () => {
            mounted = false;
        };
    }, []);

    const links = useMemo(
        () => [
            {href: "/", label: "위키로 돌아가기", key: "home"},
            {href: "/Admin", label: "Dashboard", key: "dashboard"},
            {href: "/Admin/RecentChange", label: "RecentChanges", key: "recent-changes"},
            {href: "/Admin/User", label: "User", key: "all-users"},
            {href: "/Admin/Site", label: "Site", key: "sites"},
            {href: "/Admin/Operation", label: "Operation", key: "operations"},
        ],
        [],
    );

    return (
        <Stack gap={8}>
            <Text size="xs" tt="uppercase" fw={700} c="dimmed" px={8}>
                Admin Navigation
            </Text>
            {links.map((link) => (
                <React.Fragment key={link.key}>
                    <NavLink
                        href={link.href}
                        label={link.label}
                        active={
                            activePage === link.key
                            || (activePage === "user-views" && link.key === "all-users")
                            || (activePage === "site-detail" && link.key === "sites")
                        }
                        variant={
                            activePage === link.key
                            || (activePage === "user-views" && link.key === "all-users")
                            || (activePage === "site-detail" && link.key === "sites")
                                ? "filled"
                                : "light"
                        }
                        onClick={(event) => {
                            if (link.key === "home") {
                                return;
                            }
                            event.preventDefault();
                            onNavigate(link.href);
                        }}
                    />
                    {link.key === "sites" && (activePage === "sites" || activePage === "site-detail") && (
                        <Stack gap={2} ml={8}>
                            {siteLinks.map((site) => (
                                <NavLink
                                    key={`site-${site.seq}`}
                                    href={`/Admin/Site/${site.seq}`}
                                    label={`${site.name} (#${site.seq})`}
                                    active={currentSiteSeq === String(site.seq)}
                                    variant={currentSiteSeq === String(site.seq) ? "subtle" : "light"}
                                    onClick={(event) => {
                                        event.preventDefault();
                                        onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}`);
                                    }}
                                />
                            ))}
                        </Stack>
                    )}
                </React.Fragment>
            ))}
        </Stack>
    );
}

function SchedulerTable({schedulers, runningSchedulerName, onRun, onRefresh}) {
    return (
        <Stack gap="sm">
            <Group justify="space-between">
                <Title order={4}>Schedulers</Title>
                <Button size="xs" variant="light" onClick={onRefresh}>Refresh</Button>
            </Group>
            <Progress
                size="sm"
                value={
                    schedulers.length === 0
                        ? 0
                        : (schedulers.filter((scheduler) => scheduler.running).length / schedulers.length) * 100
                }
                color="blue"
                radius="xl"
            />
            <Table striped highlightOnHover withTableBorder withColumnBorders>
                <Table.Thead>
                    <Table.Tr>
                        <Table.Th>Name</Table.Th>
                        <Table.Th>Interval</Table.Th>
                        <Table.Th>Next Delay(s)</Table.Th>
                        <Table.Th>Last Started</Table.Th>
                        <Table.Th>Last Finished</Table.Th>
                        <Table.Th>Result</Table.Th>
                        <Table.Th>Run Count</Table.Th>
                        <Table.Th>Action</Table.Th>
                    </Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                    {schedulers.map((scheduler) => (
                        <Table.Tr key={scheduler.name}>
                            <Table.Td>{scheduler.name}</Table.Td>
                            <Table.Td>{`${scheduler.minSeconds}s ~ ${scheduler.maxSeconds}s`}</Table.Td>
                            <Table.Td>{scheduler.nextDelaySeconds ?? "-"}</Table.Td>
                            <Table.Td>{scheduler.lastStartedAt ?? "-"}</Table.Td>
                            <Table.Td>{scheduler.lastFinishedAt ?? "-"}</Table.Td>
                            <Table.Td>{scheduler.lastResult ?? "-"}</Table.Td>
                            <Table.Td>{scheduler.runCount ?? 0}</Table.Td>
                            <Table.Td>
                                <Button
                                    size="xs"
                                    variant="filled"
                                    loading={runningSchedulerName === scheduler.name}
                                    disabled={scheduler.running}
                                    onClick={() => onRun(scheduler.name)}
                                >
                                    {scheduler.running ? "Running..." : "Run now"}
                                </Button>
                            </Table.Td>
                        </Table.Tr>
                    ))}
                </Table.Tbody>
            </Table>
        </Stack>
    );
}

function DailyStatTable({title, description, rows, badgeColor}) {
    const maxCount = rows.reduce((max, row) => Math.max(max, row.count ?? 0), 0);
    const bars = rows.slice(-14);

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>{title}</Title>
                <Badge color={badgeColor} variant="light">{rows.length} days</Badge>
            </Group>
            <Text size="sm" c="dimmed" mb="md">{description}</Text>
            <Stack gap={8} mb="md">
                {bars.map((row) => {
                    const count = row.count ?? 0;
                    const widthPercent = maxCount === 0 ? 0 : (count / maxCount) * 100;
                    return (
                        <Stack key={row.ymd} gap={4}>
                            <Group justify="space-between">
                                <Text size="xs" c="dimmed">{row.ymd}</Text>
                                <Text size="xs" fw={700}>{count}</Text>
                            </Group>
                            <Progress value={widthPercent} color={badgeColor} radius="xl"/>
                        </Stack>
                    );
                })}
            </Stack>
            {makeTable(
                ["Date", "Count"],
                rows.map((row) => [row.ymd, row.count]),
            )}
        </Card>
    );
}

function normalizeDailyRows(rows) {
    return [...rows]
        .map((row) => ({
            ymd: row.ymd,
            count: Number(row.count ?? 0),
        }))
        .sort((left, right) => (left.ymd > right.ymd ? 1 : -1));
}

function Sparkline({rows, color}) {
    const data = normalizeDailyRows(rows).slice(-30);

    if (data.length === 0) {
        return <Text size="xs" c="dimmed">No data</Text>;
    }

    const latest = data[data.length - 1]?.count ?? 0;
    const previous = data[data.length - 2]?.count ?? latest;
    const delta = latest - previous;
    const deltaColor = delta >= 0 ? "teal" : "red";

    return (
        <Stack gap={4}>
            <div style={{width: "100%", height: 72}} role="img" aria-label="trend sparkline">
                <ResponsiveContainer width="100%" height="100%">
                    <AreaChart data={data} margin={{top: 4, right: 0, left: 0, bottom: 0}}>
                        <defs>
                            <linearGradient id={`sparklineGradient-${color}`} x1="0" y1="0" x2="0" y2="1">
                                <stop offset="0%" stopColor={`var(--mantine-color-${color}-4)`} stopOpacity={0.35}/>
                                <stop offset="100%" stopColor={`var(--mantine-color-${color}-1)`} stopOpacity={0.1}/>
                            </linearGradient>
                        </defs>
                        <Tooltip
                            cursor={false}
                            labelFormatter={(value) => `Date: ${value}`}
                            formatter={(value) => [value, "Count"]}
                        />
                        <Area
                            type="monotone"
                            dataKey="count"
                            stroke={`var(--mantine-color-${color}-6)`}
                            strokeWidth={2}
                            fill={`url(#sparklineGradient-${color})`}
                            dot={false}
                            activeDot={{r: 3}}
                            isAnimationActive={false}
                        />
                    </AreaChart>
                </ResponsiveContainer>
            </div>
            <Group justify="space-between">
                <Text size="xs" c="dimmed">최근 30일</Text>
                <Badge color={deltaColor} variant="light" size="xs">
                    {delta >= 0 ? "+" : ""}{delta} vs yesterday
                </Badge>
            </Group>
        </Stack>
    );
}

function StatTrendCard({title, total, rows, color}) {
    return (
        <Card withBorder radius="md" padding="md">
            <Stack gap={8}>
                <Group justify="space-between" align="flex-start">
                    <Text size="sm" c="dimmed">{title}</Text>
                    <Badge color={color} variant="light">30d</Badge>
                </Group>
                <Title order={3}>{total}</Title>
                <Sparkline rows={rows} color={color}/>
            </Stack>
        </Card>
    );
}

function MultiTrendChart({series}) {
    const dateSet = new Set();
    series.forEach((line) => {
        normalizeDailyRows(line.rows).forEach((row) => {
            dateSet.add(row.ymd);
        });
    });
    const dates = [...dateSet].sort().slice(-30);

    if (dates.length === 0) {
        return <Text c="dimmed" size="sm">차트 데이터가 없습니다.</Text>;
    }

    const colorByName = {};
    const chartDataByDate = new Map(dates.map((date) => [date, {date}]));
    series.forEach((line) => {
        colorByName[line.name] = line.color;
        const indexed = new Map(normalizeDailyRows(line.rows).map((row) => [row.ymd, row.count]));
        dates.forEach((date) => {
            chartDataByDate.get(date)[line.name] = indexed.get(date) ?? 0;
        });
    });
    const chartData = dates.map((date) => chartDataByDate.get(date));

    const xAxisTickFormatter = (value) => value.slice(5);

    return (
        <Stack gap={8}>
            <div style={{width: "100%", height: 280}} role="img" aria-label="daily trends chart">
                <ResponsiveContainer width="100%" height="100%">
                    <LineChart data={chartData} margin={{top: 8, right: 12, bottom: 8, left: 0}}>
                        <CartesianGrid stroke="var(--mantine-color-gray-2)" strokeDasharray="3 3"/>
                        <XAxis dataKey="date" tickFormatter={xAxisTickFormatter} tick={{fontSize: 12}}/>
                        <YAxis allowDecimals={false} tick={{fontSize: 12}}/>
                        <Tooltip labelFormatter={(value) => `Date: ${value}`}/>
                        <Legend verticalAlign="top" height={30}/>
                        {series.map((line) => (
                            <Line
                                key={line.name}
                                type="monotone"
                                dataKey={line.name}
                                stroke={`var(--mantine-color-${colorByName[line.name]}-6)`}
                                strokeWidth={2.5}
                                dot={false}
                                activeDot={{r: 4}}
                                isAnimationActive={false}
                            />
                        ))}
                    </LineChart>
                </ResponsiveContainer>
            </div>
            <Group gap={8}>
                {series.map((line) => (
                    <Badge key={line.name} color={line.color} variant="light">
                        {line.name}
                    </Badge>
                ))}
            </Group>
        </Stack>
    );
}


function AdminContent({page, onNavigate}) {
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
        error,
    } = useAdminData(page);
    const [recentChangeLimitInput, setRecentChangeLimitInput] = useState("50");
    const [faviconFile, setFaviconFile] = useState(null);
    const [selectedSiteSeq, setSelectedSiteSeq] = useState("");
    const selectedSite = useMemo(
        () => sites.find((site) => String(site.seq) === selectedSiteSeq) ?? null,
        [sites, selectedSiteSeq],
    );
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
        [allUsers, selectedUserSeq],
    );

    useEffect(() => {
        if (page !== "site-detail") {
            return;
        }
        const siteSeqByPath = parseSiteSeqFromPathname(window.location.pathname);
        if (siteSeqByPath && selectedSiteSeq !== siteSeqByPath) {
            setSelectedSiteSeq(siteSeqByPath);
        }
    }, [page, selectedSiteSeq]);

    useEffect(() => {
        if (page === "site-detail" && selectedSiteSeq) {
            loadSiteFavicon(selectedSiteSeq);
            loadSiteTheme(selectedSiteSeq);
        }
    }, [page, selectedSiteSeq, loadSiteFavicon, loadSiteTheme]);

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
        return (
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>Site List</Title>
                    <Badge color="indigo" variant="light">{sites.length} sites</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">
                    전체 사이트 목록입니다. 상세 설정은 각 사이트의 관리 버튼으로 이동하세요.
                </Text>
                <Divider mb="md"/>
                <Table striped highlightOnHover withTableBorder withColumnBorders>
                    <Table.Thead>
                        <Table.Tr>
                            <Table.Th>Seq</Table.Th>
                            <Table.Th>Name</Table.Th>
                            <Table.Th>Domains</Table.Th>
                            <Table.Th>Users</Table.Th>
                            <Table.Th>Pages</Table.Th>
                            <Table.Th>Action</Table.Th>
                        </Table.Tr>
                    </Table.Thead>
                    <Table.Tbody>
                        {sites.map((site) => (
                            <Table.Tr key={site.seq}>
                                <Table.Td>{site.seq}</Table.Td>
                                <Table.Td>{site.name}</Table.Td>
                                <Table.Td>{(site.domains ?? []).join(", ") || "-"}</Table.Td>
                                <Table.Td>{site.userCount ?? 0}</Table.Td>
                                <Table.Td>{site.pageCount ?? 0}</Table.Td>
                                <Table.Td>
                                    <Button
                                        size="xs"
                                        variant="light"
                                        onClick={() => onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}`)}
                                    >
                                        관리
                                    </Button>
                                </Table.Td>
                            </Table.Tr>
                        ))}
                    </Table.Tbody>
                </Table>
            </Card>
        );
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
                    ["User", "Email", "Nickname", "Created"],
                    users.map((user) => [user.user, user.email, user.nickname, user.created]),
                )}
            </Card>
        );
    }

    if (page === "all-users") {
        return (
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>All Users</Title>
                    <Badge color="blue" variant="light">{allUsers.length} users</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">
                    전체 사이트 기준 사용자 목록이며, 최근 방문순으로 정렬됩니다.
                </Text>
                <Divider mb="md"/>
                <Table striped highlightOnHover withTableBorder withColumnBorders>
                    <Table.Thead>
                        <Table.Tr>
                            <Table.Th>Seq</Table.Th>
                            <Table.Th>Email</Table.Th>
                            <Table.Th>Nickname</Table.Th>
                            <Table.Th>Created</Table.Th>
                            <Table.Th>Updated</Table.Th>
                            <Table.Th>Sites</Table.Th>
                            <Table.Th>Visits</Table.Th>
                            <Table.Th>Last Viewed</Table.Th>
                            <Table.Th>Action</Table.Th>
                        </Table.Tr>
                    </Table.Thead>
                    <Table.Tbody>
                        {allUsers.map((user) => (
                            <Table.Tr key={user.seq}>
                                <Table.Td>{user.seq}</Table.Td>
                                <Table.Td>{user.email}</Table.Td>
                                <Table.Td>{user.nickname}</Table.Td>
                                <Table.Td>{user.created}</Table.Td>
                                <Table.Td>{user.updated}</Table.Td>
                                <Table.Td>{user.siteCount ?? 0}</Table.Td>
                                <Table.Td>{user.visitCount ?? 0}</Table.Td>
                                <Table.Td>{user.lastViewed ?? "-"}</Table.Td>
                                <Table.Td>
                                    <Button
                                        size="xs"
                                        variant="light"
                                        onClick={() => onNavigate(`/Admin/User/UserViewHistory?seq=${encodeURIComponent(user.seq)}`)}
                                    >
                                        열람 이력
                                    </Button>
                                </Table.Td>
                            </Table.Tr>
                        ))}
                    </Table.Tbody>
                </Table>
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
                        <Text size="sm">
                            사용자: <b>{selectedAllUser.nickname}</b> ({selectedAllUser.email})
                        </Text>
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

    if (page === "site-detail") {
        return (
            <Stack gap="lg">
                <Card withBorder radius="md" padding="lg">
                    <Group justify="space-between" mb="xs">
                        <Title order={4}>사이트 상세</Title>
                        <Badge color="blue" variant="light">Site Detail</Badge>
                    </Group>
                    <Text size="sm" c="dimmed" mb="md">
                        /Admin/Site/{`{seq}`} 경로로 접근한 사이트의 favicon/테마를 설정합니다.
                    </Text>
                    <SimpleGrid cols={{base: 1, lg: 2}} spacing="md">
                        <Paper withBorder radius="md" p="sm">
                            <Button variant="light" size="xs" onClick={() => onNavigate("/Admin/Site")}>
                                ← 사이트 목록
                            </Button>
                        </Paper>
                        <Paper withBorder radius="md" p="sm">
                            {selectedSite ? (
                                <Stack gap={4}>
                                    <Text size="xs" c="dimmed">선택된 사이트</Text>
                                    <Text fw={700}>{selectedSite.name} (#{selectedSite.seq})</Text>
                                    <Text size="sm" c="dimmed">
                                        도메인: {(selectedSite.domains ?? []).join(", ") || "-"}
                                    </Text>
                                </Stack>
                            ) : (
                                <Text size="sm" c="dimmed">유효한 사이트 seq가 필요합니다. (/Admin/Site/숫자)</Text>
                            )}
                        </Paper>
                    </SimpleGrid>
                </Card>
                <Card withBorder radius="md" padding="lg">
                    <Group justify="space-between" mb="md">
                        <Title order={3}>Site Cache Operation</Title>
                        <Badge color="orange" variant="light">Current Site</Badge>
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
                        <Title order={3}>Site Favicon</Title>
                        <Badge color="blue" variant="light">Current Site</Badge>
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
                <Card withBorder radius="md" padding="lg">
                    <Group justify="space-between" mb="md">
                        <Title order={3}>Site Header/Footer Theme</Title>
                        <Badge color="grape" variant="light">Per Site</Badge>
                    </Group>
                    <Text size="sm" c="dimmed" mb="md">
                        사이트별 헤더/푸터 배경색·전경색을 16진수(#RGB, #RRGGBB, #RRGGBBAA)로 지정할 수 있습니다. 비워두면 기본 스타일을 사용합니다.
                    </Text>
                    <SimpleGrid cols={{base: 1, lg: 2}} spacing="md">
                        <SimpleGrid cols={{base: 1, sm: 2}} spacing="md">
                            <ColorInput
                                label="Header 배경색"
                                placeholder="#FFFFFF"
                                format="hexa"
                                value={siteTheme.headerBackgroundColor}
                                onChange={(value) => setSiteTheme((prev) => ({...prev, headerBackgroundColor: value}))}
                                swatches={["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#6c5ce7", "#0b7285"]}
                                withEyeDropper={false}
                                clearable
                            />
                            <ColorInput
                                label="Header 전경색"
                                placeholder="#111111"
                                format="hexa"
                                value={siteTheme.headerForegroundColor}
                                onChange={(value) => setSiteTheme((prev) => ({...prev, headerForegroundColor: value}))}
                                swatches={["#111111", "#212529", "#495057", "#ffffff", "#f1f3f5", "#ffd43b"]}
                                withEyeDropper={false}
                                clearable
                            />
                            <ColorInput
                                label="Footer 배경색"
                                placeholder="#FFFFFF"
                                format="hexa"
                                value={siteTheme.footerBackgroundColor}
                                onChange={(value) => setSiteTheme((prev) => ({...prev, footerBackgroundColor: value}))}
                                swatches={["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#2b8a3e", "#862e9c"]}
                                withEyeDropper={false}
                                clearable
                            />
                            <ColorInput
                                label="Footer 전경색"
                                placeholder="#111111"
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
                                        backgroundColor: siteTheme.headerBackgroundColor || "#f8f9fa",
                                        color: siteTheme.headerForegroundColor || "#111111",
                                        padding: "12px 14px",
                                        fontWeight: 600,
                                    }}
                                >
                                    Header Preview
                                </div>
                                <div style={{padding: "16px 14px", backgroundColor: "#ffffff", color: "#495057"}}>
                                    콘텐츠 영역 (고정 미리보기)
                                </div>
                                <div
                                    style={{
                                        backgroundColor: siteTheme.footerBackgroundColor || "#f8f9fa",
                                        color: siteTheme.footerForegroundColor || "#111111",
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
            </Stack>
        );
    }

    if (page === "operations") {
        return (
            <Card withBorder radius="md" padding="lg">
                <SchedulerTable
                    schedulers={schedulers}
                    runningSchedulerName={runningSchedulerName}
                    onRun={runScheduler}
                    onRefresh={reloadSchedulers}
                />
            </Card>
        );
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
                    recentChanges.map((row) => [
                        row.dateTime,
                        `${row.siteName} (#${row.siteSeq})`,
                        row.name,
                        row.revision,
                        row.nickname ?? "-",
                        row.comment || "-",
                        row.remoteAddress,
                    ]),
                )}
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
                <Card withBorder radius="md" padding="md">
                    <Group justify="space-between" align="flex-start">
                        <Stack gap={2}>
                            <Text size="sm" c="dimmed">Running Schedulers</Text>
                            <Title order={2}>{schedulers.filter((scheduler) => scheduler.running).length}/{schedulers.length}</Title>
                        </Stack>
                        <ThemeIcon color="blue" variant="light" radius="xl">R</ThemeIcon>
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
                    <Button variant="light" onClick={() => onNavigate("/Admin/Operation")}>운영 작업 열기</Button>
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
                    <Badge color="pink" variant="light">{Math.min(topViewedPages.length, 10)}</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">
                    로그인 사용자 기준 페이지 조회 누적 상위 문서입니다. 핵심 10개만 표시합니다.
                </Text>
                {makeTable(
                    ["Rank", "Site", "Page", "Views", "Last Viewed"],
                    topViewedPages.slice(0, 10).map((row, index) => {
                        const siteUrl = row.siteDomain ? `https://${row.siteDomain}` : "";
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
                    recentChanges.slice(0, 10).map((row) => [
                        row.dateTime,
                        `${row.siteName} (#${row.siteSeq})`,
                        row.name,
                        row.revision,
                        row.nickname ?? "-",
                        row.comment || "-",
                    ]),
                )}
            </Card>
        </Stack>
    );
}

function AdminApp({initialPage}) {
    const [page, setPage] = useState(initialPage);
    const pageTitle = pageTitleByKey(page);

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
                <AppShell.Navbar p="md">
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
                        <AdminContent page={page} onNavigate={onNavigate}/>
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
