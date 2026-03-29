import React, {useCallback, useEffect, useMemo, useState} from "react";
import {createRoot} from "react-dom/client";
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
    Title,
} from "@mantine/core";

const LOG_PREFIX = "[AdminUI]";

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
        pageEdited: [],
    });
    const [recentChanges, setRecentChanges] = useState([]);
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
        const [siteData, userData, allUserData, schedulerData, dailyStatsData, recentChangesData] = await Promise.all([
            fetchJson("/api/Admin/Sites"),
            fetchJson("/api/Admin/SiteUsers"),
            fetchJson("/api/Admin/Users"),
            fetchJson("/api/Admin/Schedulers"),
            fetchJson("/api/Admin/DailyStats"),
            fetchJson("/api/Admin/RecentChanges?n=30"),
        ]);
        setSites(siteData);
        setUsers(userData);
        setAllUsers(allUserData);
        setSchedulers(schedulerData);
        setRecentChanges(recentChangesData);
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
                    "X-CSRF-Token": csrfToken,
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
        userViewHistories,
        loadingUserViewHistories,
        loadUserViewHistories,
        loadRecentChanges,
        runningSchedulerName,
        runScheduler,
        reloadSchedulers,
        clearSiteCache,
        clearingSiteSeq,
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
    const links = useMemo(
        () => [
            {href: "/", label: "Home", key: "home"},
            {href: "/Admin", label: "Dashboard", key: "dashboard"},
            {href: "/Admin/Site", label: "Site", key: "sites"},
            {href: "/Admin/SiteUser", label: "Site User", key: "users"},
            {href: "/Admin/User", label: "User", key: "all-users"},
            {href: "/Admin/Operation", label: "Operation", key: "operations"},
            {href: "/Admin/RecentChange", label: "Recent Change", key: "recent-changes"},
        ],
        [],
    );

    return (
        <Stack gap={8}>
            <Text size="xs" tt="uppercase" fw={700} c="dimmed" px={8}>
                Admin Navigation
            </Text>
            {links.map((link) => (
                <NavLink
                    key={link.key}
                    href={link.href}
                    label={link.label}
                    active={activePage === link.key || (activePage === "user-views" && link.key === "all-users")}
                    variant={activePage === link.key || (activePage === "user-views" && link.key === "all-users") ? "filled" : "light"}
                    onClick={(event) => {
                        if (link.key === "home") {
                            return;
                        }
                        event.preventDefault();
                        onNavigate(link.href);
                    }}
                />
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
    const width = 320;
    const height = 72;
    const data = normalizeDailyRows(rows).slice(-30);

    if (data.length === 0) {
        return <Text size="xs" c="dimmed">No data</Text>;
    }

    const max = Math.max(...data.map((item) => item.count), 1);
    const min = Math.min(...data.map((item) => item.count), 0);
    const range = Math.max(max - min, 1);

    const points = data
        .map((item, index) => {
            const x = data.length === 1 ? width / 2 : (index / (data.length - 1)) * width;
            const y = height - ((item.count - min) / range) * height;
            return `${x},${y}`;
        })
        .join(" ");

    const areaPoints = `0,${height} ${points} ${width},${height}`;
    const latest = data[data.length - 1]?.count ?? 0;
    const previous = data[data.length - 2]?.count ?? latest;
    const delta = latest - previous;
    const deltaColor = delta >= 0 ? "teal" : "red";

    return (
        <Stack gap={4}>
            <svg width="100%" viewBox={`0 0 ${width} ${height}`} role="img" aria-label="trend sparkline">
                <polyline
                    points={areaPoints}
                    fill={`var(--mantine-color-${color}-1)`}
                    stroke="none"
                />
                <polyline
                    points={points}
                    fill="none"
                    stroke={`var(--mantine-color-${color}-6)`}
                    strokeWidth="2"
                    strokeLinejoin="round"
                    strokeLinecap="round"
                />
            </svg>
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
    const width = 840;
    const height = 260;
    const padding = {top: 14, right: 20, bottom: 30, left: 28};
    const innerWidth = width - padding.left - padding.right;
    const innerHeight = height - padding.top - padding.bottom;

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

    const mappedSeries = series.map((line) => {
        const indexed = new Map(normalizeDailyRows(line.rows).map((row) => [row.ymd, row.count]));
        return {
            ...line,
            points: dates.map((date) => ({date, count: indexed.get(date) ?? 0})),
        };
    });

    const maxValue = Math.max(
        1,
        ...mappedSeries.flatMap((line) => line.points.map((point) => point.count)),
    );
    const yScale = (value) => padding.top + innerHeight - (value / maxValue) * innerHeight;
    const xScale = (index) => {
        if (dates.length === 1) {
            return padding.left + innerWidth / 2;
        }
        return padding.left + (index / (dates.length - 1)) * innerWidth;
    };

    return (
        <Stack gap={8}>
            <svg width="100%" viewBox={`0 0 ${width} ${height}`} role="img" aria-label="daily trends chart">
                {[0, 0.25, 0.5, 0.75, 1].map((tick) => {
                    const y = padding.top + innerHeight - innerHeight * tick;
                    return (
                        <line
                            key={`y-${tick}`}
                            x1={padding.left}
                            y1={y}
                            x2={padding.left + innerWidth}
                            y2={y}
                            stroke="var(--mantine-color-gray-2)"
                            strokeWidth="1"
                        />
                    );
                })}
                {mappedSeries.map((line) => {
                    const path = line.points
                        .map((point, index) => `${index === 0 ? "M" : "L"} ${xScale(index)} ${yScale(point.count)}`)
                        .join(" ");
                    return (
                        <path
                            key={line.name}
                            d={path}
                            fill="none"
                            stroke={`var(--mantine-color-${line.color}-6)`}
                            strokeWidth="2.5"
                            strokeLinecap="round"
                        />
                    );
                })}
            </svg>
            <Group gap={8}>
                {mappedSeries.map((line) => (
                    <Badge key={line.name} color={line.color} variant="light">
                        {line.name}
                    </Badge>
                ))}
            </Group>
        </Stack>
    );
}

function AdminContent({page}) {
    const {
        loading,
        sites,
        users,
        allUsers,
        schedulers,
        dailyStats,
        recentChanges,
        userViewHistories,
        loadingUserViewHistories,
        loadUserViewHistories,
        loadRecentChanges,
        runningSchedulerName,
        runScheduler,
        reloadSchedulers,
        clearSiteCache,
        clearingSiteSeq,
        error,
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
        [allUsers, selectedUserSeq],
    );

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
                    <Title order={3}>All Sites</Title>
                    <Badge color="indigo" variant="light">{sites.length} sites</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">
                    전체 사이트 목록과 함께 도메인, 사용자 수, 페이지 수를 확인할 수 있습니다.
                </Text>
                <Divider mb="md"/>
                <Title order={6} c="dimmed" mb="sm">
                    All Sites
                </Title>
                {makeTable(
                    ["Seq", "Name", "Domains", "Users", "Pages"],
                    sites.map((site) => [
                        site.seq,
                        site.name,
                        (site.domains ?? []).join(", "),
                        site.userCount ?? 0,
                        site.pageCount ?? 0,
                    ]),
                )}
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
                                        onClick={() => {
                                            window.location.href = `/Admin/User/UserViewHistory?seq=${encodeURIComponent(user.seq)}`;
                                        }}
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
                    <Button component="a" href="/Admin/User" variant="light" size="xs">
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

    if (page === "operations") {
        return (
            <Stack gap="lg">
                <Card withBorder radius="md" padding="lg">
                    <Group justify="space-between" mb="md">
                        <Title order={3}>Site Cache Operations</Title>
                        <Badge color="orange" variant="light">Careful</Badge>
                    </Group>
                    <Text size="sm" c="dimmed" mb="md">
                        사이트별 캐시를 즉시 비워서 도메인/페이지/헤더 캐시를 강제로 갱신합니다.
                    </Text>
                    <Divider mb="md"/>
                    <Table striped highlightOnHover withTableBorder withColumnBorders>
                        <Table.Thead>
                            <Table.Tr>
                                <Table.Th>Seq</Table.Th>
                                <Table.Th>Site</Table.Th>
                                <Table.Th>Domains</Table.Th>
                                <Table.Th>Action</Table.Th>
                            </Table.Tr>
                        </Table.Thead>
                        <Table.Tbody>
                            {sites.map((site) => (
                                <Table.Tr key={site.seq}>
                                    <Table.Td>{site.seq}</Table.Td>
                                    <Table.Td>{site.name}</Table.Td>
                                    <Table.Td>{(site.domains ?? []).join(", ") || "-"}</Table.Td>
                                    <Table.Td>
                                        <Button
                                            color="orange"
                                            variant="filled"
                                            size="xs"
                                            loading={clearingSiteSeq === site.seq}
                                            onClick={() => clearSiteCache(site.seq)}
                                        >
                                            Clear cache
                                        </Button>
                                    </Table.Td>
                                </Table.Tr>
                            ))}
                        </Table.Tbody>
                    </Table>
                </Card>
                <Card withBorder radius="md" padding="lg">
                    <SchedulerTable
                        schedulers={schedulers}
                        runningSchedulerName={runningSchedulerName}
                        onRun={runScheduler}
                        onRefresh={reloadSchedulers}
                    />
                </Card>
            </Stack>
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
            <SimpleGrid cols={{base: 1, sm: 3}} spacing="md">
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
                            <Text size="sm" c="dimmed">Schedulers</Text>
                            <Title order={2}>{schedulers.length}</Title>
                        </Stack>
                        <ThemeIcon color="blue" variant="light" radius="xl">R</ThemeIcon>
                    </Group>
                </Card>
            </SimpleGrid>
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
                    <Title order={3}>Sites</Title>
                    <Badge color="indigo" variant="light">{sites.length}</Badge>
                </Group>
                {makeTable(
                    ["Seq", "Name", "Domains", "Users", "Pages"],
                    sites.map((site) => [
                        site.seq,
                        site.name,
                        (site.domains ?? []).join(", "),
                        site.userCount ?? 0,
                        site.pageCount ?? 0,
                    ]),
                )}
            </Card>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>All Users</Title>
                    <Badge color="blue" variant="light">{allUsers.length}</Badge>
                </Group>
                {makeTable(
                    ["Seq", "Email", "Nickname", "Sites", "Created", "Updated"],
                    allUsers.map((user) => [
                        user.seq,
                        user.email,
                        user.nickname,
                        user.siteCount ?? 0,
                        user.created,
                        user.updated,
                    ]),
                )}
            </Card>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>Site Users (current host)</Title>
                    <Badge color="teal" variant="light">{users.length}</Badge>
                </Group>
                {makeTable(
                    ["User", "Email", "Nickname", "Created"],
                    users.map((user) => [user.user, user.email, user.nickname, user.created]),
                )}
            </Card>
            <Card withBorder radius="md" padding="lg">
                <SchedulerTable
                    schedulers={schedulers}
                    runningSchedulerName={runningSchedulerName}
                    onRun={runScheduler}
                    onRefresh={reloadSchedulers}
                />
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
                    recentChanges.map((row) => [
                        row.dateTime,
                        `${row.siteName} (#${row.siteSeq})`,
                        row.name,
                        row.revision,
                        row.nickname ?? "-",
                        row.comment || "-",
                    ]),
                )}
            </Card>
            <DailyStatTable
                title="Daily New Users"
                description="최근 30일 기준 전체 사용자 신규 생성 수를 차트와 표로 함께 제공합니다."
                rows={dailyStats.userCreated}
                badgeColor="blue"
            />
            <DailyStatTable
                title="Daily Site User Joins"
                description="최근 30일 기준 사이트 가입(UserSite) 수를 차트와 표로 함께 제공합니다."
                rows={dailyStats.siteUserCreated}
                badgeColor="teal"
            />
            <DailyStatTable
                title="Daily New Pages"
                description="최근 30일 기준 revision=1 페이지 생성 수를 차트와 표로 함께 제공합니다."
                rows={dailyStats.pageCreated}
                badgeColor="indigo"
            />
            <DailyStatTable
                title="Daily Page Edits"
                description="최근 30일 기준 페이지 전체 수정(모든 리비전) 수를 차트와 표로 함께 제공합니다."
                rows={dailyStats.pageEdited}
                badgeColor="grape"
            />
        </Stack>
    );
}

function AdminApp({initialPage}) {
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
                        <Text fw={700} size="lg">AhaWiki</Text>
                        <Text size="xs" c="dimmed">관리자 콘솔</Text>
                    </Stack>
                    <Navigation activePage={page} onNavigate={onNavigate}/>
                </AppShell.Navbar>
                <AppShell.Main>
                    <Stack gap="md">
                        <Group justify="space-between" align="center">
                            <Stack gap={2}>
                                <Title order={2}>Admin Dashboard</Title>
                                <Text size="sm" c="dimmed">운영 현황을 한눈에 확인하고 즉시 작업하세요.</Text>
                            </Stack>
                            <Badge variant="light" color="indigo" size="lg">
                                Live
                            </Badge>
                        </Group>
                        <AdminContent page={page}/>
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
