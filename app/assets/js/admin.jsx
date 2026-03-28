import React, {useCallback, useEffect, useMemo, useState} from "react";
import {createRoot} from "react-dom/client";
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
    const response = await fetch(url, {credentials: "same-origin"});
    if (!response.ok) {
        throw new Error(`HTTP ${response.status}`);
    }
    const data = await response.json();
    logInfo("fetch:success", url, {count: Array.isArray(data) ? data.length : undefined});
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
        pageEdited: [],
    });
    const [runningSchedulerName, setRunningSchedulerName] = useState("");
    const [error, setError] = useState("");

    const loadDashboard = useCallback(async () => {
        const [siteData, userData, schedulerData, dailyStatsData] = await Promise.all([
            fetchJson("/api/admin/sites"),
            fetchJson("/api/admin/site-users"),
            fetchJson("/api/admin/schedulers"),
            fetchJson("/api/admin/daily-stats"),
        ]);
        setSites(siteData);
        setUsers(userData);
        setSchedulers(schedulerData);
        setDailyStats({
            userCreated: dailyStatsData?.userCreated ?? [],
            siteUserCreated: dailyStatsData?.siteUserCreated ?? [],
            pageCreated: dailyStatsData?.pageCreated ?? [],
            pageEdited: dailyStatsData?.pageEdited ?? [],
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
                            pageEdited: [],
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
    }, [page, loadDashboard]);

    return {loading, sites, users, schedulers, dailyStats, runningSchedulerName, runScheduler, reloadSchedulers, error};
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
                            <Table.Td key={`col-${rowIndex}-${colIndex}`}>{String(column ?? "")}</Table.Td>
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
            {href: "/admin", label: "Dashboard", key: "dashboard"},
            {href: "/admin/sites", label: "All Sites", key: "sites"},
            {href: "/admin/site-users", label: "Site Users", key: "users"},
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
                    active={activePage === link.key}
                    variant={activePage === link.key ? "filled" : "light"}
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
    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>{title}</Title>
                <Badge color={badgeColor} variant="light">{rows.length} days</Badge>
            </Group>
            <Text size="sm" c="dimmed" mb="md">{description}</Text>
            {makeTable(
                ["Date", "Count"],
                rows.map((row) => [row.ymd, row.count]),
            )}
        </Card>
    );
}

function AdminContent({page}) {
    const {loading, sites, users, schedulers, dailyStats, runningSchedulerName, runScheduler, reloadSchedulers, error} = useAdminData(page);

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
                            <Text size="sm" c="dimmed">Users</Text>
                            <Title order={2}>{users.length}</Title>
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
                <Card withBorder radius="md" padding="md">
                    <Text size="sm" c="dimmed">New Users (30d)</Text>
                    <Title order={3}>{dailyStats.userCreated.reduce((sum, item) => sum + (item.count ?? 0), 0)}</Title>
                </Card>
                <Card withBorder radius="md" padding="md">
                    <Text size="sm" c="dimmed">Site User Joins (30d)</Text>
                    <Title order={3}>{dailyStats.siteUserCreated.reduce((sum, item) => sum + (item.count ?? 0), 0)}</Title>
                </Card>
                <Card withBorder radius="md" padding="md">
                    <Text size="sm" c="dimmed">New Pages (30d)</Text>
                    <Title order={3}>{dailyStats.pageCreated.reduce((sum, item) => sum + (item.count ?? 0), 0)}</Title>
                </Card>
                <Card withBorder radius="md" padding="md">
                    <Text size="sm" c="dimmed">Page Edits (30d)</Text>
                    <Title order={3}>{dailyStats.pageEdited.reduce((sum, item) => sum + (item.count ?? 0), 0)}</Title>
                </Card>
            </SimpleGrid>
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
            <DailyStatTable
                title="Daily New Users"
                description="최근 30일 기준 전체 사용자 신규 생성 수입니다."
                rows={dailyStats.userCreated}
                badgeColor="blue"
            />
            <DailyStatTable
                title="Daily Site User Joins"
                description="최근 30일 기준 사이트 가입(UserSite) 수입니다."
                rows={dailyStats.siteUserCreated}
                badgeColor="teal"
            />
            <DailyStatTable
                title="Daily New Pages"
                description="최근 30일 기준 revision=1 페이지 생성 수입니다."
                rows={dailyStats.pageCreated}
                badgeColor="indigo"
            />
            <DailyStatTable
                title="Daily Page Edits"
                description="최근 30일 기준 페이지 전체 수정(모든 리비전) 수입니다."
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
            if (window.location.pathname !== href) {
                window.history.pushState({}, "", href);
            }
            setPage(routeToPage(href));
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
