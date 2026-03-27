import React, {useCallback, useEffect, useMemo, useState} from "react";
import {createRoot} from "react-dom/client";
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
                    fetchJson("/api/admin/site-users"),
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

    return {loading, sites, users, error};
}

function makeTable(headers, rows) {
    return (
        <Table striped highlightOnHover withTableBorder withColumnBorders>
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
        <Stack gap={4}>
            {links.map((link) => (
                <NavLink
                    key={link.key}
                    href={link.href}
                    label={link.label}
                    active={activePage === link.key}
                    variant="light"
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

function AdminContent({page}) {
    const {loading, sites, users, error} = useAdminData(page);

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
                <Title order={3} mb="md">
                    All Sites
                </Title>
                {makeTable(
                    ["Seq", "Name"],
                    sites.map((site) => [site.seq, site.name]),
                )}
            </Card>
        );
    }

    if (page === "users") {
        return (
            <Card withBorder radius="md" padding="lg">
                <Title order={3} mb="md">
                    Site Users (current host)
                </Title>
                {makeTable(
                    ["User", "Email", "Nickname", "Created"],
                    users.map((user) => [user.user, user.email, user.nickname, user.created]),
                )}
            </Card>
        );
    }

    return (
        <Stack gap="lg">
            <Card withBorder radius="md" padding="lg">
                <Title order={3} mb="md">
                    Sites
                </Title>
                {makeTable(
                    ["Seq", "Name"],
                    sites.map((site) => [site.seq, site.name]),
                )}
            </Card>
            <Card withBorder radius="md" padding="lg">
                <Title order={3} mb="md">
                    Site Users (current host)
                </Title>
                {makeTable(
                    ["User", "Email", "Nickname", "Created"],
                    users.map((user) => [user.user, user.email, user.nickname, user.created]),
                )}
            </Card>
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
        <MantineProvider defaultColorScheme="light">
            <AppShell
                padding="md"
                navbar={{
                    width: 240,
                    breakpoint: "sm",
                }}
            >
                <AppShell.Navbar p="md">
                    <Navigation activePage={page} onNavigate={onNavigate}/>
                </AppShell.Navbar>
                <AppShell.Main>
                    <Stack gap="md">
                        <Group justify="space-between" align="center">
                            <Title order={2}>Admin</Title>
                            <Badge variant="light" color="blue" size="lg">
                                Mantine Enabled
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
