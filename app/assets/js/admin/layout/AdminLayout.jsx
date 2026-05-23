import React, {useEffect, useState} from "react";
import {Outlet, useNavigate} from "react-router-dom";
import {AppShell, Badge, Group, Stack, Text, Title} from "@mantine/core";
import Navigation from "../navigation.jsx";
import {AdminContext} from "../context/AdminContext.jsx";
import {fetchJson, logError} from "../api.js";

function pageTitleFromPath(pathname) {
    if (/^\/Admin\/Site\/\d+\/Permission/.test(pathname)) return "Permission";
    if (/^\/Admin\/Site\/\d+\/Config/.test(pathname)) return "Config";
    if (/^\/Admin\/Site\/\d+\/Cache/.test(pathname)) return "Cache";
    if (/^\/Admin\/Site\/\d+\/Admins/.test(pathname)) return "SiteAdmins";
    if (/^\/Admin\/Site\/\d+\/Page/.test(pathname)) return "Page";
    if (/^\/Admin\/Site\/\d+\/Detail/.test(pathname)) return "Page";
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

export default function AdminLayout() {
    const [me, setMe] = useState(null);
    const navigate = useNavigate();
    const pathname = window.location.pathname;
    const pageTitle = pageTitleFromPath(pathname);

    useEffect(() => {
        fetchJson("/api/me").then(setMe).catch((err) => {
            logError("app:me:error", err);
            setMe({loggedIn: false});
        });
    }, []);

    return (
        <AdminContext.Provider value={{me}}>
            <AppShell padding="md" navbar={{width: 240, breakpoint: "sm"}}>
                <AppShell.Navbar p="md" style={{overflowY: "auto"}}>
                    <Stack mb="md" gap={4}>
                        <Text fw={700} size="lg">AhaWiki Admin</Text>
                    </Stack>
                    <Navigation me={me} onNavigate={(href) => navigate(href)}/>
                </AppShell.Navbar>
                <AppShell.Main>
                    <Stack gap="md">
                        <Group justify="space-between" align="center">
                            <Title order={2}>{pageTitle}</Title>
                            <Badge variant="light" color="indigo" size="lg">Live</Badge>
                        </Group>
                        <Outlet/>
                    </Stack>
                </AppShell.Main>
            </AppShell>
        </AdminContext.Provider>
    );
}
