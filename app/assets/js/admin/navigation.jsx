import React, {useEffect, useMemo, useState} from "react";
import {Badge, Divider, Group, NavLink, Paper, Stack, Text} from "@mantine/core";

function fetchJson(url) {
    return fetch(url, {credentials: "same-origin"}).then((response) => {
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        return response.json();
    });
}

function parseSiteSeqFromPathname(pathname) {
    const matched = pathname.match(/^\/Admin\/(?:Site\/)?(\d+)(?:\/(?:Config|Cache|AccessLog))?$/);
    if (!matched) return "";
    const siteSeq = Number.parseInt(matched[1], 10);
    return Number.isFinite(siteSeq) && siteSeq > 0 ? String(siteSeq) : "";
}

export default function Navigation({activePage, onNavigate}) {
    const [siteLinks, setSiteLinks] = useState([]);
    const currentPathname = window.location.pathname;
    const currentSiteSeq = parseSiteSeqFromPathname(currentPathname);

    useEffect(() => {
        let mounted = true;
        fetchJson("/api/Admin/Sites")
            .then((siteData) => {
                if (mounted) setSiteLinks(Array.isArray(siteData) ? siteData : []);
            })
            .catch((caughtError) => {
                console.error("[AdminUI] navigation:sites:error", caughtError);
            });
        return () => {
            mounted = false;
        };
    }, []);

    const links = useMemo(() => [
        {href: "/Admin", label: "Dashboard", key: "dashboard", iconClassName: "fas fa-chart-line"},
        {href: "/Admin/Site", label: "Site", key: "sites", iconClassName: "fas fa-sitemap"},
        {href: "/Admin/User", label: "User", key: "all-users", iconClassName: "fas fa-users"},
        {href: "/Admin/RecentChange", label: "RecentChanges", key: "recent-changes", iconClassName: "fas fa-history"},
        {href: "/Admin/AccessLog", label: "AccessLog", key: "access-logs", iconClassName: "fas fa-network-wired"},
        {href: "/Admin/Operation", label: "Operation", key: "operations", iconClassName: "fas fa-cogs"},
        {href: "/Admin/CrawlerCache", label: "Crawler", key: "crawler-cache", iconClassName: "fas fa-spider"},
        {href: "/Admin/S3", label: "S3 Browser", key: "s3-browser", iconClassName: "fas fa-folder-open"},
    ], []);

    return <Stack gap={8}>
        <Paper withBorder radius="md" p={8}>
            <Text size="xs" c="dimmed" fw={700} tt="uppercase" mb={6}>Main Menu</Text>
            <Stack gap={4}>
                {links.map((link) => {
                    const isActive = activePage === link.key
                        || (activePage === "user-views" && link.key === "all-users")
                        || ((activePage === "site-detail" || activePage === "site-config" || activePage === "site-cache") && link.key === "sites")
                        || (activePage === "access-logs" && /^\/Admin\/\d+\/AccessLog$/.test(currentPathname) && link.key === "sites");
                    if (link.key === "crawler-cache") {
                        return <NavLink
                            key="cache-group"
                            href="/Admin/CrawlerCache"
                            label="Cache"
                            leftSection={<i className="fas fa-database" aria-hidden="true" />}
                            active={isActive}
                            opened
                            variant={isActive ? "filled" : "light"}
                            onClick={(event) => {
                                event.preventDefault();
                                onNavigate("/Admin/CrawlerCache");
                            }}
                        >
                            <NavLink
                                href={link.href}
                                label={link.label}
                                leftSection={<i className={link.iconClassName} aria-hidden="true" />}
                                active={isActive}
                                variant={isActive ? "filled" : "subtle"}
                                onClick={(event) => {
                                    event.preventDefault();
                                    onNavigate(link.href);
                                }}
                            />
                        </NavLink>;
                    }
                    return <NavLink
                        key={link.key}
                        href={link.href}
                        label={link.label}
                        leftSection={<i className={link.iconClassName} aria-hidden="true" />}
                        active={isActive}
                        variant={isActive ? "filled" : "light"}
                        onClick={(event) => {
                            event.preventDefault();
                            onNavigate(link.href);
                        }}
                    />;
                })}
            </Stack>
        </Paper>

        <Paper withBorder radius="md" p={8}>
            <Group justify="space-between" mb={6}>
                <Text size="xs" c="dimmed" fw={700} tt="uppercase">Sites</Text>
                <Badge color="indigo" variant="light" size="sm">{siteLinks.length}</Badge>
            </Group>
            <Stack gap={2}>
                {siteLinks.map((site) => <NavLink
                    key={`site-${site.seq}`}
                    href={`/Admin/Site/${site.seq}`}
                    label={`${site.name} (#${site.seq})`}
                    leftSection={<i className="fas fa-globe-asia" aria-hidden="true" />}
                    active={currentSiteSeq === String(site.seq)}
                    opened
                    variant={currentSiteSeq === String(site.seq) ? "filled" : "light"}
                    onClick={(event) => {
                        event.preventDefault();
                        onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}`);
                    }}
                >
                    <NavLink
                        href={`/Admin/Site/${site.seq}/Config`}
                        label="Config"
                        leftSection={<i className="fas fa-sliders-h" aria-hidden="true" />}
                        active={currentPathname === `/Admin/Site/${site.seq}/Config`}
                        variant={currentPathname === `/Admin/Site/${site.seq}/Config` ? "filled" : "subtle"}
                        onClick={(event) => {
                            event.preventDefault();
                            onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}/Config`);
                        }}
                    />
                    <NavLink
                        href={`/Admin/Site/${site.seq}/Cache`}
                        label="Cache"
                        leftSection={<i className="fas fa-database" aria-hidden="true" />}
                        active={currentPathname === `/Admin/Site/${site.seq}/Cache`}
                        variant={currentPathname === `/Admin/Site/${site.seq}/Cache` ? "filled" : "subtle"}
                        onClick={(event) => {
                            event.preventDefault();
                            onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}/Cache`);
                        }}
                    />
                    <NavLink
                        href={`/Admin/${site.seq}/AccessLog`}
                        label="AccessLog"
                        leftSection={<i className="fas fa-network-wired" aria-hidden="true" />}
                        active={currentPathname === `/Admin/${site.seq}/AccessLog`}
                        variant={currentPathname === `/Admin/${site.seq}/AccessLog` ? "filled" : "subtle"}
                        onClick={(event) => {
                            event.preventDefault();
                            onNavigate(`/Admin/${encodeURIComponent(site.seq)}/AccessLog`);
                        }}
                    />

                </NavLink>)}
                {siteLinks.length === 0 && <Text size="sm" c="dimmed" px="sm" py={6}>등록된 Site 가 없습니다.</Text>}
            </Stack>
        </Paper>

        <Divider my={6} label="바로가기" labelPosition="center" />
        <Paper withBorder radius="md" p={6}>
            <NavLink
                href="/"
                label="위키로 돌아가기"
                description="관리자 영역을 나가 메인 위키로 이동"
                leftSection={<i className="fas fa-arrow-left" aria-hidden="true" />}
                rightSection={<i className="fas fa-external-link-alt" aria-hidden="true" />}
                color="gray"
                variant="subtle"
            />
        </Paper>
    </Stack>;
}
