import React, {useEffect, useMemo, useState} from "react";
import {Badge, Divider, Group, NavLink, Paper, Skeleton, Stack, Text} from "@mantine/core";

function fetchJson(url) {
    return fetch(url, {credentials: "same-origin"}).then((response) => {
        if (!response.ok) throw new Error(`HTTP ${response.status}`);
        return response.json();
    });
}

function parseSiteSeqFromPathname(pathname) {
    const matched = pathname.match(/^\/Admin\/(?:Site\/)?(\d+)(?:\/(?:Config|Cache|Permission|AccessLog|Admins))?$/);
    if (!matched) return "";
    const siteSeq = Number.parseInt(matched[1], 10);
    return Number.isFinite(siteSeq) && siteSeq > 0 ? String(siteSeq) : "";
}

function SiteNavItem({site, currentPathname, currentSiteSeq, isAdmin, onNavigate}) {
    const siteSeq = String(site.seq);
    const isCurrentSite = siteSeq === String(currentSiteSeq);
    const isActiveSite = parseSiteSeqFromPathname(currentPathname) === siteSeq;

    return (
        <NavLink
            href={`/Admin/Site/${site.seq}`}
            label={`${site.name} (#${site.seq})`}
            leftSection={<i className="fas fa-globe-asia" aria-hidden="true"/>}
            active={isActiveSite}
            opened={isCurrentSite || isActiveSite}
            variant={isActiveSite ? "filled" : "light"}
            onClick={(event) => {
                event.preventDefault();
                onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}`);
            }}
        >
            <NavLink
                href={`/Admin/Site/${site.seq}`}
                label="Meta목록"
                leftSection={<i className="fas fa-list" aria-hidden="true"/>}
                active={currentPathname === `/Admin/Site/${site.seq}`}
                variant={currentPathname === `/Admin/Site/${site.seq}` ? "filled" : "subtle"}
                onClick={(event) => {
                    event.preventDefault();
                    onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}`);
                }}
            />
            <NavLink
                href={`/Admin/Site/${site.seq}/Config`}
                label="Config"
                leftSection={<i className="fas fa-sliders-h" aria-hidden="true"/>}
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
                leftSection={<i className="fas fa-database" aria-hidden="true"/>}
                active={currentPathname === `/Admin/Site/${site.seq}/Cache`}
                variant={currentPathname === `/Admin/Site/${site.seq}/Cache` ? "filled" : "subtle"}
                onClick={(event) => {
                    event.preventDefault();
                    onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}/Cache`);
                }}
            />
            <NavLink
                href={`/Admin/Site/${site.seq}/Permission`}
                label="Permission"
                leftSection={<i className="fas fa-key" aria-hidden="true"/>}
                active={currentPathname === `/Admin/Site/${site.seq}/Permission`}
                variant={currentPathname === `/Admin/Site/${site.seq}/Permission` ? "filled" : "subtle"}
                onClick={(event) => {
                    event.preventDefault();
                    onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}/Permission`);
                }}
            />
            {isAdmin && (
                <NavLink
                    href={`/Admin/${site.seq}/AccessLog`}
                    label="AccessLog"
                    leftSection={<i className="fas fa-network-wired" aria-hidden="true"/>}
                    active={currentPathname === `/Admin/${site.seq}/AccessLog`}
                    variant={currentPathname === `/Admin/${site.seq}/AccessLog` ? "filled" : "subtle"}
                    onClick={(event) => {
                        event.preventDefault();
                        onNavigate(`/Admin/${encodeURIComponent(site.seq)}/AccessLog`);
                    }}
                />
            )}
            {isAdmin && (
                <NavLink
                    href={`/Admin/Site/${site.seq}/Admins`}
                    label="SiteAdmins"
                    leftSection={<i className="fas fa-user-shield" aria-hidden="true"/>}
                    active={currentPathname === `/Admin/Site/${site.seq}/Admins`}
                    variant={currentPathname === `/Admin/Site/${site.seq}/Admins` ? "filled" : "subtle"}
                    onClick={(event) => {
                        event.preventDefault();
                        onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}/Admins`);
                    }}
                />
            )}
        </NavLink>
    );
}

export default function Navigation({activePage, onNavigate, me}) {
    const [siteLinks, setSiteLinks] = useState([]);
    const currentPathname = window.location.pathname;

    useEffect(() => {
        if (!me?.loggedIn) return;
        let mounted = true;
        fetchJson("/api/Admin/Sites")
            .then((siteData) => {
                if (mounted) setSiteLinks(Array.isArray(siteData) ? siteData : []);
            })
            .catch((err) => {
                console.error("[AdminUI] navigation:sites:error", err);
            });
        return () => { mounted = false; };
    }, [me]);

    const globalLinks = useMemo(() => [
        {href: "/Admin", label: "Dashboard", key: "dashboard", iconClassName: "fas fa-chart-line"},
        {href: "/Admin/User", label: "User", key: "all-users", iconClassName: "fas fa-users"},
        {href: "/Admin/RecentChange", label: "RecentChanges", key: "recent-changes", iconClassName: "fas fa-history"},
        {href: "/Admin/AccessLog", label: "AccessLog", key: "access-logs", iconClassName: "fas fa-network-wired"},
        {href: "/Admin/CrawlerCache", label: "Crawler Cache", key: "crawler-cache", iconClassName: "fas fa-spider"},
        {href: "/Admin/S3", label: "S3 Browser", key: "s3-browser", iconClassName: "fas fa-folder-open"},
    ], []);

    if (!me) {
        return (
            <Stack gap={8}>
                <Paper withBorder radius="md" p={8}>
                    <Skeleton height={12} mb={8} radius="sm"/>
                    {[1, 2, 3].map((i) => <Skeleton key={i} height={32} mb={4} radius="sm"/>)}
                </Paper>
            </Stack>
        );
    }

    const isAdmin = me?.isAdmin ?? false;
    const currentSiteSeq = me?.currentSiteSeq ?? "";

    return (
        <Stack gap={8}>
            <Paper withBorder radius="md" p={8}>
                <Group justify="space-between" mb={6}>
                    <Text size="xs" c="dimmed" fw={700} tt="uppercase">Sites</Text>
                    {isAdmin && <Badge color="indigo" variant="light" size="sm">{siteLinks.length}</Badge>}
                </Group>
                <Stack gap={2}>
                    {siteLinks.map((site) => (
                        <SiteNavItem
                            key={`site-${site.seq}`}
                            site={site}
                            currentPathname={currentPathname}
                            currentSiteSeq={currentSiteSeq}
                            isAdmin={isAdmin}
                            onNavigate={onNavigate}
                        />
                    ))}
                    {siteLinks.length === 0 && (
                        <Text size="sm" c="dimmed" px="sm" py={6}>등록된 Site 가 없습니다.</Text>
                    )}
                </Stack>
            </Paper>

            {isAdmin && (
                <Paper withBorder radius="md" p={8}>
                    <Text size="xs" c="dimmed" fw={700} tt="uppercase" mb={6}>전체 관리</Text>
                    <Stack gap={4}>
                        {globalLinks.map((link) => {
                            const isActive = activePage === link.key
                                || (activePage === "user-views" && link.key === "all-users");
                            return (
                                <NavLink
                                    key={link.key}
                                    href={link.href}
                                    label={link.label}
                                    leftSection={<i className={link.iconClassName} aria-hidden="true"/>}
                                    active={isActive}
                                    variant={isActive ? "filled" : "light"}
                                    onClick={(event) => {
                                        event.preventDefault();
                                        onNavigate(link.href);
                                    }}
                                />
                            );
                        })}
                    </Stack>
                </Paper>
            )}

            <Divider my={6} label="바로가기" labelPosition="center"/>
            <Paper withBorder radius="md" p={6}>
                <NavLink
                    href="/"
                    label="위키로 돌아가기"
                    description="관리자 영역을 나가 메인 위키로 이동"
                    leftSection={<i className="fas fa-arrow-left" aria-hidden="true"/>}
                    rightSection={<i className="fas fa-external-link-alt" aria-hidden="true"/>}
                    color="gray"
                    variant="subtle"
                />
            </Paper>
        </Stack>
    );
}
