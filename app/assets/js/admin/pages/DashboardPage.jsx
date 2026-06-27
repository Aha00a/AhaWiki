import React, {useEffect, useMemo} from "react";
import {useNavigate} from "react-router-dom";
import {Anchor, Badge, Button, Card, Group, SimpleGrid, Stack, Text, ThemeIcon, Title} from "@mantine/core";
import {useAdminContext} from "../context/AdminContext.jsx";
import {useDashboardData} from "../hooks/useDashboardData.js";
import {SiteListCard} from "../../site/siteWidgets.jsx";
import {DailyStatTable, MultiTrendChart, StatTrendCard} from "../mainWidgets.jsx";
import {makeFlagCell, makeFlagHeader, makeFlagLegend, makeTable} from "../../component/commonWidgets.jsx";
import {resolveSiteUrl} from "../utils.js";

const recentChangeFlagLegend = [
    {label: "Minor edit", symbol: "✏️"},
    {label: "Via API", symbol: "🔌"},
];

export default function DashboardPage() {
    const {me} = useAdminContext();
    const navigate = useNavigate();
    const {loading, error, sites, allUsers, dailyStats, recentChanges, topViewedPages, memoryCacheStats, load, loadSitesOnly, loadMemoryCacheStats} = useDashboardData();

    useEffect(() => {
        if (me === null) return;
        if (me?.isAdmin) {
            load();
            loadMemoryCacheStats();
        } else {
            loadSitesOnly();
        }
    }, [me, load, loadMemoryCacheStats, loadSitesOnly]);

    const siteDomainBySeq = useMemo(() => new Map(sites.map((s) => [s.seq, (s.domains ?? []).find((d) => !!d) ?? ""])), [sites]);

    if (loading) return null;

    if (!me?.isAdmin) {
        return (
            <Stack gap="md">
                <Card withBorder radius="md" padding="lg">
                    <Title order={3} mb="xs">내 담당 사이트</Title>
                    <Text size="sm" c="dimmed" mb="md">SiteAdmin으로 등록된 사이트 목록입니다.</Text>
                    <SimpleGrid cols={{base: 1, sm: 2}} spacing="md">
                        {sites.map((site) => <SiteListCard key={site.seq} sites={[site]} onNavigate={(href) => navigate(href)}/>)}
                    </SimpleGrid>
                    {sites.length === 0 && <Text c="dimmed" size="sm">담당 사이트가 없습니다.</Text>}
                </Card>
            </Stack>
        );
    }

    return (
        <Stack gap="lg">
            <SimpleGrid cols={{base: 1, sm: 2, lg: 3}} spacing="md">
                <Card withBorder radius="md" padding="md">
                    <Group justify="space-between" align="flex-start">
                        <Stack gap={2}><Text size="sm" c="dimmed">Sites</Text><Title order={2}>{sites.length}</Title></Stack>
                        <ThemeIcon color="indigo" variant="light" radius="xl">S</ThemeIcon>
                    </Group>
                </Card>
                <Card withBorder radius="md" padding="md">
                    <Group justify="space-between" align="flex-start">
                        <Stack gap={2}><Text size="sm" c="dimmed">All Users</Text><Title order={2}>{allUsers.length}</Title></Stack>
                        <ThemeIcon color="teal" variant="light" radius="xl">U</ThemeIcon>
                    </Group>
                </Card>
                <Card withBorder radius="md" padding="md">
                    <Group justify="space-between" align="flex-start">
                        <Stack gap={2}><Text size="sm" c="dimmed">30일 문서 수정</Text><Title order={2}>{dailyStats.pageEdited.reduce((sum, item) => sum + (item.count ?? 0), 0)}</Title></Stack>
                        <ThemeIcon color="grape" variant="light" radius="xl">E</ThemeIcon>
                    </Group>
                </Card>
            </SimpleGrid>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md"><Title order={3}>빠른 이동</Title><Badge color="indigo" variant="light">Quick Access</Badge></Group>
                <SimpleGrid cols={{base: 1, sm: 3}} spacing="sm">
                    <Button variant="light" onClick={() => navigate("/Admin/RecentChange")}>최근 변경 보기</Button>
                    <Button variant="light" onClick={() => navigate("/Admin/User")}>사용자 목록 보기</Button>
                </SimpleGrid>
            </Card>
            <SimpleGrid cols={{base: 1, sm: 2, lg: 3}} spacing="md">
                <StatTrendCard title="New Users" color="blue" rows={dailyStats.userCreated} total={dailyStats.userCreated.reduce((sum, item) => sum + (item.count ?? 0), 0)}/>
                <StatTrendCard title="New Pages" color="indigo" rows={dailyStats.pageCreated} total={dailyStats.pageCreated.reduce((sum, item) => sum + (item.count ?? 0), 0)}/>
                <StatTrendCard title="Page Edits" color="grape" rows={dailyStats.pageEdited} total={dailyStats.pageEdited.reduce((sum, item) => sum + (item.count ?? 0), 0)}/>
            </SimpleGrid>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md"><Title order={3}>30일 운영 추이 차트</Title><Badge color="blue" variant="light">Chart</Badge></Group>
                <MultiTrendChart series={[{name: "New Users", color: "blue", rows: dailyStats.userCreated}, {name: "New Pages", color: "indigo", rows: dailyStats.pageCreated}, {name: "Page Edits", color: "grape", rows: dailyStats.pageEdited}]}/>
            </Card>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md"><Title order={3}>Most Viewed Pages</Title><Badge color="pink" variant="light">{Math.min(topViewedPages.length, 30)}</Badge></Group>
                {makeTable(["Rank", "Site", "Page", "Views", "Last Viewed"], topViewedPages.slice(0, 30).map((row, index) => {
                    const siteUrl = resolveSiteUrl(row, siteDomainBySeq);
                    const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(row.pageName)}` : "";
                    return [index + 1, siteUrl ? <Anchor href={siteUrl} target="_blank">{row.siteName} (#{row.siteSeq})</Anchor> : `${row.siteName} (#${row.siteSeq})`, pageUrl ? <Anchor href={pageUrl} target="_blank">{row.pageName}</Anchor> : row.pageName, row.viewCount, row.lastViewedAt];
                }))}
            </Card>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md"><Title order={3}>Recent Changes (All Sites)</Title><Badge color="violet" variant="light">{recentChanges.length}</Badge></Group>
                {makeFlagLegend(recentChangeFlagLegend)}
                {makeTable(["When", "Site", "Page", "Revision", "Editor", makeFlagHeader("Minor edit", "✏️"), makeFlagHeader("Via API", "🔌"), "Comment"], recentChanges.map((row) => {
                    const siteUrl = resolveSiteUrl(row, siteDomainBySeq);
                    const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(row.name)}` : "";
                    const revisionUrl = pageUrl ? `${pageUrl}?rev=${row.revision}` : "";
                    return [row.dateTime, siteUrl ? <Anchor href={siteUrl} target="_blank">{row.siteName} (#{row.siteSeq})</Anchor> : `${row.siteName} (#${row.siteSeq})`, pageUrl ? <Anchor href={pageUrl} target="_blank">{row.name}</Anchor> : row.name, revisionUrl ? <Anchor href={revisionUrl} target="_blank">{row.revision}</Anchor> : row.revision, row.nickname ?? "-", makeFlagCell(row.isMinorEdit, "Minor edit", "✏️"), makeFlagCell(row.viaApi, "Via API", "🔌", row.userApiKeyName), row.comment || "-"];
                }))}
            </Card>
        </Stack>
    );
}
