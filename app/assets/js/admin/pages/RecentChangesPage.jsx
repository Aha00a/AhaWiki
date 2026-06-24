import React, {useEffect, useMemo, useState} from "react";
import {Anchor, Badge, Button, Card, Group, Switch, Text, TextInput, Title} from "@mantine/core";
import {useRecentChangesData} from "../hooks/useRecentChangesData.js";
import {makeTable} from "../../component/commonWidgets.jsx";
import {resolveSiteUrl} from "../utils.js";

export default function RecentChangesPage() {
    const {loading, recentChanges, sites, loadRecentChanges, loadSites} = useRecentChangesData();
    const [limitInput, setLimitInput] = useState("50");
    const [showBotEdits, setShowBotEdits] = useState(true);

    useEffect(() => { loadSites(); loadRecentChanges(50); }, []);

    const siteDomainBySeq = useMemo(() => new Map(sites.map((s) => [s.seq, (s.domains ?? []).find((d) => !!d) ?? ""])), [sites]);
    const visibleRecentChanges = useMemo(
        () => recentChanges.filter((row) => showBotEdits || !row.viaApi),
        [recentChanges, showBotEdits],
    );

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>Recent Changes (All Sites)</Title>
                <Badge color="violet" variant="light">{visibleRecentChanges.length} rows</Badge>
            </Group>
            <Text size="sm" c="dimmed" mb="md">사이트 전체 최근 변경 기록을 n개 단위로 조회할 수 있습니다.</Text>
            <Group align="flex-end" mb="md">
                <TextInput label="조회 개수 n" value={limitInput} onChange={(e) => setLimitInput(e.currentTarget.value)} placeholder="1 ~ 500"/>
                <Switch label="Bot edit 포함" checked={showBotEdits} onChange={(e) => setShowBotEdits(e.currentTarget.checked)}/>
                <Button variant="filled" onClick={() => {
                    const parsed = Number.parseInt(limitInput, 10);
                    const n = Number.isFinite(parsed) ? Math.min(500, Math.max(1, parsed)) : 50;
                    setLimitInput(String(n));
                    loadRecentChanges(n);
                }}>조회</Button>
            </Group>
            {makeTable(["When", "Site", "Page", "Revision", "Editor", "Bot", "Comment", "IP"], visibleRecentChanges.map((row) => {
                const siteUrl = resolveSiteUrl(row, siteDomainBySeq);
                const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(row.name)}` : "";
                const revisionUrl = pageUrl ? `${pageUrl}?rev=${row.revision}` : "";
                const editorUrl = row.nickname ? `/Admin/User?query=${encodeURIComponent(row.nickname)}` : "";
                return [row.dateTime, siteUrl ? <Anchor href={siteUrl} target="_blank">{row.siteName} (#{row.siteSeq})</Anchor> : `${row.siteName} (#${row.siteSeq})`, pageUrl ? <Anchor href={pageUrl} target="_blank">{row.name}</Anchor> : row.name, revisionUrl ? <Anchor href={revisionUrl} target="_blank">{row.revision}</Anchor> : row.revision, editorUrl ? <Anchor href={editorUrl}>{row.nickname}</Anchor> : (row.nickname ?? "-"), row.viaApi ? "Y" : "N", row.comment || "-", row.remoteAddress];
            }))}
        </Card>
    );
}
