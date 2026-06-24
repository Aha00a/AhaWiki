import React, {useEffect, useMemo, useState} from "react";
import {Anchor, Badge, Button, Card, Group, Switch, Text, TextInput, Title} from "@mantine/core";
import {useRecentChangesData} from "../hooks/useRecentChangesData.js";
import {makeFlagCell, makeFlagHeader, makeFlagLegend, makeTable} from "../../component/commonWidgets.jsx";
import {resolveSiteUrl} from "../utils.js";

const recentChangeFlagLegend = [
    {label: "Minor edit", symbol: "✏️"},
    {label: "Via API", symbol: "🔌"},
];

export default function RecentChangesPage() {
    const {loading, recentChanges, sites, loadRecentChanges, loadSites} = useRecentChangesData();
    const [limitInput, setLimitInput] = useState("50");
    const [showMinorEdits, setShowMinorEdits] = useState(false);
    const [showViaApiEdits, setShowViaApiEdits] = useState(false);

    useEffect(() => { loadSites(); loadRecentChanges(50, false, false); }, []);

    const siteDomainBySeq = useMemo(() => new Map(sites.map((s) => [s.seq, (s.domains ?? []).find((d) => !!d) ?? ""])), [sites]);

    const normalizeLimit = () => {
        const parsed = Number.parseInt(limitInput, 10);
        const n = Number.isFinite(parsed) ? Math.min(500, Math.max(1, parsed)) : 50;
        setLimitInput(String(n));
        return n;
    };

    const reloadRecentChanges = (includeMinorEdit = showMinorEdits, includeViaApi = showViaApiEdits) => {
        loadRecentChanges(normalizeLimit(), includeMinorEdit, includeViaApi);
    };

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>Recent Changes (All Sites)</Title>
                <Badge color="violet" variant="light">{recentChanges.length} rows</Badge>
            </Group>
            <Text size="sm" c="dimmed" mb="md">사이트 전체 최근 변경 기록을 n개 단위로 조회할 수 있습니다.</Text>
            <Group align="flex-end" mb="md">
                <TextInput label="조회 개수 n" value={limitInput} onChange={(e) => setLimitInput(e.currentTarget.value)} placeholder="1 ~ 500"/>
                <Switch label="Minor edit 포함" checked={showMinorEdits} onChange={(e) => {
                    const next = e.currentTarget.checked;
                    setShowMinorEdits(next);
                    reloadRecentChanges(next, showViaApiEdits);
                }}/>
                <Switch label="viaApi 포함" checked={showViaApiEdits} onChange={(e) => {
                    const next = e.currentTarget.checked;
                    setShowViaApiEdits(next);
                    reloadRecentChanges(showMinorEdits, next);
                }}/>
                <Button variant="filled" onClick={() => {
                    reloadRecentChanges();
                }}>조회</Button>
            </Group>
            {makeFlagLegend(recentChangeFlagLegend)}
            {makeTable(["When", "Site", "Page", "Revision", "Editor", makeFlagHeader("Minor edit", "✏️"), makeFlagHeader("Via API", "🔌"), "Comment", "IP"], recentChanges.map((row) => {
                const siteUrl = resolveSiteUrl(row, siteDomainBySeq);
                const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(row.name)}` : "";
                const revisionUrl = pageUrl ? `${pageUrl}?rev=${row.revision}` : "";
                const editorUrl = row.nickname ? `/Admin/User?query=${encodeURIComponent(row.nickname)}` : "";
                return [row.dateTime, siteUrl ? <Anchor href={siteUrl} target="_blank">{row.siteName} (#{row.siteSeq})</Anchor> : `${row.siteName} (#${row.siteSeq})`, pageUrl ? <Anchor href={pageUrl} target="_blank">{row.name}</Anchor> : row.name, revisionUrl ? <Anchor href={revisionUrl} target="_blank">{row.revision}</Anchor> : row.revision, editorUrl ? <Anchor href={editorUrl}>{row.nickname}</Anchor> : (row.nickname ?? "-"), makeFlagCell(row.isMinorEdit, "Minor edit", "✏️"), makeFlagCell(row.viaApi, "Via API", "🔌"), row.comment || "-", row.remoteAddress];
            }))}
        </Card>
    );
}
