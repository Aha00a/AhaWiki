import React, {useEffect} from "react";
import {useOutletContext} from "react-router-dom";
import {Badge, Button, Card, Group, Stack, Table, Text, Title} from "@mantine/core";
import {useSiteCacheData} from "../hooks/useSiteCacheData.js";

export default function SiteCachePage() {
    const {site, siteSeq} = useOutletContext();
    const {clearing, memoryCacheStats, error, clearSiteCache, loadMemoryCacheStats} = useSiteCacheData(siteSeq);

    useEffect(() => { loadMemoryCacheStats(); }, [siteSeq]);

    return (
        <>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>Operation · Site Cache Operation</Title>
                    <Badge color="orange" variant="light">유지보수</Badge>
                </Group>
                <Text size="sm" c="dimmed" mb="md">현재 보고 있는 사이트의 캐시를 즉시 초기화합니다.</Text>
                {error ? <Text c="red" size="sm" mb="md">{error}</Text> : null}
                <Group justify="space-between" align="center">
                    <Stack gap={2}>
                        <Text size="xs" c="dimmed">대상 사이트</Text>
                        <Text fw={700}>{site ? `${site.name} (#${site.seq})` : "선택된 사이트 없음"}</Text>
                    </Stack>
                    <Button color="orange" variant="filled" disabled={!site} loading={clearing} onClick={() => clearSiteCache()}>Clear current site cache</Button>
                </Group>
            </Card>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md">
                    <Title order={3}>Memory Cache Status (All Instances)</Title>
                    <Button variant="light" onClick={loadMemoryCacheStats}>Refresh</Button>
                </Group>
                <Table striped highlightOnHover withTableBorder withColumnBorders>
                    <Table.Thead>
                        <Table.Tr><Table.Th>Port</Table.Th><Table.Th>Key Count</Table.Th><Table.Th>Value Count</Table.Th><Table.Th>Captured At</Table.Th></Table.Tr>
                    </Table.Thead>
                    <Table.Tbody>
                        {memoryCacheStats.map((row) => (
                            <Table.Tr key={String(row.instancePort)}>
                                <Table.Td>{row.instancePort}</Table.Td>
                                <Table.Td>{row.stats?.linksCacheKeyCount ?? 0}</Table.Td>
                                <Table.Td>{row.stats?.linksCacheValueCount ?? 0}</Table.Td>
                                <Table.Td>{row.stats?.capturedAtIso8601 ?? "-"}</Table.Td>
                            </Table.Tr>
                        ))}
                    </Table.Tbody>
                </Table>
            </Card>
        </>
    );
}
