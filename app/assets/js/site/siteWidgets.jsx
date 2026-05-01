import React from "react";
import {Badge, Button, Card, Divider, Group, Table, Text, Title} from "@mantine/core";

export function SiteListCard({sites, onNavigate}) {
    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>Site List</Title>
                <Badge color="indigo" variant="light">{sites.length} sites</Badge>
            </Group>
            <Text size="sm" c="dimmed" mb="md">전체 사이트 목록입니다. 상세 설정은 각 사이트의 관리 버튼으로 이동하세요.</Text>
            <Divider mb="md"/>
            <Table striped highlightOnHover withTableBorder withColumnBorders>
                <Table.Thead><Table.Tr><Table.Th>Seq</Table.Th><Table.Th>Name</Table.Th><Table.Th>Domains</Table.Th><Table.Th>Users</Table.Th><Table.Th>Pages</Table.Th><Table.Th>Action</Table.Th></Table.Tr></Table.Thead>
                <Table.Tbody>
                    {sites.map((site) => (
                        <Table.Tr key={site.seq}>
                            <Table.Td>{site.seq}</Table.Td><Table.Td>{site.name}</Table.Td><Table.Td>{(site.domains ?? []).join(", ") || "-"}</Table.Td><Table.Td>{site.userCount ?? 0}</Table.Td><Table.Td>{site.pageCount ?? 0}</Table.Td>
                            <Table.Td><Button size="xs" variant="light" onClick={() => onNavigate(`/Admin/Site/${encodeURIComponent(site.seq)}`)}>관리</Button></Table.Td>
                        </Table.Tr>
                    ))}
                </Table.Tbody>
            </Table>
        </Card>
    );
}
