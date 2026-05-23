import React from "react";
import {useOutletContext} from "react-router-dom";
import {Badge, Card, Group, Stack, Text, Title} from "@mantine/core";

export default function SiteDashboardPage() {
    const {site, sitePageNames} = useOutletContext();

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>Dashboard</Title>
                <Badge color="indigo" variant="light">Site</Badge>
            </Group>
            <Stack gap="sm">
                <Group>
                    <Text size="sm" c="dimmed" w={120}>사이트 이름</Text>
                    <Text fw={500}>{site?.name ?? "-"}</Text>
                </Group>
                <Group>
                    <Text size="sm" c="dimmed" w={120}>도메인</Text>
                    <Text fw={500}>{(site?.domains ?? []).join(", ") || "-"}</Text>
                </Group>
                <Group>
                    <Text size="sm" c="dimmed" w={120}>페이지 수</Text>
                    <Text fw={500}>{sitePageNames ? sitePageNames.length : "-"}</Text>
                </Group>
            </Stack>
        </Card>
    );
}
