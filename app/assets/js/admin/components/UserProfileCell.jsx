import React from "react";
import {Avatar, Group, Stack, Text} from "@mantine/core";

export default function UserProfileCell({user}) {
    const fallbackText = user?.nickname?.[0] || user?.email?.[0] || "?";
    return (
        <Group gap={8} wrap="nowrap">
            <Avatar src={user?.profileImageUrl || null} radius="xl" size="sm">{fallbackText}</Avatar>
            <Stack gap={0}>
                <Text size="sm" fw={600}>{user?.nickname || "-"}</Text>
                <Text size="xs" c="dimmed">{user?.email || "-"}</Text>
            </Stack>
        </Group>
    );
}
