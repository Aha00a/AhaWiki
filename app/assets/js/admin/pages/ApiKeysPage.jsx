import React, {useEffect, useState} from "react";
import {Badge, Button, Card, Group, Text, Title} from "@mantine/core";
import {DataTable} from "mantine-datatable";
import {fetchCsrfToken, fetchJson} from "../api.js";
import {formatDateTimeInClientTimezone} from "../utils.js";

export default function ApiKeysPage() {
    const [loading, setLoading] = useState(false);
    const [revokingSeq, setRevokingSeq] = useState(null);
    const [apiKeys, setApiKeys] = useState([]);
    const [error, setError] = useState("");

    const loadApiKeys = async () => {
        setLoading(true);
        setError("");
        try {
            const data = await fetchJson("/api/Admin/ApiKeys");
            setApiKeys(Array.isArray(data) ? data : []);
        } catch (err) {
            setError(err.message || String(err));
            setApiKeys([]);
        } finally {
            setLoading(false);
        }
    };

    const revokeApiKey = async (seq) => {
        if (!window.confirm(`Revoke API key #${seq}?`)) return;
        setRevokingSeq(seq);
        setError("");
        try {
            const csrf = await fetchCsrfToken();
            const response = await fetch(`/api/Admin/ApiKeys/${encodeURIComponent(seq)}`, {
                method: "DELETE",
                credentials: "same-origin",
                headers: {"Csrf-Token": csrf.value, "X-CSRF-Token": csrf.value},
            });
            const data = await response.json();
            if (!response.ok) throw new Error(data?.error || `HTTP ${response.status}`);
            await loadApiKeys();
        } catch (err) {
            setError(err.message || String(err));
        } finally {
            setRevokingSeq(null);
        }
    };

    useEffect(() => { loadApiKeys(); }, []);

    const activeCount = apiKeys.filter((row) => !row.revoked).length;

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>API Keys</Title>
                <Group gap={8}>
                    <Badge color="green" variant="light">{activeCount} active</Badge>
                    <Badge color="gray" variant="light">{apiKeys.length} total</Badge>
                </Group>
            </Group>
            {error ? <Text c="red" size="sm" mb="md">{error}</Text> : null}
            <Group align="flex-end" mb="md">
                <Button variant="filled" loading={loading} onClick={loadApiKeys} leftSection={<i className="fas fa-sync-alt" aria-hidden="true"/>}>Refresh</Button>
            </Group>
            <DataTable
                withTableBorder
                striped
                highlightOnHover
                fetching={loading}
                records={apiKeys}
                minHeight={360}
                columns={[
                    {accessor: "seq", title: "ID", width: 80},
                    {accessor: "userNickname", title: "User", render: (row) => row.userNickname ? `${row.userNickname} (#${row.user})` : `#${row.user}`},
                    {accessor: "name", title: "Name"},
                    {accessor: "keyPrefix", title: "Key", render: (row) => <code>{row.keyPrefix}</code>},
                    {accessor: "dateInserted", title: "Created", render: (row) => formatDateTimeInClientTimezone(row.dateInserted)},
                    {accessor: "dateLastUsed", title: "Last Used", render: (row) => row.dateLastUsed ? formatDateTimeInClientTimezone(row.dateLastUsed) : "-"},
                    {accessor: "status", title: "Status", render: (row) => row.revoked ? <Badge color="red" variant="light">Revoked</Badge> : <Badge color="green" variant="light">Active</Badge>},
                    {accessor: "action", title: "Action", render: (row) => row.revoked ? "-" : <Button size="xs" color="red" variant="light" loading={revokingSeq === row.seq} onClick={() => revokeApiKey(row.seq)}>Revoke</Button>},
                ]}
            />
        </Card>
    );
}
