import React, {useCallback, useEffect, useState} from "react";
import {Badge, Button, Card, Group, SegmentedControl, Text, Title} from "@mantine/core";
import {DataTable} from "mantine-datatable";
import {fetchJson, sendJson} from "../api.js";
import {siteRequestMailto} from "../siteRequestMail.js";
import {formatDateTimeInClientTimezone} from "../utils.js";

const STATUS_FILTERS = ["Pending", "Approved", "Rejected", "Canceled", "All"];

export default function SiteRequestsPage() {
    const [status, setStatus] = useState("Pending");
    const [rows, setRows] = useState([]);
    const [loading, setLoading] = useState(false);
    const [busySeq, setBusySeq] = useState(null);
    const [error, setError] = useState("");

    const load = useCallback(async () => {
        setLoading(true);
        setError("");
        try {
            const data = await fetchJson(`/api/Admin/SiteRequests?status=${encodeURIComponent(status)}`);
            setRows(Array.isArray(data) ? data : []);
        } catch (err) {
            setError(err.message || String(err));
            setRows([]);
        } finally {
            setLoading(false);
        }
    }, [status]);

    useEffect(() => { load(); }, [load]);

    const review = async (row, decision) => {
        let body;
        if (decision === "reject") {
            const reason = window.prompt(`Why is ${row.domain} rejected?`, "");
            if (reason === null || reason.trim() === "") return;
            body = {rejectReason: reason.trim()};
        } else if (!window.confirm(`Create ${row.domain} for ${row.userNickname ?? `#${row.user}`}?`)) {
            return;
        }

        setBusySeq(row.seq);
        setError("");
        try {
            await sendJson(`/api/Admin/SiteRequests/${encodeURIComponent(row.seq)}/${decision}`, "POST", body);
            await load();
        } catch (err) {
            setError(err.message || String(err));
        } finally {
            setBusySeq(null);
        }
    };

    const statusBadge = (row) => {
        const color = {Pending: "yellow", Approved: "green", Rejected: "red", Canceled: "gray"}[row.status] ?? "gray";
        return <Badge color={color} variant="light">{row.status}</Badge>;
    };

    // The address is the Google account that becomes the wiki's administrator, so it is worth
    // seeing before approving, and it is where the answer goes afterwards.
    const requester = (row) => (
        <>
            {row.userNickname ? `${row.userNickname} (#${row.user})` : `#${row.user}`}
            {row.email ? <Text size="xs" c="dimmed">{row.email}</Text> : null}
        </>
    );

    const action = (row) => {
        if (row.status === "Pending") {
            return (
                <Group gap={6}>
                    <Button size="xs" variant="light" color="green" loading={busySeq === row.seq} onClick={() => review(row, "approve")}>Approve</Button>
                    <Button size="xs" variant="light" color="red" loading={busySeq === row.seq} onClick={() => review(row, "reject")}>Reject</Button>
                </Group>
            );
        }
        // The app sends nothing to the requester. This opens the owner's own mail program with
        // the letter written (siteRequestMail.js).
        const mailto = siteRequestMailto(row);
        return mailto
            ? <Button component="a" href={mailto} size="xs" variant="light" title={`Write to ${row.email}`} leftSection={<i className="fas fa-envelope" aria-hidden="true"/>}>Email</Button>
            : "-";
    };

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>Site Requests</Title>
                <Badge color="grape" variant="light">{rows.length} rows</Badge>
            </Group>
            {error ? <Text c="red" size="sm" mb="md">{error}</Text> : null}
            <Group align="flex-end" mb="md">
                <SegmentedControl value={status} onChange={setStatus} data={STATUS_FILTERS}/>
                <Button variant="filled" loading={loading} onClick={load} leftSection={<i className="fas fa-sync-alt" aria-hidden="true"/>}>Refresh</Button>
            </Group>
            <DataTable
                withTableBorder
                striped
                highlightOnHover
                fetching={loading}
                records={rows}
                minHeight={360}
                idAccessor="seq"
                columns={[
                    {accessor: "seq", title: "ID", width: 80},
                    {accessor: "userNickname", title: "User", render: requester},
                    {accessor: "name", title: "Name"},
                    {accessor: "domain", title: "Address"},
                    {accessor: "isPublic", title: "Reading", render: (row) => row.isPublic ? "Public" : "Private"},
                    {accessor: "purpose", title: "Purpose"},
                    {accessor: "status", title: "Status", render: statusBadge},
                    {accessor: "rejectReason", title: "Reason", render: (row) => row.rejectReason || "-"},
                    {accessor: "dateInserted", title: "Asked", render: (row) => formatDateTimeInClientTimezone(row.dateInserted)},
                    {accessor: "action", title: "Action", render: action},
                ]}
            />
        </Card>
    );
}
