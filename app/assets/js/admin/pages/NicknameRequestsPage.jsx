import React, {useCallback, useEffect, useState} from "react";
import {Badge, Button, Card, Group, SegmentedControl, Text, Title} from "@mantine/core";
import {DataTable} from "mantine-datatable";
import {fetchJson, sendJson} from "../api.js";
import {formatDateTimeInClientTimezone} from "../utils.js";

const STATUS_FILTERS = ["Pending", "Approved", "Rejected", "Canceled", "All"];

export default function NicknameRequestsPage() {
    const [status, setStatus] = useState("Pending");
    const [rows, setRows] = useState([]);
    const [loading, setLoading] = useState(false);
    const [busySeq, setBusySeq] = useState(null);
    const [error, setError] = useState("");

    const load = useCallback(async () => {
        setLoading(true);
        setError("");
        try {
            const data = await fetchJson(`/api/Admin/NicknameRequests?status=${encodeURIComponent(status)}`);
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
        // The reason is the whole point of a rejection, so a rejection without one is not
        // sent. Approval asks for confirmation because it renames somebody.
        let body;
        if (decision === "reject") {
            const reason = window.prompt(`Why is "${row.requestedNickname}" rejected?`, "");
            if (reason === null || reason.trim() === "") return;
            body = {rejectReason: reason.trim()};
        } else if (!window.confirm(`Rename ${row.userNickname ?? `#${row.user}`} to "${row.requestedNickname}"?`)) {
            return;
        }

        setBusySeq(row.seq);
        setError("");
        try {
            await sendJson(`/api/Admin/NicknameRequests/${encodeURIComponent(row.seq)}/${decision}`, "POST", body);
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

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>Nickname Requests</Title>
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
                columns={[
                    {accessor: "seq", title: "ID", width: 80},
                    // The requester's name as the database has it now, which is what the
                    // response carries — not whatever their session still says.
                    {accessor: "userNickname", title: "User", render: (row) => row.userNickname ? `${row.userNickname} (#${row.user})` : `#${row.user}`},
                    {accessor: "requestedNickname", title: "Requested"},
                    {accessor: "status", title: "Status", render: statusBadge},
                    {accessor: "rejectReason", title: "Reason", render: (row) => row.rejectReason || "-"},
                    {accessor: "dateInserted", title: "Asked", render: (row) => formatDateTimeInClientTimezone(row.dateInserted)},
                    {accessor: "dateReviewed", title: "Answered", render: (row) => row.dateReviewed ? formatDateTimeInClientTimezone(row.dateReviewed) : "-"},
                    {
                        accessor: "action",
                        title: "Action",
                        render: (row) => row.status !== "Pending" ? "-" : (
                            <Group gap={6}>
                                <Button size="xs" variant="light" color="green" loading={busySeq === row.seq} onClick={() => review(row, "approve")}>Approve</Button>
                                <Button size="xs" variant="light" color="red" loading={busySeq === row.seq} onClick={() => review(row, "reject")}>Reject</Button>
                            </Group>
                        ),
                    },
                ]}
            />
        </Card>
    );
}
