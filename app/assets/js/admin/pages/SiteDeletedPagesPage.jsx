import React, {useEffect, useState} from "react";
import {useOutletContext} from "react-router-dom";
import {Badge, Button, Code, Group, Modal, ScrollArea, Text} from "@mantine/core";
import {DataTable} from "mantine-datatable";
import {fetchJson, logError, sendJson} from "../api.js";
import {formatDateTimeInClientTimezone} from "../utils.js";

// Deleted pages are kept until a site admin restores them or deletes them for good
// (PageTrashLogic). Nothing removes them on a schedule.
export default function SiteDeletedPagesPage() {
    const {siteSeq} = useOutletContext();
    const [rows, setRows] = useState([]);
    const [loading, setLoading] = useState(false);
    const [busySeq, setBusySeq] = useState(null);
    const [error, setError] = useState("");
    const [message, setMessage] = useState("");
    const [preview, setPreview] = useState(null);

    const base = `/api/Admin/Site/${siteSeq}/DeletedPages`;

    const load = async () => {
        setLoading(true);
        setError("");
        try {
            const data = await fetchJson(base);
            setRows(Array.isArray(data) ? data : []);
        } catch (err) {
            logError("deletedPages:load", err);
            setError(String(err?.message ?? err));
        } finally {
            setLoading(false);
        }
    };

    useEffect(() => { if (siteSeq) load(); }, [siteSeq]);

    const act = async (row, label, run) => {
        setBusySeq(row.seq);
        setError("");
        setMessage("");
        try {
            await run();
            setMessage(`${row.name}: ${label}`);
            await load();
        } catch (err) {
            logError("deletedPages:act", err);
            setError(`${row.name}: ${String(err?.message ?? err)}`);
        } finally {
            setBusySeq(null);
        }
    };

    const restore = (row) => act(row, "되살렸습니다", () => sendJson(`${base}/${row.seq}/restore`, "POST"));

    const purge = (row) => {
        if (!window.confirm(`Delete "${row.name}" for good? Its ${row.revisionCount} revision(s) and ${row.attachmentCount} attachment(s) cannot be brought back.`)) return;
        return act(row, "영구 삭제했습니다", () => sendJson(`${base}/${row.seq}`, "DELETE"));
    };

    const showContent = async (row) => {
        try {
            setPreview(await fetchJson(`${base}/${row.seq}`));
        } catch (err) {
            logError("deletedPages:preview", err);
            setError(String(err?.message ?? err));
        }
    };

    return (
        <>
            <Group justify="flex-end" mb="md">
                <Badge color="gray" variant="light">{rows.length} deleted</Badge>
            </Group>
            <Text size="sm" c="dimmed" mb="md">
                지운 페이지는 리비전과 첨부까지 여기 보관됩니다. 되살리면 같은 이름으로 모든 리비전과 첨부가 돌아오고, 같은 이름의 페이지가 이미 있으면 되살리지 않습니다. 영구 삭제는 되돌릴 수 없습니다.
            </Text>
            {error ? <Text c="red" size="sm" mb="sm">{error}</Text> : null}
            {message ? <Text c="teal" size="sm" mb="sm">{message}</Text> : null}
            <DataTable
                withTableBorder borderRadius="md" striped highlightOnHover
                fetching={loading}
                records={rows}
                idAccessor="seq"
                columns={[
                    {accessor: "name", title: "Page", render: (row) => <Button variant="subtle" size="compact-sm" onClick={() => showContent(row)}>{row.name}</Button>},
                    {accessor: "dateDeleted", title: "Deleted", render: (row) => formatDateTimeInClientTimezone(row.dateDeleted)},
                    {accessor: "deletedByNickname", title: "By", render: (row) => `${row.deletedByNickname ?? "-"}${row.viaApi ? " (API)" : ""}`},
                    {accessor: "revisionCount", title: "Revisions"},
                    {accessor: "attachmentCount", title: "Attachments"},
                    {accessor: "actions", title: "Actions", render: (row) => (
                        <Group gap="xs" wrap="nowrap">
                            <Button size="xs" variant="light" loading={busySeq === row.seq} onClick={() => restore(row)}>되살리기</Button>
                            <Button size="xs" variant="light" color="red" disabled={busySeq === row.seq} onClick={() => purge(row)}>영구 삭제</Button>
                        </Group>
                    )},
                ]}
                minHeight={160}
                noRecordsText="지운 페이지가 없습니다"
            />
            <Modal opened={preview !== null} onClose={() => setPreview(null)} title={preview ? `${preview.name} (r${preview.lastRevision})` : ""} size="xl">
                <ScrollArea h={480}>
                    <Code block>{preview?.content ?? ""}</Code>
                </ScrollArea>
            </Modal>
        </>
    );
}
