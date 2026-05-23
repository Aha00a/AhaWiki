import React, {useEffect, useState} from "react";
import {useOutletContext} from "react-router-dom";
import {Badge, Button, Group, Text} from "@mantine/core";
import {DataTable} from "mantine-datatable";
import {useAdminPageMetaData} from "../hooks/useAdminPageMetaData.js";
import {IconChevronUp, IconSelector} from "../../component/commonWidgets.jsx";
import {ADMIN_PAGE_META_PAGE_SIZE} from "../constants.js";
import {formatDateTimeInClientTimezone} from "../utils.js";
import {fetchJson, fetchCsrfToken, logError} from "../api.js";
import {Anchor, Image} from "@mantine/core";

export default function SiteDetailPage() {
    const {site, siteSeq, sitePageNames, refreshPageNames} = useOutletContext();
    const {rows, count, load} = useAdminPageMetaData(siteSeq);
    const [metaPage, setMetaPage] = useState(1);
    const [searchInput, setSearchInput] = useState("");
    const [search, setSearch] = useState("");
    const [sortBy, setSortBy] = useState("dateUpdated");
    const [sortOrder, setSortOrder] = useState("desc");
    const [calculating, setCalculating] = useState(false);
    const [calculateMessage, setCalculateMessage] = useState("");
    const totalPages = Math.max(1, Math.ceil(count / ADMIN_PAGE_META_PAGE_SIZE));

    useEffect(() => { if (siteSeq) load({siteSeq, page: 1, pageSize: ADMIN_PAGE_META_PAGE_SIZE, search: "", sortBy: "dateUpdated", sortOrder: "desc"}); }, [siteSeq]);

    const runCalculate = async (pageName) => {
        if (!siteSeq) return;
        setCalculating(true);
        try {
            const csrfToken = await fetchCsrfToken();
            const suffix = pageName?.trim() ? `?pageName=${encodeURIComponent(pageName.trim())}` : "";
            const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Calculate${suffix}`, {method: "POST", credentials: "same-origin", headers: {"Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value, "Content-Type": "application/x-www-form-urlencoded; charset=UTF-8"}, body: `${encodeURIComponent(csrfToken.name)}=${encodeURIComponent(csrfToken.value)}`});
            if (!response.ok) { const p = await response.json().catch(() => null); throw new Error(p?.error || `HTTP ${response.status}`); }
            return await response.json();
        } catch (err) { logError("site:calculate:error", err); return null; }
        finally { setCalculating(false); }
    };

    return (
        <>
            <Group mb="md" align="flex-end">
                <input type="text" value={searchInput} onChange={(e) => setSearchInput(e.target.value)} placeholder="page name, image" style={{border: "1px solid #ced4da", borderRadius: 6, padding: "6px 10px"}}/>
                <Button variant="filled" onClick={() => { setMetaPage(1); setSearch(searchInput); load({siteSeq, page: 1, pageSize: ADMIN_PAGE_META_PAGE_SIZE, search: searchInput, sortBy, sortOrder}); }}>검색</Button>
                <Badge color="indigo" variant="light">{count} rows</Badge>
            </Group>
            <DataTable
                withTableBorder borderRadius="md" striped highlightOnHover
                sortIcons={{sorted: <IconChevronUp size={14}/>, unsorted: <IconSelector size={14}/>}}
                records={rows}
                columns={[
                    {accessor: "name", title: "Page", sortable: true},
                    {accessor: "revision", title: "Revision", sortable: true},
                    {accessor: "image", title: "Image", sortable: true, render: (row) => row.image ? (<Anchor href={row.image} target="_blank" rel="noopener"><Image src={row.image} alt={`${row.name} image`} h={44} w={72} fit="cover" radius="sm" fallbackSrc="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw=="/></Anchor>) : "-"},
                    {accessor: "dateUpdated", title: "Date Updated", sortable: true, render: (row) => formatDateTimeInClientTimezone(row.dateUpdated)},
                    {accessor: "size", title: "Size", sortable: true},
                    {accessor: "actions", title: "Actions", render: (row) => (
                        <Button size="xs" variant="light" color="teal" disabled={!siteSeq} loading={calculating} onClick={async () => {
                            const res = await runCalculate(row.name);
                            if (res) setCalculateMessage(`선택 페이지: ${res?.pageName ?? row.name} (queued)`);
                        }}>재계산</Button>
                    )},
                ]}
                sortStatus={{columnAccessor: sortBy, direction: sortOrder}}
                onSortStatusChange={(next) => {
                    const dir = next.direction ?? "desc";
                    setMetaPage(1); setSortBy(next.columnAccessor); setSortOrder(dir);
                    load({siteSeq, page: 1, pageSize: ADMIN_PAGE_META_PAGE_SIZE, search, sortBy: next.columnAccessor, sortOrder: dir});
                }}
                totalRecords={count}
                recordsPerPage={ADMIN_PAGE_META_PAGE_SIZE}
                page={metaPage}
                onPageChange={(nextPage) => { setMetaPage(nextPage); load({siteSeq, page: nextPage, pageSize: ADMIN_PAGE_META_PAGE_SIZE, search, sortBy, sortOrder}); }}
                paginationText={({from, to, totalRecords}) => `${from}-${to} / ${totalRecords}`}
                minHeight={320}
            />
            {calculateMessage ? <Text size="sm" c="teal" mt="xs">{calculateMessage}</Text> : null}
            <Text size="xs" c="dimmed" mt="xs">Page {metaPage} / {totalPages}</Text>
        </>
    );
}
