import React, {useEffect, useMemo, useState} from "react";
import {useParams} from "react-router-dom";
import {Anchor, Badge, Button, Group, Pagination, Text, TextInput} from "@mantine/core";
import {DataTable} from "mantine-datatable";
import {useAccessLogData} from "../hooks/useAccessLogData.js";
import {IconChevronUp, IconSelector} from "../../component/commonWidgets.jsx";
import {ACCESS_LOG_PAGE_SIZE} from "../constants.js";

export default function AccessLogsPage() {
    const params = useParams();
    const siteSeq = params.siteSeq ?? "";
    const {loading, accessLogs, accessLogCount, sites, loadAccessLogs, loadSites} = useAccessLogData();
    const [page, setPage] = useState(1);
    const [searchInput, setSearchInput] = useState("");
    const [sortBy, setSortBy] = useState("seq");
    const [sortOrder, setSortOrder] = useState("desc");
    const totalPages = Math.max(1, Math.ceil(accessLogCount / ACCESS_LOG_PAGE_SIZE));
    const selectedSite = useMemo(() => sites.find((s) => String(s.seq) === siteSeq) ?? null, [sites, siteSeq]);

    useEffect(() => {
        loadSites();
        loadAccessLogs({page: 1, pageSize: ACCESS_LOG_PAGE_SIZE, search: "", sortBy: "seq", sortOrder: "desc", siteSeq});
    }, [siteSeq]);

    return (
        <>
            <Group justify="flex-end" mb="md">
                <Badge color="cyan" variant="light">{accessLogs.length} / {accessLogCount} rows</Badge>
            </Group>
            <Text size="sm" c="dimmed" mb="md">검색과 헤더 정렬, 페이지 이동으로 요청 로그를 조회할 수 있습니다.</Text>
            <Group align="flex-end" mb="md">
                <TextInput label="search" value={searchInput} onChange={(e) => setSearchInput(e.currentTarget.value)} placeholder="site/method/uri/ip/user-agent"/>
                <Button variant="filled" onClick={() => { setPage(1); loadAccessLogs({page: 1, pageSize: ACCESS_LOG_PAGE_SIZE, search: searchInput, sortBy, sortOrder, siteSeq}); }}>조회</Button>
            </Group>
            <DataTable
                sortIcons={{sorted: <IconChevronUp size={14}/>, unsorted: <IconSelector size={14}/>}}
                withTableBorder borderRadius="sm" striped highlightOnHover minHeight={420}
                records={Array.isArray(accessLogs) ? accessLogs : []}
                idAccessor="seq"
                columns={[
                    {accessor: "seq", title: "seq", sortable: true},
                    {accessor: "dateInserted", title: "DateInserted", sortable: true},
                    {accessor: "siteName", title: "Site", render: (row) => `${row.siteName} (#${row.siteSeq})`},
                    {accessor: "ipDenySeq", title: "IpDeny", render: (row) => row.ipDenySeq ?? "-"},
                    {accessor: "userSeq", title: "User", render: (row) => row.userSeq ?? "-"},
                    {accessor: "method", title: "Method", sortable: true},
                    {accessor: "uri", title: "Request URL", sortable: true, render: (row) => { const href = `${row.scheme}://${row.host}${row.uri}`; return <Anchor href={href} target="_blank" rel="noopener noreferrer">{href}</Anchor>; }},
                    {accessor: "status", title: "Status", sortable: true},
                    {accessor: "remoteAddress", title: "IP"},
                    {accessor: "durationMilli", title: "Duration(ms)", sortable: true},
                    {accessor: "userAgent", title: "User-Agent"},
                ]}
                sortStatus={{columnAccessor: sortBy, direction: sortOrder}}
                onSortStatusChange={(next) => {
                    const dir = next.direction ?? "desc";
                    setPage(1); setSortBy(next.columnAccessor); setSortOrder(dir);
                    loadAccessLogs({page: 1, pageSize: ACCESS_LOG_PAGE_SIZE, search: searchInput, sortBy: next.columnAccessor, sortOrder: dir, siteSeq});
                }}
            />
            <Group mt="md" justify="space-between">
                <Text size="sm" c="dimmed">Page {page} / {totalPages}</Text>
                <Pagination value={page} onChange={(nextPage) => { setPage(nextPage); loadAccessLogs({page: nextPage, pageSize: ACCESS_LOG_PAGE_SIZE, search: searchInput, sortBy, sortOrder, siteSeq}); }} total={totalPages} siblings={1} boundaries={1} size="sm"/>
            </Group>
        </>
    );
}
