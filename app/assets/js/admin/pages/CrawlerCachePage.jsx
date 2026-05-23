import React, {useEffect, useState} from "react";
import {Anchor, Badge, Button, Card, Group, Image, Text, TextInput, Title} from "@mantine/core";
import {DataTable} from "mantine-datatable";
import {useCrawlerCacheData} from "../hooks/useCrawlerCacheData.js";
import {IconChevronUp, IconSelector} from "../../component/commonWidgets.jsx";
import {CRAWLER_CACHE_PAGE_SIZE} from "../constants.js";
import {formatDateTimeInClientTimezone, formatCrawlerStatus} from "../utils.js";

export default function CrawlerCachePage() {
    const {loading, crawlerCaches, crawlerCacheCount, refreshingUrl, deletingUrl, loadCrawlerCaches, refreshCrawlerCache, deleteCrawlerCache} = useCrawlerCacheData();
    const [page, setPage] = useState(1);
    const [searchInput, setSearchInput] = useState("");
    const [search, setSearch] = useState("");
    const [sortBy, setSortBy] = useState("id");
    const [sortOrder, setSortOrder] = useState("desc");
    const totalPages = Math.max(1, Math.ceil(crawlerCacheCount / CRAWLER_CACHE_PAGE_SIZE));
    const normalizedPage = Math.min(page, totalPages);
    const currentParams = {page, pageSize: CRAWLER_CACHE_PAGE_SIZE, search, sortBy, sortOrder};

    useEffect(() => { loadCrawlerCaches(); }, []);

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>Crawler Cache</Title>
                <Badge color="lime" variant="light">{crawlerCacheCount} rows</Badge>
            </Group>
            <Text size="sm" c="dimmed" mb="md">URL별 크롤링 캐시 조회/삭제/강제갱신을 수행합니다.</Text>
            <Group align="flex-end" mb="md">
                <Button variant="filled" onClick={() => loadCrawlerCaches(currentParams)}>조회</Button>
            </Group>
            <Group mb="sm" align="end">
                <TextInput label="search" value={searchInput} onChange={(e) => setSearchInput(e.currentTarget.value)} placeholder="url, title, image, description"/>
                <Button variant="light" onClick={() => { setPage(1); setSearch(searchInput); loadCrawlerCaches({page: 1, pageSize: CRAWLER_CACHE_PAGE_SIZE, search: searchInput, sortBy, sortOrder}); }}>검색</Button>
            </Group>
            <DataTable
                sortIcons={{sorted: <IconChevronUp size={14}/>, unsorted: <IconSelector size={14}/>}}
                withTableBorder striped highlightOnHover
                records={crawlerCaches}
                columns={[
                    {accessor: "id", title: "ID", sortable: true},
                    {accessor: "url", title: "URL", sortable: true, render: (row) => <Text size="sm" style={{wordBreak: "break-all"}}>{row.url}</Text>},
                    {accessor: "status", title: "Status", sortable: true, render: (row) => formatCrawlerStatus(row.status)},
                    {accessor: "title", title: "Title", sortable: true, render: (row) => row.title || "-"},
                    {accessor: "image", title: "Image", sortable: true, render: (row) => {
                        if (!row.image) return "-";
                        return <Anchor href={row.image} target="_blank" rel="noreferrer"><Image src={row.image} alt={row.title || row.url || "crawler image"} h={44} w={72} fit="cover" radius="sm" fallbackSrc="data:image/gif;base64,R0lGODlhAQABAIAAAAAAAP///ywAAAAAAQABAAACAUwAOw=="/></Anchor>;
                    }},
                    {accessor: "description", title: "Description", sortable: true, render: (row) => row.description || "-"},
                    {accessor: "dateUpdated", title: "Updated", sortable: true, render: (row) => formatDateTimeInClientTimezone(row.dateUpdated)},
                    {accessor: "action", title: "Action", render: (row) => (
                        <Group gap={6}>
                            <Button size="xs" variant="light" loading={refreshingUrl === row.url} onClick={() => refreshCrawlerCache(row.url, currentParams)}>갱신</Button>
                            <Button size="xs" color="red" variant="light" loading={deletingUrl === row.url} onClick={() => deleteCrawlerCache(row.url, currentParams)}>삭제</Button>
                        </Group>
                    )},
                ]}
                sortStatus={{columnAccessor: sortBy, direction: sortOrder}}
                onSortStatusChange={(next) => {
                    const dir = next.direction ?? "desc";
                    setPage(1); setSortBy(next.columnAccessor); setSortOrder(dir);
                    loadCrawlerCaches({page: 1, pageSize: CRAWLER_CACHE_PAGE_SIZE, search, sortBy: next.columnAccessor, sortOrder: dir});
                }}
                totalRecords={crawlerCacheCount}
                recordsPerPage={CRAWLER_CACHE_PAGE_SIZE}
                page={normalizedPage}
                onPageChange={(nextPage) => { setPage(nextPage); loadCrawlerCaches({page: nextPage, pageSize: CRAWLER_CACHE_PAGE_SIZE, search, sortBy, sortOrder}); }}
                paginationText={({from, to, totalRecords}) => `${from}-${to} / ${totalRecords}`}
                minHeight={380}
            />
        </Card>
    );
}
