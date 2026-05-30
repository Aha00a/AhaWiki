import React, {useEffect, useState} from "react";
import {Badge, Button, Card, Group, Table, Text, Title} from "@mantine/core";
import {useS3Data} from "../hooks/useS3Data.js";
import {formatDateTimeInClientTimezone} from "../utils.js";

export default function S3BrowserPage() {
    const {loading, error, itemsByPrefix, selectedS3Keys, setSelectedS3Keys, deletingS3, expandedS3Nodes, loadingPrefixes, expandingAll, loadS3Objects, toggleS3Node, expandAllS3Nodes, deleteS3Selected, downloadS3Object} = useS3Data();
    const [sortBy, setSortBy] = useState("key");
    const [sortDir, setSortDir] = useState("asc");

    useEffect(() => { loadS3Objects(); }, []);

    const handleSort = (col) => {
        if (sortBy === col) setSortDir(d => d === "asc" ? "desc" : "asc");
        else { setSortBy(col); setSortDir("asc"); }
    };

    const computeLoadedSize = (prefix) => {
        return (itemsByPrefix[prefix] || []).reduce((sum, item) => {
            if (item.isDirectory) return sum + computeLoadedSize(item.key.replace(/\/$/, ""));
            return sum + Number(item.size ?? 0);
        }, 0);
    };

    const computeMaxLastModified = (prefix) => {
        return (itemsByPrefix[prefix] || []).reduce((max, item) => {
            const val = item.isDirectory ? computeMaxLastModified(item.key.replace(/\/$/, "")) : (item.lastModified || "");
            return val > max ? val : max;
        }, "");
    };

    const hasUnloadedSubdirs = (prefix) => {
        return (itemsByPrefix[prefix] || []).some(item => {
            if (!item.isDirectory) return false;
            const childPath = item.key.replace(/\/$/, "");
            if (!(childPath in itemsByPrefix)) return true;
            return hasUnloadedSubdirs(childPath);
        });
    };

    const buildRows = (prefix, depth) => {
        const items = itemsByPrefix[prefix];
        if (!items) return [];
        const parentTotal = computeLoadedSize(prefix);
        const rows = [];
        [...items]
            .sort((a, b) => {
                if (a.isDirectory !== b.isDirectory) return a.isDirectory ? -1 : 1;
                const aPath = a.key.replace(/\/$/, "");
                const bPath = b.key.replace(/\/$/, "");
                let cmp = 0;
                if (sortBy === "size") {
                    const aSize = a.isDirectory ? computeLoadedSize(aPath) : Number(a.size ?? 0);
                    const bSize = b.isDirectory ? computeLoadedSize(bPath) : Number(b.size ?? 0);
                    cmp = aSize - bSize;
                } else if (sortBy === "lastModified") {
                    const aDate = a.isDirectory ? computeMaxLastModified(aPath) : (a.lastModified || "");
                    const bDate = b.isDirectory ? computeMaxLastModified(bPath) : (b.lastModified || "");
                    cmp = aDate.localeCompare(bDate);
                } else {
                    cmp = a.key.localeCompare(b.key);
                }
                return sortDir === "desc" ? -cmp : cmp;
            })
            .forEach(item => {
                const path = item.key.replace(/\/$/, "");
                const name = path.split("/").pop();
                const itemSize = item.isDirectory ? computeLoadedSize(path) : Number(item.size ?? 0);
                const itemLastModified = item.isDirectory ? computeMaxLastModified(path) : (item.lastModified || "");
                const parentPercent = parentTotal > 0 ? itemSize / parentTotal * 100 : 0;
                if (item.isDirectory) {
                    const isExpanded = !!expandedS3Nodes[path];
                    const isLoading = loadingPrefixes.has(path);
                    rows.push({depth, name, path, isFile: false, isDir: true, isExpanded, isLoading, itemSize, itemLastModified, parentPercent, meta: null});
                    if (isExpanded) {
                        if (isLoading) {
                            rows.push({depth: depth + 1, name: null, path: path + "/__loading__", isLoadingRow: true});
                        } else {
                            rows.push(...buildRows(path, depth + 1));
                        }
                    }
                } else {
                    rows.push({depth, name, path, isFile: true, isDir: false, itemSize, itemLastModified, parentPercent, meta: item});
                }
            });
        return rows;
    };

    const rows = buildRows("", 0);
    const loadedFileRows = rows.filter(r => r.isFile);
    const rootTotal = computeLoadedSize("");

    const SortIcon = ({col}) => {
        if (sortBy !== col) return <i className="fas fa-sort" style={{opacity: 0.3, marginLeft: 4}}/>;
        return <i className={`fas fa-sort-${sortDir === "asc" ? "up" : "down"}`} style={{marginLeft: 4}}/>;
    };

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>S3 파일 브라우저</Title>
                <Badge color="red" variant="light">Admin Only</Badge>
            </Group>
            {error ? <Text c="red" size="sm" mb="md">{error}</Text> : null}
            <Group align="flex-end" mb="md">
                <Button loading={loading} onClick={() => loadS3Objects()} leftSection={<i className="fas fa-sync-alt" aria-hidden="true"/>}>새로고침</Button>
                <Button variant="light" loading={expandingAll} onClick={() => expandAllS3Nodes()} leftSection={<i className="fas fa-angle-double-down" aria-hidden="true"/>}>모두 펼치기</Button>
                <Button color="red" variant="light" disabled={selectedS3Keys.length === 0} loading={deletingS3} onClick={() => deleteS3Selected(selectedS3Keys)} leftSection={<i className="fas fa-trash-alt" aria-hidden="true"/>}>선택 삭제 ({selectedS3Keys.length})</Button>
            </Group>
            <Table striped highlightOnHover withTableBorder withColumnBorders>
                <Table.Thead>
                    <Table.Tr>
                        <Table.Th style={{width: 36}}><input type="checkbox" checked={loadedFileRows.length > 0 && selectedS3Keys.length === loadedFileRows.length} onChange={(e) => { setSelectedS3Keys(e.currentTarget.checked ? loadedFileRows.map(r => r.path) : []); }}/></Table.Th>
                        <Table.Th style={{cursor: "pointer"}} onClick={() => handleSort("key")}>Key<SortIcon col="key"/></Table.Th>
                        <Table.Th style={{width: 140, textAlign: "right", cursor: "pointer"}} onClick={() => handleSort("size")}>Size(bytes)<SortIcon col="size"/></Table.Th>
                        <Table.Th style={{width: 180, textAlign: "center", cursor: "pointer"}} onClick={() => handleSort("lastModified")}>Last Modified<SortIcon col="lastModified"/></Table.Th>
                    </Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                    {rows.map((row) => {
                        const {depth, name, path, isFile, isDir, isExpanded, isLoadingRow, itemSize, itemLastModified, parentPercent, meta} = row;
                        if (isLoadingRow) {
                            return (
                                <Table.Tr key={path}>
                                    <Table.Td colSpan={5}>
                                        <div style={{paddingLeft: `${depth * 22 + 28}px`}}>
                                            <i className="fas fa-spinner fa-spin" aria-hidden="true"/> 로딩 중...
                                        </div>
                                    </Table.Td>
                                </Table.Tr>
                            );
                        }
                        const checked = selectedS3Keys.includes(path);
                        const showSize = isFile || (isDir && isExpanded);
                        const showLastModified = isFile || (isDir && isExpanded);
                        const sizeIncomplete = isDir && isExpanded && hasUnloadedSubdirs(path);
                        const percent = rootTotal > 0 ? itemSize / rootTotal * 100 : 0;
                        return (
                            <Table.Tr key={path}>
                                <Table.Td><input type="checkbox" disabled={!isFile} checked={checked} onChange={(e) => { if (e.currentTarget.checked) setSelectedS3Keys(prev => [...prev, path]); else setSelectedS3Keys(prev => prev.filter(k => k !== path)); }}/></Table.Td>
                                <Table.Td>
                                    <div style={{display: "flex", alignItems: "center", gap: 8, paddingLeft: `${depth * 22}px`, cursor: isDir ? "pointer" : undefined}} onClick={isDir ? () => toggleS3Node(path) : undefined}>
                                        {isDir ? <Button size="compact-xs" variant="subtle" tabIndex={-1}><i className={`fas ${isExpanded ? "fa-chevron-down" : "fa-chevron-right"}`} aria-hidden="true"/></Button> : <span style={{display: "inline-block", width: 20}}/>}
                                        <span><i className={`fas ${isFile ? "fa-file-alt" : "fa-folder"}`} aria-hidden="true"/></span>
                                        <span style={{flex: 1, background: percent > 0 ? `linear-gradient(to right, rgba(100, 149, 237, 0.25) ${percent}%, transparent ${percent}%)` : undefined, cursor: isFile ? "pointer" : undefined}} onClick={isFile ? () => downloadS3Object(path) : undefined}>{name}</span>
                                    </div>
                                </Table.Td>
                                <Table.Td style={{textAlign: "right", opacity: sizeIncomplete ? 0.4 : 1, ...(showSize && parentPercent > 0 ? {background: `linear-gradient(to right, rgba(100, 149, 237, 0.25) ${parentPercent}%, transparent ${parentPercent}%)`} : {})}}>
                                    {showSize ? Number(itemSize).toLocaleString() : "-"}
                                </Table.Td>
                                <Table.Td style={{textAlign: "center", opacity: sizeIncomplete ? 0.4 : 1}}>
                                    {showLastModified ? formatDateTimeInClientTimezone(itemLastModified) : "-"}
                                </Table.Td>
                            </Table.Tr>
                        );
                    })}
                </Table.Tbody>
            </Table>
        </Card>
    );
}
