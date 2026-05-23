import React, {useEffect} from "react";
import {Badge, Button, Card, Group, Table, Text, Title} from "@mantine/core";
import {useS3Data} from "../hooks/useS3Data.js";
import {formatDateTimeInClientTimezone} from "../utils.js";

export default function S3BrowserPage() {
    const {loading, error, s3Items, selectedS3Keys, setSelectedS3Keys, deletingS3, expandedS3Nodes, loadS3Objects, toggleS3Node, expandAllS3Nodes, deleteS3Selected, downloadS3Object} = useS3Data();

    useEffect(() => { loadS3Objects(); }, []);

    const fileRows = Array.isArray(s3Items) ? s3Items.filter((item) => !item.isDirectory) : [];
    const root = {children: {}};
    fileRows.forEach((item) => {
        const parts = String(item.key || "").split("/").filter(Boolean);
        if (parts.length === 0) return;
        let node = root;
        parts.forEach((part, index) => {
            const currentPath = parts.slice(0, index + 1).join("/");
            if (!node.children[part]) node.children[part] = {name: part, path: currentPath, isFile: index === parts.length - 1, children: {}, meta: null};
            if (index === parts.length - 1) { node.children[part].isFile = true; node.children[part].meta = item; }
            node = node.children[part];
        });
    });
    const rows = [];
    const visit = (node, depth = 0) => {
        Object.values(node.children).sort((a, b) => { if (a.isFile === b.isFile) return a.name.localeCompare(b.name); return a.isFile ? 1 : -1; }).forEach((child) => {
            rows.push({depth, node: child});
            if (!child.isFile && expandedS3Nodes[child.path]) visit(child, depth + 1);
        });
    };
    visit(root, 0);

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>S3 파일 브라우저</Title>
                <Badge color="red" variant="light">Admin Only</Badge>
            </Group>
            {error ? <Text c="red" size="sm" mb="md">{error}</Text> : null}
            <Group align="flex-end" mb="md">
                <Button loading={loading} onClick={() => loadS3Objects()} leftSection={<i className="fas fa-sync-alt" aria-hidden="true"/>}>새로고침</Button>
                <Button variant="light" onClick={() => expandAllS3Nodes(s3Items)} leftSection={<i className="fas fa-angle-double-down" aria-hidden="true"/>}>모두 펼치기</Button>
                <Button color="red" variant="light" disabled={selectedS3Keys.length === 0} loading={deletingS3} onClick={() => deleteS3Selected(selectedS3Keys)} leftSection={<i className="fas fa-trash-alt" aria-hidden="true"/>}>선택 삭제 ({selectedS3Keys.length})</Button>
            </Group>
            <Table striped highlightOnHover withTableBorder withColumnBorders>
                <Table.Thead>
                    <Table.Tr>
                        <Table.Th><input type="checkbox" checked={selectedS3Keys.length > 0 && selectedS3Keys.length === fileRows.length} onChange={(e) => { setSelectedS3Keys(e.currentTarget.checked ? fileRows.map((item) => item.key) : []); }}/></Table.Th>
                        <Table.Th>Key</Table.Th>
                        <Table.Th style={{textAlign: "right"}}>Size(bytes)</Table.Th>
                        <Table.Th style={{textAlign: "center"}}>Last Modified</Table.Th>
                        <Table.Th style={{textAlign: "center"}}>Action</Table.Th>
                    </Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                    {rows.map(({depth, node}) => {
                        const key = node.path;
                        const checked = selectedS3Keys.includes(key);
                        return (
                            <Table.Tr key={key}>
                                <Table.Td><input type="checkbox" disabled={!node.isFile} checked={checked} onChange={(e) => { if (e.currentTarget.checked) setSelectedS3Keys((prev) => [...prev, key]); else setSelectedS3Keys((prev) => prev.filter((k) => k !== key)); }}/></Table.Td>
                                <Table.Td>
                                    <div style={{display: "flex", alignItems: "center", gap: 8, paddingLeft: `${depth * 22}px`}}>
                                        {!node.isFile ? <Button size="compact-xs" variant="subtle" onClick={() => toggleS3Node(node.path)}><i className={`fas ${expandedS3Nodes[node.path] ? "fa-chevron-down" : "fa-chevron-right"}`} aria-hidden="true"/></Button> : <span style={{display: "inline-block", width: 20}}/>}
                                        <span><i className={`fas ${node.isFile ? "fa-file-alt" : "fa-folder"}`} aria-hidden="true"/></span>
                                        <span>{node.name}</span>
                                    </div>
                                </Table.Td>
                                <Table.Td style={{textAlign: "right"}}>{node.isFile ? Number(node.meta?.size ?? 0).toLocaleString() : "-"}</Table.Td>
                                <Table.Td style={{textAlign: "center"}}>{node.isFile ? formatDateTimeInClientTimezone(node.meta?.lastModified) : "-"}</Table.Td>
                                <Table.Td style={{textAlign: "center"}}>{node.isFile ? <Button size="xs" variant="light" onClick={() => downloadS3Object(key)} leftSection={<i className="fas fa-download" aria-hidden="true"/>}>다운로드</Button> : "-"}</Table.Td>
                            </Table.Tr>
                        );
                    })}
                </Table.Tbody>
            </Table>
        </Card>
    );
}
