import React, {useEffect, useMemo, useState} from "react";
import {useOutletContext} from "react-router-dom";
import {Autocomplete, Badge, Button, Group, Paper, Select, SimpleGrid, Text, TextInput} from "@mantine/core";
import {DataTable} from "mantine-datatable";
import {usePermissionData} from "../hooks/usePermissionData.js";
import {IconChevronUp, IconSelector} from "../../component/commonWidgets.jsx";
import {PERMISSION_TARGET_TYPE_OPTIONS, PERMISSION_ACTOR_TYPE_OPTIONS} from "../constants.js";
import {PERMISSION_ACTION_OPTIONS, formatPermissionAction, compareValuesForSort} from "../utils.js";

export default function SitePermissionPage() {
    const {siteSeq, sitePageNames} = useOutletContext();
    const {permissionRows, permissionDiagnose, saving, deletingKey, error, loadPermissions, savePermission, deletePermission, diagnosePermission} = usePermissionData(siteSeq);
    const [form, setForm] = useState({targetType: "All", target: "", actorType: "All", actor: "", action: "Read"});
    const [diagnoseForm, setDiagnoseForm] = useState({pageName: "", actor: "", action: "Read"});
    const [sortBy, setSortBy] = useState("specificity");
    const [sortOrder, setSortOrder] = useState("desc");

    useEffect(() => { if (siteSeq) loadPermissions(); }, [siteSeq]);

    const sortedRows = useMemo(() => [...permissionRows].sort((a, b) => {
        const leftValue = sortBy === "actionLabel" ? formatPermissionAction(a.actionName, a.action) : a[sortBy];
        const rightValue = sortBy === "actionLabel" ? formatPermissionAction(b.actionName, b.action) : b[sortBy];
        return compareValuesForSort(leftValue, rightValue, sortOrder);
    }), [permissionRows, sortBy, sortOrder]);

    return (
        <>
            <Group justify="flex-end" mb="md">
                <Badge color="grape" variant="light">{permissionRows.length} rows</Badge>
            </Group>
            {error ? <Text c="red" size="sm" mb="sm">{error}</Text> : null}
            <SimpleGrid cols={{base: 1, md: 5}} spacing="sm" mb="sm">
                <Select label="targetType" data={PERMISSION_TARGET_TYPE_OPTIONS} value={form.targetType} onChange={(v) => setForm({...form, targetType: v ?? "All", target: v === "All" ? "" : form.target})}/>
                <Autocomplete label="target" data={sitePageNames} value={form.target} onChange={(v) => setForm({...form, target: v})} placeholder="page name or prefix" disabled={form.targetType === "All"}/>
                <Select label="actorType" data={PERMISSION_ACTOR_TYPE_OPTIONS} value={form.actorType} onChange={(v) => setForm({...form, actorType: v ?? "All", actor: v === "All" || v === "Login" ? "" : form.actor})}/>
                <TextInput label="actor" value={form.actor} onChange={(e) => setForm({...form, actor: e.currentTarget.value})} placeholder="email or @domain"/>
                <Select label="action" data={PERMISSION_ACTION_OPTIONS} value={form.action} onChange={(v) => setForm({...form, action: v ?? "Read"})}/>
            </SimpleGrid>
            <Group mb="md">
                <Button size="xs" loading={saving} onClick={async () => {
                    const saved = await savePermission(form);
                    if (saved) setForm({targetType: "All", target: "", actorType: "All", actor: "", action: "Read"});
                }}>Save</Button>
            </Group>
            <SimpleGrid cols={{base: 1, md: 4}} spacing="sm" mb="sm">
                <Autocomplete label="pageName" data={sitePageNames} value={diagnoseForm.pageName} onChange={(v) => setDiagnoseForm({...diagnoseForm, pageName: v})}/>
                <TextInput label="actor" value={diagnoseForm.actor} onChange={(e) => setDiagnoseForm({...diagnoseForm, actor: e.currentTarget.value})} placeholder="empty means anonymous"/>
                <Select label="action" data={PERMISSION_ACTION_OPTIONS} value={diagnoseForm.action} onChange={(v) => setDiagnoseForm({...diagnoseForm, action: v ?? "Read"})}/>
                <Button mt={22} variant="light" onClick={() => diagnosePermission(diagnoseForm.pageName, diagnoseForm.actor, diagnoseForm.action)}>Diagnose</Button>
            </SimpleGrid>
            {permissionDiagnose ? (
                <Paper withBorder radius="sm" p="sm" mb="md">
                    <Group gap="xs">
                        <Badge color={permissionDiagnose.permitted ? "green" : "red"} variant="light">{permissionDiagnose.permitted ? "allowed" : "denied"}</Badge>
                        <Text size="sm">{permissionDiagnose.matchedPermission ? `${permissionDiagnose.matchedPermission.targetType}/${permissionDiagnose.matchedPermission.actorType}/${formatPermissionAction(permissionDiagnose.matchedPermission.actionName, permissionDiagnose.matchedPermission.action)}` : "No matching row"}</Text>
                    </Group>
                </Paper>
            ) : null}
            <DataTable
                withTableBorder borderRadius="md" striped highlightOnHover
                records={sortedRows}
                columns={[
                    {accessor: "targetType", title: "Target Type", sortable: true},
                    {accessor: "target", title: "Target", sortable: true, render: (row) => row.target || "*"},
                    {accessor: "actorType", title: "Actor Type", sortable: true},
                    {accessor: "actor", title: "Actor", sortable: true, render: (row) => row.actor || "*"},
                    {accessor: "action", title: "Action", sortable: true, render: (row) => formatPermissionAction(row.actionName, row.action)},
                    {accessor: "specificity", title: "Specificity", sortable: true},
                    {accessor: "actions", title: "Actions", render: (row) => {
                        const key = `${row.targetType}:${row.target}:${row.actorType}:${row.actor}`;
                        return (
                            <Group gap={6}>
                                <Button size="xs" variant="light" onClick={() => setForm({targetType: row.targetType, target: row.target, actorType: row.actorType, actor: row.actor, action: row.actionName})}>Edit</Button>
                                <Button size="xs" variant="light" color="red" loading={deletingKey === key} onClick={() => deletePermission(row)}>Delete</Button>
                            </Group>
                        );
                    }},
                ]}
                sortStatus={{columnAccessor: sortBy, direction: sortOrder}}
                onSortStatusChange={(next) => { setSortBy(next.columnAccessor); setSortOrder(next.direction ?? "asc"); }}
                minHeight={220}
            />
        </>
    );
}
