import React, {useEffect, useState} from "react";
import {useOutletContext, useNavigate} from "react-router-dom";
import {Badge, Button, Group, Stack, Text, TextInput} from "@mantine/core";
import {DataTable} from "mantine-datatable";
import {useSiteAdminsData} from "../hooks/useSiteAdminsData.js";
import {formatDateTimeInClientTimezone} from "../utils.js";

export default function SiteAdminsPage() {
    const {site, siteSeq} = useOutletContext();
    const navigate = useNavigate();
    const {siteAdmins, adding, deletingUserSeq, error, loadSiteAdmins, insertSiteAdmin, deleteSiteAdmin} = useSiteAdminsData(siteSeq);
    const [userSeqInput, setUserSeqInput] = useState("");

    useEffect(() => { if (siteSeq) loadSiteAdmins(); }, [siteSeq]);

    return (
        <>
            <Group justify="flex-end" mb="md">
                <Badge color="orange" variant="light">{siteAdmins.length} admins</Badge>
            </Group>
            <Text size="sm" c="dimmed" mb="md">이 사이트의 관리자 목록입니다. Site Admin은 해당 사이트의 Permission, 페이지 등을 관리할 수 있습니다.</Text>
            {error ? <Text c="red" size="sm" mb="sm">{error}</Text> : null}
            <Group mb="md" align="end">
                <TextInput label="User Seq" placeholder="추가할 유저의 seq (숫자)" value={userSeqInput} onChange={(e) => setUserSeqInput(e.currentTarget.value)} type="number" min="1"/>
                <Button variant="filled" loading={adding} disabled={!siteSeq || !userSeqInput.trim()} onClick={async () => {
                    const userSeq = Number.parseInt(userSeqInput, 10);
                    if (!Number.isFinite(userSeq) || userSeq <= 0) return;
                    const ok = await insertSiteAdmin(userSeq);
                    if (ok) setUserSeqInput("");
                }}>추가</Button>
            </Group>
            <DataTable
                withTableBorder borderRadius="md" striped highlightOnHover
                records={siteAdmins}
                columns={[
                    {accessor: "user", title: "User Seq"},
                    {accessor: "dateInserted", title: "Date Inserted", render: (row) => formatDateTimeInClientTimezone(row.dateInserted)},
                    {accessor: "actions", title: "Actions", render: (row) => (
                        <Button size="xs" variant="light" color="red" loading={deletingUserSeq === row.user} onClick={() => deleteSiteAdmin(row.user)}>삭제</Button>
                    )},
                ]}
                minHeight={160}
            />
        </>
    );
}
