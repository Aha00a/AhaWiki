import React, {useEffect, useMemo} from "react";
import {useNavigate, useSearchParams} from "react-router-dom";
import {Anchor, Badge, Button, Card, Group, Loader, Text, Title} from "@mantine/core";
import {useUserViewsData} from "../hooks/useUserViewsData.js";
import UserProfileCell from "../components/UserProfileCell.jsx";
import {makeTable} from "../../component/commonWidgets.jsx";
import {fetchJson, logError} from "../api.js";
import {useState} from "react";

export default function UserViewsPage() {
    const navigate = useNavigate();
    const [searchParams] = useSearchParams();
    const {loading, userViewHistories, loadUserViewHistories} = useUserViewsData();
    const [allUsers, setAllUsers] = useState([]);

    const userSeq = useMemo(() => {
        const bySeq = Number.parseInt(searchParams.get("seq") ?? "", 10);
        if (Number.isFinite(bySeq) && bySeq > 0) return bySeq;
        const byLegacy = Number.parseInt(searchParams.get("userSeq") ?? "", 10);
        return Number.isFinite(byLegacy) && byLegacy > 0 ? byLegacy : 0;
    }, [searchParams]);

    const selectedUser = useMemo(() => allUsers.find((u) => u.seq === userSeq) ?? null, [allUsers, userSeq]);

    useEffect(() => {
        fetchJson("/api/Admin/Users").then((data) => {
            const rows = Array.isArray(data?.array) ? data.array : (Array.isArray(data) ? data : []);
            setAllUsers(rows);
        }).catch((err) => logError("user-views:users:error", err));
    }, []);

    useEffect(() => {
        if (userSeq > 0) loadUserViewHistories(userSeq, 200);
    }, [userSeq]);

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>User View Histories</Title>
                <Badge color="cyan" variant="light">{userViewHistories.length} rows</Badge>
            </Group>
            <Text size="sm" c="dimmed" mb="md">선택한 사용자의 페이지 열람 이력입니다.</Text>
            <Group mb="md" justify="space-between">
                <Button variant="light" size="xs" onClick={() => navigate("/Admin/User")}>← User</Button>
                {selectedUser ? (
                    <Group gap={8}><Text size="sm">사용자:</Text><UserProfileCell user={selectedUser}/></Group>
                ) : (
                    <Text size="sm" c="dimmed">seq를 지정해 주세요. (/Admin/User/UserViewHistory?seq=숫자)</Text>
                )}
            </Group>
            {loading ? (
                <Group><Loader size="sm"/><Text size="sm" c="dimmed">열람 이력을 불러오는 중입니다...</Text></Group>
            ) : makeTable(
                ["When", "Site", "Page", "History Seq"],
                userViewHistories.map((history) => {
                    const siteUrl = history.siteDomain ? `https://${history.siteDomain}` : "";
                    const pageUrl = siteUrl ? `${siteUrl}/w/${encodeURIComponent(history.pageName)}` : "";
                    return [history.viewedAt, siteUrl ? <Anchor href={siteUrl} target="_blank">{history.siteName} (#{history.site})</Anchor> : `${history.siteName} (#${history.site})`, pageUrl ? <Anchor href={pageUrl} target="_blank">{history.pageName}</Anchor> : history.pageName, history.seq];
                }),
            )}
        </Card>
    );
}
