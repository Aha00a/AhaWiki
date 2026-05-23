import React, {useEffect, useState} from "react";
import {useNavigate} from "react-router-dom";
import {Badge, Button, Card, Divider, Group, Text, TextInput, Title} from "@mantine/core";
import {DataTable} from "mantine-datatable";
import {useAllUsersData} from "../hooks/useAllUsersData.js";
import UserProfileCell from "../components/UserProfileCell.jsx";
import {IconChevronUp, IconSelector} from "../../component/commonWidgets.jsx";
import {ACCESS_LOG_PAGE_SIZE} from "../constants.js";

export default function AllUsersPage() {
    const navigate = useNavigate();
    const {loading, allUsers, allUserCount, loadAllUsers} = useAllUsersData();
    const [page, setPage] = useState(1);
    const [searchInput, setSearchInput] = useState("");
    const [sortBy, setSortBy] = useState("seq");
    const [sortOrder, setSortOrder] = useState("desc");
    const totalPages = Math.max(1, Math.ceil(allUserCount / ACCESS_LOG_PAGE_SIZE));

    useEffect(() => { loadAllUsers(); }, []);

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md">
                <Title order={3}>All Users</Title>
                <Badge color="blue" variant="light">{allUserCount} users</Badge>
            </Group>
            <Text size="sm" c="dimmed" mb="md">전체 사이트 기준 사용자 목록이며, 최근 방문순으로 정렬됩니다.</Text>
            <Divider mb="md"/>
            <Group mb="sm" align="end">
                <TextInput label="search" value={searchInput} onChange={(e) => setSearchInput(e.currentTarget.value)} placeholder="email, nickname, seq"/>
                <Button variant="light" onClick={() => { setPage(1); loadAllUsers({page: 1, pageSize: ACCESS_LOG_PAGE_SIZE, search: searchInput, sortBy, sortOrder}); }}>검색</Button>
            </Group>
            <DataTable
                sortIcons={{sorted: <IconChevronUp size={14}/>, unsorted: <IconSelector size={14}/>}}
                withTableBorder striped highlightOnHover
                records={allUsers}
                columns={[
                    {accessor: "seq", title: "Seq", sortable: true},
                    {accessor: "profile", title: "Profile", render: (row) => <UserProfileCell user={row}/>},
                    {accessor: "email", title: "Email", sortable: true},
                    {accessor: "nickname", title: "Nickname", sortable: true},
                    {accessor: "created", title: "Created", sortable: true},
                    {accessor: "updated", title: "Updated", sortable: true},
                    {accessor: "visitCount", title: "Visits", sortable: true},
                    {accessor: "lastViewed", title: "Last Viewed", sortable: true, render: (row) => row.lastViewed ?? "-"},
                    {accessor: "action", title: "Action", render: (row) => <Button size="xs" variant="light" onClick={() => navigate(`/Admin/User/UserViewHistory?seq=${encodeURIComponent(row.seq)}`)}>열람 이력</Button>},
                ]}
                sortStatus={{columnAccessor: sortBy, direction: sortOrder}}
                onSortStatusChange={(next) => {
                    const dir = next.direction ?? "desc";
                    setPage(1); setSortBy(next.columnAccessor); setSortOrder(dir);
                    loadAllUsers({page: 1, pageSize: ACCESS_LOG_PAGE_SIZE, search: searchInput, sortBy: next.columnAccessor, sortOrder: dir});
                }}
                totalRecords={allUserCount}
                recordsPerPage={ACCESS_LOG_PAGE_SIZE}
                page={page}
                onPageChange={(nextPage) => { setPage(nextPage); loadAllUsers({page: nextPage, pageSize: ACCESS_LOG_PAGE_SIZE, search: searchInput, sortBy, sortOrder}); }}
                paginationText={({from, to, totalRecords}) => `${from}-${to} / ${totalRecords}`}
                minHeight={380}
            />
            <Text size="xs" c="dimmed" mt="xs">Page {page} / {totalPages}</Text>
        </Card>
    );
}
