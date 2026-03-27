import React, { useEffect, useState } from "https://esm.sh/react@18.3.1";
import { createRoot } from "https://esm.sh/react-dom@18.3.1/client";
import {
  AppShell,
  Badge,
  Card,
  Container,
  Group,
  Loader,
  MantineProvider,
  Paper,
  Stack,
  Table,
  Text,
  Title,
} from "https://esm.sh/@mantine/core@8.3.6";

function useJson(url) {
  const [state, setState] = useState({
    loading: true,
    error: "",
    data: [],
  });

  useEffect(() => {
    let isMounted = true;

    fetch(url, { credentials: "same-origin" })
      .then((res) => {
        if (!res.ok) throw new Error(`HTTP ${res.status}`);
        return res.json();
      })
      .then((data) => {
        if (isMounted) {
          setState({ loading: false, error: "", data });
        }
      })
      .catch((err) => {
        if (isMounted) {
          setState({ loading: false, error: String(err), data: [] });
        }
      });

    return () => {
      isMounted = false;
    };
  }, [url]);

  return state;
}

function LoadingOrError({ loading, error, loadingText, errorPrefix }) {
  if (loading) {
    return (
      <Group gap="xs">
        <Loader size="sm" />
        <Text size="sm">{loadingText}</Text>
      </Group>
    );
  }

  if (error) {
    return (
      <Text c="red" size="sm">
        {errorPrefix}: {error}
      </Text>
    );
  }

  return null;
}

function SitesSection({ state }) {
  const stateView = (
    <LoadingOrError
      loading={state.loading}
      error={state.error}
      loadingText="사이트 목록을 불러오는 중..."
      errorPrefix="사이트 목록 로딩 실패"
    />
  );

  if (state.loading || state.error) return stateView;

  return (
    <Table striped withTableBorder withColumnBorders>
      <Table.Thead>
        <Table.Tr>
          <Table.Th>Seq</Table.Th>
          <Table.Th>Name</Table.Th>
        </Table.Tr>
      </Table.Thead>
      <Table.Tbody>
        {state.data.map((site) => (
          <Table.Tr key={site.seq}>
            <Table.Td>{site.seq}</Table.Td>
            <Table.Td>{site.name}</Table.Td>
          </Table.Tr>
        ))}
      </Table.Tbody>
    </Table>
  );
}

function UsersSection({ state }) {
  const stateView = (
    <LoadingOrError
      loading={state.loading}
      error={state.error}
      loadingText="사용자 목록을 불러오는 중..."
      errorPrefix="사용자 목록 로딩 실패"
    />
  );

  if (state.loading || state.error) return stateView;

  return (
    <Table striped withTableBorder withColumnBorders>
      <Table.Thead>
        <Table.Tr>
          <Table.Th>User</Table.Th>
          <Table.Th>Email</Table.Th>
          <Table.Th>Nickname</Table.Th>
          <Table.Th>Created</Table.Th>
        </Table.Tr>
      </Table.Thead>
      <Table.Tbody>
        {state.data.map((user) => (
          <Table.Tr key={`${user.user}-${user.created}`}>
            <Table.Td>{user.user}</Table.Td>
            <Table.Td>{user.email}</Table.Td>
            <Table.Td>{user.nickname}</Table.Td>
            <Table.Td>{user.created}</Table.Td>
          </Table.Tr>
        ))}
      </Table.Tbody>
    </Table>
  );
}

function Main() {
  const sites = useJson("/api/admin/sites");
  const users = useJson("/api/admin/site-users");

  return (
    <MantineProvider defaultColorScheme="light">
      <AppShell padding="md" header={{ height: 56 }}>
        <AppShell.Header>
          <Group h="100%" px="md" justify="space-between">
            <Title order={4}>Admin</Title>
            <Badge variant="light" color="blue">
              React + Mantine
            </Badge>
          </Group>
        </AppShell.Header>

        <AppShell.Main>
          <Container size="lg">
            <Stack gap="md">
              <Paper p="md" withBorder>
                <Text>
                  Admin 페이지를 Mantine UI와 JSX 기반 단일 파일(`admin.es6`) 구조로 변경했습니다.
                </Text>
              </Paper>

              <Card withBorder shadow="sm" radius="md" padding="lg">
                <Stack gap="sm">
                  <Title order={3}>Sites</Title>
                  <SitesSection state={sites} />
                </Stack>
              </Card>

              <Card withBorder shadow="sm" radius="md" padding="lg">
                <Stack gap="sm">
                  <Title order={3}>Site Users (current host)</Title>
                  <UsersSection state={users} />
                </Stack>
              </Card>
            </Stack>
          </Container>
        </AppShell.Main>
      </AppShell>
    </MantineProvider>
  );
}

function pageLoad() {
  const rootElement = document.getElementById("main");
  if (!rootElement) return;

  createRoot(rootElement).render(<Main />);
}

window.addEventListener("DOMContentLoaded", pageLoad);
