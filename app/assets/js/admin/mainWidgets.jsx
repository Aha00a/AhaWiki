import React from "react";
import {Area, AreaChart, CartesianGrid, Legend, Line, LineChart, ResponsiveContainer, Tooltip, XAxis, YAxis} from "recharts";
import {Badge, Button, Card, Group, Progress, Stack, Table, Text, Title} from "@mantine/core";
import {makeTable} from "../component/commonWidgets";

export function SchedulerTable({schedulers, runningSchedulerName, onRun, onRefresh}) {
    const schedulersWithoutCalculate = schedulers.filter((scheduler) => scheduler.name !== "Calculate");
    return (
        <Stack gap="sm">
            <Group justify="space-between">
                <Title order={4}>Schedulers</Title>
                <Button size="xs" variant="light" onClick={onRefresh}>Refresh</Button>
            </Group>
            <Text size="xs" c="dimmed">Calculate는 사이트 상세(/Admin/Site/:seq)에서 실행하세요.</Text>
            <Progress size="sm" value={schedulersWithoutCalculate.length === 0 ? 0 : (schedulersWithoutCalculate.filter((scheduler) => scheduler.running).length / schedulersWithoutCalculate.length) * 100} color="blue" radius="xl"/>
            <Table striped highlightOnHover withTableBorder withColumnBorders>
                <Table.Thead><Table.Tr><Table.Th>Name</Table.Th><Table.Th>Interval</Table.Th><Table.Th>Next Delay(s)</Table.Th><Table.Th>Last Started</Table.Th><Table.Th>Last Finished</Table.Th><Table.Th>Result</Table.Th><Table.Th>Run Count</Table.Th><Table.Th>Action</Table.Th></Table.Tr></Table.Thead>
                <Table.Tbody>{schedulersWithoutCalculate.map((scheduler) => <Table.Tr key={scheduler.name}><Table.Td>{scheduler.name}</Table.Td><Table.Td>{`${scheduler.minSeconds}s ~ ${scheduler.maxSeconds}s`}</Table.Td><Table.Td>{scheduler.nextDelaySeconds ?? "-"}</Table.Td><Table.Td>{scheduler.lastStartedAt ?? "-"}</Table.Td><Table.Td>{scheduler.lastFinishedAt ?? "-"}</Table.Td><Table.Td>{scheduler.lastResult ?? "-"}</Table.Td><Table.Td>{scheduler.runCount ?? 0}</Table.Td><Table.Td><Button size="xs" variant="filled" loading={runningSchedulerName === scheduler.name} disabled={scheduler.running} onClick={() => onRun(scheduler.name)}>{scheduler.running ? "Running..." : "Run now"}</Button></Table.Td></Table.Tr>)}</Table.Tbody>
            </Table>
        </Stack>
    );
}

export function normalizeDailyRows(rows) { return [...rows].map((row) => ({ymd: row.ymd, count: Number(row.count ?? 0)})).sort((left, right) => (left.ymd > right.ymd ? 1 : -1)); }

function Sparkline({rows, color}) {
    const data = normalizeDailyRows(rows).slice(-30);
    if (data.length === 0) return <Text size="xs" c="dimmed">No data</Text>;
    const latest = data[data.length - 1]?.count ?? 0; const previous = data[data.length - 2]?.count ?? latest; const delta = latest - previous;
    return <Stack gap={4}><div style={{width: "100%", height: 72}} role="img" aria-label="trend sparkline"><ResponsiveContainer width="100%" height="100%"><AreaChart data={data} margin={{top: 4, right: 0, left: 0, bottom: 0}}><defs><linearGradient id={`sparklineGradient-${color}`} x1="0" y1="0" x2="0" y2="1"><stop offset="0%" stopColor={`var(--mantine-color-${color}-4)`} stopOpacity={0.35}/><stop offset="100%" stopColor={`var(--mantine-color-${color}-1)`} stopOpacity={0.1}/></linearGradient></defs><Tooltip cursor={false} labelFormatter={(value) => `Date: ${value}`} formatter={(value) => [value, "Count"]}/><Area type="monotone" dataKey="count" stroke={`var(--mantine-color-${color}-6)`} strokeWidth={2} fill={`url(#sparklineGradient-${color})`} dot={false} activeDot={{r: 3}} isAnimationActive={false}/></AreaChart></ResponsiveContainer></div><Group justify="space-between"><Text size="xs" c="dimmed">최근 30일</Text><Badge color={delta >= 0 ? "teal" : "red"} variant="light" size="xs">{delta >= 0 ? "+" : ""}{delta} vs yesterday</Badge></Group></Stack>;
}

export function StatTrendCard({title, total, rows, color}) { return <Card withBorder radius="md" padding="md"><Stack gap={8}><Group justify="space-between" align="flex-start"><Text size="sm" c="dimmed">{title}</Text><Badge color={color} variant="light">30d</Badge></Group><Title order={3}>{total}</Title><Sparkline rows={rows} color={color}/></Stack></Card>; }

export function DailyStatTable({title, description, rows, badgeColor}) {
    const maxCount = rows.reduce((max, row) => Math.max(max, row.count ?? 0), 0); const bars = rows.slice(-14);
    return <Card withBorder radius="md" padding="lg"><Group justify="space-between" mb="md"><Title order={3}>{title}</Title><Badge color={badgeColor} variant="light">{rows.length} days</Badge></Group><Text size="sm" c="dimmed" mb="md">{description}</Text><Stack gap={8} mb="md">{bars.map((row) => {const count = row.count ?? 0; const widthPercent = maxCount === 0 ? 0 : (count / maxCount) * 100; return <Stack key={row.ymd} gap={4}><Group justify="space-between"><Text size="xs" c="dimmed">{row.ymd}</Text><Text size="xs" fw={700}>{count}</Text></Group><Progress value={widthPercent} color={badgeColor} radius="xl"/></Stack>;})}</Stack>{makeTable(["Date", "Count"], rows.map((row) => [row.ymd, row.count]))}</Card>;
}

export function MultiTrendChart({series}) {
    const dateSet = new Set(); series.forEach((line) => normalizeDailyRows(line.rows).forEach((row) => dateSet.add(row.ymd)));
    const dates = [...dateSet].sort().slice(-30); if (dates.length === 0) return <Text c="dimmed" size="sm">차트 데이터가 없습니다.</Text>;
    const colorByName = {}; const chartDataByDate = new Map(dates.map((date) => [date, {date}]));
    series.forEach((line) => { colorByName[line.name] = line.color; const indexed = new Map(normalizeDailyRows(line.rows).map((row) => [row.ymd, row.count])); dates.forEach((date) => { chartDataByDate.get(date)[line.name] = indexed.get(date) ?? 0; }); });
    const chartData = dates.map((date) => chartDataByDate.get(date));
    return <Stack gap={8}><div style={{width: "100%", height: 280}} role="img" aria-label="daily trends chart"><ResponsiveContainer width="100%" height="100%"><LineChart data={chartData} margin={{top: 8, right: 12, bottom: 8, left: 0}}><CartesianGrid stroke="var(--mantine-color-gray-2)" strokeDasharray="3 3"/><XAxis dataKey="date" tickFormatter={(value) => value.slice(5)} tick={{fontSize: 12}}/><YAxis allowDecimals={false} tick={{fontSize: 12}}/><Tooltip labelFormatter={(value) => `Date: ${value}`}/><Legend verticalAlign="top" height={30}/>{series.map((line) => <Line key={line.name} type="monotone" dataKey={line.name} stroke={`var(--mantine-color-${colorByName[line.name]}-6)`} strokeWidth={2.5} dot={false} activeDot={{r: 4}} isAnimationActive={false}/>)}</LineChart></ResponsiveContainer></div><Group gap={8}>{series.map((line) => <Badge key={line.name} color={line.color} variant="light">{line.name}</Badge>)}</Group></Stack>;
}
