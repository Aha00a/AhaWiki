import React, {useCallback, useEffect, useState} from "react";
import {Badge, Button, Card, Group, Stack, Table, Text, TextInput, Title} from "@mantine/core";
import {fetchJson, logError, sendForm} from "../api.js";

// The colour each schema class gives a page that wrote none and inherits none (logics.PageColor,
// wiki page InterpreterSchema «클래스 색»). One row per class the site's pages declare, most used
// first, saved one at a time.

const validColour = (v) => /^#([0-9a-f]{3}|[0-9a-f]{6})$/i.test(v.trim());
const longColour = (v) => v.trim().length === 4 ? "#" + v.trim().slice(1).split("").map((c) => c + c).join("") : v.trim();

// The shade a stripe of `colour` is drawn in on the wiki's background in the current theme:
// AhaWiki.Color.js is the one that decides it on the pages, so it decides it here too.
function shownOnTheme(colour) {
    const color = window.AhaWiki?.Color;
    if (!color || !validColour(colour)) return "";
    const bg = color.resolveColor(getComputedStyle(document.documentElement).getPropertyValue("--color-bg").trim());
    return color.forBackgrounds(longColour(colour).toLowerCase(), bg ? [bg] : []);
}

function Swatch({colour, title}) {
    return <span title={title} style={{display: "inline-block", width: 18, height: 18, borderRadius: 4, verticalAlign: "middle", background: colour || "transparent", border: "1px solid rgba(127,127,127,0.4)"}}/>;
}

export default function SchemaColorsCard({siteSeq}) {
    const [classes, setClasses] = useState([]);
    const [pageColors, setPageColors] = useState([]);
    const [drafts, setDrafts] = useState({});
    const [saving, setSaving] = useState("");
    const [error, setError] = useState("");

    const apply = (data) => {
        setClasses(data?.classes ?? []);
        setPageColors(data?.pageColors ?? []);
        setDrafts(Object.fromEntries((data?.classes ?? []).map((c) => [c.cls, c.color])));
    };

    const load = useCallback(async () => {
        if (!siteSeq) return;
        setError("");
        try { apply(await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/SchemaColors`)); }
        catch (err) { logError("schema-colors:load:error", err); setError(err.message); }
    }, [siteSeq]);

    useEffect(() => { load(); }, [load]);

    const save = async (cls, color) => {
        setSaving(cls);
        setError("");
        try { apply(await sendForm(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/SchemaColors`, "PUT", {cls, color})); }
        catch (err) { logError("schema-colors:save:error", err); setError(err.message); }
        finally { setSaving(""); }
    };

    return (
        <Card withBorder radius="md" padding="lg">
            <Group justify="space-between" mb="md"><Title order={3}>Schema 클래스 색</Title><Badge color="grape" variant="light">디자인</Badge></Group>
            <Text size="sm" c="dimmed" mb="xs">
                자기 <code>themeColor</code> 도, <code>isPartOf</code> 로 물려받는 색도 없는 페이지가 그 클래스의 색을 씁니다.
                정보 상자의 띠, 링크 미리보기, 인접 페이지 그래프의 점에 쓰입니다. 클래스는 적힌 그대로만 봅니다 — Movie 에 색이 없다고 CreativeWork 의 색을 쓰지 않습니다.
            </Text>
            {pageColors.length ? (
                <Stack gap={4} mb="md">
                    <Text size="xs" c="dimmed">페이지가 직접 적은 색 — 주제 색과 겹치지 않게 고르세요</Text>
                    <Group gap={10}>
                        {pageColors.map((p) => <Group key={p.page} gap={4}><Swatch colour={p.color} title={p.color}/><Text size="xs">{p.page}</Text></Group>)}
                    </Group>
                </Stack>
            ) : null}
            {error ? <Text c="red" size="sm" mb="md">{error}</Text> : null}
            <Table striped withTableBorder verticalSpacing={4}>
                <Table.Thead>
                    <Table.Tr><Table.Th>클래스</Table.Th><Table.Th>페이지</Table.Th><Table.Th>색</Table.Th><Table.Th>이 테마에서</Table.Th><Table.Th/></Table.Tr>
                </Table.Thead>
                <Table.Tbody>
                    {classes.map((c) => {
                        const draft = drafts[c.cls] ?? "";
                        const invalid = draft.trim() !== "" && !validColour(draft);
                        const shown = shownOnTheme(draft);
                        return (
                            <Table.Tr key={c.cls}>
                                <Table.Td><a href={`/w/schema:${encodeURIComponent(c.cls)}`} target="_blank" rel="noopener">{c.cls}</a></Table.Td>
                                <Table.Td>{c.pages}</Table.Td>
                                <Table.Td>
                                    <Group gap={6} wrap="nowrap">
                                        <input type="color" value={validColour(draft) ? longColour(draft) : "#000000"} onChange={(e) => setDrafts({...drafts, [c.cls]: e.target.value})} style={{width: 32, height: 26, padding: 0, border: "none", background: "none"}}/>
                                        <TextInput size="xs" w={100} placeholder="#rrggbb" value={draft} error={invalid} onChange={(e) => setDrafts({...drafts, [c.cls]: e.currentTarget.value})}/>
                                    </Group>
                                </Table.Td>
                                <Table.Td>{shown ? <Group gap={4} wrap="nowrap"><Swatch colour={shown} title={shown}/><Text size="xs" c="dimmed">{shown}</Text></Group> : null}</Table.Td>
                                <Table.Td>
                                    <Group gap={4} wrap="nowrap">
                                        <Button size="compact-xs" variant="light" color="grape" loading={saving === c.cls} disabled={invalid || draft.trim() === c.color} onClick={() => save(c.cls, draft.trim())}>저장</Button>
                                        <Button size="compact-xs" variant="subtle" color="gray" disabled={!c.color} onClick={() => save(c.cls, "")}>지우기</Button>
                                    </Group>
                                </Table.Td>
                            </Table.Tr>
                        );
                    })}
                </Table.Tbody>
            </Table>
        </Card>
    );
}
