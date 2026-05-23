import React, {useEffect, useState} from "react";
import {Outlet, useNavigate, useParams} from "react-router-dom";
import {Badge, Button, Card, Group, Stack, Text, TextInput, Title} from "@mantine/core";
import {useAdminContext} from "../context/AdminContext.jsx";
import {useSiteData} from "../hooks/useSiteData.js";

export default function SiteLayout() {
    const {siteSeq} = useParams();
    const {me} = useAdminContext();
    const navigate = useNavigate();
    const {site, sitePageNames, saveSiteMeta, savingSiteMeta, refreshPageNames, error} = useSiteData(siteSeq);
    const [siteMetaForm, setSiteMetaForm] = useState({abbr: "", mainDomain: "", publicListedOrder: ""});

    useEffect(() => {
        setSiteMetaForm({
            abbr: site?.abbr ?? "",
            mainDomain: site?.mainDomain ?? "",
            publicListedOrder: site?.publicListedOrder == null ? "" : String(site.publicListedOrder),
        });
    }, [site]);

    const selectedSiteDomainsText = (site?.domains ?? []).join(", ") || "-";

    return (
        <Stack gap="lg">
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="xs">
                    <Title order={4}>사이트 상세</Title>
                    <Badge color="blue" variant="light">Site Detail</Badge>
                </Group>
                <Stack gap="sm">
                    <Group justify="space-between" align="flex-start" wrap="wrap">
                        <Stack gap={2}>
                            <Text size="xs" c="dimmed">선택된 사이트</Text>
                            <Text fw={700}>{site ? `${site.name} (#${site.seq})` : "선택된 사이트 없음"}</Text>
                            <Text size="sm" c="dimmed">도메인: {selectedSiteDomainsText}</Text>
                        </Stack>
                        <Button variant="light" size="xs" onClick={() => navigate("/Admin/Site")}>← 사이트 목록</Button>
                    </Group>
                    <Group align="end" mt="sm">
                        <TextInput label="Abbr" value={siteMetaForm.abbr} onChange={(e) => setSiteMetaForm({...siteMetaForm, abbr: e.currentTarget.value})} disabled={!site}/>
                        <TextInput label="Main Domain" value={siteMetaForm.mainDomain} onChange={(e) => setSiteMetaForm({...siteMetaForm, mainDomain: e.currentTarget.value})} disabled={!site}/>
                        <TextInput label="Public Listed Order" type="number" min="0" step="0.01" placeholder="empty means hidden" value={siteMetaForm.publicListedOrder} onChange={(e) => setSiteMetaForm({...siteMetaForm, publicListedOrder: e.currentTarget.value})} disabled={!site}/>
                        <Button variant="light" disabled={!site || !siteMetaForm.abbr.trim()} loading={savingSiteMeta} onClick={() => saveSiteMeta(siteMetaForm)}>Save site meta</Button>
                    </Group>
                    {error ? <Text size="sm" c="red">{error}</Text> : null}
                </Stack>
            </Card>
            <Outlet context={{site, siteSeq, sitePageNames, me, refreshPageNames}}/>
        </Stack>
    );
}
