import React, {useEffect, useState} from "react";
import {useNavigate, useOutletContext} from "react-router-dom";
import {Anchor, Badge, Button, Card, ColorInput, Group, Paper, SimpleGrid, Stack, Text, TextInput, Title} from "@mantine/core";
import {useSiteConfigData} from "../hooks/useSiteConfigData.js";

export default function SiteConfigPage() {
    const {site, siteSeq, saveSiteMeta, savingSiteMeta, error: siteMetaError} = useOutletContext();
    const navigate = useNavigate();
    const [siteMetaForm, setSiteMetaForm] = useState({abbr: "", mainDomain: "", publicListedOrder: ""});

    useEffect(() => {
        setSiteMetaForm({
            abbr: site?.abbr ?? "",
            mainDomain: site?.mainDomain ?? "",
            publicListedOrder: site?.publicListedOrder == null ? "" : String(site.publicListedOrder),
        });
    }, [site]);
    const {faviconUrl, faviconObjectKey, uploadingFavicon, deletingFavicon, siteTheme, setSiteTheme, savingTheme, error, loadFavicon, uploadFavicon, resetFavicon, loadTheme, saveTheme} = useSiteConfigData(siteSeq);
    const [faviconFile, setFaviconFile] = useState(null);

    useEffect(() => {
        if (!siteSeq) return;
        loadFavicon();
        loadTheme();
    }, [siteSeq]);

    const selectedSiteDomainsText = (site?.domains ?? []).join(", ") || "-";

    return (
        <>
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
                    {siteMetaError ? <Text size="sm" c="red">{siteMetaError}</Text> : null}
                </Stack>
            </Card>
            <SimpleGrid cols={{base: 1, xl: 2}} spacing="lg">
                <Card withBorder radius="md" padding="lg">
                    <Group justify="space-between" mb="md"><Title order={3}>Favicon</Title><Badge color="blue" variant="light">브랜딩</Badge></Group>
                    <Text size="sm" c="dimmed" mb="md">선택한 사이트의 favicon을 관리자 업로드로 교체합니다.</Text>
                    {error ? <Text c="red" size="sm" mb="md">{error}</Text> : null}
                    <Group align="flex-start" grow mb="md">
                        <Stack gap={6}>
                            <Text size="sm" fw={600}>현재 favicon</Text>
                            <img src={faviconUrl} alt="Current favicon" style={{width: 32, height: 32, borderRadius: 6, border: "1px solid #e5e7eb"}}/>
                            <Text size="xs" c="dimmed" style={{wordBreak: "break-all"}}>{faviconObjectKey || "/public/favicon.png"}</Text>
                            <Anchor size="xs" href={faviconUrl} target="_blank" rel="noopener">새 탭으로 보기</Anchor>
                        </Stack>
                        <Stack gap={8}>
                            <Text size="sm" fw={600}>새 favicon 업로드</Text>
                            <input type="file" accept="image/*,.ico" onChange={(e) => setFaviconFile(e.currentTarget.files?.[0] ?? null)}/>
                            <Group>
                                <Button variant="filled" loading={uploadingFavicon} disabled={!faviconFile || !siteSeq} onClick={async () => { await uploadFavicon(faviconFile); await loadFavicon(); }}>Upload favicon</Button>
                                <Button variant="light" disabled={!siteSeq} onClick={() => loadFavicon()}>Refresh</Button>
                                <Button color="red" variant="light" loading={deletingFavicon} disabled={!siteSeq} onClick={async () => { await resetFavicon(); await loadFavicon(); setFaviconFile(null); }}>Reset to default</Button>
                            </Group>
                            <Text size="xs" c="dimmed">권장: 32x32 또는 48x48 PNG/ICO</Text>
                        </Stack>
                    </Group>
                </Card>
            </SimpleGrid>
            <Card withBorder radius="md" padding="lg">
                <Group justify="space-between" mb="md"><Title order={3}>Theme</Title><Badge color="grape" variant="light">디자인</Badge></Group>
                <Text size="sm" c="dimmed" mb="md">사이트별 헤더/푸터 배경색·전경색을 16진수(#RGB, #RRGGBB, #RRGGBBAA)로 지정할 수 있습니다.</Text>
                {(() => {
                    const hue = siteTheme.defaultHue === "" ? 220 : Number(siteTheme.defaultHue);
                    return (
                        <Stack gap={4} mb="md">
                            <Text size="sm" fw={500}>기본 색조 (Default Hue) <Text span size="xs" c="dimmed">— 사용자 미설정 시 초기값, 미지정 시 220</Text></Text>
                            <Group gap="sm" align="center">
                                <input
                                    type="range" min={0} max={360}
                                    value={hue}
                                    onChange={(e) => setSiteTheme((prev) => ({...prev, defaultHue: e.target.value}))}
                                    style={{flex: 1, height: 8, borderRadius: 4, appearance: "none", WebkitAppearance: "none", background: "linear-gradient(to right, hsl(0,80%,55%), hsl(30,80%,55%), hsl(60,80%,55%), hsl(90,80%,55%), hsl(120,80%,55%), hsl(150,80%,55%), hsl(180,80%,55%), hsl(210,80%,55%), hsl(240,80%,55%), hsl(270,80%,55%), hsl(300,80%,55%), hsl(330,80%,55%), hsl(360,80%,55%))", cursor: "pointer", outline: "none", border: "none"}}
                                />
                                <div style={{width: 24, height: 24, borderRadius: 6, backgroundColor: `hsl(${hue},80%,55%)`, border: "1px solid rgba(0,0,0,0.15)", flexShrink: 0}}/>
                                <Text size="sm" style={{width: 32, textAlign: "right"}}>{hue}</Text>
                            </Group>
                        </Stack>
                    );
                })()}
                <SimpleGrid cols={{base: 1, lg: 2}} spacing="md">
                    <SimpleGrid cols={{base: 1, sm: 2}} spacing="md">
                        {[
                            {key: "headerBackgroundColor", label: "Header 배경색", placeholder: "#EEEEEE", swatches: ["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#6c5ce7", "#0b7285"]},
                            {key: "headerForegroundColor", label: "Header 전경색", placeholder: "#000000", swatches: ["#111111", "#212529", "#495057", "#ffffff", "#f1f3f5", "#ffd43b"]},
                            {key: "bodyBackgroundColor", label: "Body 배경색", placeholder: "#FFFFFF", swatches: ["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#f1f3f5", "#e9ecef"]},
                            {key: "bodyForegroundColor", label: "Body 전경색", placeholder: "#000000", swatches: ["#111111", "#212529", "#495057", "#000000", "#343a40", "#ffffff"]},
                            {key: "footerBackgroundColor", label: "Footer 배경색", placeholder: "#EEEEEE", swatches: ["#ffffff", "#f8f9fa", "#1f2937", "#111111", "#2b8a3e", "#862e9c"]},
                            {key: "footerForegroundColor", label: "Footer 전경색", placeholder: "#000000", swatches: ["#111111", "#212529", "#495057", "#ffffff", "#f1f3f5", "#ff922b"]},
                        ].map(({key, label, placeholder, swatches}) => (
                            <ColorInput key={key} label={label} placeholder={placeholder} format="hexa" value={siteTheme[key]} onChange={(value) => setSiteTheme((prev) => ({...prev, [key]: value}))} swatches={swatches} withEyeDropper={false} clearable/>
                        ))}
                    </SimpleGrid>
                    <Paper withBorder radius="md" p="md" style={{overflow: "hidden"}}>
                        <Text size="sm" fw={600} mb={8}>미리보기</Text>
                        <Stack gap={0} style={{borderRadius: 10, overflow: "hidden", border: "1px solid #e9ecef"}}>
                            <div style={{backgroundColor: siteTheme.headerBackgroundColor || "#EEEEEE", color: siteTheme.headerForegroundColor || "#000000", padding: "12px 14px", fontWeight: 600}}>Header Preview</div>
                            <div style={{padding: "16px 14px", backgroundColor: siteTheme.bodyBackgroundColor || "#FFFFFF", color: siteTheme.bodyForegroundColor || "#000000"}}>
                                <div>콘텐츠 영역 (고정 미리보기)</div>
                                {(() => {
                                    const hue = siteTheme.defaultHue === "" ? 220 : Number(siteTheme.defaultHue);
                                    return (
                                        <div style={{marginTop: 8, display: "flex", gap: 8, alignItems: "center", flexWrap: "wrap"}}>
                                            <span style={{color: `hsl(${hue},99%,43%)`, textDecoration: "underline", fontSize: 13, cursor: "pointer"}}>링크</span>
                                            <span style={{padding: "1px 6px", borderRadius: 4, backgroundColor: `hsl(${hue},100%,7%)`, color: `hsl(${hue},100%,80%)`, fontFamily: "monospace", fontSize: 12}}>code</span>
                                            <span style={{padding: "2px 10px", borderRadius: 5, backgroundColor: `hsl(${hue},89%,47%)`, color: "#fff", fontSize: 12, fontWeight: 600}}>버튼</span>
                                            <span style={{display: "block", width: "100%", height: 6, borderRadius: 3, background: `linear-gradient(to right, hsl(${hue},100%,96%), hsl(${hue},89%,47%))`, marginTop: 4}}/>
                                        </div>
                                    );
                                })()}
                            </div>
                            <div style={{backgroundColor: siteTheme.footerBackgroundColor || "#EEEEEE", color: siteTheme.footerForegroundColor || "#000000", padding: "12px 14px", fontWeight: 600}}>Footer Preview</div>
                        </Stack>
                    </Paper>
                </SimpleGrid>
                <Group mt="md">
                    <Button variant="filled" color="grape" loading={savingTheme} disabled={!siteSeq} onClick={async () => { await saveTheme(siteTheme); await loadTheme(); }}>Save theme</Button>
                    <Button variant="light" disabled={!siteSeq} onClick={() => loadTheme()}>Refresh</Button>
                    <Button color="gray" variant="light" disabled={!siteSeq} onClick={async () => {
                        const empty = {headerBackgroundColor: "", headerForegroundColor: "", bodyBackgroundColor: "", bodyForegroundColor: "", footerBackgroundColor: "", footerForegroundColor: "", defaultHue: ""};
                        setSiteTheme(empty);
                        await saveTheme(empty);
                        await loadTheme();
                    }}>Reset</Button>
                </Group>
            </Card>
        </>
    );
}
