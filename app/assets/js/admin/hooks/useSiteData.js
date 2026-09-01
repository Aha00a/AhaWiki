import {useCallback, useEffect, useState} from "react";
import {fetchJson, logError, sendForm} from "../api.js";

export function useSiteData(siteSeq) {
    const [sites, setSites] = useState([]);
    const [sitePageNames, setSitePageNames] = useState([]);
    const [savingSiteMeta, setSavingSiteMeta] = useState(false);
    const [error, setError] = useState("");

    useEffect(() => {
        fetchJson("/api/Admin/Sites").then(setSites).catch((err) => logError("site-data:sites:error", err));
    }, []);

    useEffect(() => {
        if (!siteSeq) return;
        fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/PageNames`)
            .then((data) => setSitePageNames(Array.isArray(data) ? data : []))
            .catch((err) => logError("site-data:page-names:error", err));
    }, [siteSeq]);

    const site = sites.find((s) => String(s.seq) === String(siteSeq)) ?? null;

    const saveSiteMeta = useCallback(async (nextMeta) => {
        if (!siteSeq) return null;
        setSavingSiteMeta(true);
        setError("");
        try {
            const updated = await sendForm(`/api/Admin/Site/${encodeURIComponent(siteSeq)}`, "PUT", {
                abbr: nextMeta?.abbr,
                mainDomain: nextMeta?.mainDomain,
                publicListedOrder: nextMeta?.publicListedOrder,
            });
            setSites((prev) => prev.map((s) => s.seq === updated.seq ? {...s, ...updated} : s));
            return updated;
        } catch (err) {
            logError("site-meta:save:error", err);
            setError(err.message || String(err));
            return null;
        } finally {
            setSavingSiteMeta(false);
        }
    }, [siteSeq]);

    const refreshPageNames = useCallback(async () => {
        if (!siteSeq) return [];
        const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/PageNames`);
        const names = Array.isArray(data) ? data : [];
        setSitePageNames(names);
        return names;
    }, [siteSeq]);

    return {site, sites, sitePageNames, saveSiteMeta, savingSiteMeta, refreshPageNames, error};
}
