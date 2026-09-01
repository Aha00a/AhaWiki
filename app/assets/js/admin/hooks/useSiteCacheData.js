import {useCallback, useState} from "react";
import {fetchJson, logError, sendJson} from "../api.js";

export function useSiteCacheData(siteSeq) {
    const [clearing, setClearing] = useState(false);
    const [memoryCacheStats, setMemoryCacheStats] = useState([]);
    const [error, setError] = useState("");

    const clearSiteCache = useCallback(async () => {
        if (!siteSeq) return;
        setClearing(true);
        setError("");
        try {
            await sendJson(`/api/cache/${siteSeq}`, "DELETE");
        } catch (err) {
            logError("cache:clear:error", siteSeq, err);
            setError(err.message || String(err));
        } finally {
            setClearing(false);
        }
    }, [siteSeq]);

    const loadMemoryCacheStats = useCallback(async () => {
        try {
            const data = await fetchJson("/api/Admin/MemoryCacheStats");
            setMemoryCacheStats(Array.isArray(data) ? data : []);
        } catch (err) {
            logError("memory-cache:load:error", err);
        }
    }, []);

    return {clearing, memoryCacheStats, error, clearSiteCache, loadMemoryCacheStats};
}
