import {useCallback, useState} from "react";
import {fetchJson, fetchCsrfToken, logError} from "../api.js";

export function useSiteCacheData(siteSeq) {
    const [clearing, setClearing] = useState(false);
    const [memoryCacheStats, setMemoryCacheStats] = useState([]);
    const [error, setError] = useState("");

    const clearSiteCache = useCallback(async () => {
        if (!siteSeq) return;
        setClearing(true);
        setError("");
        try {
            const csrfToken = await fetchCsrfToken();
            const response = await fetch(`/api/cache/${siteSeq}`, {method: "DELETE", credentials: "same-origin", headers: {"Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value}});
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
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
