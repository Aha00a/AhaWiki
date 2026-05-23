import {useCallback, useState} from "react";
import {fetchJson, logError} from "../api.js";

export function useRecentChangesData() {
    const [loading, setLoading] = useState(true);
    const [recentChanges, setRecentChanges] = useState([]);
    const [sites, setSites] = useState([]);

    const loadRecentChanges = useCallback(async (n = 50) => {
        setLoading(true);
        try {
            const data = await fetchJson(`/api/Admin/RecentChanges?n=${encodeURIComponent(n)}`);
            setRecentChanges(data);
        } catch (err) {
            logError("recent-changes:load:error", err);
        } finally {
            setLoading(false);
        }
    }, []);

    const loadSites = useCallback(async () => {
        try {
            const data = await fetchJson("/api/Admin/Sites");
            setSites(Array.isArray(data) ? data : []);
        } catch (err) {
            logError("recent-changes:sites:load:error", err);
        }
    }, []);

    return {loading, recentChanges, sites, loadRecentChanges, loadSites};
}
