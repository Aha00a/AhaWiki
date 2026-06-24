import {useCallback, useState} from "react";
import {fetchJson, logError} from "../api.js";

export function useRecentChangesData() {
    const [loading, setLoading] = useState(true);
    const [recentChanges, setRecentChanges] = useState([]);
    const [sites, setSites] = useState([]);

    const loadRecentChanges = useCallback(async (n = 50, includeMinorEdit = false, includeViaApi = false) => {
        setLoading(true);
        try {
            const params = new URLSearchParams({
                n: String(n),
                includeMinorEdit: includeMinorEdit ? "1" : "0",
                includeViaApi: includeViaApi ? "1" : "0",
            });
            const data = await fetchJson(`/api/Admin/RecentChanges?${params.toString()}`);
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
