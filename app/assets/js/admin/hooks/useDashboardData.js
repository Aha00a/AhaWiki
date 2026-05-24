import {useCallback, useState} from "react";
import {fetchJson} from "../api.js";
import {logError} from "../api.js";

export function useDashboardData() {
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState("");
    const [sites, setSites] = useState([]);
    const [allUsers, setAllUsers] = useState([]);
    const [dailyStats, setDailyStats] = useState({userCreated: [], pageCreated: [], pageEdited: []});
    const [recentChanges, setRecentChanges] = useState([]);
    const [topViewedPages, setTopViewedPages] = useState([]);
    const [memoryCacheStats, setMemoryCacheStats] = useState([]);

    const load = useCallback(async () => {
        setLoading(true);
        setError("");
        try {
            const [siteData, allUserData, dailyStatsData, recentChangesData, topViewedPagesData] = await Promise.all([
                fetchJson("/api/Admin/Sites"),
                fetchJson("/api/Admin/Users"),
                fetchJson("/api/Admin/DailyStats"),
                fetchJson("/api/Admin/RecentChanges?n=30"),
                fetchJson("/api/Admin/TopViewedPages?n=30"),
            ]);
            const allUserRows = Array.isArray(allUserData?.array) ? allUserData.array : (Array.isArray(allUserData) ? allUserData : []);
            setSites(siteData);
            setAllUsers(allUserRows);
            setRecentChanges(recentChangesData);
            setTopViewedPages(topViewedPagesData);
            setDailyStats({
                userCreated: dailyStatsData?.userCreated ?? [],
                pageCreated: dailyStatsData?.pageCreated ?? [],
                pageEdited: dailyStatsData?.pageEdited ?? [],
            });
        } catch (err) {
            logError("dashboard:load:error", err);
            setError(err.message || String(err));
        } finally {
            setLoading(false);
        }
    }, []);

    const loadSitesOnly = useCallback(async () => {
        setLoading(true);
        setError("");
        try {
            const siteData = await fetchJson("/api/Admin/Sites");
            setSites(Array.isArray(siteData) ? siteData : []);
            setAllUsers([]);
            setRecentChanges([]);
            setTopViewedPages([]);
            setDailyStats({userCreated: [], pageCreated: [], pageEdited: []});
        } catch (err) {
            logError("dashboard:sites-only:load:error", err);
            setError(err.message || String(err));
        } finally {
            setLoading(false);
        }
    }, []);

    const loadMemoryCacheStats = useCallback(async () => {
        try {
            const data = await fetchJson("/api/Admin/MemoryCacheStats");
            setMemoryCacheStats(Array.isArray(data) ? data : []);
        } catch (err) {
            logError("memory-cache:load:error", err);
        }
    }, []);

    return {loading, error, sites, allUsers, dailyStats, recentChanges, topViewedPages, memoryCacheStats, load, loadSitesOnly, loadMemoryCacheStats};
}
