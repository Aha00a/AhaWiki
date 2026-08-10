import {useCallback, useState} from "react";
import {fetchJson, logError, pagedParams, unwrapPaged} from "../api.js";
import {ACCESS_LOG_PAGE_SIZE} from "../constants.js";

export function useAccessLogData() {
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState("");
    const [accessLogs, setAccessLogs] = useState([]);
    const [accessLogCount, setAccessLogCount] = useState(0);
    const [sites, setSites] = useState([]);

    const loadAccessLogs = useCallback(async ({page = 1, pageSize = ACCESS_LOG_PAGE_SIZE, search = "", sortBy = "seq", sortOrder = "desc", siteSeq = ""} = {}) => {
        setLoading(true);
        setError("");
        try {
            const params = pagedParams({page, pageSize, search, sortBy, sortOrder});
            if (siteSeq) params.set("siteSeq", String(siteSeq));
            const data = await fetchJson(`/api/Admin/AccessLogs?${params.toString()}`);
            const {rows, count} = unwrapPaged(data);
            setAccessLogs(rows);
            setAccessLogCount(count);
        } catch (err) {
            logError("access-logs:load:error", err);
            setError(err.message || String(err));
        } finally {
            setLoading(false);
        }
    }, []);

    const loadSites = useCallback(async () => {
        try {
            const data = await fetchJson("/api/Admin/Sites");
            setSites(Array.isArray(data) ? data : []);
        } catch (err) {
            logError("access-logs:sites:load:error", err);
        }
    }, []);

    return {loading, error, accessLogs, accessLogCount, sites, loadAccessLogs, loadSites};
}
