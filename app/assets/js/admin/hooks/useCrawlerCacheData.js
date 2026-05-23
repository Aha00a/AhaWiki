import {useCallback, useState} from "react";
import {fetchJson, fetchCsrfToken, logError} from "../api.js";
import {CRAWLER_CACHE_PAGE_SIZE} from "../constants.js";

export function useCrawlerCacheData() {
    const [loading, setLoading] = useState(true);
    const [crawlerCaches, setCrawlerCaches] = useState([]);
    const [crawlerCacheCount, setCrawlerCacheCount] = useState(0);
    const [refreshingUrl, setRefreshingUrl] = useState("");
    const [deletingUrl, setDeletingUrl] = useState("");

    const loadCrawlerCaches = useCallback(async ({page = 1, pageSize = CRAWLER_CACHE_PAGE_SIZE, search = "", sortBy = "id", sortOrder = "desc"} = {}) => {
        setLoading(true);
        try {
            const params = new URLSearchParams({page: String(page), pageSize: String(pageSize), search, sortBy, sortOrder});
            const data = await fetchJson(`/api/Admin/CrawlerCache?${params.toString()}`);
            const rows = Array.isArray(data?.array) ? data.array : (Array.isArray(data) ? data : []);
            setCrawlerCaches(rows);
            setCrawlerCacheCount(Number(data?.count ?? rows.length));
        } catch (err) {
            logError("crawler-cache:load:error", err);
        } finally {
            setLoading(false);
        }
    }, []);

    const refreshCrawlerCache = useCallback(async (url, currentParams) => {
        setRefreshingUrl(url);
        try {
            const csrfToken = await fetchCsrfToken();
            const payload = new URLSearchParams();
            payload.set("url", url);
            payload.set(csrfToken.name, csrfToken.value);
            const response = await fetch("/api/Admin/CrawlerCache/Refresh", {
                method: "POST",
                credentials: "same-origin",
                headers: {"Content-Type": "application/x-www-form-urlencoded; charset=UTF-8", "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value},
                body: payload.toString(),
            });
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            await loadCrawlerCaches(currentParams);
        } finally {
            setRefreshingUrl("");
        }
    }, [loadCrawlerCaches]);

    const deleteCrawlerCache = useCallback(async (url, currentParams) => {
        setDeletingUrl(url);
        try {
            const csrfToken = await fetchCsrfToken();
            const response = await fetch(`/api/Admin/CrawlerCache?url=${encodeURIComponent(url)}`, {
                method: "DELETE",
                credentials: "same-origin",
                headers: {"Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value},
            });
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            await loadCrawlerCaches(currentParams);
        } finally {
            setDeletingUrl("");
        }
    }, [loadCrawlerCaches]);

    return {loading, crawlerCaches, crawlerCacheCount, refreshingUrl, deletingUrl, loadCrawlerCaches, refreshCrawlerCache, deleteCrawlerCache};
}
