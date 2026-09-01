import {useCallback, useState} from "react";
import {fetchJson, logError, pagedParams, sendForm, sendJson, unwrapPaged} from "../api.js";
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
            const params = pagedParams({page, pageSize, search, sortBy, sortOrder});
            const data = await fetchJson(`/api/Admin/CrawlerCache?${params.toString()}`);
            const {rows, count} = unwrapPaged(data);
            setCrawlerCaches(rows);
            setCrawlerCacheCount(count);
        } catch (err) {
            logError("crawler-cache:load:error", err);
        } finally {
            setLoading(false);
        }
    }, []);

    const refreshCrawlerCache = useCallback(async (url, currentParams) => {
        setRefreshingUrl(url);
        try {
            await sendForm("/api/Admin/CrawlerCache/Refresh", "POST", {url});
            await loadCrawlerCaches(currentParams);
        } finally {
            setRefreshingUrl("");
        }
    }, [loadCrawlerCaches]);

    const deleteCrawlerCache = useCallback(async (url, currentParams) => {
        setDeletingUrl(url);
        try {
            await sendJson(`/api/Admin/CrawlerCache?url=${encodeURIComponent(url)}`, "DELETE");
            await loadCrawlerCaches(currentParams);
        } finally {
            setDeletingUrl("");
        }
    }, [loadCrawlerCaches]);

    return {loading, crawlerCaches, crawlerCacheCount, refreshingUrl, deletingUrl, loadCrawlerCaches, refreshCrawlerCache, deleteCrawlerCache};
}
