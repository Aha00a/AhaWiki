import {useCallback, useState} from "react";
import {fetchJson, logError} from "../api.js";
import {ADMIN_PAGE_META_PAGE_SIZE} from "../constants.js";

export function useAdminPageMetaData(siteSeq) {
    const [rows, setRows] = useState([]);
    const [count, setCount] = useState(0);

    const load = useCallback(async ({page = 1, pageSize = ADMIN_PAGE_META_PAGE_SIZE, search = "", sortBy = "dateUpdated", sortOrder = "desc"} = {}) => {
        if (!siteSeq) { setRows([]); setCount(0); return; }
        try {
            const params = new URLSearchParams({page: String(page), pageSize: String(pageSize), search, sortBy, sortOrder});
            const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/PageMetaList?${params.toString()}`);
            const result = Array.isArray(data?.array) ? data.array : (Array.isArray(data) ? data : []);
            setRows(result);
            setCount(Number(data?.count ?? result.length));
        } catch (err) {
            logError("page-meta:load:error", err);
        }
    }, [siteSeq]);

    return {rows, count, load};
}
