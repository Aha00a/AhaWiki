import {useCallback, useState} from "react";
import {fetchJson, logError, pagedParams, unwrapPaged} from "../api.js";
import {ADMIN_PAGE_META_PAGE_SIZE} from "../constants.js";

export function useAdminPageMetaData(siteSeq) {
    const [rows, setRows] = useState([]);
    const [count, setCount] = useState(0);

    const load = useCallback(async ({page = 1, pageSize = ADMIN_PAGE_META_PAGE_SIZE, search = "", sortBy = "dateUpdated", sortOrder = "desc"} = {}) => {
        if (!siteSeq) { setRows([]); setCount(0); return; }
        try {
            const params = pagedParams({page, pageSize, search, sortBy, sortOrder});
            const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/PageMetaList?${params.toString()}`);
            const paged = unwrapPaged(data);
            setRows(paged.rows);
            setCount(paged.count);
        } catch (err) {
            logError("page-meta:load:error", err);
        }
    }, [siteSeq]);

    return {rows, count, load};
}
