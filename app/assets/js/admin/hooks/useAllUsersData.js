import {useCallback, useState} from "react";
import {fetchJson, logError, pagedParams, unwrapPaged} from "../api.js";
import {ACCESS_LOG_PAGE_SIZE} from "../constants.js";

export function useAllUsersData() {
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState("");
    const [allUsers, setAllUsers] = useState([]);
    const [allUserCount, setAllUserCount] = useState(0);

    const loadAllUsers = useCallback(async ({page = 1, pageSize = ACCESS_LOG_PAGE_SIZE, search = "", sortBy = "seq", sortOrder = "desc"} = {}) => {
        setLoading(true);
        setError("");
        try {
            const params = pagedParams({page, pageSize, search, sortBy, sortOrder});
            const data = await fetchJson(`/api/Admin/Users?${params.toString()}`);
            const {rows, count} = unwrapPaged(data);
            setAllUsers(rows);
            setAllUserCount(count);
        } catch (err) {
            logError("all-users:load:error", err);
            setError(err.message || String(err));
        } finally {
            setLoading(false);
        }
    }, []);

    return {loading, error, allUsers, allUserCount, loadAllUsers};
}
