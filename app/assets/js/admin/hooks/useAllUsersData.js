import {useCallback, useState} from "react";
import {fetchJson, logError} from "../api.js";
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
            const params = new URLSearchParams({page: String(page), pageSize: String(pageSize), search, sortBy, sortOrder});
            const data = await fetchJson(`/api/Admin/Users?${params.toString()}`);
            const rows = Array.isArray(data?.array) ? data.array : (Array.isArray(data) ? data : []);
            setAllUsers(rows);
            setAllUserCount(Number(data?.count ?? rows.length));
        } catch (err) {
            logError("all-users:load:error", err);
            setError(err.message || String(err));
        } finally {
            setLoading(false);
        }
    }, []);

    return {loading, error, allUsers, allUserCount, loadAllUsers};
}
