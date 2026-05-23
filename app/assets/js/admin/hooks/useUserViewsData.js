import {useCallback, useState} from "react";
import {fetchJson, logError} from "../api.js";

export function useUserViewsData() {
    const [loading, setLoading] = useState(false);
    const [userViewHistories, setUserViewHistories] = useState([]);

    const loadUserViewHistories = useCallback(async (userSeq, n = 200) => {
        setLoading(true);
        try {
            const clampedN = Math.min(1000, Math.max(1, Number.parseInt(String(n), 10) || 200));
            const data = await fetchJson(`/api/Admin/UserViews?userSeq=${encodeURIComponent(userSeq)}&n=${encodeURIComponent(clampedN)}`);
            setUserViewHistories(data);
        } catch (err) {
            logError("user-views:load:error", err);
        } finally {
            setLoading(false);
        }
    }, []);

    return {loading, userViewHistories, loadUserViewHistories};
}
