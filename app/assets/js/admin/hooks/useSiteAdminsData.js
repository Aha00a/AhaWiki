import {useCallback, useState} from "react";
import {fetchJson, fetchCsrfToken, logError} from "../api.js";

export function useSiteAdminsData(siteSeq) {
    const [siteAdmins, setSiteAdmins] = useState([]);
    const [adding, setAdding] = useState(false);
    const [deletingUserSeq, setDeletingUserSeq] = useState(0);
    const [error, setError] = useState("");

    const loadSiteAdmins = useCallback(async () => {
        if (!siteSeq) { setSiteAdmins([]); return; }
        try {
            const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Admins`);
            setSiteAdmins(Array.isArray(data) ? data : []);
        } catch (err) { logError("site-admins:load:error", err); }
    }, [siteSeq]);

    const insertSiteAdmin = useCallback(async (userSeq) => {
        if (!siteSeq || !userSeq) return false;
        setAdding(true);
        try {
            const csrfToken = await fetchCsrfToken();
            const payload = new URLSearchParams();
            payload.set("user", String(userSeq));
            payload.set(csrfToken.name, csrfToken.value);
            const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Admins`, {method: "POST", credentials: "same-origin", headers: {"Content-Type": "application/x-www-form-urlencoded; charset=UTF-8", "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value}, body: payload.toString()});
            if (!response.ok) { const p = await response.json().catch(() => null); throw new Error(p?.error || `HTTP ${response.status}`); }
            await loadSiteAdmins();
            return true;
        } catch (err) { logError("site-admin:insert:error", err); setError(err.message); return false; }
        finally { setAdding(false); }
    }, [siteSeq, loadSiteAdmins]);

    const deleteSiteAdmin = useCallback(async (userSeq) => {
        if (!siteSeq || !userSeq) return;
        setDeletingUserSeq(userSeq);
        try {
            const csrfToken = await fetchCsrfToken();
            const response = await fetch(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Admins/${encodeURIComponent(userSeq)}`, {method: "DELETE", credentials: "same-origin", headers: {"Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value}});
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            await loadSiteAdmins();
        } catch (err) { logError("site-admin:delete:error", err); setError(err.message); }
        finally { setDeletingUserSeq(0); }
    }, [siteSeq, loadSiteAdmins]);

    return {siteAdmins, adding, deletingUserSeq, error, loadSiteAdmins, insertSiteAdmin, deleteSiteAdmin};
}
