import {useCallback, useState} from "react";
import {fetchJson, logError, sendForm, sendJson} from "../api.js";

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
            await sendForm(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Admins`, "POST", {user: String(userSeq)});
            await loadSiteAdmins();
            return true;
        } catch (err) { logError("site-admin:insert:error", err); setError(err.message); return false; }
        finally { setAdding(false); }
    }, [siteSeq, loadSiteAdmins]);

    const deleteSiteAdmin = useCallback(async (userSeq) => {
        if (!siteSeq || !userSeq) return;
        setDeletingUserSeq(userSeq);
        try {
            await sendJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Admins/${encodeURIComponent(userSeq)}`, "DELETE");
            await loadSiteAdmins();
        } catch (err) { logError("site-admin:delete:error", err); setError(err.message); }
        finally { setDeletingUserSeq(0); }
    }, [siteSeq, loadSiteAdmins]);

    return {siteAdmins, adding, deletingUserSeq, error, loadSiteAdmins, insertSiteAdmin, deleteSiteAdmin};
}
