import {useCallback, useState} from "react";
import {fetchJson, logError, sendForm, sendJson} from "../api.js";

// Until the rows arrive there is nothing to offer. Never a guessed list: a wrong action
// number here is the bug this endpoint was changed to remove.
const emptyVocabulary = {targetTypes: [], actorTypes: [], actions: []};

export function usePermissionData(siteSeq) {
    const [permissionRows, setPermissionRows] = useState([]);
    const [permissionVocabulary, setPermissionVocabulary] = useState(emptyVocabulary);
    const [permissionDiagnose, setPermissionDiagnose] = useState(null);
    const [saving, setSaving] = useState(false);
    const [deletingKey, setDeletingKey] = useState("");
    const [error, setError] = useState("");

    const loadPermissions = useCallback(async () => {
        if (!siteSeq) { setPermissionRows([]); setPermissionVocabulary(emptyVocabulary); return; }
        try {
            const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Permissions`);
            setPermissionRows(Array.isArray(data?.permissions) ? data.permissions : []);
            setPermissionVocabulary({
                targetTypes: Array.isArray(data?.targetTypes) ? data.targetTypes : [],
                actorTypes: Array.isArray(data?.actorTypes) ? data.actorTypes : [],
                actions: Array.isArray(data?.actions) ? data.actions : [],
            });
        } catch (err) { logError("permission:load:error", err); }
    }, [siteSeq]);

    const savePermission = useCallback(async (permission) => {
        if (!siteSeq) return false;
        setSaving(true);
        try {
            // Only the five the endpoint parses, so an extra field on the form object cannot
            // ride along into the request.
            const fields = Object.fromEntries(
                ["targetType", "target", "actorType", "actor", "action"].map((key) => [key, permission[key]])
            );
            await sendForm(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Permissions`, "POST", fields);
            await loadPermissions();
            return true;
        } catch (err) { logError("permission:save:error", err); setError(err.message); return false; }
        finally { setSaving(false); }
    }, [siteSeq, loadPermissions]);

    const deletePermission = useCallback(async (permission) => {
        if (!siteSeq || !permission) return;
        const key = `${permission.targetType}:${permission.target}:${permission.actorType}:${permission.actor}`;
        setDeletingKey(key);
        try {
            // The row's identity is its four key columns, so the delete carries them in the
            // query string rather than a body.
            const params = new URLSearchParams({targetType: permission.targetType ?? "", target: permission.target ?? "", actorType: permission.actorType ?? "", actor: permission.actor ?? ""});
            await sendJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Permissions?${params.toString()}`, "DELETE");
            await loadPermissions();
        } catch (err) { logError("permission:delete:error", err); setError(err.message); }
        finally { setDeletingKey(""); }
    }, [siteSeq, loadPermissions]);

    const diagnosePermission = useCallback(async (pageName, actor, action) => {
        if (!siteSeq) return;
        const params = new URLSearchParams({pageName: pageName ?? "", actor: actor ?? "", action: action || "Read"});
        const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/PermissionDiagnose?${params.toString()}`);
        setPermissionDiagnose(data);
    }, [siteSeq]);

    return {permissionRows, permissionVocabulary, permissionDiagnose, saving, deletingKey, error, loadPermissions, savePermission, deletePermission, diagnosePermission};
}
