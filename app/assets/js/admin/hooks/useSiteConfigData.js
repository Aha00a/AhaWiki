import {useCallback, useState} from "react";
import {fetchJson, fetchCsrfToken, logError} from "../api.js";

const DEFAULT_THEME = {defaultHue: ""};

export function useSiteConfigData(siteSeq) {
    const [faviconUrl, setFaviconUrl] = useState("/public/favicon.png");
    const [faviconObjectKey, setFaviconObjectKey] = useState("");
    const [uploadingFavicon, setUploadingFavicon] = useState(false);
    const [deletingFavicon, setDeletingFavicon] = useState(false);
    const [siteTheme, setSiteTheme] = useState(DEFAULT_THEME);
    const [savingTheme, setSavingTheme] = useState(false);
    const [error, setError] = useState("");

    const loadFavicon = useCallback(async () => {
        if (!siteSeq) { setFaviconUrl("/public/favicon.png"); setFaviconObjectKey(""); return; }
        try {
            const data = await fetchJson(`/api/Admin/Favicon?siteSeq=${encodeURIComponent(siteSeq)}`);
            setFaviconUrl(data?.faviconUrl || "/public/favicon.png");
            setFaviconObjectKey(data?.objectKey || "");
        } catch (err) { logError("favicon:load:error", err); setError(err.message); }
    }, [siteSeq]);

    const uploadFavicon = useCallback(async (file) => {
        if (!file || !siteSeq) return;
        setUploadingFavicon(true);
        setError("");
        try {
            const csrfToken = await fetchCsrfToken();
            const formData = new FormData();
            formData.append("file", file);
            formData.append("siteSeq", String(siteSeq));
            formData.append(csrfToken.name, csrfToken.value);
            const response = await fetch("/api/Admin/Favicon", {method: "POST", credentials: "same-origin", headers: {"Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value}, body: formData});
            if (!response.ok) { const p = await response.json().catch(() => null); throw new Error(p?.error || `HTTP ${response.status}`); }
            const data = await response.json();
            setFaviconUrl(data?.faviconUrl || "/public/favicon.png");
            setFaviconObjectKey(data?.objectKey || "");
        } catch (err) { logError("favicon:upload:error", err); setError(err.message); }
        finally { setUploadingFavicon(false); }
    }, [siteSeq]);

    const resetFavicon = useCallback(async () => {
        if (!siteSeq) return;
        setDeletingFavicon(true);
        setError("");
        try {
            const csrfToken = await fetchCsrfToken();
            const response = await fetch(`/api/Admin/Favicon?siteSeq=${encodeURIComponent(siteSeq)}`, {method: "DELETE", credentials: "same-origin", headers: {"Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value}});
            if (!response.ok) { const p = await response.json().catch(() => null); throw new Error(p?.error || `HTTP ${response.status}`); }
            setFaviconUrl("/public/favicon.png");
            setFaviconObjectKey("");
        } catch (err) { logError("favicon:delete:error", err); setError(err.message); }
        finally { setDeletingFavicon(false); }
    }, [siteSeq]);

    const loadTheme = useCallback(async () => {
        if (!siteSeq) { setSiteTheme(DEFAULT_THEME); return; }
        try {
            const data = await fetchJson(`/api/Admin/SiteTheme?siteSeq=${encodeURIComponent(siteSeq)}`);
            setSiteTheme({defaultHue: data?.defaultHue ?? ""});
        } catch (err) { logError("site-theme:load:error", err); setError(err.message); }
    }, [siteSeq]);

    const saveTheme = useCallback(async (theme) => {
        if (!siteSeq) return;
        setSavingTheme(true);
        setError("");
        try {
            const csrfToken = await fetchCsrfToken();
            const payload = new URLSearchParams();
            payload.set("siteSeq", String(siteSeq));
            Object.entries(theme).forEach(([k, v]) => payload.set(k, v ?? ""));
            payload.set(csrfToken.name, csrfToken.value);
            const response = await fetch("/api/Admin/SiteTheme", {method: "PUT", credentials: "same-origin", headers: {"Content-Type": "application/x-www-form-urlencoded; charset=UTF-8", "Csrf-Token": csrfToken.value, "X-CSRF-Token": csrfToken.value}, body: payload.toString()});
            if (!response.ok) { const p = await response.json().catch(() => null); throw new Error(p?.error || `HTTP ${response.status}`); }
            const data = await response.json();
            setSiteTheme({defaultHue: data?.defaultHue ?? ""});
        } catch (err) { logError("site-theme:save:error", err); setError(err.message); }
        finally { setSavingTheme(false); }
    }, [siteSeq]);

    return {faviconUrl, faviconObjectKey, uploadingFavicon, deletingFavicon, siteTheme, setSiteTheme, savingTheme, error, loadFavicon, uploadFavicon, resetFavicon, loadTheme, saveTheme};
}
