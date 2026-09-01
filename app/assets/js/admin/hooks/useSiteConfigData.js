import {useCallback, useState} from "react";
import {fetchJson, logError, sendForm, sendJson, sendMultipart} from "../api.js";

const DEFAULT_THEME = {defaultHue: ""};
const DEFAULT_TELEGRAM = {chatId: ""};

export function useSiteConfigData(siteSeq) {
    const [faviconUrl, setFaviconUrl] = useState("/public/favicon.png");
    const [faviconObjectKey, setFaviconObjectKey] = useState("");
    const [uploadingFavicon, setUploadingFavicon] = useState(false);
    const [deletingFavicon, setDeletingFavicon] = useState(false);
    const [siteTheme, setSiteTheme] = useState(DEFAULT_THEME);
    const [savingTheme, setSavingTheme] = useState(false);
    const [telegram, setTelegram] = useState(DEFAULT_TELEGRAM);
    const [savingTelegram, setSavingTelegram] = useState(false);
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
            const data = await sendMultipart("/api/Admin/Favicon", "POST", {file, siteSeq: String(siteSeq)});
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
            await sendJson(`/api/Admin/Favicon?siteSeq=${encodeURIComponent(siteSeq)}`, "DELETE");
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
            const data = await sendForm("/api/Admin/SiteTheme", "PUT", {siteSeq: String(siteSeq), ...theme});
            setSiteTheme({defaultHue: data?.defaultHue ?? ""});
        } catch (err) { logError("site-theme:save:error", err); setError(err.message); }
        finally { setSavingTheme(false); }
    }, [siteSeq]);

    const loadTelegram = useCallback(async () => {
        if (!siteSeq) { setTelegram(DEFAULT_TELEGRAM); return; }
        try {
            const data = await fetchJson(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Telegram`);
            setTelegram({chatId: data?.chatId ?? ""});
        } catch (err) { logError("telegram:load:error", err); setError(err.message); }
    }, [siteSeq]);

    const saveTelegram = useCallback(async (telegramData) => {
        if (!siteSeq) return;
        setSavingTelegram(true);
        setError("");
        try {
            const data = await sendForm(`/api/Admin/Site/${encodeURIComponent(siteSeq)}/Telegram`, "PUT", {chatId: telegramData.chatId});
            setTelegram({chatId: data?.chatId ?? ""});
        } catch (err) { logError("telegram:save:error", err); setError(err.message); }
        finally { setSavingTelegram(false); }
    }, [siteSeq]);

    return {faviconUrl, faviconObjectKey, uploadingFavicon, deletingFavicon, siteTheme, setSiteTheme, savingTheme, telegram, setTelegram, savingTelegram, error, loadFavicon, uploadFavicon, resetFavicon, loadTheme, saveTheme, loadTelegram, saveTelegram};
}
