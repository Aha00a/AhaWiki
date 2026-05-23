import {useCallback, useState} from "react";
import {fetchJson, fetchCsrfToken, logError} from "../api.js";

export function useS3Data() {
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState("");
    const [s3Items, setS3Items] = useState([]);
    const [selectedS3Keys, setSelectedS3Keys] = useState([]);
    const [deletingS3, setDeletingS3] = useState(false);
    const [expandedS3Nodes, setExpandedS3Nodes] = useState({});

    const loadS3Objects = useCallback(async () => {
        setLoading(true);
        setError("");
        try {
            const params = new URLSearchParams({prefix: "", maxKeys: "5000", recursive: "true"});
            const data = await fetchJson(`/api/Admin/S3Objects?${params.toString()}`);
            setS3Items(Array.isArray(data?.items) ? data.items : []);
            setSelectedS3Keys([]);
            setExpandedS3Nodes({"__root__": true});
        } catch (err) {
            logError("s3:load:error", err);
            setError(`S3 조회 실패: ${err.message}`);
        } finally {
            setLoading(false);
        }
    }, []);

    const toggleS3Node = useCallback((key) => {
        setExpandedS3Nodes((prev) => ({...prev, [key]: !prev[key]}));
    }, []);

    const expandAllS3Nodes = useCallback((items) => {
        const next = {"__root__": true};
        (Array.isArray(items) ? items : []).filter((item) => !item.isDirectory).forEach((item) => {
            const parts = String(item.key || "").split("/").filter(Boolean);
            for (let i = 1; i < parts.length; i += 1) {
                next[parts.slice(0, i).join("/")] = true;
            }
        });
        setExpandedS3Nodes(next);
    }, []);

    const deleteS3Selected = useCallback(async (keys) => {
        if (keys.length === 0) return;
        setDeletingS3(true);
        try {
            const csrf = await fetchCsrfToken();
            const response = await fetch("/api/Admin/S3Objects", {
                method: "DELETE",
                credentials: "same-origin",
                headers: {"Content-Type": "application/json", "Csrf-Token": csrf.value, "X-CSRF-Token": csrf.value},
                body: JSON.stringify({keys}),
            });
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            await loadS3Objects();
        } catch (err) {
            logError("s3:delete:error", err);
            setError(`S3 삭제 실패: ${err.message}`);
        } finally {
            setDeletingS3(false);
        }
    }, [loadS3Objects]);

    const downloadS3Object = useCallback(async (key) => {
        try {
            const params = new URLSearchParams({key});
            const data = await fetchJson(`/api/Admin/S3DownloadUrl?${params.toString()}`);
            if (data?.url) window.open(data.url, "_blank", "noopener,noreferrer");
        } catch (err) {
            logError("s3:download:error", err);
            setError(`다운로드 URL 생성 실패: ${err.message}`);
        }
    }, []);

    return {loading, error, s3Items, selectedS3Keys, setSelectedS3Keys, deletingS3, expandedS3Nodes, loadS3Objects, toggleS3Node, expandAllS3Nodes, deleteS3Selected, downloadS3Object};
}
