import {useCallback, useRef, useState} from "react";
import {fetchJson, fetchCsrfToken, logError} from "../api.js";

export function useS3Data() {
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState("");
    const [itemsByPrefix, setItemsByPrefix] = useState({});
    const [selectedS3Keys, setSelectedS3Keys] = useState([]);
    const [deletingS3, setDeletingS3] = useState(false);
    const [expandedS3Nodes, setExpandedS3Nodes] = useState({});
    const [loadingPrefixes, setLoadingPrefixes] = useState(new Set());
    const [expandingAll, setExpandingAll] = useState(false);
    const loadedRef = useRef(new Set());
    const loadingRef = useRef(new Set());
    const itemsByPrefixRef = useRef({});

    const loadPrefix = useCallback(async (prefix) => {
        if (loadedRef.current.has(prefix) || loadingRef.current.has(prefix)) return;
        loadingRef.current.add(prefix);
        setLoadingPrefixes(prev => { const next = new Set(prev); next.add(prefix); return next; });
        try {
            const apiPrefix = prefix ? prefix + "/" : "";
            const params = new URLSearchParams({prefix: apiPrefix, maxKeys: "1000", recursive: "false"});
            const data = await fetchJson(`/api/Admin/S3Objects?${params.toString()}`);
            const items = Array.isArray(data?.items) ? data.items : [];
            loadedRef.current.add(prefix);
            itemsByPrefixRef.current = {...itemsByPrefixRef.current, [prefix]: items};
            setItemsByPrefix({...itemsByPrefixRef.current});
        } catch (err) {
            logError("s3:load:error", err);
            setError(`S3 조회 실패: ${err.message}`);
        } finally {
            loadingRef.current.delete(prefix);
            setLoadingPrefixes(prev => { const next = new Set(prev); next.delete(prefix); return next; });
        }
    }, []);

    const loadS3Objects = useCallback(async () => {
        setLoading(true);
        setError("");
        loadedRef.current = new Set();
        loadingRef.current = new Set();
        itemsByPrefixRef.current = {};
        setItemsByPrefix({});
        setSelectedS3Keys([]);
        setExpandedS3Nodes({});
        setLoadingPrefixes(new Set());
        try {
            await loadPrefix("");
        } finally {
            setLoading(false);
        }
    }, [loadPrefix]);

    const toggleS3Node = useCallback((path) => {
        setExpandedS3Nodes(prev => {
            const expanding = !prev[path];
            if (expanding) loadPrefix(path);
            return {...prev, [path]: expanding};
        });
    }, [loadPrefix]);

    const expandAllS3Nodes = useCallback(async () => {
        setExpandingAll(true);
        try {
            const queue = [];
            const enqueue = (prefix) => {
                (itemsByPrefixRef.current[prefix] || [])
                    .filter(item => item.isDirectory)
                    .forEach(item => queue.push(item.key.replace(/\/$/, "")));
            };
            enqueue("");
            let i = 0;
            while (i < queue.length) {
                const path = queue[i];
                setExpandedS3Nodes(prev => ({...prev, [path]: true}));
                await loadPrefix(path);
                enqueue(path);
                i++;
            }
        } finally {
            setExpandingAll(false);
        }
    }, [loadPrefix]);

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
            loadedRef.current = new Set();
            loadingRef.current = new Set();
            itemsByPrefixRef.current = {};
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

    return {loading, error, itemsByPrefix, selectedS3Keys, setSelectedS3Keys, deletingS3, expandedS3Nodes, loadingPrefixes, expandingAll, loadS3Objects, toggleS3Node, expandAllS3Nodes, deleteS3Selected, downloadS3Object};
}
