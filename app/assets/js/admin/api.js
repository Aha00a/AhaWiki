const LOG_PREFIX = "[AdminUI]";

export function logInfo(...args) { console.log(LOG_PREFIX, ...args); }
export function logError(...args) { console.error(LOG_PREFIX, ...args); }

export async function fetchJson(url) {
    logInfo("fetch:start", url);
    const response = await fetch(url, {credentials: "same-origin"});
    if (!response.ok) throw new Error(`HTTP ${response.status}`);
    const data = await response.json();
    logInfo("fetch:success", url, {count: Array.isArray(data) ? data.length : undefined});
    return data;
}

export async function fetchCsrfToken() {
    const response = await fetch("/api/csrf", {credentials: "same-origin"});
    if (!response.ok) throw new Error(`CSRF HTTP ${response.status}`);
    const token = await response.json();
    return {name: token?.name ?? "csrfToken", value: token?.value ?? ""};
}
