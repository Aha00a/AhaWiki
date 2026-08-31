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

/**
 * 쓰기 요청 하나. CSRF 헤더, credentials, 그리고 실패했을 때 `{error}` 봉투를 푸는 것까지
 * 같은 자리에 둔다. 읽기가 `fetchJson` 하나로 모여 있는 것과 짝이다.
 *
 * body 를 주면 JSON 으로 보내고, 없으면 헤더만 붙인다 — DELETE 와 인자 없는 POST 가 그렇다.
 */
export async function sendJson(url, method, body) {
    const csrf = await fetchCsrfToken();
    const headers = {"Csrf-Token": csrf.value, "X-CSRF-Token": csrf.value};
    if (body !== undefined) headers["Content-Type"] = "application/json";
    const response = await fetch(url, {
        method,
        credentials: "same-origin",
        headers,
        body: body === undefined ? undefined : JSON.stringify(body),
    });
    const data = await response.json().catch(() => null);
    if (!response.ok) throw new Error(data?.error || `HTTP ${response.status}`);
    return data;
}

// 페이지네이션 목록 응답의 봉투. 서버는 JsonResults.pagedJson 한 곳에서 만들고,
// 푸는 쪽도 여기 하나다. hook 마다 풀면 한 endpoint 가 모양을 바꿔도 그 hook 만 고쳐진다.
export function unwrapPaged(data) {
    const rows = Array.isArray(data?.array) ? data.array : (Array.isArray(data) ? data : []);
    return {rows, count: Number(data?.count ?? rows.length)};
}

// 목록 질의 파라미터. 이름과 직렬화 방식이 서버 시그니처와 짝이라 한 곳에 둔다.
export function pagedParams({page, pageSize, search = "", sortBy, sortOrder}) {
    return new URLSearchParams({
        page: String(page),
        pageSize: String(pageSize),
        search,
        sortBy,
        sortOrder,
    });
}
