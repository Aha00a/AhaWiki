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

// 내보내지 않는다. 밖에서 token 을 집어 갈 수 있으면 헤더와 오류 처리를 손으로 다시
// 조립하게 되고, 실제로 열 곳이 그렇게 돼 있었다. 쓰기는 아래 셋으로만 나간다.
async function fetchCsrfToken() {
    const response = await fetch("/api/csrf", {credentials: "same-origin"});
    if (!response.ok) throw new Error(`CSRF HTTP ${response.status}`);
    const token = await response.json();
    return {name: token?.name ?? "csrfToken", value: token?.value ?? ""};
}

/**
 * 쓰기 요청 하나. token 조회, credentials, CSRF 헤더 한 쌍, 그리고 실패했을 때 `{error}`
 * 봉투를 푸는 것까지 여기 모인다. 읽기가 `fetchJson` 하나로 모여 있는 것과 짝이다.
 *
 * 아래 셋은 이 위에 wire format 만 얹는다. 형식이 다른 것은 진짜로 다르기 때문이다 —
 * 서버가 JSON body 를 읽는 endpoint, form 필드를 읽는 endpoint, 파일을 받는 endpoint 가
 * 각각 있다. 그 셋 말고 나머지는 전부 같아야 한다.
 *
 * 성공 응답이 비어 있으면 `null` 을 돌려준다. 본문을 쓰지 않는 호출부가 많다.
 */
async function send(url, method, build) {
    const csrf = await fetchCsrfToken();
    const {headers = {}, body} = build(csrf);
    const response = await fetch(url, {
        method,
        credentials: "same-origin",
        headers: {"Csrf-Token": csrf.value, "X-CSRF-Token": csrf.value, ...headers},
        body,
    });
    const data = await response.json().catch(() => null);
    if (!response.ok) throw new Error(data?.error || `HTTP ${response.status}`);
    return data;
}

/** body 를 주면 JSON 으로, 없으면 헤더만 — DELETE 와 인자 없는 POST 가 후자다. */
export function sendJson(url, method, body) {
    return send(url, method, () => body === undefined
        ? {}
        : {headers: {"Content-Type": "application/json"}, body: JSON.stringify(body)});
}

/** Play 의 form endpoint 는 token 을 헤더뿐 아니라 body 에서도 받는다. 둘 다 넣는다. */
export function sendForm(url, method, fields) {
    return send(url, method, (csrf) => {
        const payload = new URLSearchParams();
        Object.entries(fields ?? {}).forEach(([key, value]) => payload.set(key, value ?? ""));
        payload.set(csrf.name, csrf.value);
        return {
            headers: {"Content-Type": "application/x-www-form-urlencoded; charset=UTF-8"},
            body: payload.toString(),
        };
    });
}

/** 파일 업로드. `Content-Type` 을 직접 적지 않는다 — boundary 는 브라우저가 붙인다. */
export function sendMultipart(url, method, fields) {
    return send(url, method, (csrf) => {
        const payload = new FormData();
        Object.entries(fields ?? {}).forEach(([key, value]) => payload.append(key, value));
        payload.append(csrf.name, csrf.value);
        return {body: payload};
    });
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
