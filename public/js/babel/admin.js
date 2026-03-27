const LOG_PREFIX = "[AdminUI]";

function logInfo(...args) {
  console.log(LOG_PREFIX, ...args);
}

function logError(...args) {
  console.error(LOG_PREFIX, ...args);
}

function escapeHtml(value) {
  return String(value ?? "")
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;")
    .replaceAll("'", "&#39;");
}

function navHtml(active) {
  const links = [
    ["/", "Home", false],
    ["/admin", "Dashboard", active === "dashboard"],
    ["/admin/sites", "All Sites", active === "sites"],
    ["/admin/site-users", "Site Users", active === "users"],
  ];

  return `<nav style="display:flex;gap:8px;flex-wrap:wrap;margin-bottom:16px;">
    ${links
      .map(
        ([href, label, isActive]) =>
          `<a href="${href}" style="padding:8px 12px;border:1px solid #ced4da;border-radius:8px;text-decoration:none;color:#1c1f23;${
            isActive ? "background:#e7f5ff;border-color:#74c0fc;" : "background:#fff;"
          }">${label}</a>`,
      )
      .join("")}
  </nav>`;
}

function tableHtml(headers, rows) {
  return `
    <div style="overflow:auto;">
      <table style="width:100%;border-collapse:collapse;background:#fff;border:1px solid #dee2e6;">
        <thead style="background:#f8f9fa;">
          <tr>
            ${headers.map((h) => `<th style="border:1px solid #dee2e6;padding:10px;text-align:left;">${escapeHtml(h)}</th>`).join("")}
          </tr>
        </thead>
        <tbody>
          ${rows
            .map(
              (cols, idx) => `<tr style="background:${idx % 2 === 0 ? "#fff" : "#fcfcfd"};">
                ${cols.map((c) => `<td style="border:1px solid #dee2e6;padding:10px;">${escapeHtml(c)}</td>`).join("")}
              </tr>`,
            )
            .join("")}
        </tbody>
      </table>
    </div>
  `;
}

async function fetchJson(url) {
  logInfo("fetch:start", url);
  const res = await fetch(url, { credentials: "same-origin" });
  if (!res.ok) {
    throw new Error(`HTTP ${res.status}`);
  }
  const data = await res.json();
  logInfo("fetch:success", url, { count: Array.isArray(data) ? data.length : undefined });
  return data;
}

function renderLayout(root, page, contentHtml) {
  root.innerHTML = `
    <section style="max-width:1100px;margin:0 auto;padding:16px;">
      <header style="display:flex;justify-content:space-between;align-items:center;gap:8px;flex-wrap:wrap;margin-bottom:12px;">
        <h1 style="margin:0;font-size:24px;">Admin</h1>
        <span style="background:#e7f5ff;color:#1864ab;padding:6px 10px;border-radius:999px;font-size:13px;">Admin JS Active</span>
      </header>
      ${navHtml(page)}
      ${contentHtml}
    </section>
  `;
}

async function renderPage(root, page) {
  if (page === "sites") {
    renderLayout(root, page, "<p>Sites 로딩 중...</p>");
    const sites = await fetchJson("/api/admin/sites");
    renderLayout(root, page, `
      <h2 style="margin-top:0;">All Sites</h2>
      ${tableHtml(
        ["Seq", "Name"],
        sites.map((site) => [site.seq, site.name]),
      )}
    `);
    return;
  }

  if (page === "users") {
    renderLayout(root, page, "<p>Site Users 로딩 중...</p>");
    const users = await fetchJson("/api/admin/site-users");
    renderLayout(root, page, `
      <h2 style="margin-top:0;">Site Users (current host)</h2>
      ${tableHtml(
        ["User", "Email", "Nickname", "Created"],
        users.map((user) => [user.user, user.email, user.nickname, user.created]),
      )}
    `);
    return;
  }

  renderLayout(root, page, "<p>Dashboard 로딩 중...</p>");
  const [sites, users] = await Promise.all([fetchJson("/api/admin/sites"), fetchJson("/api/admin/site-users")]);
  renderLayout(root, page, `
    <h2 style="margin-top:0;">Sites</h2>
    ${tableHtml(
      ["Seq", "Name"],
      sites.map((site) => [site.seq, site.name]),
    )}
    <h2>Site Users (current host)</h2>
    ${tableHtml(
      ["User", "Email", "Nickname", "Created"],
      users.map((user) => [user.user, user.email, user.nickname, user.created]),
    )}
  `);
}

async function pageLoad() {
  logInfo("pageLoad:start", window.location.pathname);
  const root = document.getElementById("main");
  if (!root) {
    logError("pageLoad:no-root", "#main not found");
    return;
  }

  const page = root.dataset.adminPage || "dashboard";
  logInfo("pageLoad:render", { page });

  try {
    await renderPage(root, page);
    logInfo("pageLoad:rendered", { page });
  } catch (error) {
    logError("pageLoad:error", error);
    root.insertAdjacentHTML(
      "beforeend",
      `<p style="color:#c92a2a;">클라이언트 렌더링 오류가 발생했습니다: ${escapeHtml(error.message || String(error))}</p>`,
    );
  }
}

window.addEventListener("error", (event) => {
  logError("window:error", event.message, event.error);
});

window.addEventListener("unhandledrejection", (event) => {
  logError("window:unhandledrejection", event.reason);
});

window.addEventListener("DOMContentLoaded", () => {
  pageLoad();
});
