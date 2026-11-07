// Fetches AhaWiki.Kanban.js for a page that has a #!Kanban. It is 238 KB and few pages have a
// board, and until 2026-11-08 every page carried it, a scraper's requests included. The address
// comes from this script's data-library attribute, because only the template can give it its digest.
//
// Until the board script arrives, AhaWiki.Kanban is this loader, so the editor's preview can call
// initAll the same way either way. The board script replaces it with the real one and draws the
// boards already on the page itself.
(() => {
    if(typeof window.AhaWiki === "undefined") {
        window.AhaWiki = {};
    }

    const libraryUrl = document.currentScript.dataset.library;
    const selector = ".InterpreterKanban";

    const loader = {
        initAll: container => {
            const scope = container || document;
            if (!scope.querySelector(selector)) return Promise.resolve();
            return window.AhaWiki.loadScript(libraryUrl).then(() => {
                if (window.AhaWiki.Kanban !== loader) window.AhaWiki.Kanban.initAll(scope);
            });
        },
    };
    window.AhaWiki.Kanban = loader;

    document.addEventListener("DOMContentLoaded", () => loader.initAll(document));
})();
