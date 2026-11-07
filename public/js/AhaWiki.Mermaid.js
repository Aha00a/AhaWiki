// Draws the page's Mermaid diagrams. The library is 5.5 MB and few pages have a diagram, so it is
// fetched the first time one needs drawing, not by the page skeleton. The address comes from this
// script's data-library attribute, because only the template can give it its digest.
(() => {
    if(typeof window.AhaWiki === "undefined") {
        window.AhaWiki = {};
    }

    const libraryUrl = document.currentScript.dataset.library;
    const selector = ".mermaid:not([data-processed])";
    let library = null;

    const load = () => library ??= window.AhaWiki.loadScript(libraryUrl)
        .then(() => mermaid.initialize({ startOnLoad: false }));

    window.AhaWiki.Mermaid = {
        renderAll: () => {
            if (!document.querySelector(selector)) return Promise.resolve();
            return load().then(() => mermaid.run({ querySelector: selector }));
        },
    };

    document.addEventListener("DOMContentLoaded", () => window.AhaWiki.Mermaid.renderAll());
})();
