// Fetches MathJax for a page that has a formula. The library is 1 MB and few pages have one, so the
// page skeleton only names it. <body> is no-mathjax, so a .mathjax element is the only thing MathJax
// would typeset, and it typesets on its own once loaded. The address comes from this script's
// data-library attribute, because only the template can give it its digest. Not shared with
// AhaWiki.Mermaid.js: that one must wait for its library and keep the promise, this one only starts it.
(() => {
    const libraryUrl = document.currentScript.dataset.library;

    document.addEventListener("DOMContentLoaded", () => {
        if (!document.querySelector(".mathjax")) return;
        const script = document.createElement("script");
        script.src = libraryUrl;
        document.head.append(script);
    });
})();
