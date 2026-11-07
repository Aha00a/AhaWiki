// Loads a script the page skeleton only names, once however many times it is asked for. Shared by
// the loaders of the libraries few pages need: AhaWiki.Math.js, AhaWiki.Mermaid.js and
// AhaWiki.KanbanLoader.js. The skeleton loads this before them.
(() => {
    if(typeof window.AhaWiki === "undefined") {
        window.AhaWiki = {};
    }

    const loading = new Map();

    window.AhaWiki.loadScript = url => {
        if (!loading.has(url)) {
            loading.set(url, new Promise((resolve, reject) => {
                const script = document.createElement("script");
                script.src = url;
                script.onload = () => resolve();
                script.onerror = () => reject(new Error(`${url} did not load`));
                document.head.append(script);
            }));
        }
        return loading.get(url);
    };
})();
