(() => {
    if(typeof window.AhaWiki === "undefined") {
        window.AhaWiki = {};
    }

    window.AhaWiki.Loader = {
        open: () => $("body").append($('<div class="ahaWikiLoader bg"></div><div class="ahaWikiLoader circle"/>')),
        close: () => $(".ahaWikiLoader").remove(),
    };
})();
