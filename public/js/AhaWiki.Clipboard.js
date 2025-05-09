(() => {
    function copyToClipboardFallback(text) {
        const textarea = document.createElement('textarea');
        textarea.value = text;
        document.body.appendChild(textarea);
        textarea.select();
        try {
            document.execCommand('copy');
            console.log('Fallback: Text copied');
        } catch (err) {
            console.error('Fallback: Failed to copy', err);
        }
        document.body.removeChild(textarea);
    }

    function copy(text) {
        if (navigator.clipboard) {
            navigator.clipboard.writeText(text).then(() => {
                console.log('Copied with clipboard API');
            }).catch(err => {
                console.error('Clipboard API failed, using fallback:', err);
                copyToClipboardFallback(text);
            });
        } else {
            copyToClipboardFallback(text);
        }
    }

    if(typeof window.AhaWiki === "undefined") {
        window.AhaWiki = {};
    }

    window.AhaWiki.Clipboard = {
        copy,
    };
})();
