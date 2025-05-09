(() => {
    if(typeof window.AhaWiki === "undefined") {
        window.AhaWiki = {};
    }

    window.AhaWiki.DomUtil = {
        input: {
            resizeInputToContent: input => {
                const mirror = document.createElement('span');
                document.body.appendChild(mirror);

                const style = getComputedStyle(input);
                mirror.style.position = 'absolute';
                mirror.style.visibility = 'hidden';
                mirror.style.whiteSpace = 'pre';
                mirror.style.fontSize = style.fontSize;
                mirror.style.fontFamily = style.fontFamily;
                mirror.style.fontWeight = style.fontWeight;
                mirror.style.letterSpacing = style.letterSpacing;
                mirror.style.padding = style.padding;
                mirror.style.border = style.border;

                mirror.textContent = input.value || input.placeholder || " ";

                let width = mirror.offsetWidth;

                if (style.boxSizing === 'border-box') {
                    width += parseFloat(style.paddingLeft) + parseFloat(style.paddingRight)
                        + parseFloat(style.borderLeftWidth) + parseFloat(style.borderRightWidth);
                }

                input.style.width = (width + 1) + 'px';

                document.body.removeChild(mirror);
            },
        },
    };

    document.addEventListener('DOMContentLoaded', function () {
        document.querySelectorAll('input.resizeInputToContent').forEach(input => {
            window.AhaWiki.DomUtil.input.resizeInputToContent(input);
        });
        document.querySelectorAll('input.auto.resizeInputToContent').forEach(input => {
            input.addEventListener('input', function () {
                window.AhaWiki.DomUtil.input.resizeInputToContent(input);
            });
        });
    });
})();
