$(function () {
    var targetElement = document.querySelector('.diff2HtmlResult');
    var diffText = document.querySelector('.unifiedDiff').textContent;

    function isDarkTheme() {
        return document.documentElement.classList.contains('theme-ahawiki-dark');
    }

    function draw(outputFormat) {
        var diff2htmlUi = new Diff2HtmlUI(targetElement, diffText, {
            outputFormat: outputFormat,
            drawFileList: false,
            matching: 'lines'
        });
        diff2htmlUi.draw();
        targetElement.classList.toggle('d2h-dark-color-scheme', isDarkTheme());
    }

    $('.selectOutputFormat').change(function () {
        draw($(this).val());
    }).change();

    new MutationObserver(function () {
        targetElement.classList.toggle('d2h-dark-color-scheme', isDarkTheme());
    }).observe(document.documentElement, { attributes: true, attributeFilter: ['class'] });
});
