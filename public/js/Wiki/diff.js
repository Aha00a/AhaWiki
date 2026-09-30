$(function () {
    var targetElement = document.querySelector('.diff2HtmlResult');
    var diffText = document.querySelector('.unifiedDiff').textContent;

    function isDarkTheme() {
        return document.documentElement.classList.contains('theme-ahawiki-dark');
    }

    // diff.less wraps long lines. Side by side, each half is its own table, so a line that wraps
    // on one side only would push every row below it out of step with the other side.
    function alignSideBySideRows() {
        $(targetElement).find('.d2h-files-diff').each(function () {
            var sides = $(this).children('.d2h-file-side-diff');
            if (sides.length !== 2)
                return;
            var left = sides.eq(0).find('tr');
            var right = sides.eq(1).find('tr');
            left.add(right).css('height', '');
            var heights = left.map(function (i) {
                return Math.max(this.getBoundingClientRect().height, right[i] ? right[i].getBoundingClientRect().height : 0);
            }).get();
            left.each(function (i) {
                $(this).css('height', heights[i] + 'px');
                if (right[i])
                    $(right[i]).css('height', heights[i] + 'px');
            });
        });
    }

    function draw(outputFormat) {
        // No diffStyle: the default 'word' already marks a changed Korean particle alone,
        // because the word splitter underneath only joins Latin letters into words.
        var diff2htmlUi = new Diff2HtmlUI(targetElement, diffText, {
            outputFormat: outputFormat,
            drawFileList: false,
            matching: 'lines',
            fileContentToggle: false
        });
        diff2htmlUi.draw();
        targetElement.classList.toggle('d2h-dark-color-scheme', isDarkTheme());
        alignSideBySideRows();
    }

    $('.selectOutputFormat').change(function () {
        draw($(this).val());
    }).change();

    $(window).on('resize', alignSideBySideRows);

    new MutationObserver(function () {
        targetElement.classList.toggle('d2h-dark-color-scheme', isDarkTheme());
    }).observe(document.documentElement, { attributes: true, attributeFilter: ['class'] });
});
