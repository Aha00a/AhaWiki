$(function () {
    $('.selectOutputFormat').change(function () {
        var diff2htmlUi = new Diff2HtmlUI({ diff: $('.unifiedDiff').html() });
        diff2htmlUi.draw('.diff2HtmlResult', { outputFormat: $(this).val() });
    }).change();
});
