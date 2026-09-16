// Pure text operations for the formatting toolbar. Given the full editor text and a selection
// [s, e), each returns the minimal edit to apply: replace [rangeStart, rangeEnd) with replacement,
// then select [selectionStart, selectionEnd). No DOM here, so it is unit-tested directly and both
// the CodeMirror and textarea paths in edit.page.js apply the same result.
(function () {
    window.AhaWiki = window.AhaWiki || {};
    window.AhaWiki.Format = {
        // Wrap the selection with prefix/suffix (e.g. ''' ''' for bold). With an empty selection the
        // caret lands between prefix and suffix; with a selection the wrapped text stays selected.
        wrap: function (value, s, e, prefix, suffix) {
            var selected = value.slice(s, e);
            var caret = s + prefix.length;
            return {
                rangeStart: s,
                rangeEnd: e,
                replacement: prefix + selected + suffix,
                selectionStart: caret,
                selectionEnd: selected.length ? caret + selected.length : caret
            };
        },
        // Add linePrefix at the start of every line the selection touches (headings, list items).
        // Operates over the whole touched block and leaves the rewritten block selected.
        prefixLines: function (value, s, e, linePrefix) {
            var start = value.lastIndexOf('\n', s - 1) + 1;
            var probe = e > s ? e - 1 : s;
            var nl = value.indexOf('\n', probe);
            var end = nl === -1 ? value.length : nl;
            var replacement = value.slice(start, end).split('\n').map(function (l) { return linePrefix + l; }).join('\n');
            return {
                rangeStart: start,
                rangeEnd: end,
                replacement: replacement,
                selectionStart: start,
                selectionEnd: start + replacement.length
            };
        }
    };
})();
