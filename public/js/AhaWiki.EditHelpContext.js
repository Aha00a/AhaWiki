// Which row of the editor's formatting help (logics.EditHelp) is about what the cursor is
// on. Pure text in, a kind out -- the same strings as EditHelp's `kind` -- so it is unit-tested
// directly (test/ahawiki.edit-help-context.test.mjs) and edit.page.js only has to light the rows.
//
// The order of the questions is the order a reader would answer them: inside a [[[#!Name ...]]]
// block, the block is what they are writing; otherwise the inline markup right under the cursor
// (a link inside a list item is a link); otherwise what the line is.
(function () {
    window.AhaWiki = window.AhaWiki || {};

    // How far up to look for the start of a block. A block longer than this is rare, and a
    // cursor move must not walk a whole long page.
    var blockScanLimit = 500;

    var blockStart = /^\[\[\[(?:#!\s*([A-Za-z][\w-]*))?/;
    var blockEnd = /^\]\]\]/;

    // The name of the block the cursor is in, in lower case; "text" for a block with no #! line;
    // null outside any block.
    function blockAt(getLine, line) {
        var lowest = Math.max(0, line - blockScanLimit);
        for (var i = line; i >= lowest; i--) {
            var text = getLine(i) || '';
            if (i < line && blockEnd.test(text)) return null;
            var m = text.match(blockStart);
            if (m) return (m[1] || 'text').toLowerCase();
        }
        return null;
    }

    // Each inline form, with the pattern that finds it on a line. The first whose match covers
    // the cursor wins; code first, because '' and [ ] inside backticks are not markup.
    var inlineForms = [
        ['code', /`[^`]+`/g],
        ['include', /\[\[Include\([^)]*\)\]\]/g],
        ['link', /(?<!\[)\[(?!\[)([^\[\]]*\S[^\[\]]*)\](?!\])/g],
        ['emphasis', /'''.+?'''|''.+?''/g]
    ];

    function inlineAt(text, ch) {
        for (var i = 0; i < inlineForms.length; i++) {
            var kind = inlineForms[i][0];
            var pattern = new RegExp(inlineForms[i][1].source, 'g');
            var m;
            while ((m = pattern.exec(text)) !== null) {
                if (m.index <= ch && ch <= m.index + m[0].length) return kind;
                if (m[0].length === 0) pattern.lastIndex++;
            }
        }
        return null;
    }

    function lineKind(text) {
        if (/^=+\s*\S/.test(text)) return 'heading';
        if (/^-{4,}\s*$/.test(text)) return 'hr';
        if (/^\s+[*-]\s*(\[ \]|\[\[CB\()/.test(text)) return 'checkbox';
        if (/^\s+[*-](\s|$)/.test(text)) return 'list';
        if (/^\s+(\d+|[a-zA-Z]+|[가나다라마바사아자차카타파하]+|[ㄱㄴㄷㄹㅁㅂㅅㅇㅈㅊㅋㅌㅍㅎ]+)\.(\s|$)/.test(text)) return 'numbered';
        return null;
    }

    window.AhaWiki.EditHelpContext = {
        // getLine(i) answers the text of line i (0-based); line and ch are the cursor's.
        kindAt: function (getLine, line, ch) {
            var block = blockAt(getLine, line);
            if (block) return block;
            var text = getLine(line) || '';
            return inlineAt(text, ch) || lineKind(text) || '';
        }
    };
})();
