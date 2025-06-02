(() => {
    const ExecCommand = {
        insertText: text => document.execCommand("insertText", false, text),
        backspace: () => document.execCommand("delete"),
        delete: () => document.execCommand("forwardDelete"),
    };
    const openClose = [
        {open: '(', close: ')'},
        {open: '{', close: '}'},
        {open: '[', close: ']'},
        {open: '"', close: '"'},
        {open: "'", close: "'"},
        {open: "`", close: "`"},
    ];

    function addEventListener(selector) {
        $(selector).on('keydown', function (e) {
            const value = this.value;
            const selectionStart = this.selectionStart;
            const selectionEnd = this.selectionEnd;
            const selected = value.substring(selectionStart, selectionEnd);

            if (e.key === 'Tab') {
                if (selectionStart === 0 || value.length === selectionStart)
                    return;

                const lastNewlineIndex = selectionStart === 0 ? -1 : value.lastIndexOf('\n', selectionStart - 1);
                const currentLine = value.substring(lastNewlineIndex + 1, value.indexOf('\n', selectionStart) === -1 ? value.length : value.indexOf('\n', selectionStart));
                const listPattern = /^\s+(?:[*-]|(?:\d+|[a-zA-Z]+|[ivxIVX]+|[가나다라마바사아자차카타파하]+|[ㄱㄴㄷㄹㅁㅂㅅㅇㅈㅊㅋㅌㅍㅎ]+)\.)\s/;
                if (listPattern.test(currentLine)) {
                    const lineStart = value.lastIndexOf('\n', selectionStart - 1) + 1;
                    this.selectionStart = lineStart;
                    this.selectionEnd = lineStart;
                    const indentation = currentLine.match(/^[\t ]*/)[0];
                    if (e.shiftKey) {
                        if (indentation.length > 0) {
                            ExecCommand.delete();
                        }
                    } else {
                        ExecCommand.insertText(indentation[0] === '\t' ? '\t' : ' ');
                    }
                    this.selectionStart = this.selectionEnd = selectionStart + (e.shiftKey ? -1 : 1);
                    return false;
                }
                ExecCommand.insertText('\t');
                return false;
            }

            if (e.key === '[') {
                if (value.slice(selectionStart - 3, selectionStart) === '[[[') {
                    this.selectionStart = this.selectionEnd = selectionEnd;
                    ExecCommand.delete();
                    this.selectionStart = selectionStart;
                    ExecCommand.insertText('[' + selected + '\n]');
                    this.selectionStart = selectionStart + 1;
                    this.selectionEnd = selectionEnd + 1;
                    return false;
                } else if (value.slice(selectionStart - 2, selectionStart) === '--') {
                    const dateStr = dayjs().format('YYYY-MM-DD');
                    const dateTimeStr = dayjs().format('YYYY-MM-DDTHH:mm:ss');
                    ExecCommand.insertText(`[${dateStr} ${dateTimeStr}]`);
                    this.selectionStart = selectionStart + 11;
                    this.selectionEnd = this.selectionEnd - 1;
                    return false;
                } else if (value.slice(selectionStart - 2, selectionStart) === '[[') {
                    ExecCommand.insertText('[' + selected + '\n]');
                    this.selectionStart = selectionStart + 1;
                    this.selectionEnd = selectionEnd + 1;
                    return false;
                }
            }

            for (const { open, close } of openClose) {
                if (e.key === open) {
                    this.selectionStart = this.selectionEnd = selectionStart;
                    ExecCommand.insertText(open);
                    this.selectionStart = this.selectionEnd = selectionEnd + 1;
                    ExecCommand.insertText(close);
                    this.selectionStart = selectionStart + 1;
                    this.selectionEnd = selectionEnd + 1;
                    return false;
                }
            }

            if (e.key === 'Backspace') {
                if (
                    value.slice(selectionStart - 3, selectionStart) === '[[[' &&
                    value[selectionEnd] === '\n' &&
                    value.slice(selectionEnd + 1, selectionEnd + 4) === ']]]'
                ) {
                    if (value[selectionStart - 4] === '[' && value[selectionEnd + 4] === ']') {
                        ExecCommand.delete();
                        ExecCommand.delete();
                        ExecCommand.backspace();
                        ExecCommand.insertText('\n');
                        this.selectionStart = this.selectionEnd = this.selectionStart - 1;
                        return false;
                    }
                    ExecCommand.backspace();
                    ExecCommand.delete();
                    ExecCommand.delete();
                    return false;
                }

                for (const { open, close } of openClose) {
                    if (value[selectionStart - 1] === open && value[selectionEnd] === close) {
                        ExecCommand.backspace();
                        ExecCommand.delete();
                        return false;
                    }
                }
            }
        });
    }

    if(typeof window.AhaWiki === "undefined") {
        window.AhaWiki = {};
    }

    window.AhaWiki.Editor = {
        addEventListener,
    };
})();
