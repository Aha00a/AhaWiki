(() => {
    const openClose = [
        {open: '(', close: ')'},
        {open: '{', close: '}'},
        {open: '[', close: ']'},
        {open: '"', close: '"'},
        {open: "'", close: "'"},
        {open: '`', close: '`'},
    ];

    function replaceRange(value, start, end, text) {
        return value.substring(0, start) + text + value.substring(end);
    }

    function normalizeKey({key, code, shiftKey, isComposing}) {
        if (isComposing)
            return { key, fromCode: false };

        const fallbackAllowed = key === 'Unidentified' || key === '' || key === 'Process';
        if (!fallbackAllowed)
            return { key, fromCode: false };

        const byCode = {
            BracketLeft: shiftKey ? '{' : '[',
            BracketRight: shiftKey ? '}' : ']',
            Quote: shiftKey ? '"' : "'",
            Backquote: shiftKey ? '~' : '`',
            Digit9: shiftKey ? '(' : '9',
            Digit0: shiftKey ? ')' : '0',
        };

        return { key: byCode[code] ?? key, fromCode: !!byCode[code] };
    }

    function applyEditorRule({value, selectionStart, selectionEnd, key, code, shiftKey, isComposing}) {
        const normalized = normalizeKey({key, code, shiftKey, isComposing});
        const normalizedKey = normalized.key;
        const selected = value.substring(selectionStart, selectionEnd);

        if (normalizedKey === 'Tab') {
            if (selectionStart === 0 || value.length === selectionStart)
                return { handled: false };

            const lastNewlineIndex = selectionStart === 0 ? -1 : value.lastIndexOf('\n', selectionStart - 1);
            const lineEnd = value.indexOf('\n', selectionStart) === -1 ? value.length : value.indexOf('\n', selectionStart);
            const currentLine = value.substring(lastNewlineIndex + 1, lineEnd);
            const listPattern = /^\s+(?:[*-]|(?:\d+|[a-zA-Z]+|[ivxIVX]+|[가나다라마바사아자차카타파하]+|[ㄱㄴㄷㄹㅁㅂㅅㅇㅈㅊㅋㅌㅍㅎ]+)\.)\s/;
            if (listPattern.test(currentLine)) {
                const lineStart = lastNewlineIndex + 1;
                const indentation = currentLine.match(/^[\t ]*/)[0];
                if (shiftKey) {
                    if (!indentation.length)
                        return { handled: true };
                    const newValue = replaceRange(value, lineStart, lineStart + 1, '');
                    const newPos = Math.max(lineStart, selectionStart - 1);
                    return {
                        handled: true,
                        value: newValue,
                        selectionStart: newPos,
                        selectionEnd: newPos,
                    };
                }

                const insertText = indentation[0] === '\t' ? '\t' : ' ';
                const newValue = replaceRange(value, lineStart, lineStart, insertText);
                const newPos = selectionStart + 1;
                return {
                    handled: true,
                    value: newValue,
                    selectionStart: newPos,
                    selectionEnd: newPos,
                };
            }

            const newValue = replaceRange(value, selectionStart, selectionEnd, '\t');
            const newPos = selectionStart + 1;
            return {
                handled: true,
                value: newValue,
                selectionStart: newPos,
                selectionEnd: newPos,
            };
        }

        if (normalizedKey === '[' && !(normalized.fromCode && key === 'Process')) {
            if (value.slice(selectionStart - 3, selectionStart) === '[[[') {
                const insertText = '[' + selected + '\n]';
                const newValue = replaceRange(value, selectionStart, selectionEnd, insertText);
                return {
                    handled: true,
                    value: newValue,
                    selectionStart: selectionStart + 1,
                    selectionEnd: selectionStart + 1 + selected.length,
                };
            }

            if (value.slice(selectionStart - 2, selectionStart) === '--') {
                const dateStr = dayjs().format('YYYY-MM-DD');
                const dateTimeStr = dayjs().format('YYYY-MM-DDTHH:mm:ss');
                const insertText = `[${dateStr} ${dateTimeStr}]`;
                const newValue = replaceRange(value, selectionStart, selectionEnd, insertText);
                return {
                    handled: true,
                    value: newValue,
                    selectionStart: selectionStart + 11,
                    selectionEnd: selectionStart + 11,
                };
            }

            if (value.slice(selectionStart - 2, selectionStart) === '[[') {
                const insertText = '[' + selected + '\n]';
                const newValue = replaceRange(value, selectionStart, selectionEnd, insertText);
                return {
                    handled: true,
                    value: newValue,
                    selectionStart: selectionStart + 1,
                    selectionEnd: selectionStart + 1 + selected.length,
                };
            }
        }

        for (const { open, close } of openClose) {
            if (normalizedKey === open) {
                if (normalized.fromCode && key === 'Process') {
                    const newValue = replaceRange(value, selectionStart, selectionEnd, selected + close);
                    const newPos = selectionStart + selected.length;
                    return {
                        handled: true,
                        value: newValue,
                        selectionStart: newPos,
                        selectionEnd: newPos,
                    };
                }

                const insertText = open + selected + close;
                const newValue = replaceRange(value, selectionStart, selectionEnd, insertText);
                return {
                    handled: true,
                    value: newValue,
                    selectionStart: selectionStart + 1,
                    selectionEnd: selectionStart + 1 + selected.length,
                };
            }
        }

        if (normalizedKey === 'Backspace') {
            if (
                value.slice(selectionStart - 3, selectionStart) === '[[[' &&
                value[selectionEnd] === '\n' &&
                value.slice(selectionEnd + 1, selectionEnd + 4) === ']]]'
            ) {
                let start = selectionStart - 3;
                let end = selectionEnd + 4;
                if (value[selectionStart - 4] === '[' && value[selectionEnd + 4] === ']') {
                    start = selectionStart - 4;
                    end = selectionEnd + 5;
                }

                const inner = value.substring(selectionStart, selectionEnd);
                const insertText = value[selectionStart - 4] === '[' && value[selectionEnd + 4] === ']'
                    ? inner + '\n'
                    : inner;
                const newValue = replaceRange(value, start, end, insertText);
                const newPos = start;
                return {
                    handled: true,
                    value: newValue,
                    selectionStart: newPos,
                    selectionEnd: newPos,
                };
            }

            for (const { open, close } of openClose) {
                if (value[selectionStart - 1] === open && value[selectionEnd] === close) {
                    const newValue = replaceRange(value, selectionStart - 1, selectionEnd + 1, selected);
                    const newPosStart = Math.max(0, selectionStart - 1);
                    const newPosEnd = newPosStart + selected.length;
                    return {
                        handled: true,
                        value: newValue,
                        selectionStart: newPosStart,
                        selectionEnd: newPosEnd,
                    };
                }
            }
        }

        return { handled: false };
    }

    function applyToTextareaElement(el, e) {
        const result = applyEditorRule({
            value: el.value,
            selectionStart: el.selectionStart,
            selectionEnd: el.selectionEnd,
            key: e.key,
            code: e.code,
            shiftKey: !!e.shiftKey,
            isComposing: !!e.isComposing,
        });
        if (!result.handled)
            return;

        e.preventDefault();
        el.value = result.value;
        el.selectionStart = result.selectionStart;
        el.selectionEnd = result.selectionEnd;
        $(el).trigger('input');
    }

    function applyToCodeMirrorInstance(cm, e) {
        const doc = cm.getDoc();
        const from = doc.getCursor('from');
        const to = doc.getCursor('to');
        const value = doc.getValue();
        const selectionStart = cm.indexFromPos(from);
        const selectionEnd = cm.indexFromPos(to);

        const result = applyEditorRule({
            value,
            selectionStart,
            selectionEnd,
            key: e.key,
            code: e.code,
            shiftKey: !!e.shiftKey,
            isComposing: !!e.isComposing,
        });

        if (!result.handled)
            return;

        e.preventDefault();
        doc.setValue(result.value);
        doc.setSelection(cm.posFromIndex(result.selectionStart), cm.posFromIndex(result.selectionEnd));
        cm.focus();
    }

    function addEventListener(selector) {
        $(selector).on('keydown', function (e) {
            applyToTextareaElement(this, e);
        });
    }

    function addCodeMirrorEventListener(cm) {
        if (!cm || typeof cm.on !== 'function')
            return;
        cm.on('keydown', function (instance, e) {
            applyToCodeMirrorInstance(instance, e);
        });
    }

    if(typeof window.AhaWiki === 'undefined') {
        window.AhaWiki = {};
    }

    window.AhaWiki.Editor = {
        addEventListener,
        addCodeMirrorEventListener,
    };
})();
