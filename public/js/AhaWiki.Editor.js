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

        if (normalizedKey === 'Enter' && !isComposing) {
            const lastNewlineIndex = selectionStart === 0 ? -1 : value.lastIndexOf('\n', selectionStart - 1);
            const lineStart = lastNewlineIndex + 1;
            const linePrefix = value.substring(lineStart, selectionStart);
            const listMatch = linePrefix.match(/^(\s*)([*-]|(?:[a-zA-Z]+|\d+|[가나다라마바사아자차카타파하]+|[ㄱㄴㄷㄹㅁㅂㅅㅇㅈㅊㅋㅌㅍㅎ]+)\.)\s/);
            if (listMatch) {
                const prefix = listMatch[1] + listMatch[2] + ' ';
                const insertText = '\n' + prefix;
                const newValue = replaceRange(value, selectionStart, selectionEnd, insertText);
                const newPos = selectionStart + insertText.length;
                return {
                    handled: true,
                    value: newValue,
                    selectionStart: newPos,
                    selectionEnd: newPos,
                };
            }
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
                const insertText = '[#!' + selected + '\n]';
                const newValue = replaceRange(value, selectionStart, selectionEnd, insertText);
                return {
                    handled: true,
                    value: newValue,
                    selectionStart: selectionStart + 3,
                    selectionEnd: selectionStart + 3 + selected.length,
                };
            }
        }

        for (const { open, close } of openClose) {
            if (normalizedKey === open) {
                if (normalized.fromCode && key === 'Process') {
                    const newValue = replaceRange(value, selectionStart, selectionEnd, selected + close);
                    // 커서를 선택 영역 앞에 두면 IME가 삽입하는 open 문자가 선택 영역 앞에 붙음
                    // e.g. "def]" → 커서(4) → IME가 "[" 삽입 → "[def]"
                    const newPos = selectionStart;
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

    function applyToTextareaElement(el, e, selectionOverride) {
        const result = applyEditorRule({
            value: el.value,
            selectionStart: selectionOverride ? selectionOverride.start : el.selectionStart,
            selectionEnd:   selectionOverride ? selectionOverride.end   : el.selectionEnd,
            key: e.key,
            code: e.code,
            shiftKey: !!e.shiftKey,
            isComposing: !!e.isComposing,
        });
        if (!result.handled)
            return false;

        e.preventDefault();
        el.value = result.value;
        el.selectionStart = result.selectionStart;
        el.selectionEnd = result.selectionEnd;
        $(el).trigger('input');
        return true;
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
        $(selector).each(function () {
            const el = this;
            // 한글 IME는 keydown 전에 선택 범위를 강제 해제하는 경우가 있음.
            // mouseup/keyup 시점에 비선택(collapsed) 상태가 아닐 때만 저장해 두고,
            // keydown에서 IME(key==='Process')로 선택이 사라져 있으면 복원.
            let savedSel = null;

            function updateSavedSel() {
                savedSel = el.selectionStart !== el.selectionEnd
                    ? { start: el.selectionStart, end: el.selectionEnd, value: el.value }
                    : null;
            }

            el.addEventListener('mouseup', updateSavedSel);
            el.addEventListener('keyup',   updateSavedSel);

            $(el).on('keydown', function (e) {
                let override = null;
                if (
                    e.key === 'Process' &&
                    el.selectionStart === el.selectionEnd &&
                    savedSel !== null &&
                    savedSel.value === el.value
                ) {
                    override = { start: savedSel.start, end: savedSel.end };
                }
                const handled = applyToTextareaElement(el, e, override);
                // override를 사용해 처리된 경우: IME가 open 문자(1글자)를 삽입한 뒤
                // 선택을 복원한다. setTimeout(fn,0)은 한글 IME 타이밍 이슈의 표준 해법으로,
                // 현재 이벤트 루프(IME 문자 삽입 포함)가 끝난 뒤 실행된다.
                if (override && handled) {
                    const restoreStart = override.start + 1; // open 문자 1글자만큼 뒤로
                    const restoreEnd   = override.end   + 1;
                    setTimeout(() => {
                        el.selectionStart = restoreStart;
                        el.selectionEnd   = restoreEnd;
                        // 이후 IME 연속 입력에 대비해 savedSel도 갱신
                        savedSel = { start: restoreStart, end: restoreEnd, value: el.value };
                    }, 0);
                }
            });
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
