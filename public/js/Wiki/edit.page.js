const AhaWikiEditConfig = window.AhaWikiEditConfig || {};
AhaWikiEditConfig.api = AhaWikiEditConfig.api || {};

        function adjustEditorLayoutHeight() {
            var windowInnerHeight = $(window).innerHeight();
            var flashInnerHeight = $('.flash').innerHeight();
            var baseColumnHeight = Math.max(280, windowInnerHeight - flashInnerHeight - 300);

            var leftFixedHeight = ($('.left > .toolbar').outerHeight(true) || 0) +
                ($('.left > .editHelp').outerHeight(true) || 0) +
                ($('.left > .editorAdditional').outerHeight(true) || 0) +
                ($('.left > .attachmentList').outerHeight(true) || 0);
            var rightFixedHeight = ($('.right > .toolbar').outerHeight(true) || 0) +
                ($('.right > .previewAdditional').outerHeight(true) || 0);

            var editorHeight = Math.max(220, baseColumnHeight - leftFixedHeight - 9);
            var previewHeight = Math.max(220, baseColumnHeight - rightFixedHeight);

            $('.previewPane').css({ height: previewHeight, overflowY: 'auto'});
            $("[name=text]").css({ height: editorHeight });
            if (window.AhaWikiCodeMirrorEditor) {
                window.AhaWikiCodeMirrorEditor.setSize(null, editorHeight);
            }

            const tableBodyHeight = editorHeight;
            $('.tableInlineEditorBody').css({ height: Math.max(120, tableBodyHeight) });

            var leftTotalHeight = $('.left').outerHeight();
            var rightTotalHeight = $('.right').outerHeight();
            if (leftTotalHeight && rightTotalHeight) {
                if (leftTotalHeight < rightTotalHeight) {
                    var editorDelta = rightTotalHeight - leftTotalHeight;
                    var nextEditorHeight = editorHeight + editorDelta;
                    $("[name=text]").css({ height: nextEditorHeight });
                    if (window.AhaWikiCodeMirrorEditor) {
                        window.AhaWikiCodeMirrorEditor.setSize(null, nextEditorHeight);
                    }
                    $('.tableInlineEditorBody').css({ height: Math.max(120, nextEditorHeight) });
                } else if (rightTotalHeight < leftTotalHeight) {
                    var previewDelta = leftTotalHeight - rightTotalHeight;
                    $('.previewPane').css({ height: previewHeight + previewDelta, overflowY: 'auto'});
                }
            }

            if (typeof window.AhaWikiSyncPreviewScrollNow === 'function') {
                window.AhaWikiSyncPreviewScrollNow();
            }
        }

        function preview() {
            if (window.AhaWikiCodeMirrorEditor)
                window.AhaWikiCodeMirrorEditor.save();
            let previewText = $('textarea[name=text]').val();
            if ($('#ganttInlineEditor').hasClass('visible')) {
                const lines = previewText.split('\n');
                const shebangLine = (lines[0] || '').trim();
                if (/^#!gantt/i.test(shebangLine)) {
                    const sectionIdx = lines.findIndex((l, i) => i > 0 && /^={1,6}[\s=]/.test(l.trim()));
                    if (sectionIdx !== -1) previewText = lines.slice(0, sectionIdx).join('\n');
                }
            }
            $.post('/preview', {
                csrfToken: $('[name=csrfToken]').val(),
                name: AhaWikiEditConfig.pageName,
                text: previewText,
                lineStart: $('input[name=lineStart]').val(),
                lineEnd: $('input[name=lineEnd]').val()
            }, function (data, textStatus, jqXHR) {
                var $previewPane = $('.previewPane');
                $previewPane.html(data);
                mermaid.init();
                if (window.AhaWiki && window.AhaWiki.Gantt) window.AhaWiki.Gantt.initAll($previewPane[0]);
                adjustEditorLayoutHeight();
                $previewPane.find('.paperContent .page').each(function() {
                    var $page = $(this);
                    var $content = $page.find('.pageContent > div').first();
                    var $footer  = $page.find('.pageFooter').first();
                    if (!$content.length || !$footer.length) return;
                    var contentBottom = $content.offset().top + $content.outerHeight(true);
                    var footerTop     = $footer.offset().top;
                    if (contentBottom > footerTop) {
                        $page.addClass('pageOverflow');
                    }
                });
            });
        }

        var timer;
        $(function(){
            const $editHelp = $('.editHelp');
            const editHelpElement = $editHelp.get(0);
            if (editHelpElement) {
                editHelpElement.addEventListener('toggle', function () {
                    setTimeout(adjustEditorLayoutHeight, 0);
                });
                const openAttributeObserver = new MutationObserver(function () {
                    setTimeout(adjustEditorLayoutHeight, 0);
                });
                openAttributeObserver.observe(editHelpElement, { attributes: true, attributeFilter: ['open'] });
            }
            $(window).on('resize', adjustEditorLayoutHeight);
            adjustEditorLayoutHeight();

            $.fn.insertAtCaret = function(myValue) {
                return this.each(function() {
                    var me = this;
                    if (document.selection) { // IE
                        me.focus();
                        sel = document.selection.createRange();
                        sel.text = myValue;
                        me.focus();
                    } else if (me.selectionStart || me.selectionStart == '0') { // Real browsers
                        var startPos = me.selectionStart, endPos = me.selectionEnd, scrollTop = me.scrollTop;
                        me.value = me.value.substring(0, startPos) + myValue + me.value.substring(endPos, me.value.length);
                        me.focus();
                        me.selectionStart = startPos + myValue.length;
                        me.selectionEnd = startPos + myValue.length;
                        me.scrollTop = scrollTop;
                    } else {
                        me.value += myValue;
                        me.focus();
                    }
                });
            };

            const $textarea = $('textarea[name=text]');
            let editor;
            const $uploadInput = $('#attachmentUploadInput');
            const $attachmentList = $('#attachmentList');
            const $uploadDragOverlay = $('#uploadDragOverlay');
            const $macroAutocomplete = $('<div id="macroAutocomplete" class="macroAutocomplete"></div>').appendTo('body');
            const $tableInlineEditor = $('#tableInlineEditor');
            const $tableInlineEditorGrid = $('#tableInlineEditorGrid');
            const $tableInlineOption = $('#tableInlineOption');
            let tableInlineAutoApplyTimer = null;
            let tableInlineCursor = { row: 0, col: 0 };
            const $ganttInlineEditor = $('#ganttInlineEditor');
            const $ganttInlineEditorGrid = $('#ganttInlineEditorGrid');
            let ganttAutoApplyTimer = null;
            let ganttCursor = { row: 0, col: 0 };
            const GANTT_COL_NAME = 0, GANTT_COL_START = 1, GANTT_COL_EST = 2, GANTT_COL_COUNT = 3;

            function detectTableBlock(value) {
                const lines = value.split('\n');
                if (!lines.length)
                    return null;
                const firstLine = (lines[0] || '').trim();
                const inlineWrappedMatch = firstLine.match(/^\[\[\[#!table(?:\s+(.+))?$/i);
                if (inlineWrappedMatch) {
                    const optionInline = (inlineWrappedMatch[1] || 'tsv').trim().toLowerCase();
                    const delimiterInline = optionInline.startsWith('csv') ? ',' : '\t';
                    const hasClosingWrapperInline = (lines[lines.length - 1] || '').trim() === ']]]';
                    return {
                        delimiter: delimiterInline,
                        option: optionInline,
                        dataStartLineIndex: 1,
                        dataEndExclusiveLineIndex: hasClosingWrapperInline ? lines.length - 1 : lines.length
                    };
                }
                const hasBlockWrapper = /^\[\[\[$/.test(firstLine);
                const shebangLineIndex = hasBlockWrapper ? 1 : 0;
                const shebang = (lines[shebangLineIndex] || '').trim();
                const match = shebang.match(/^#!table(?:\s+(.+))?$/i);
                if (!match)
                    return null;
                const option = (match[1] || 'tsv').trim().toLowerCase();
                const delimiter = option.startsWith('csv') ? ',' : '\t';
                const hasClosingWrapper = hasBlockWrapper && (lines[lines.length - 1] || '').trim() === ']]]';
                return {
                    delimiter,
                    option,
                    dataStartLineIndex: shebangLineIndex + 1,
                    dataEndExclusiveLineIndex: hasClosingWrapper ? lines.length - 1 : lines.length
                };
            }

                function detectGanttBlock(value) {
                    const lines = value.split('\n');
                    if (!lines.length) return null;
                    const firstLine = (lines[0] || '').trim();
                    const inlineWrappedMatch = firstLine.match(/^\[\[\[#!gantt(?:\s+.*)?$/i);
                    if (inlineWrappedMatch) {
                        const closingIdx = lines.findIndex((l, i) => i > 0 && l.trim() === ']]]');
                        return { dataStartLineIndex: 1, dataEndExclusiveLineIndex: closingIdx !== -1 ? closingIdx : lines.length };
                    }
                    const hasBlockWrapper = /^\[\[\[$/.test(firstLine);
                    const shebangLineIndex = hasBlockWrapper ? 1 : 0;
                    const shebang = (lines[shebangLineIndex] || '').trim();
                    if (!/^#!gantt(?:\s+.*)?$/i.test(shebang)) return null;
                    if (hasBlockWrapper) {
                        const closingIdx = lines.findIndex((l, i) => i > shebangLineIndex && l.trim() === ']]]');
                        return { dataStartLineIndex: shebangLineIndex + 1, dataEndExclusiveLineIndex: closingIdx !== -1 ? closingIdx : lines.length };
                    }
                    // #!gantt 전용 페이지: 섹션 헤더(==)를 데이터 끝으로 인식
                    const sectionIdx = lines.findIndex((l, i) => i > shebangLineIndex && /^={1,6}[\s=]/.test(l.trim()));
                    return { dataStartLineIndex: shebangLineIndex + 1, dataEndExclusiveLineIndex: sectionIdx !== -1 ? sectionIdx : lines.length };
                }

            function parseCells(value, tableInfo) {
                const lines = value.split('\n');
                return lines
                    .slice(tableInfo.dataStartLineIndex, tableInfo.dataEndExclusiveLineIndex)
                    .map(line => line.split(tableInfo.delimiter));
            }

            function showStandardEditor() {
                const cm = window.AhaWikiCodeMirrorEditor;
                if (cm && cm.getWrapperElement) {
                    $(cm.getWrapperElement()).show();
                    $textarea.hide();
                } else {
                    $textarea.show();
                }
            }

            function hideStandardEditor() {
                const cm = window.AhaWikiCodeMirrorEditor;
                if (cm && cm.getWrapperElement) {
                    $(cm.getWrapperElement()).hide();
                    $textarea.hide();
                } else {
                    $textarea.hide();
                }
            }
            function renderTableGrid($grid, rows) {
                const maxCol = Math.max(1, ...rows.map(r => r.length));
                const html = rows.map((row, rIdx) => {
                    const tds = Array.from({length: maxCol}).map((_, cIdx) => `<td><input type="text" data-r="${rIdx}" data-c="${cIdx}" value="${$('<div>').text(row[cIdx] || '').html()}"></td>`).join('');
                    return `<tr>${tds}</tr>`;
                }).join('');
                $grid.html(html || '<tr><td><input type="text" data-r="0" data-c="0" value=""></td></tr>');
                focusTableCell(tableInlineCursor.row, tableInlineCursor.col, false);
            }
            function getGridSize() {
                const rowCount = $tableInlineEditorGrid.find('tr').length;
                const colCount = Math.max(1, $tableInlineEditorGrid.find('tr').first().find('input').length || 1);
                return { rowCount, colCount };
            }
            function focusTableCell(row, col, withFocus) {
                const size = getGridSize();
                if (!size.rowCount)
                    return null;
                const nextRow = Math.max(0, Math.min(row, size.rowCount - 1));
                const nextCol = Math.max(0, Math.min(col, size.colCount - 1));
                tableInlineCursor = { row: nextRow, col: nextCol };
                $tableInlineEditorGrid.find('input').removeClass('tableInlineEditorCellActive');
                const $target = $tableInlineEditorGrid.find(`input[data-r="${nextRow}"][data-c="${nextCol}"]`);
                $target.addClass('tableInlineEditorCellActive');
                if (withFocus && $target.length) {
                    $target.trigger('focus');
                    const value = $target.val();
                    $target[0].setSelectionRange(value.length, value.length);
                }
                return $target;
            }
            function gridAsMatrix() {
                const matrix = [];
                $tableInlineEditorGrid.find('tr').each(function () {
                    const cols = [];
                    $(this).find('input').each(function () { cols.push($(this).val()); });
                    matrix.push(cols);
                });
                return matrix.length ? matrix : [['']];
            }
            function rerenderWithCursor(rows, row, col, withFocus) {
                tableInlineCursor = { row, col };
                renderTableGrid($tableInlineEditorGrid, rows);
                focusTableCell(row, col, withFocus);
                scheduleInlineAutoApply();
            }

            function openInlineTableEditor() {
                const src = window.AhaWikiCodeMirrorEditor ? window.AhaWikiCodeMirrorEditor.getValue() : $textarea.val();
                const table = detectTableBlock(src);
                if (!table)
                    return false;
                $tableInlineOption.val(table.option || 'tsv');
                renderTableGrid($tableInlineEditorGrid, parseCells(src, table));
                focusTableCell(0, 0, false);
                $tableInlineEditor.addClass('visible');
                hideStandardEditor();
                $('.toolbarTableOnly').hide();
                $('.toggleTableEditor').text('Switch to Raw Editor');
                return true;
            }
            function closeInlineTableEditor() {
                if (tableInlineAutoApplyTimer) {
                    clearTimeout(tableInlineAutoApplyTimer);
                    tableInlineAutoApplyTimer = null;
                }
                applyTableBlockEditor();
                syncEditorToTextarea();
                $tableInlineEditor.removeClass('visible');
                showStandardEditor();
                $('.toolbarTableOnly').show();
                $('.toggleTableEditor').text('Switch to Table Editor');
            }

            function parseGanttSource(src) {
                const block = detectGanttBlock(src);
                if (!block) return [];
                return src.split('\n')
                    .slice(block.dataStartLineIndex, block.dataEndExclusiveLineIndex)
                    .map(line => {
                        if (!line.trim()) return null;
                        const cols = line.split('\t');
                        let depth = 0, name = '', nameIdx = -1;
                        for (let i = 0; i < cols.length; i++) {
                            if (cols[i].trim() !== '') { depth = i; name = cols[i].trim(); nameIdx = i; break; }
                        }
                        if (nameIdx === -1) return null;
                        const rest = [];
                        for (let j = nameIdx + 1; j < cols.length; j++) {
                            const v = cols[j].trim();
                            if (v) rest.push(v);
                        }
                        let est = '', startOrRef = '';
                        if (rest.length > 0 && /^\d+$/.test(rest[rest.length - 1])) est = rest.pop();
                        if (rest.length > 0) startOrRef = rest[rest.length - 1];
                        return { depth, name, startOrRef, est };
                    })
                    .filter(r => r !== null);
            }

            function renderGanttGrid(rows) {
                if (!rows.length) rows = [{ depth: 0, name: '', startOrRef: '', est: '' }];
                const html = rows.map((row, rIdx) => {
                    const paddingLeft = row.depth * 20 + 6;
                    const nameTd = `<td><input class="ganttNameInput" type="text" data-r="${rIdx}" data-c="${GANTT_COL_NAME}" style="padding-left:${paddingLeft}px" value="${$('<div>').text(row.name).html()}"></td>`;
                    const startTd = `<td><input class="ganttStartOrRefInput" type="text" data-r="${rIdx}" data-c="${GANTT_COL_START}" value="${$('<div>').text(row.startOrRef).html()}"></td>`;
                    const estTd = `<td><input class="ganttEstInput" type="number" min="1" data-r="${rIdx}" data-c="${GANTT_COL_EST}" value="${$('<div>').text(row.est).html()}"></td>`;
                    return `<tr data-depth="${row.depth}">${nameTd}${startTd}${estTd}</tr>`;
                }).join('');
                $ganttInlineEditorGrid.html(html);
                focusGanttCell(ganttCursor.row, ganttCursor.col, false);
            }

            function focusGanttCell(row, col, withFocus) {
                const rowCount = $ganttInlineEditorGrid.find('tr').length;
                if (!rowCount) return null;
                const nextRow = Math.max(0, Math.min(row, rowCount - 1));
                const nextCol = Math.max(0, Math.min(col, GANTT_COL_COUNT - 1));
                ganttCursor = { row: nextRow, col: nextCol };
                $ganttInlineEditorGrid.find('input').removeClass('tableInlineEditorCellActive');
                const $target = $ganttInlineEditorGrid.find(`input[data-r="${nextRow}"][data-c="${nextCol}"]`);
                $target.addClass('tableInlineEditorCellActive');
                if (withFocus && $target.length) {
                    $target.trigger('focus');
                    const val = $target.val();
                    if ($target[0].type !== 'number') $target[0].setSelectionRange(val.length, val.length);
                }
                return $target;
            }

            function ganttGridAsRows() {
                const rows = [];
                $ganttInlineEditorGrid.find('tr').each(function () {
                    const depth = parseInt($(this).attr('data-depth'), 10) || 0;
                    const name = $(this).find(`input[data-c="${GANTT_COL_NAME}"]`).val() || '';
                    const startOrRef = $(this).find(`input[data-c="${GANTT_COL_START}"]`).val() || '';
                    const est = $(this).find(`input[data-c="${GANTT_COL_EST}"]`).val() || '';
                    rows.push({ depth, name, startOrRef, est });
                });
                return rows.length ? rows : [{ depth: 0, name: '', startOrRef: '', est: '' }];
            }

            function serializeGanttRow(row) {
                let line = '\t'.repeat(row.depth) + row.name;
                if (row.startOrRef) line += '\t' + row.startOrRef;
                if (row.est) line += '\t' + row.est;
                return line;
            }

            function rerenderGanttWithCursor(rows, row, col, withFocus) {
                ganttCursor = { row, col };
                renderGanttGrid(rows);
                focusGanttCell(row, col, withFocus);
                scheduleGanttAutoApply();
            }

            function applyGanttBlockEditor() {
                const src = window.AhaWikiCodeMirrorEditor ? window.AhaWikiCodeMirrorEditor.getValue() : $textarea.val();
                const block = detectGanttBlock(src);
                if (!block) return;
                const lines = src.split('\n');
                const dataLines = ganttGridAsRows().map(serializeGanttRow);
                const before = lines.slice(0, block.dataStartLineIndex);
                const after = lines.slice(block.dataEndExclusiveLineIndex);
                const next = before.concat(dataLines, after).join('\n');
                if (window.AhaWikiCodeMirrorEditor) {
                    window.AhaWikiCodeMirrorEditor.setValue(next);
                    window.AhaWikiCodeMirrorEditor.save();
                } else {
                    $textarea.val(next);
                }
                $textarea.trigger('input').trigger('change');
            }

            function scheduleGanttAutoApply() {
                if (ganttAutoApplyTimer) { clearTimeout(ganttAutoApplyTimer); ganttAutoApplyTimer = null; }
                ganttAutoApplyTimer = setTimeout(applyGanttBlockEditor, 120);
            }

            function openInlineGanttEditor() {
                const src = window.AhaWikiCodeMirrorEditor ? window.AhaWikiCodeMirrorEditor.getValue() : $textarea.val();
                if (!detectGanttBlock(src)) return false;
                ganttCursor = { row: 0, col: 0 };
                renderGanttGrid(parseGanttSource(src));
                $('#ganttEditorBar').hide();
                $ganttInlineEditor.addClass('visible');
                hideStandardEditor();
                $('.toolbarTableOnly').hide();
                $('.toggleTableEditor').text('Switch to Raw Editor');
                return true;
            }

            function closeInlineGanttEditor() {
                if (ganttAutoApplyTimer) { clearTimeout(ganttAutoApplyTimer); ganttAutoApplyTimer = null; }
                applyGanttBlockEditor();
                syncEditorToTextarea();
                $ganttInlineEditor.removeClass('visible');
                showStandardEditor();
                $('.toolbarTableOnly').show();
                $('.toggleTableEditor').text('Switch to Gantt Editor');
                $('#ganttEditorBar').show();
            }

            function applyTableBlockEditor() {
                const src = window.AhaWikiCodeMirrorEditor ? window.AhaWikiCodeMirrorEditor.getValue() : $textarea.val();
                const table = detectTableBlock(src);
                if (!table) {
                    alert('Partial text가 #!table 블록이 아닙니다.');
                    return;
                }
                const option = ($tableInlineOption.val() || table.option || 'tsv').toString().trim().toLowerCase();
                const delimiter = option.startsWith('csv') ? ',' : '\t';
                const lines = src.split('\n');
                const rows = [];
                $tableInlineEditorGrid.find('tr').each(function () {
                    const cols = [];
                    $(this).find('input').each(function () { cols.push($(this).val()); });
                    rows.push(cols.join(delimiter));
                });
                const before = lines.slice(0, table.dataStartLineIndex);
                if (table.dataStartLineIndex > 0) {
                    before[table.dataStartLineIndex - 1] = before[table.dataStartLineIndex - 1].replace(/(#!table)\b.*/i, '$1 ' + option);
                }
                const after = lines.slice(table.dataEndExclusiveLineIndex);
                const next = before.concat(rows, after).join('\n');
                if (window.AhaWikiCodeMirrorEditor) {
                    window.AhaWikiCodeMirrorEditor.setValue(next);
                    window.AhaWikiCodeMirrorEditor.save();
                } else {
                    $textarea.val(next);
                }
                $textarea.trigger('input').trigger('change');
            }
            function scheduleInlineAutoApply() {
                if (tableInlineAutoApplyTimer) {
                    clearTimeout(tableInlineAutoApplyTimer);
                    tableInlineAutoApplyTimer = null;
                }
                tableInlineAutoApplyTimer = setTimeout(function () { applyTableBlockEditor(); }, 120);
            }
            $('.toggleTableEditor').on('click', function () {
                if ($tableInlineEditor.hasClass('visible')) {
                    closeInlineTableEditor();
                    return false;
                }
                if ($ganttInlineEditor.hasClass('visible')) {
                    closeInlineGanttEditor();
                    return false;
                }
                const src = window.AhaWikiCodeMirrorEditor ? window.AhaWikiCodeMirrorEditor.getValue() : $textarea.val();
                if (detectGanttBlock(src)) {
                    openInlineGanttEditor();
                } else {
                    openInlineTableEditor();
                }
                return false;
            });
            $ganttInlineEditor.on('input', 'input', scheduleGanttAutoApply);
            $ganttInlineEditor.on('focus', 'input', function () {
                ganttCursor = {
                    row: parseInt($(this).attr('data-r'), 10) || 0,
                    col: parseInt($(this).attr('data-c'), 10) || 0
                };
                focusGanttCell(ganttCursor.row, ganttCursor.col, false);
            });
            $ganttInlineEditor.on('keydown', 'input', function (e) {
                const row = parseInt($(this).attr('data-r'), 10) || 0;
                const col = parseInt($(this).attr('data-c'), 10) || 0;
                if (e.key === 'Tab' && col === GANTT_COL_NAME) {
                    e.preventDefault();
                    const rows = ganttGridAsRows();
                    rows[row].depth = e.shiftKey ? Math.max(0, rows[row].depth - 1) : rows[row].depth + 1;
                    rerenderGanttWithCursor(rows, row, col, true);
                } else if (e.key === 'Enter') {
                    e.preventDefault();
                    const rowCount = $ganttInlineEditorGrid.find('tr').length;
                    if (row >= rowCount - 1) {
                        const rows = ganttGridAsRows();
                        const currentDepth = rows[row] ? rows[row].depth : 0;
                        rows.push({ depth: currentDepth, name: '', startOrRef: '', est: '' });
                        rerenderGanttWithCursor(rows, row + 1, col, true);
                    } else {
                        focusGanttCell(row + 1, col, true);
                    }
                } else if (e.key === 'ArrowUp') { e.preventDefault(); focusGanttCell(row - 1, col, true); }
                else if (e.key === 'ArrowDown') { e.preventDefault(); focusGanttCell(row + 1, col, true); }
                else if (e.key === 'ArrowLeft' && this.selectionStart === 0 && this.selectionEnd === 0) { e.preventDefault(); focusGanttCell(row, col - 1, true); }
                else if (e.key === 'ArrowRight' && this.selectionStart === this.value.length && this.selectionEnd === this.value.length) { e.preventDefault(); focusGanttCell(row, col + 1, true); }
            });
            $('#ganttAddRow').on('click', function () {
                const rows = ganttGridAsRows();
                const currentDepth = rows[ganttCursor.row] ? rows[ganttCursor.row].depth : 0;
                rows.splice(ganttCursor.row + 1, 0, { depth: currentDepth, name: '', startOrRef: '', est: '' });
                rerenderGanttWithCursor(rows, ganttCursor.row + 1, ganttCursor.col, true);
                return false;
            });
            $('#ganttDeleteRow').on('click', function () {
                const rows = ganttGridAsRows();
                if (rows.length <= 1) return false;
                rows.splice(ganttCursor.row, 1);
                rerenderGanttWithCursor(rows, Math.max(0, ganttCursor.row - 1), ganttCursor.col, true);
                return false;
            });
            $('#ganttIndent').on('click', function () {
                const rows = ganttGridAsRows();
                rows[ganttCursor.row].depth += 1;
                rerenderGanttWithCursor(rows, ganttCursor.row, ganttCursor.col, true);
                return false;
            });
            $('#ganttOutdent').on('click', function () {
                const rows = ganttGridAsRows();
                rows[ganttCursor.row].depth = Math.max(0, rows[ganttCursor.row].depth - 1);
                rerenderGanttWithCursor(rows, ganttCursor.row, ganttCursor.col, true);
                return false;
            });
            $('#ganttToggleRaw').on('click', function () {
                closeInlineGanttEditor();
                return false;
            });
            $('#openGanttEditor').on('click', function () {
                $('#ganttEditorBar').hide();
                openInlineGanttEditor();
                return false;
            });
            $tableInlineEditor.on('input', 'input', scheduleInlineAutoApply);
            $tableInlineEditor.on('focus', 'input', function () {
                tableInlineCursor = {
                    row: parseInt($(this).attr('data-r'), 10) || 0,
                    col: parseInt($(this).attr('data-c'), 10) || 0
                };
                focusTableCell(tableInlineCursor.row, tableInlineCursor.col, false);
            });
            $tableInlineEditor.on('keydown', 'input', function (e) {
                const row = parseInt($(this).attr('data-r'), 10) || 0;
                const col = parseInt($(this).attr('data-c'), 10) || 0;
                if (e.key === 'Enter') {
                    e.preventDefault();
                    const size = getGridSize();
                    if (row >= size.rowCount - 1) {
                        const rows = gridAsMatrix();
                        const newRow = Array.from({ length: size.colCount }, () => '');
                        rows.push(newRow);
                        rerenderWithCursor(rows, row + 1, col, true);
                    } else {
                        focusTableCell(row + 1, col, true);
                    }
                }
                else if (e.key === 'ArrowUp') { e.preventDefault(); focusTableCell(row - 1, col, true); }
                else if (e.key === 'ArrowDown') { e.preventDefault(); focusTableCell(row + 1, col, true); }
                else if (e.key === 'ArrowLeft' && this.selectionStart === 0 && this.selectionEnd === 0) { e.preventDefault(); focusTableCell(row, col - 1, true); }
                else if (e.key === 'ArrowRight' && this.selectionStart === this.value.length && this.selectionEnd === this.value.length) { e.preventDefault(); focusTableCell(row, col + 1, true); }
            });
            $('#tableAddRow').on('click', function () {
                const size = getGridSize();
                const rows = gridAsMatrix();
                const newRow = Array.from({ length: size.colCount }, () => '');
                rows.splice(tableInlineCursor.row + 1, 0, newRow);
                rerenderWithCursor(rows, tableInlineCursor.row + 1, tableInlineCursor.col, true);
                return false;
            });
            $('#tableDeleteRow').on('click', function () {
                const rows = gridAsMatrix();
                if (rows.length <= 1) return false;
                rows.splice(tableInlineCursor.row, 1);
                rerenderWithCursor(rows, Math.max(0, tableInlineCursor.row - 1), tableInlineCursor.col, true);
                return false;
            });
            $('#tableAddCol').on('click', function () {
                const rows = gridAsMatrix();
                rows.forEach(r => r.splice(tableInlineCursor.col + 1, 0, ''));
                rerenderWithCursor(rows, tableInlineCursor.row, tableInlineCursor.col + 1, true);
                return false;
            });
            $('#tableDeleteCol').on('click', function () {
                const rows = gridAsMatrix();
                const colCount = rows[0] ? rows[0].length : 1;
                if (colCount <= 1) return false;
                rows.forEach(r => r.splice(tableInlineCursor.col, 1));
                rerenderWithCursor(rows, tableInlineCursor.row, Math.max(0, tableInlineCursor.col - 1), true);
                return false;
            });
            let globalDragCounter = 0;
            let serverAttachments = [];
            const uploadingMap = new Map();
            let macroNames = [];
            let interpreterNames = [];
            let schemaClassNames = [];
            const schemaPropertyNamesByClass = new Map();
            const schemaPropertyLoadingByClass = new Set();
            let pageNames = [];
            const tableOptionNames = ['tsv', 'csv', 'tsv 1', 'tsv 1 1', 'tsv 1 1 tablesorter'];
            let macroSuggestions = [];
            let activeMacroSuggestionIndex = 0;
            let macroAutocompleteContext = null;
            let autocompleteSuggestionType = null;

            function getMacroPrefix(value, cursorIndex) {
                const left = value.slice(0, cursorIndex);
                const match = left.match(/\[\[([A-Za-z0-9_]*)$/);
                if (!match)
                    return null;
                return {
                    start: cursorIndex - match[1].length,
                    prefix: match[1]
                };
            }

            function getPagePrefix(value, cursorIndex) {
                const left = value.slice(0, cursorIndex);
                const quotedMatch = left.match(/(^|[^\[])\["([^"\n]*)$/);
                if (quotedMatch)
                    return {
                        start: cursorIndex - quotedMatch[2].length,
                        prefix: quotedMatch[2]
                    };

                const match = left.match(/(^|[^\[])\[([^\[\]\n]*)$/);
                if (!match)
                    return null;
                return {
                    start: cursorIndex - match[2].length,
                    prefix: match[2]
                };
            }

            function getInterpreterPrefix(value, cursorIndex) {
                const left = value.slice(0, cursorIndex);
                const match = left.match(/\[\[\[#!([A-Za-z0-9_]*)$/);
                if (!match)
                    return null;
                return {
                    start: cursorIndex - match[1].length,
                    prefix: match[1]
                };
            }

            function getTableOptionPrefix(value, cursorIndex) {
                const left = value.slice(0, cursorIndex);
                const match = left.match(/\[\[\[#!table(?:\s+([^\]\n]*))?$/i);
                if (!match)
                    return null;
                const optionInput = match[1] || '';
                const tokenMatch = optionInput.match(/(?:^|\s)([^\s]*)$/);
                const prefix = tokenMatch ? tokenMatch[1] : '';
                return {
                    start: cursorIndex - prefix.length,
                    prefix: prefix
                };
            }

            function getSchemaBlockStart(left) {
                const openIndex = left.lastIndexOf('[[[#!');
                if (openIndex < 0)
                    return null;
                const closeIndex = left.lastIndexOf(']]]');
                if (closeIndex > openIndex)
                    return null;
                return openIndex;
            }

            function getSchemaClassPrefix(value, cursorIndex) {
                const left = value.slice(0, cursorIndex);
                const schemaBlockStart = getSchemaBlockStart(left);
                if (schemaBlockStart == null)
                    return null;
                const shebangLineStart = schemaBlockStart;
                const lineStart = left.lastIndexOf('\n') + 1;
                if (lineStart !== shebangLineStart)
                    return null;
                const shebangText = left.slice(shebangLineStart);
                const match = shebangText.match(/^\[\[\[#!\s*Schema(?:\s+([A-Za-z0-9_]*))?$/i);
                if (!match)
                    return null;
                const prefix = match[1] || '';
                return {
                    start: cursorIndex - prefix.length,
                    prefix: prefix
                };
            }

            function getSchemaPropertyPrefix(value, cursorIndex) {
                const left = value.slice(0, cursorIndex);
                const schemaBlockStart = getSchemaBlockStart(left);
                if (schemaBlockStart == null)
                    return null;

                const shebangLineEnd = value.indexOf('\n', schemaBlockStart);
                if (shebangLineEnd < 0 || cursorIndex <= shebangLineEnd)
                    return null;
                const schemaShebangLine = value.slice(schemaBlockStart, shebangLineEnd);
                const schemaClassMatch = schemaShebangLine.match(/^\[\[\[#!\s*Schema(?:\s+([A-Za-z0-9_]+))?$/i);
                if (!schemaClassMatch)
                    return null;
                const schemaClass = schemaClassMatch[1] || '';

                const lineStart = left.lastIndexOf('\n') + 1;
                const lineLeft = left.slice(lineStart);
                if (lineLeft.includes('\t') || /\s{2,}/.test(lineLeft))
                    return null;
                if (/^\s*#/.test(lineLeft))
                    return null;
                const match = lineLeft.match(/^\s*([A-Za-z0-9_]*)$/);
                if (!match)
                    return null;

                const prefix = match[1] || '';
                return {
                    start: cursorIndex - prefix.length,
                    prefix: prefix,
                    schemaClass: schemaClass
                };
            }

            function schemaPropertyCacheKey(schemaClass) {
                return schemaClass || '__ALL__';
            }

            function ensureSchemaPropertyNames(schemaClass) {
                const cacheKey = schemaPropertyCacheKey(schemaClass);
                if (schemaPropertyNamesByClass.has(cacheKey) || schemaPropertyLoadingByClass.has(cacheKey))
                    return;

                schemaPropertyLoadingByClass.add(cacheKey);
                const query = schemaClass
                    ? { schemaClass: schemaClass, source: 'class-or-recommended' }
                    : {};
                $.get(AhaWikiEditConfig.api.schemaPropertyNames, query)
                    .done(function (data) {
                        const names = Array.isArray(data) ? data : [];
                        schemaPropertyNamesByClass.set(cacheKey, names);
                        const cm = getEditor();
                        if (cm) {
                            updateMacroAutocompleteForCodeMirror(cm);
                        } else {
                            updateMacroAutocompleteForTextarea();
                        }
                    })
                    .fail(function () {
                        schemaPropertyNamesByClass.set(cacheKey, []);
                    })
                    .always(function () {
                        schemaPropertyLoadingByClass.delete(cacheKey);
                    });
            }

            function hideMacroAutocomplete() {
                $macroAutocomplete.hide().empty();
                macroSuggestions = [];
                activeMacroSuggestionIndex = 0;
                macroAutocompleteContext = null;
                autocompleteSuggestionType = null;
            }

            function renderMacroAutocomplete() {
                $macroAutocomplete.empty();
                macroSuggestions.forEach(function (name, index) {
                    const $item = $('<div class="macroAutocompleteItem"></div>')
                        .text(name)
                        .toggleClass('active', index === activeMacroSuggestionIndex)
                        .on('mousedown', function (e) {
                            e.preventDefault();
                            applyMacroAutocompleteSuggestion(index);
                        });
                    $macroAutocomplete.append($item);
                });
                const $activeItem = $macroAutocomplete.find('.macroAutocompleteItem.active').first();
                if ($activeItem.length) {
                    $activeItem.get(0).scrollIntoView({ block: 'nearest' });
                }
            }

            function applyMacroAutocompleteSuggestion(index) {
                if (!macroAutocompleteContext || !macroSuggestions.length)
                    return;
                const selected = macroSuggestions[index];
                if (!selected)
                    return;

                const selectionStart = macroAutocompleteContext.start;
                const selectionEnd = macroAutocompleteContext.cursor;
                const isMacroAutocomplete = autocompleteSuggestionType === 'macro';
                const insertText = isMacroAutocomplete ? (selected + '()') : selected;
                const cursorOffset = isMacroAutocomplete ? (selected.length + 1) : selected.length;
                if (macroAutocompleteContext.type === 'codemirror') {
                    const cm = getEditor();
                    if (!cm) {
                        hideMacroAutocomplete();
                        return;
                    }
                    const from = cm.posFromIndex(selectionStart);
                    const to = cm.posFromIndex(selectionEnd);
                    cm.replaceRange(insertText, from, to);
                    const cursor = cm.posFromIndex(selectionStart + cursorOffset);
                    cm.setCursor(cursor);
                    cm.focus();
                    cm.save();
                    $textarea.trigger('input');
                } else {
                    const element = $textarea.get(0);
                    if (!element) {
                        hideMacroAutocomplete();
                        return;
                    }
                    const value = element.value;
                    element.value = value.substring(0, selectionStart) + insertText + value.substring(selectionEnd);
                    element.selectionStart = selectionStart + cursorOffset;
                    element.selectionEnd = selectionStart + cursorOffset;
                    $(element).focus().trigger('input');
                }

                hideMacroAutocomplete();
            }

            function moveMacroSuggestion(delta) {
                if (!macroSuggestions.length)
                    return;
                const len = macroSuggestions.length;
                activeMacroSuggestionIndex = (activeMacroSuggestionIndex + delta + len) % len;
                renderMacroAutocomplete();
            }

            function hasExactAutocompleteMatch(prefix, list) {
                if (!prefix)
                    return false;
                const prefixLower = prefix.toLowerCase();
                return list.some(function (name) {
                    return name.toLowerCase() === prefixLower;
                });
            }

            function shouldSkipAutocompleteApply(currentValue, index) {
                if (!macroAutocompleteContext || !macroSuggestions.length)
                    return true;
                const selected = macroSuggestions[index];
                if (!selected)
                    return true;
                const currentToken = currentValue.slice(macroAutocompleteContext.start, macroAutocompleteContext.cursor);
                return currentToken.toLowerCase() === selected.toLowerCase();
            }

            function showMacroAutocomplete(context) {
                const pagePrefixInfo = getPagePrefix(context.value, context.cursor);
                const macroPrefixInfo = getMacroPrefix(context.value, context.cursor);
                const interpreterPrefixInfo = getInterpreterPrefix(context.value, context.cursor);
                const tableOptionPrefixInfo = getTableOptionPrefix(context.value, context.cursor);
                const schemaClassPrefixInfo = getSchemaClassPrefix(context.value, context.cursor);
                const schemaPropertyPrefixInfo = getSchemaPropertyPrefix(context.value, context.cursor);
                const autocompleteType = interpreterPrefixInfo
                    ? 'interpreter'
                    : tableOptionPrefixInfo
                        ? 'table-option'
                    : schemaClassPrefixInfo
                        ? 'schema-class'
                    : schemaPropertyPrefixInfo
                        ? 'schema-property'
                    : macroPrefixInfo
                        ? 'macro'
                        : pagePrefixInfo
                            ? 'page'
                            : null;
                const prefixInfo = interpreterPrefixInfo || tableOptionPrefixInfo || schemaClassPrefixInfo || schemaPropertyPrefixInfo || macroPrefixInfo || pagePrefixInfo;
                if (autocompleteType === 'schema-property')
                    ensureSchemaPropertyNames((prefixInfo && prefixInfo.schemaClass) || '');
                const sourceNames = autocompleteType === 'interpreter'
                    ? interpreterNames
                    : autocompleteType === 'table-option'
                        ? tableOptionNames
                    : autocompleteType === 'schema-class'
                        ? schemaClassNames
                    : autocompleteType === 'schema-property'
                        ? (schemaPropertyNamesByClass.get(schemaPropertyCacheKey((prefixInfo && prefixInfo.schemaClass) || '')) || [])
                    : autocompleteType === 'macro'
                        ? macroNames
                        : autocompleteType === 'page'
                            ? pageNames
                            : [];

                if (!prefixInfo || !sourceNames.length) {
                    hideMacroAutocomplete();
                    return;
                }

                const prefixLower = prefixInfo.prefix.toLowerCase();
                const list = sourceNames.filter(function (name) {
                    return !prefixLower || name.toLowerCase().indexOf(prefixLower) === 0;
                }).slice(0, 30);

                if (!list.length) {
                    hideMacroAutocomplete();
                    return;
                }

                if (hasExactAutocompleteMatch(prefixInfo.prefix, list)) {
                    hideMacroAutocomplete();
                    return;
                }

                macroSuggestions = list;
                activeMacroSuggestionIndex = 0;
                macroAutocompleteContext = {
                    type: context.type,
                    start: prefixInfo.start,
                    cursor: context.cursor
                };
                autocompleteSuggestionType = autocompleteType;
                renderMacroAutocomplete();
                const left = context.rect.left + window.scrollX;
                const top = context.rect.bottom + window.scrollY + 4;
                $macroAutocomplete.css({ left: left + 'px', top: top + 'px' }).show();
            }

            function textareaCursorRect(element) {
                const rect = element.getBoundingClientRect();
                return { left: rect.left + 12, bottom: rect.top + 28 };
            }

            function updateMacroAutocompleteForTextarea() {
                if (getEditor()) {
                    hideMacroAutocomplete();
                    return;
                }
                const element = $textarea.get(0);
                if (!element)
                    return;
                showMacroAutocomplete({
                    type: 'textarea',
                    value: element.value,
                    cursor: element.selectionStart,
                    rect: textareaCursorRect(element)
                });
            }

            function updateMacroAutocompleteForCodeMirror(cm) {
                const cursor = cm.getDoc().getCursor();
                const cursorCoords = cm.cursorCoords(cursor, 'window');
                showMacroAutocomplete({
                    type: 'codemirror',
                    value: cm.getValue(),
                    cursor: cm.indexFromPos(cursor),
                    rect: { left: cursorCoords.left, bottom: cursorCoords.bottom }
                });
            }

            function loadMacroNames() {
                $.get(AhaWikiEditConfig.api.macroNames)
                    .done(function (data) {
                        macroNames = Array.isArray(data) ? data : [];
                    })
                    .fail(function () {
                        macroNames = [];
                    });
            }

            function loadInterpreterNames() {
                $.get(AhaWikiEditConfig.api.interpreterNames)
                    .done(function (data) {
                        interpreterNames = Array.isArray(data) ? data : [];
                    })
                    .fail(function () {
                        interpreterNames = [];
                    });
            }

            function loadSchemaClassNames() {
                $.get(AhaWikiEditConfig.api.schemaClassNames)
                    .done(function (data) {
                        schemaClassNames = Array.isArray(data) ? data : [];
                    })
                    .fail(function () {
                        schemaClassNames = [];
                    });
            }

            function loadSchemaPropertyNames() {
                ensureSchemaPropertyNames('');
            }

            function loadPageNames() {
                $.get(AhaWikiEditConfig.api.pageNames)
                    .done(function (data) {
                        pageNames = Array.isArray(data) ? data : [];
                    })
                    .fail(function () {
                        pageNames = [];
                    });
            }

            function getEditor() {
                return editor || window.AhaWikiCodeMirrorEditor;
            }

            function syncEditorToTextarea() {
                const cm = getEditor();
                if (cm)
                    cm.save();
            }

            function insertTextAtCursor(text) {
                const cm = getEditor();
                if (cm) {
                    cm.replaceSelection(text, 'end');
                    cm.focus();
                    cm.save();
                    $textarea.trigger('change');
                    return;
                }
                $textarea.insertAtCaret(text);
                $textarea.change();
            }

            function hasFiles(event) {
                const dataTransfer = event.originalEvent && event.originalEvent.dataTransfer;
                const types = dataTransfer && dataTransfer.types;
                return !!(types && Array.from(types).indexOf('Files') !== -1);
            }

            function formatFileSize(bytes) {
                if (bytes < 1024)
                    return bytes + ' B';
                if (bytes < 1024 * 1024)
                    return (bytes / 1024).toFixed(1) + ' KB';
                return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
            }

            function filenameFromObjectKey(objectKey) {
                if (!objectKey)
                    return '';
                const segments = objectKey.split('/');
                return segments[segments.length - 1] || objectKey;
            }

            function renderThumbnail(attachment) {
                const contentType = attachment.contentType || '';
                const fileUrl = attachment.fileUrl || '';
                if (contentType.indexOf('image/') === 0 && fileUrl) {
                    return $('<img/>', {
                        class: 'attachmentThumb',
                        src: fileUrl,
                        alt: attachment.originalFilename || attachment.objectKey
                    });
                }
                return $('<i/>', { class: 'far fa-file' });
            }

            function renderAttachmentList() {
                $attachmentList.empty();
                if (!uploadingMap.size && (!serverAttachments || !serverAttachments.length)) {
                    return;
                }

                Array.from(uploadingMap.values()).forEach(function (uploading) {
                    const $item = $('<div/>', { class: 'attachmentItem' });
                    const $body = $('<div/>', { class: 'attachmentItemBody' });
                    const $thumbWrap = $('<div/>', { class: 'attachmentThumbWrap' }).append($('<i/>', { class: 'fas fa-upload' }));
                    const $content = $('<div/>', { class: 'attachmentContent' }).css('flex', '1');
                    const $meta = $('<div/>', { class: 'attachmentItemMeta' });
                    const $name = $('<div/>', { class: 'attachmentName' }).text(uploading.originalFilename);
                    const stateClass = uploading.failed ? 'attachmentMeta failed' : 'attachmentMeta uploading';
                    const stateText = uploading.failed ? '업로드 실패' : ('업로드중 ' + uploading.progress + '%');
                    const $subMeta = $('<div/>', { class: stateClass }).text(stateText);
                    const $track = $('<div/>', { class: 'uploadProgressTrack' });
                    const $bar = $('<div/>', { class: 'uploadProgressBar' }).css('width', (uploading.progress || 0) + '%');

                    $track.append($bar);
                    $meta.append($name);
                    $content.append($meta, $subMeta, $track);
                    $body.append($thumbWrap, $content);
                    $item.append($body);
                    $attachmentList.append($item);
                });

                serverAttachments.forEach(function (attachment) {
                    const $item = $('<div/>', { class: 'attachmentItem' });
                    const $body = $('<div/>', { class: 'attachmentItemBody' });
                    const $thumbWrap = $('<div/>', { class: 'attachmentThumbWrap' }).append(renderThumbnail(attachment));
                    const $content = $('<div/>', { class: 'attachmentContent' }).css('flex', '1');
                    const $meta = $('<div/>', { class: 'attachmentItemMeta' });
                    const displayName = filenameFromObjectKey(attachment.objectKey) || attachment.originalFilename || attachment.objectKey;
                    const $name = $('<div/>', { class: 'attachmentName' }).text(displayName);
                    const $actions = $('<div/>', { class: 'attachmentActions' });
                    const $insert = $('<button/>', { type: 'button', class: 'attachmentButton insert', title: '삽입' })
                        .html('<i class="fas fa-file-import"></i>');
                    const $delete = $('<button/>', { type: 'button', class: 'attachmentButton delete', title: '삭제' })
                        .html('<i class="fas fa-trash-alt"></i>');
                    const integrityStatus = attachment.integrityStatus || 'UNKNOWN';
                    const integrityLabel = integrityStatus === 'OK' ? '' : (integrityStatus === 'S3_ONLY' ? ' · S3_ONLY (DB 레코드 없음)' : ' · DB_ONLY (S3 확인 필요)');
                    const metaClass = integrityStatus === 'OK' ? 'attachmentMeta' : 'attachmentMeta failed';
                    const $subMeta = $('<div/>', { class: metaClass }).text((attachment.contentType || '') + ' · ' + Number(attachment.fileSize || 0).toLocaleString() + ' bytes' + integrityLabel);

                    $insert.on('click', function () {
                        if (!attachment.attachmentMacro)
                            return;
                        insertTextAtCursor(attachment.attachmentMacro + '\n');
                    });

                    $delete.on('click', function () {
                        if (!confirm('첨부파일을 삭제하시겠습니까?'))
                            return;
                        $.post('/api/deleteAttachment', {
                            csrfToken: $('[name=csrfToken]').val(),
                            pageName: AhaWikiEditConfig.pageName,
                            objectKey: attachment.objectKey
                        }).done(function () {
                            loadAttachments();
                        }).fail(function (jqXHR) {
                            alert('Attachment delete failed. ' + (jqXHR.responseText || jqXHR.statusText));
                        });
                    });

                    if (integrityStatus !== 'S3_ONLY') {
                        $actions.append($insert, $delete);
                    }
                    $meta.append($name, $actions);
                    $content.append($meta, $subMeta);
                    $body.append($thumbWrap, $content);
                    $item.append($body);
                    $attachmentList.append($item);
                });
            }

            function loadAttachments() {
                $.get('/api/pageAttachments', { pageName: AhaWikiEditConfig.pageName })
                    .done(function (data) {
                        serverAttachments = data && data.attachments ? data.attachments : [];
                        renderAttachmentList();
                    })
                    .fail(function (jqXHR) {
                        alert('Attachment list load failed. ' + (jqXHR.responseText || jqXHR.statusText));
                    });
            }

            function uploadFile(file) {
                const uploadId = Date.now() + '-' + Math.random().toString(36).slice(2);
                uploadingMap.set(uploadId, {
                    originalFilename: file.name,
                    progress: 0,
                    failed: false
                });
                renderAttachmentList();

                const formData = new FormData();
                formData.append('csrfToken', $('[name=csrfToken]').val());
                formData.append('pageName', AhaWikiEditConfig.pageName);
                formData.append('file', file, file.name);

                return $.ajax({
                    url: '/api/uploadAttachment',
                    method: 'POST',
                    data: formData,
                    processData: false,
                    contentType: false,
                    xhr: function () {
                        const xhr = $.ajaxSettings.xhr();
                        if (xhr.upload) {
                            xhr.upload.addEventListener('progress', function (e) {
                                if (!e.lengthComputable)
                                    return;
                                const percent = Math.max(0, Math.min(100, Math.floor((e.loaded / e.total) * 100)));
                                const uploading = uploadingMap.get(uploadId);
                                if (!uploading)
                                    return;
                                uploading.progress = percent;
                                uploadingMap.set(uploadId, uploading);
                                renderAttachmentList();
                            });
                        }
                        return xhr;
                    }
                }).done(function (data) {
                    if (data && data.attachmentMacro) {
                        insertTextAtCursor(data.attachmentMacro + '\n');
                    } else {
                        alert('File upload failed.');
                    }
                    uploadingMap.delete(uploadId);
                    loadAttachments();
                }).fail(function (jqXHR) {
                    const uploading = uploadingMap.get(uploadId);
                    if (uploading) {
                        uploading.progress = 100;
                        uploading.failed = true;
                        uploadingMap.set(uploadId, uploading);
                        renderAttachmentList();
                    }
                    alert('File upload failed. ' + (jqXHR.responseText || jqXHR.statusText));
                });
            }

            function uploadFiles(fileList) {
                const files = Array.from(fileList || []);
                if (!files.length)
                    return;
                files.forEach(uploadFile);
            }

            $('.openAttachmentUpload').on('click', function (e) {
                e.preventDefault();
                $uploadInput.trigger('click');
            });
            $uploadInput.on('change', function () {
                uploadFiles(this.files);
                $(this).val('');
            });
            loadAttachments();
            loadMacroNames();
            loadInterpreterNames();
            loadSchemaClassNames();
            loadSchemaPropertyNames();
            loadPageNames();

            $(document).on('dragenter', function (e) {
                if (!hasFiles(e))
                    return;
                e.preventDefault();
                globalDragCounter += 1;
                $uploadDragOverlay.addClass('visible');
            });
            $(document).on('dragover', function (e) {
                if (!hasFiles(e))
                    return;
                e.preventDefault();
            });
            $(document).on('dragleave', function (e) {
                if (!hasFiles(e))
                    return;
                e.preventDefault();
                globalDragCounter = Math.max(0, globalDragCounter - 1);
                if (globalDragCounter === 0)
                    $uploadDragOverlay.removeClass('visible');
            });
            $(document).on('drop', function (e) {
                if (!hasFiles(e))
                    return;
                e.preventDefault();
                uploadFiles(e.originalEvent.dataTransfer.files);
                globalDragCounter = 0;
                $uploadDragOverlay.removeClass('visible');
            });

            $("html").pasteImageReader(function(results) {
                $.post('/api/uploadClipboardImage', {
                    csrfToken: $('[name=csrfToken]').val(),
                    pageName: AhaWikiEditConfig.pageName,
                    dataUrl: results.dataURL
                }, function (data) {
                    if (data && data.attachmentMacro) {
                        insertTextAtCursor(data.attachmentMacro + '\n');
                        loadAttachments();
                    } else {
                        alert('Image upload failed.');
                    }
                }).fail(function (jqXHR) {
                    alert('Image upload failed. ' + (jqXHR.responseText || jqXHR.statusText));
                });
            });




            var $form = $(".form");
            $form.validate({
                rules: { comment: "required" },
                errorPlacement: function () { }
            });
            $form.ajaxForm({
                success: function(result) {
                    location.href = location.pathname;
                },
                error: function (jqXHR) {
                    if(jqXHR.status === 403) {
                        alert(`Permission denied - ${jqXHR.responseText}`);
                        return;
                    }
                    if(jqXHR.status === 409) {
                        alert(`Conflict - ${jqXHR.responseText}`);
                        return;
                    }
                    if(jqXHR.status === 400 && jqXHR.responseText === "body == latestText") {
                        alert("You are trying to save the same content");
                        return;
                    }

                    alert(jqXHR.statusText);
                }
            });

            AhaWiki.Editor.addEventListener('textarea[name=text]');

            if (window.CodeMirror && window.AhaWikiCodeMirrorAhaMarkMode) {
                const modeName = window.AhaWikiCodeMirrorAhaMarkMode();
                editor = CodeMirror.fromTextArea($textarea.get(0), {
                    mode: modeName,
                    lineNumbers: true,
                    lineWrapping: false,
                    indentUnit: 4,
                    tabSize: 4
                });
                window.AhaWikiCodeMirrorEditor = editor;
                editor.on('change', function (cm) {
                    cm.save();
                    $textarea.trigger('input');
                    updateMacroAutocompleteForCodeMirror(cm);
                });
                AhaWiki.Editor.addCodeMirrorEventListener(editor);
                editor.on('keydown', function (cm, e) {
                    if (!$macroAutocomplete.is(':visible'))
                        return;
                    if (e.key === 'ArrowDown') {
                        e.preventDefault();
                        moveMacroSuggestion(1);
                        return;
                    }
                    if (e.key === 'ArrowUp') {
                        e.preventDefault();
                        moveMacroSuggestion(-1);
                        return;
                    }
                    if (e.key === 'Enter' || e.key === 'Tab') {
                        if (shouldSkipAutocompleteApply(cm.getValue(), activeMacroSuggestionIndex)) {
                            hideMacroAutocomplete();
                            return;
                        }
                        e.preventDefault();
                        applyMacroAutocompleteSuggestion(activeMacroSuggestionIndex);
                        return;
                    }
                    if (e.key === 'Escape') {
                        e.preventDefault();
                        hideMacroAutocomplete();
                    }
                });
                editor.on('blur', function () {
                    setTimeout(hideMacroAutocomplete, 120);
                });
                editor.on('cursorActivity', function () {
                    scrollPreviewToCurrentLine();
                });
            }

            $textarea.on('input click keyup', function () {
                updateMacroAutocompleteForTextarea();
            });
            $textarea.on('click keyup', function () {
                scrollPreviewToCurrentLine();
            });
            $textarea.on('keydown', function (e) {
                if (!$macroAutocomplete.is(':visible'))
                    return;
                if (e.key === 'ArrowDown') {
                    e.preventDefault();
                    moveMacroSuggestion(1);
                    return;
                }
                if (e.key === 'ArrowUp') {
                    e.preventDefault();
                    moveMacroSuggestion(-1);
                    return;
                }
                if (e.key === 'Enter' || e.key === 'Tab') {
                    const element = $textarea.get(0);
                    const currentValue = element ? element.value : '';
                    if (shouldSkipAutocompleteApply(currentValue, activeMacroSuggestionIndex)) {
                        hideMacroAutocomplete();
                        return;
                    }
                    e.preventDefault();
                    applyMacroAutocompleteSuggestion(activeMacroSuggestionIndex);
                    return;
                }
                if (e.key === 'Escape') {
                    e.preventDefault();
                    hideMacroAutocomplete();
                }
            });
            $textarea.on('blur', function () {
                setTimeout(hideMacroAutocomplete, 120);
            });
            $(window).on('resize', hideMacroAutocomplete);
            $(document).on('scroll click', function (e) {
                if (!$(e.target).closest('#macroAutocomplete').length && !$(e.target).closest('.CodeMirror, textarea[name=text]').length) {
                    hideMacroAutocomplete();
                }
            });

            function clamp(min, value, max) {
                return Math.max(min, Math.min(value, max));
            }

            function getPartialEditLineOffset() {
                const lineStartVal = $('input[name=lineStart]').val();
                const lineStart = lineStartVal ? parseInt(lineStartVal, 10) : NaN;
                return (Number.isFinite(lineStart) && lineStart > 1) ? lineStart - 1 : 0;
            }

            function getCurrentEditorLine() {
                const offset = getPartialEditLineOffset();
                const cm = getEditor();
                if (cm) {
                    const cursor = cm.getCursor();
                    return (cursor ? cursor.line : 0) + 1 + offset;
                }
                const textarea = $textarea.get(0);
                if (!textarea)
                    return 1 + offset;
                const position = textarea.selectionStart || 0;
                const textUntilCursor = textarea.value.slice(0, position);
                return textUntilCursor.split('\n').length + offset;
            }

            function scrollPreviewToCurrentLine() {
                const line = getCurrentEditorLine();
                const previewPane = document.querySelector('.previewPane');
                if (!previewPane)
                    return;
                const $previewPane = $(previewPane);
                const lineTaggedNodes = Array.from(previewPane.querySelectorAll('[data-line-start]'));
                if (!lineTaggedNodes.length)
                    return;

                const candidates = lineTaggedNodes
                    .map((node) => {
                        const lineStart = parseInt(node.getAttribute('data-line-start') || '0', 10);
                        const rawLineEnd = parseInt(node.getAttribute('data-line-end') || '0', 10);
                        if (!Number.isFinite(lineStart))
                            return null;
                        const lineEnd = Number.isFinite(rawLineEnd) && rawLineEnd > lineStart ? rawLineEnd : lineStart + 1;
                        return {
                            node,
                            lineStart,
                            lineEnd,
                            span: Math.max(1, lineEnd - lineStart),
                            isRangeWrapper: node.classList.contains('InterpreterRenderMetaWrapper')
                        };
                    })
                    .filter(Boolean);

                const containing = candidates
                    .filter((item) => item.lineStart <= line && line < item.lineEnd)
                    .sort((a, b) => (a.span - b.span) || (b.isRangeWrapper - a.isRangeWrapper))[0];
                const nearest = candidates.sort((a, b) => Math.abs(a.lineStart - line) - Math.abs(b.lineStart - line))[0];
                const target = (containing || nearest || {}).node;
                if (!target)
                    return;

                const targetRect = target.getBoundingClientRect();
                const paneRect = previewPane.getBoundingClientRect();
                const targetTop = targetRect.top - paneRect.top + previewPane.scrollTop;
                const targetHeight = targetRect.height || 0;
                const paneHeight = previewPane.clientHeight || 0;
                const nextScrollTop = Math.max(0, targetTop - ((paneHeight - targetHeight) / 2));

                $previewPane.stop(true).animate({ scrollTop: nextScrollTop }, 260, 'swing');
                target.classList.remove('previewSyncBlink');
                void target.offsetWidth;
                target.classList.add('previewSyncBlink');
            }

            window.AhaWikiSyncPreviewScrollNow = scrollPreviewToCurrentLine;

            $textarea.bind('input cut paste keydown keyup keypress blur change', function () {
                if(timer) {
                    clearTimeout(timer);
                    timer = null;
                }

                timer = setTimeout(preview, clamp(200, this.value.length, 1000))
            });

            $('.checkboxWrap').off('change').on('change', function () {
                const cm = getEditor();
                if (cm) {
                    cm.setOption('lineWrapping', $(this).prop('checked'));
                    return;
                }
                $textarea.css('white-space', $(this).prop('checked') ? 'inherit' : 'pre');
            }).trigger('change');

            function getOrCreateEditorSenderId() {
                var cookieName = 'ahaWikiEditorSenderId';
                var tabStorageKey = 'ahaWikiEditorTabSenderId';
                var cookieMaxAgeSeconds = 365 * 24 * 60 * 60;

                function readCookie(name) {
                    var escaped = String(name || '').replace(/[-/\\^$*+?.()|[\]{}]/g, '\\$&');
                    var match = document.cookie.match(new RegExp('(?:^|; )' + escaped + '=([^;]*)'));
                    return match ? decodeURIComponent(match[1]) : '';
                }

                function writeCookie(name, value, maxAgeSeconds) {
                    document.cookie = name + '=' + encodeURIComponent(value) + '; Path=/; Max-Age=' + String(maxAgeSeconds) + '; SameSite=Lax';
                }

                function createSenderId() {
                    return (window.crypto && window.crypto.randomUUID)
                        ? window.crypto.randomUUID()
                        : ('sender-' + Date.now() + '-' + Math.random().toString(36).slice(2));
                }

                try {
                    var browserId = readCookie(cookieName);
                    if (!browserId) {
                        browserId = createSenderId();
                    }
                    writeCookie(cookieName, browserId, cookieMaxAgeSeconds);

                    var tabId = window.sessionStorage.getItem(tabStorageKey);
                    if (!tabId) {
                        tabId = createSenderId();
                        window.sessionStorage.setItem(tabStorageKey, tabId);
                    }
                    return browserId + ':' + tabId;
                } catch (e) {
                    return createSenderId() + ':' + createSenderId();
                }
            }

            $('[name=saveSenderId]').val(getOrCreateEditorSenderId());

            $('.form').on('submit', function () {
                syncEditorToTextarea();
            });
            preview();
            const initialSrc = window.AhaWikiCodeMirrorEditor ? window.AhaWikiCodeMirrorEditor.getValue() : $textarea.val();
            if (detectGanttBlock(initialSrc))
                openInlineGanttEditor();
            else if ($('input[name=lineStart]').length || detectTableBlock(initialSrc))
                openInlineTableEditor();
        });
        function preventXss(html) {
            const div = document.createElement('div');
            div.textContent = html;
            return div.innerHTML;
        }

        $(function () {
            const wikipediaToSchema = window.AhaWiki.WikipediaToSchema;
            const convertWikipediaToSchemaOrg = wikipediaToSchema.convertWikipediaToSchemaOrg;
            $('.fetchInfoBoxFromWikipedia').click(e => {
                (async e => {
                    const page = prompt('Enter the Wikipedia page name', AhaWikiEditConfig.pageName);
                    if (!page)
                        return false;

                    AhaWiki.Loader.open();
                    const arrayResult = (await Promise.all(['en', 'ko'/*, 'ja'*/].map(async lang => ({
                        lang,
                        arrayArrayValue: await wikipediaToSchema.fetchInfoBoxFromWikipedia(lang, page),
                    }))).finally(() => AhaWiki.Loader.close()))
                        .filter(v => v.arrayArrayValue && Array.isArray(v.arrayArrayValue))
                        .map(v => ({...v, content: convertWikipediaToSchemaOrg(v.arrayArrayValue)}))
                        .filter(v => v.content);

                    if(!arrayResult.length) {
                        alert('No infobox found');
                        return false;
                    }

                    const schemaText = arrayResult
                        .map(v => `
[[[#!Schema
${v.content}
# API URL\t${wikipediaToSchema.getApiUrlWikipedia(v.lang, page)}
sameAs\t${wikipediaToSchema.getUrlWikipedia(v.lang, page)}
]]]
                        `.trim() + '\n\n')
                                    .join('\n\n');
                    if (window.AhaWikiCodeMirrorEditor) {
                        window.AhaWikiCodeMirrorEditor.replaceSelection(schemaText, 'end');
                        window.AhaWikiCodeMirrorEditor.focus();
                        window.AhaWikiCodeMirrorEditor.save();
                        $('textarea[name=text]').change();
                    } else {
                        $('textarea[name=text]').insertAtCaret(schemaText).change();
                    }
                    return false;
                })(e)
                return false;
            });
        });
