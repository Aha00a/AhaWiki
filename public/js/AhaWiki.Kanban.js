document.addEventListener('DOMContentLoaded', function () {
    var kanbanInterpreters = document.querySelectorAll('.InterpreterKanban');

    var requestAddList = function (pageName, title, lineStart) {
        if (!pageName) {
            return Promise.resolve(null);
        }

        return fetch('/api/csrf', {
            credentials: 'same-origin'
        }).then(function (csrfResponse) {
            return csrfResponse.json().catch(function () {
                return {};
            });
        }).then(function (csrfToken) {
            var tokenValue = csrfToken && csrfToken.value ? csrfToken.value : '';

            return fetch('/api/Kanban/' + encodeURIComponent(pageName) + '/list', {
                method: 'PUT',
                credentials: 'same-origin',
                headers: {
                    'Content-Type': 'application/json',
                    'Csrf-Token': tokenValue,
                    'X-CSRF-Token': tokenValue
                },
                body: JSON.stringify({
                    title: title,
                    lineStart: lineStart
                })
            }).then(function (response) {
                return response.json().catch(function () {
                    return {};
                }).then(function (payload) {
                    if (!response.ok) {
                        throw new Error(payload.message || payload.error || 'Failed to add list.');
                    }
                    return payload;
                });
            });
        });
    };

    var requestAddCard = function (pageName, text, lineStart) {
        if (!pageName) {
            return Promise.resolve(null);
        }

        return fetch('/api/csrf', {
            credentials: 'same-origin'
        }).then(function (csrfResponse) {
            return csrfResponse.json().catch(function () {
                return {};
            });
        }).then(function (csrfToken) {
            var tokenValue = csrfToken && csrfToken.value ? csrfToken.value : '';

            return fetch('/api/Kanban/' + encodeURIComponent(pageName) + '/card', {
                method: 'PUT',
                credentials: 'same-origin',
                headers: {
                    'Content-Type': 'application/json',
                    'Csrf-Token': tokenValue,
                    'X-CSRF-Token': tokenValue
                },
                body: JSON.stringify({
                    text: text,
                    lineStart: lineStart
                })
            }).then(function (response) {
                return response.json().catch(function () {
                    return {};
                }).then(function (payload) {
                    if (!response.ok) {
                        throw new Error(payload.message || payload.error || 'Failed to add card.');
                    }
                    return payload;
                });
            });
        });
    };
    var requestSaveKanban = function (pageName, lineStart, lineEnd, content, actionType, actionMeta) {
        if (!pageName) {
            return Promise.resolve(null);
        }
        return fetch('/api/csrf', { credentials: 'same-origin' })
            .then(function (csrfResponse) { return csrfResponse.json().catch(function () { return {}; }); })
            .then(function (csrfToken) {
                var tokenValue = csrfToken && csrfToken.value ? csrfToken.value : '';
                return fetch('/api/Kanban/' + encodeURIComponent(pageName) + '/save', {
                    method: 'PUT',
                    credentials: 'same-origin',
                    headers: {
                        'Content-Type': 'application/json',
                        'Csrf-Token': tokenValue,
                        'X-CSRF-Token': tokenValue
                    },
                    body: JSON.stringify({ lineStart: lineStart, lineEnd: lineEnd, content: content, actionType: actionType || null, actionMeta: actionMeta || null })
                }).then(function (response) {
                    return response.json().catch(function () { return {}; }).then(function (payload) {
                        if (!response.ok) {
                            throw new Error(payload.message || payload.error || 'Failed to save kanban.');
                        }
                        return payload;
                    });
                });
            });
    };


    var shiftMetaLineRangeAfterInsert = function (anchorWrapper, insertedLineNumber, delta) {
        if (!anchorWrapper || !Number.isFinite(insertedLineNumber) || !Number.isFinite(delta) || delta === 0) {
            return;
        }

        var wrappers = Array.prototype.slice.call(document.querySelectorAll('.InterpreterRenderMetaWrapper'));
        wrappers.forEach(function (wrapper) {
            var rawStart = Number(wrapper.getAttribute('data-line-start'));
            var rawEnd = Number(wrapper.getAttribute('data-line-end'));
            if (!Number.isFinite(rawStart) || !Number.isFinite(rawEnd)) {
                return;
            }

            if (wrapper === anchorWrapper) {
                wrapper.setAttribute('data-line-end', String(rawEnd + delta));
                return;
            }

            if (rawStart >= insertedLineNumber) {
                wrapper.setAttribute('data-line-start', String(rawStart + delta));
                wrapper.setAttribute('data-line-end', String(rawEnd + delta));
            } else if (rawEnd > insertedLineNumber) {
                wrapper.setAttribute('data-line-end', String(rawEnd + delta));
            }
        });
    };
    var COMMENT_PREFIX = '  * ';

    var buildCommentLine = function (message) {
        var nowIso = new Date().toISOString().replace(/\.\d{3}Z$/, '');
        var author = window.AhaWikiCurrentUserNickname || 'Anonymous';
        return '[User:' + author + '] [' + nowIso.slice(0, 10) + ']' + nowIso.slice(10) + ' - ' + message;
    };

    var updateCardCommentCount = function (card) {
        if (!card || !card.commentCountElement) {
            return;
        }
        var count = (card.comments || []).length;
        card.commentCountElement.textContent = '💬 ' + count;
    };
    var parseKanbanText = function (text, interpreterStartLine) {
        var lines = text.split(/\r?\n/);
        var columns = [];
        var currentColumn = null;
        var currentCard = null;

        lines.forEach(function (line, lineIndex) {
            var sectionMatch = line.match(/^==\s+(.+)$/);
            if (sectionMatch) {
                currentColumn = {
                    title: sectionMatch[1].trim(),
                    lineNumber: interpreterStartLine + lineIndex,
                    cards: []
                };
                columns.push(currentColumn);
                currentCard = null;
                return;
            }

            var commentMatch = line.match(/^\s{2,}\*\s+(.+)$/);
            if (commentMatch && currentCard) {
                currentCard.comments = currentCard.comments || [];
                currentCard.comments.push(commentMatch[1].trim());
                return;
            }

            var cardMatch = line.match(/^\s\*\s+(.+)$/);
            if (cardMatch && currentColumn) {
                currentCard = {
                    text: cardMatch[1].trim(),
                    lineNumber: interpreterStartLine + lineIndex,
                    comments: []
                };
                currentColumn.cards.push(currentCard);
            }
        });

        return columns;
    };

    var openCardDetail = function () {};
    var createColumnElement = function (root, columns, column, index, shiftLineNumbersAfterInsert, getCardInsertLineStart, enqueueMutation, rerenderColumns, persistColumns) {
        var columnElement = document.createElement('div');
        columnElement.className = 'kanban-column';
        columnElement.setAttribute('data-column-index', String(index));
        columnElement.setAttribute('data-column-line-number', String(column.lineNumber || 1));
        columnElement.style.minWidth = '220px';
        columnElement.style.background = '#f6f7f9';
        columnElement.style.border = '1px solid #d7dce2';
        columnElement.style.borderRadius = '6px';
        columnElement.style.padding = '10px';
        columnElement.style.boxSizing = 'border-box';

        var titleRow = document.createElement('div');
        titleRow.style.display = 'flex';
        titleRow.style.alignItems = 'center';
        titleRow.style.justifyContent = 'space-between';
        titleRow.style.marginBottom = '8px';

        var title = document.createElement('div');
        title.textContent = column.title;
        title.style.fontWeight = 'bold';
        title.style.cursor = 'pointer';
        title.title = 'Click to edit list name';

        var deleteListButton = document.createElement('button');
        deleteListButton.type = 'button';
        deleteListButton.textContent = '🗑';
        deleteListButton.title = 'Delete list';
        deleteListButton.style.border = 'none';
        deleteListButton.style.background = 'transparent';
        deleteListButton.style.cursor = 'pointer';
        deleteListButton.style.fontSize = '14px';

        titleRow.appendChild(title);
        titleRow.appendChild(deleteListButton);
        columnElement.appendChild(titleRow);

        var titleEditor = document.createElement('input');
        titleEditor.type = 'text';
        titleEditor.value = column.title || '';
        titleEditor.style.display = 'none';
        titleEditor.style.width = '100%';
        titleEditor.style.boxSizing = 'border-box';
        titleEditor.style.marginBottom = '8px';
        titleEditor.style.border = '1px solid #b6c2cf';
        titleEditor.style.borderRadius = '4px';
        titleEditor.style.padding = '6px 8px';
        columnElement.appendChild(titleEditor);

        var closeTitleEditor = function () {
            titleEditor.style.display = 'none';
            title.style.display = 'block';
            titleEditor.value = column.title || '';
        };

        var openTitleEditor = function () {
            title.style.display = 'none';
            titleEditor.style.display = 'block';
            titleEditor.value = column.title || '';
            titleEditor.focus();
            titleEditor.select();
        };

        var submitTitleEditor = function () {
            var nextTitle = (titleEditor.value || '').trim();
            if (!nextTitle || nextTitle === column.title) {
                closeTitleEditor();
                return;
            }

            var previousTitle = column.title || '';
            column.title = nextTitle;
            title.textContent = nextTitle;
            closeTitleEditor();

            enqueueMutation(function () {
                return persistColumns('list:rename', { fromTitle: previousTitle, toTitle: nextTitle || '' }).catch(function (error) {
                    console.error('[Kanban] failed to save list title', error);
                });
            });
        };

        title.addEventListener('click', openTitleEditor);
        titleEditor.addEventListener('blur', submitTitleEditor);
        titleEditor.addEventListener('keydown', function (evt) {
            if (evt.key === 'Enter') {
                evt.preventDefault();
                submitTitleEditor();
                return;
            }
            if (evt.key === 'Escape') {
                evt.preventDefault();
                closeTitleEditor();
            }
        });

        deleteListButton.addEventListener('click', function () {
            var shouldDelete = window.confirm('Delete list "' + (column.title || '') + '" and all cards?');
            if (!shouldDelete) {
                return;
            }

            var removedTitle = column.title || '';
            columns.splice(index, 1);
            shiftLineNumbersAfterInsert();
            rerenderColumns();

            enqueueMutation(function () {
                return persistColumns('list:delete', { listTitle: removedTitle }).catch(function (error) {
                    console.error('[Kanban] failed to delete list', error);
                });
            });
        });

        var cardList = document.createElement('div');
        cardList.className = 'kanban-card-list';
        cardList.style.minHeight = '20px';

        (column.cards || []).forEach(function (card) {
            var cardElement = document.createElement('div');
            cardElement.className = 'kanban-card';
            cardElement.setAttribute('data-line-number', String(card.lineNumber));
            cardElement.style.background = '#fff';
            cardElement.style.border = '1px solid #cfd5dd';
            cardElement.style.borderRadius = '6px';
            cardElement.style.padding = '8px';
            cardElement.style.marginBottom = '8px';
            cardElement.style.cursor = 'pointer';

            var cardText = document.createElement('div');
            cardText.textContent = card.text;
            cardText.style.marginBottom = '6px';

            var cardMeta = document.createElement('div');
            cardMeta.style.fontSize = '12px';
            cardMeta.style.color = '#6b778c';

            card.commentCountElement = document.createElement('span');
            updateCardCommentCount(card);
            cardMeta.appendChild(card.commentCountElement);

            cardElement.appendChild(cardText);
            cardElement.appendChild(cardMeta);
            cardElement.addEventListener('click', function () {
                openCardDetail(card);
            });
            cardList.appendChild(cardElement);
        });

        columnElement.appendChild(cardList);

        var cardComposer = document.createElement('div');
        cardComposer.style.marginTop = '6px';

        var addCardButton = document.createElement('button');
        addCardButton.type = 'button';
        addCardButton.textContent = 'Add a Card';
        addCardButton.style.display = 'block';
        addCardButton.style.width = '100%';
        addCardButton.style.padding = '8px';
        addCardButton.style.textAlign = 'left';
        addCardButton.style.border = 'none';
        addCardButton.style.borderRadius = '6px';
        addCardButton.style.background = 'transparent';
        addCardButton.style.color = '#44546f';
        addCardButton.style.cursor = 'pointer';

        var cardEditor = document.createElement('div');
        cardEditor.style.display = 'none';

        var cardTextarea = document.createElement('textarea');
        cardTextarea.rows = 3;
        cardTextarea.placeholder = 'Enter a Title';
        cardTextarea.style.width = '100%';
        cardTextarea.style.boxSizing = 'border-box';
        cardTextarea.style.border = '1px solid #b6c2cf';
        cardTextarea.style.borderRadius = '6px';
        cardTextarea.style.padding = '8px';
        cardTextarea.style.resize = 'vertical';

        var cardActions = document.createElement('div');
        cardActions.style.marginTop = '8px';
        cardActions.style.display = 'flex';
        cardActions.style.alignItems = 'center';
        cardActions.style.gap = '8px';

        var submitCardButton = document.createElement('button');
        submitCardButton.type = 'button';
        submitCardButton.textContent = 'Add a Card';
        submitCardButton.style.border = 'none';
        submitCardButton.style.borderRadius = '4px';
        submitCardButton.style.padding = '8px 12px';
        submitCardButton.style.background = '#0c66e4';
        submitCardButton.style.color = '#fff';
        submitCardButton.style.cursor = 'pointer';

        var cancelCardButton = document.createElement('button');
        cancelCardButton.type = 'button';
        cancelCardButton.textContent = 'Cancel';
        cancelCardButton.style.border = 'none';
        cancelCardButton.style.background = 'transparent';
        cancelCardButton.style.color = '#44546f';
        cancelCardButton.style.cursor = 'pointer';

        cardActions.appendChild(submitCardButton);
        cardActions.appendChild(cancelCardButton);
        cardEditor.appendChild(cardTextarea);
        cardEditor.appendChild(cardActions);
        cardComposer.appendChild(addCardButton);
        cardComposer.appendChild(cardEditor);
        columnElement.appendChild(cardComposer);

        var closeCardEditor = function () {
            cardEditor.style.display = 'none';
            addCardButton.style.display = 'block';
            cardTextarea.value = '';
        };

        addCardButton.addEventListener('click', function () {
            addCardButton.style.display = 'none';
            cardEditor.style.display = 'block';
            cardTextarea.focus();
        });

        cancelCardButton.addEventListener('click', function () {
            closeCardEditor();
        });

        var submitCard = function () {
            var value = (cardTextarea.value || '').trim();
            if (!value) {
                cardTextarea.focus();
                return;
            }
            submitCardButton.disabled = true;
            closeCardEditor();

            enqueueMutation(function () {
                var cardLineStart = getCardInsertLineStart(index);
                var newCard = {
                    text: value,
                    lineNumber: cardLineStart,
                    comments: [buildCommentLine('Created')]
                };

                column.cards = column.cards || [];
                column.cards.push(newCard);
                shiftLineNumbersAfterInsert();
                rerenderColumns();

                root.dispatchEvent(new CustomEvent('kanban:cardAdded', {
                    detail: {
                        text: value,
                        columnIndex: index,
                        cardIndex: column.cards.length - 1
                    }
                }));

                return requestAddCard(root.getAttribute('data-page-name') || '', value, cardLineStart)
                    .then(function (result) {
                        if (result && Number.isFinite(result.lineStart)) {
                            shiftLineNumbersAfterInsert();
                            shiftMetaLineRangeAfterInsert(root.closest('.InterpreterRenderMetaWrapper'), result.lineStart, 1);
                        }
                        rerenderColumns();
                        console.info('[Kanban] card added', result);
                    })
                    .catch(function (error) {
                        console.error('[Kanban] failed to call card add api', error);
                    })
                    .finally(function () {
                        submitCardButton.disabled = false;
                    });
            });
        };

        submitCardButton.addEventListener('click', submitCard);

        cardTextarea.addEventListener('keydown', function (evt) {
            if (evt.key === 'Enter' && !evt.shiftKey) {
                evt.preventDefault();
                submitCard();
            }
        });

        if (window.Sortable) {
            Sortable.create(cardList, {
                group: root.id || 'kanban-default',
                animation: 120,
                onEnd: function (evt) {
                    var movedLine = Number(evt.item.getAttribute('data-line-number'));
                    var fromColumnIndex = Number(evt.from.parentElement.getAttribute('data-column-index'));
                    var toColumnIndex = Number(evt.to.parentElement.getAttribute('data-column-index'));

                    var previousCard = evt.item.previousElementSibling;
                    var nextCard = evt.item.nextElementSibling;
                    var targetLine;

                    if (previousCard && previousCard.getAttribute('data-line-number')) {
                        targetLine = Number(previousCard.getAttribute('data-line-number')) + 1;
                    } else if (nextCard && nextCard.getAttribute('data-line-number')) {
                        targetLine = Number(nextCard.getAttribute('data-line-number'));
                    } else {
                        targetLine = Number(evt.to.parentElement.getAttribute('data-column-line-number')) + 1;
                    }

                    console.info('[Kanban] card moved', {
                        movedLine: movedLine,
                        targetLine: targetLine,
                        fromColumnIndex: fromColumnIndex,
                        toColumnIndex: toColumnIndex,
                        oldIndex: evt.oldIndex,
                        newIndex: evt.newIndex
                    });

                    root.dispatchEvent(new CustomEvent('kanban:cardMoved', {
                        detail: {
                            movedLine: movedLine,
                            targetLine: targetLine,
                            fromColumnIndex: fromColumnIndex,
                            toColumnIndex: toColumnIndex,
                            oldIndex: evt.oldIndex,
                            newIndex: evt.newIndex
                        }
                    }));

                    enqueueMutation(function () {
                        var fromColumn = columns[fromColumnIndex];
                        var toColumn = columns[toColumnIndex];
                        var fromTitle = (fromColumn && fromColumn.title) ? fromColumn.title : '';
                        var toTitle = (toColumn && toColumn.title) ? toColumn.title : '';
                        var movedCard = columns[fromColumnIndex].cards.splice(evt.oldIndex, 1)[0];
                        movedCard.comments = movedCard.comments || [];
                        movedCard.comments.push(buildCommentLine('Moved from [#' + fromTitle + '] to [#' + toTitle + ']'));
                        updateCardCommentCount(movedCard);
                        columns[toColumnIndex].cards.splice(evt.newIndex, 0, movedCard);
                        shiftLineNumbersAfterInsert();
                        rerenderColumns();
                        return persistColumns('card:move', { cardTitle: movedCard.text || '', fromOrder: String((evt.oldIndex || 0) + 1), toOrder: String((evt.newIndex || 0) + 1) }).catch(function (error) {
                            console.error('[Kanban] failed to save card order', error);
                        });
                    });
                }
            });
        }

        return columnElement;
    };

    kanbanInterpreters.forEach(function (root) {
        var pre = root.querySelector('pre[data-shebang]');
        var board = root.querySelector('.kanban-board');
        if (!pre || !board) {
            return;
        }

        var pageName = root.getAttribute('data-page-name') || '';
        var metaWrapper = root.closest('.InterpreterRenderMetaWrapper');
        var metaLineStart = Number(metaWrapper ? metaWrapper.getAttribute('data-line-start') : 1) || 1;
        var hasShebang = Boolean(pre.getAttribute('data-shebang'));
        var interpreterStartLine = Math.max(1, metaLineStart + (hasShebang ? 1 : 0));
        var columns = parseKanbanText(pre.textContent || '', interpreterStartLine);
        var interpreterLineEnd = Number(metaWrapper ? metaWrapper.getAttribute('data-line-end') : 1) || 1;
        var mutationQueue = Promise.resolve();
        var serializeColumns = function () {
            return columns.map(function (column) {
                var lines = ['== ' + (column.title || '')];
                (column.cards || []).forEach(function (card) {
                    lines.push(' * ' + (card.text || ''));
                    (card.comments || []).forEach(function (comment) {
                        lines.push(COMMENT_PREFIX + comment);
                    });
                });
                return lines.join('\n');
            }).join('\n');
        };
        var persistColumns = function (actionType, actionMeta) {
            var latestLineEnd = Number(metaWrapper ? metaWrapper.getAttribute('data-line-end') : interpreterLineEnd) || interpreterLineEnd;
            var requestLineEnd = Math.max(interpreterStartLine, latestLineEnd - 1);
            return requestSaveKanban(pageName, interpreterStartLine, requestLineEnd, serializeColumns(), actionType, actionMeta)
                .then(function (result) {
                    var nextLineEnd = Number(result && result.lineEnd);
                    if (metaWrapper && Number.isFinite(nextLineEnd) && nextLineEnd >= interpreterStartLine) {
                        var delta = nextLineEnd - requestLineEnd;
                        if (delta !== 0) {
                            shiftMetaLineRangeAfterInsert(metaWrapper, requestLineEnd, delta);
                        }
                        metaWrapper.setAttribute('data-line-end', String(nextLineEnd + 1));
                    }
                    return result;
                });
        };
        var enqueueMutation = function (executor) {
            mutationQueue = mutationQueue
                .then(function () {
                    return executor();
                })
                .catch(function (error) {
                    console.error('[Kanban] queued mutation failed', error);
                });
            return mutationQueue;
        };
        var getNextListInsertLineStart = function () {
            var currentLineEnd = Number(metaWrapper ? metaWrapper.getAttribute('data-line-end') : interpreterLineEnd) || interpreterLineEnd;
            return Math.max(1, currentLineEnd - 1);
        };
        pre.style.display = 'none';
        board.style.display = 'flex';
        board.style.gap = '12px';
        board.style.alignItems = 'flex-start';
        board.style.overflowX = 'auto';
        board.style.background = 'linear-gradient(135deg, #eef 0%, #eff 100%)';
        board.style.borderRadius = '10px';
        board.style.padding = '12px';

        var addListWrapper = document.createElement('div');
        addListWrapper.style.flex = '0 0 280px';
        addListWrapper.style.minWidth = '280px';

        var addListButton = document.createElement('button');
        addListButton.type = 'button';
        addListButton.textContent = '+ Add a List';
        addListButton.style.width = '100%';
        addListButton.style.padding = '10px 12px';
        addListButton.style.textAlign = 'left';
        addListButton.style.border = '1px solid #d7dce2';
        addListButton.style.borderRadius = '8px';
        addListButton.style.background = 'rgba(9, 30, 66, 0.04)';
        addListButton.style.cursor = 'pointer';

        var addListEditor = document.createElement('div');
        addListEditor.style.display = 'none';
        addListEditor.style.background = '#f6f7f9';
        addListEditor.style.border = '1px solid #d7dce2';
        addListEditor.style.borderRadius = '8px';
        addListEditor.style.padding = '10px';

        var addListInput = document.createElement('input');
        addListInput.type = 'text';
        addListInput.placeholder = 'Enter list name';
        addListInput.style.width = '100%';
        addListInput.style.boxSizing = 'border-box';
        addListInput.style.border = '1px solid #b6c2cf';
        addListInput.style.borderRadius = '4px';
        addListInput.style.padding = '8px';

        var addListActions = document.createElement('div');
        addListActions.style.display = 'flex';
        addListActions.style.gap = '8px';
        addListActions.style.marginTop = '8px';

        var submitListButton = document.createElement('button');
        submitListButton.type = 'button';
        submitListButton.textContent = 'Add a List';
        submitListButton.style.border = 'none';
        submitListButton.style.borderRadius = '4px';
        submitListButton.style.padding = '8px 12px';
        submitListButton.style.background = '#0c66e4';
        submitListButton.style.color = '#fff';

        var cancelListButton = document.createElement('button');
        cancelListButton.type = 'button';
        cancelListButton.textContent = 'Cancel';
        cancelListButton.style.border = 'none';
        cancelListButton.style.background = 'transparent';

        addListActions.appendChild(submitListButton);
        addListActions.appendChild(cancelListButton);
        addListEditor.appendChild(addListInput);
        addListEditor.appendChild(addListActions);
        addListWrapper.appendChild(addListButton);
        addListWrapper.appendChild(addListEditor);


        var normalizeLineNumbers = function () {
            var cursor = interpreterStartLine;
            columns.forEach(function (targetColumn) {
                targetColumn.lineNumber = cursor;
                cursor += 1;

                (targetColumn.cards || []).forEach(function (targetCard) {
                    targetCard.lineNumber = cursor;
                    cursor += 1;
                });
            });
        };

        var renderColumns = function () {
            Array.prototype.slice.call(board.querySelectorAll('.kanban-column')).forEach(function (node) {
                board.removeChild(node);
            });

            columns.forEach(function (column, index) {
                board.insertBefore(createColumnElement(root, columns, column, index, normalizeLineNumbers, getCardInsertLineStart, enqueueMutation, renderColumns, persistColumns), addListWrapper);
            });
        };

        var getCardInsertLineStart = function (columnIndex) {
            var column = columns[columnIndex] || {};
            var cards = column.cards || [];
            var lastCard = cards.length > 0 ? cards[cards.length - 1] : null;

            if (lastCard && Number.isFinite(lastCard.lineNumber) && lastCard.lineNumber > 0) {
                return lastCard.lineNumber + 1;
            }

            for (var i = columnIndex + 1; i < columns.length; i += 1) {
                var nextColumnLine = Number(columns[i].lineNumber);
                if (Number.isFinite(nextColumnLine) && nextColumnLine > 0) {
                    return nextColumnLine;
                }
            }

            var fallbackLine = getNextListInsertLineStart();
            if (Number.isFinite(fallbackLine) && fallbackLine > 0) {
                return fallbackLine;
            }

            var columnLine = Number(column.lineNumber);
            return Number.isFinite(columnLine) && columnLine > 0 ? columnLine + 1 : 1;
        };

        var closeListEditor = function () {
            addListEditor.style.display = 'none';
            addListButton.style.display = 'block';
            addListInput.value = '';
        };

        addListButton.addEventListener('click', function () {
            addListButton.style.display = 'none';
            addListEditor.style.display = 'block';
            addListInput.focus();
        });

        cancelListButton.addEventListener('click', function () {
            closeListEditor();
        });

        var submitList = function () {
            var trimmed = (addListInput.value || '').trim();
            if (!trimmed) {
                addListInput.focus();
                return;
            }

            submitListButton.disabled = true;
            closeListEditor();

            enqueueMutation(function () {
                var requestLineStart = getNextListInsertLineStart();
                var localLineNumber = Math.max(1, requestLineStart);
                var newColumn = {
                    title: trimmed,
                    lineNumber: localLineNumber,
                    cards: []
                };
                columns.push(newColumn);
                normalizeLineNumbers();
                renderColumns();

                root.dispatchEvent(new CustomEvent('kanban:listAdded', {
                    detail: {
                        title: trimmed,
                        listIndex: columns.length - 1
                    }
                }));

                return requestAddList(pageName, trimmed, requestLineStart)
                    .then(function (result) {
                        if (result && Number.isFinite(result.lineStart)) {
                            normalizeLineNumbers();
                            shiftMetaLineRangeAfterInsert(metaWrapper, result.lineStart, 1);
                        }
                        renderColumns();
                        console.info('[Kanban] list added', result);
                    })
                    .catch(function (error) {
                        console.error('[Kanban] failed to call list add api', error);
                    })
                    .finally(function () {
                        submitListButton.disabled = false;
                    });
            });
        };

        submitListButton.addEventListener('click', submitList);

        addListInput.addEventListener('keydown', function (evt) {
            if (evt.key === 'Enter') {
                evt.preventDefault();
                submitList();
            }
        });

        board.appendChild(addListWrapper);
        renderColumns();

        openCardDetail = function (card) {
            if (!card) {
                return;
            }
            var existing = document.querySelector('.kanban-card-detail-overlay');
            if (existing && existing.parentNode) {
                existing.parentNode.removeChild(existing);
            }
            var overlay = document.createElement('div');
            overlay.className = 'kanban-card-detail-overlay';
            overlay.style.position = 'fixed';
            overlay.style.inset = '0';
            overlay.style.background = 'rgba(9, 30, 66, 0.5)';
            overlay.style.zIndex = '1000';
            overlay.style.display = 'flex';
            overlay.style.alignItems = 'center';
            overlay.style.justifyContent = 'center';
            var modal = document.createElement('div');
            modal.style.width = 'min(640px, calc(100vw - 24px))';
            modal.style.maxHeight = 'calc(100vh - 24px)';
            modal.style.overflowY = 'auto';
            modal.style.background = '#fff';
            modal.style.borderRadius = '10px';
            modal.style.padding = '16px';
            var title = document.createElement('h3');
            title.textContent = card.text || '';
            title.style.cursor = 'pointer';
            title.title = 'Click to edit title';

            var titleEditor = document.createElement('input');
            titleEditor.type = 'text';
            titleEditor.value = card.text || '';
            titleEditor.style.display = 'none';
            titleEditor.style.width = '100%';
            titleEditor.style.boxSizing = 'border-box';
            titleEditor.style.marginBottom = '8px';
            titleEditor.style.border = '1px solid #b6c2cf';
            titleEditor.style.borderRadius = '4px';
            titleEditor.style.padding = '6px 8px';

            var closeTitleEditor = function () {
                titleEditor.style.display = 'none';
                title.style.display = 'block';
                titleEditor.value = card.text || '';
            };

            var openTitleEditor = function () {
                title.style.display = 'none';
                titleEditor.style.display = 'block';
                titleEditor.value = card.text || '';
                titleEditor.focus();
                titleEditor.select();
            };

            var submitTitleEditor = function () {
                var nextTitle = (titleEditor.value || '').trim();
                if (!nextTitle || nextTitle === card.text) {
                    closeTitleEditor();
                    return;
                }

                var previousCardTitle = card.text || '';
                card.text = nextTitle;
                card.comments = card.comments || [];
                card.comments.push(buildCommentLine('Renamed Card Title - ["' + previousCardTitle + '"] to ["' + nextTitle + '"]'));
                updateCardCommentCount(card);
                title.textContent = nextTitle;
                closeTitleEditor();
                renderColumns();

                enqueueMutation(function () {
                    return persistColumns('card:rename', { fromTitle: previousCardTitle, toTitle: nextTitle || '' }).catch(function (error) {
                        console.error('[Kanban] failed to save card title', error);
                    });
                });
            };

            title.addEventListener('click', openTitleEditor);
            titleEditor.addEventListener('blur', submitTitleEditor);
            titleEditor.addEventListener('keydown', function (evt) {
                if (evt.key === 'Enter') {
                    evt.preventDefault();
                    submitTitleEditor();
                    return;
                }
                if (evt.key === 'Escape') {
                    evt.preventDefault();
                    closeTitleEditor();
                }
            });

            var textarea = document.createElement('textarea');
            textarea.placeholder = 'Write a comment...';
            textarea.rows = 3;
            textarea.style.width = '100%';
            var submit = document.createElement('button');
            submit.type = 'button';
            submit.textContent = 'Add Comment';
            submit.style.marginTop = '8px';

            var deleteCardButton = document.createElement('button');
            deleteCardButton.type = 'button';
            deleteCardButton.textContent = 'Delete Card';
            deleteCardButton.style.marginTop = '8px';
            deleteCardButton.style.marginLeft = '8px';
            var comments = document.createElement('div');
            comments.style.marginTop = '12px';
            var renderComments = function () {
                comments.innerHTML = '';
                (card.comments || []).slice().reverse().forEach(function (line) {
                    var row = document.createElement('div');
                    row.textContent = line;
                    row.style.padding = '8px 0';
                    row.style.borderBottom = '1px solid #eceff3';
                    comments.appendChild(row);
                });
            };
            submit.addEventListener('click', function () {
                var body = (textarea.value || '').trim();
                if (!body) {
                    return;
                }
                var commentLine = buildCommentLine(body);
                card.comments = card.comments || [];
                card.comments.push(commentLine);
                updateCardCommentCount(card);
                textarea.value = '';
                renderComments();
                enqueueMutation(function () {
                    return persistColumns('card:comment:add', { cardTitle: card.text || '', comment: body }).catch(function (error) {
                        console.error('[Kanban] failed to save comments', error);
                    });
                });
            });

            deleteCardButton.addEventListener('click', function () {
                var shouldDelete = window.confirm('Delete card "' + (card.text || '') + '"?');
                if (!shouldDelete) {
                    return;
                }

                columns.forEach(function (targetColumn) {
                    var cardIndex = (targetColumn.cards || []).indexOf(card);
                    if (cardIndex >= 0) {
                        targetColumn.cards.splice(cardIndex, 1);
                    }
                });
                renderColumns();
                if (overlay.parentNode) {
                    overlay.parentNode.removeChild(overlay);
                }

                enqueueMutation(function () {
                    return persistColumns('card:delete', { cardTitle: card.text || '' }).catch(function (error) {
                        console.error('[Kanban] failed to delete card', error);
                    });
                });
            });
            overlay.addEventListener('click', function () {
                if (overlay.parentNode) {
                    overlay.parentNode.removeChild(overlay);
                }
            });
            modal.addEventListener('click', function (evt) { evt.stopPropagation(); });
            modal.appendChild(title);
            modal.appendChild(titleEditor);
            modal.appendChild(textarea);
            modal.appendChild(submit);
            modal.appendChild(deleteCardButton);
            modal.appendChild(comments);
            overlay.appendChild(modal);
            document.body.appendChild(overlay);
            renderComments();
        };

        if (window.Sortable) {
            Sortable.create(board, {
                draggable: '.kanban-column',
                animation: 120,
                onEnd: function (evt) {
                    if (evt.oldIndex === evt.newIndex || evt.oldIndex < 0 || evt.newIndex < 0) {
                        return;
                    }

                    var movedColumn = columns.splice(evt.oldIndex, 1)[0];
                    columns.splice(evt.newIndex, 0, movedColumn);
                    normalizeLineNumbers();
                    renderColumns();

                    root.dispatchEvent(new CustomEvent('kanban:listMoved', {
                        detail: {
                            oldIndex: evt.oldIndex,
                            newIndex: evt.newIndex
                        }
                    }));

                    enqueueMutation(function () {
                        return persistColumns('list:move', { listTitle: movedColumn.title || '', fromOrder: String((evt.oldIndex || 0) + 1), toOrder: String((evt.newIndex || 0) + 1) }).catch(function (error) {
                            console.error('[Kanban] failed to save list order', error);
                        });
                    });
                }
            });
        }
    });
});
