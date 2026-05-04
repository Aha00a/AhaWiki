document.addEventListener('DOMContentLoaded', function () {
    var kanbanInterpreters = document.querySelectorAll('.InterpreterKanban');
    var getHashCardId = function () {
        return (window.location.hash || '').replace(/^#/, '').trim();
    };
    var setHashCardId = function (cardId) {
        if (!cardId) {
            return;
        }
        var nextHash = '#' + cardId;
        if (window.location.hash === nextHash) {
            return;
        }
        if (window.history && window.history.replaceState) {
            window.history.replaceState(null, '', nextHash);
            return;
        }
        window.location.hash = cardId;
    };
    var clearHashCardId = function (cardId) {
        var currentHash = getHashCardId();
        if (!currentHash || (cardId && currentHash !== cardId)) {
            return;
        }
        var baseUrl = window.location.pathname + window.location.search;
        if (window.history && window.history.replaceState) {
            window.history.replaceState(null, '', baseUrl);
            return;
        }
        window.location.hash = '';
    };

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

    var requestAddCard = function (pageName, text, lineStart, creationComment) {
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
                    lineStart: lineStart,
                    creationComment: creationComment
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

    var requestUploadClipboardImage = function (pageName, dataUrl) {
        if (!pageName || !dataUrl) {
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
            var params = new URLSearchParams();
            params.set('pageName', pageName);
            params.set('dataUrl', dataUrl);

            return fetch('/api/uploadClipboardImage', {
                method: 'POST',
                credentials: 'same-origin',
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8',
                    'Csrf-Token': tokenValue,
                    'X-CSRF-Token': tokenValue
                },
                body: params.toString()
            }).then(function (response) {
                return response.json().catch(function () {
                    return {};
                }).then(function (payload) {
                    if (!response.ok) {
                        throw new Error(payload.error || payload.message || 'Failed to upload clipboard image.');
                    }
                    return payload;
                });
            });
        });
    };

    var requestRenderInlineComment = function (pageName, comment) {
        if (!pageName) {
            return Promise.resolve('');
        }
        return fetch('/api/csrf', { credentials: 'same-origin' })
            .then(function (csrfResponse) { return csrfResponse.json().catch(function () { return {}; }); })
            .then(function (csrfToken) {
                var tokenValue = csrfToken && csrfToken.value ? csrfToken.value : '';
                return fetch('/api/Kanban/' + encodeURIComponent(pageName) + '/renderAhaMark', {
                    method: 'POST',
                    credentials: 'same-origin',
                    headers: {
                        'Content-Type': 'application/json',
                        'Csrf-Token': tokenValue,
                        'X-CSRF-Token': tokenValue
                    },
                    body: JSON.stringify({ comment: comment || '' })
                }).then(function (response) {
                    return response.json().catch(function () { return {}; }).then(function (payload) {
                        if (!response.ok) {
                            throw new Error(payload.message || payload.error || 'Failed to render comment.');
                        }
                        return payload.html || '';
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
    var COMMENT_PREFIX = ' * ';
    var COMMENT_DETAIL_PREFIX = '  * ';

    var buildCommentEntry = function (details) {
        var nowIso = new Date().toISOString().replace(/\.\d{3}Z$/, '');
        var author = window.AhaWikiCurrentUserNickname || 'Anonymous';
        return {
            header: '[User:' + author + '] [' + nowIso.slice(0, 10) + ']' + nowIso.slice(10),
            details: (details || []).filter(function (item) { return Boolean((item || '').trim()); })
        };
    };
    var buildCardLinkText = function (cardId, cardName) {
        var safeCardName = (cardName || '').trim();
        var safeCardId = (cardId || '').trim();
        if (!safeCardId) {
            return '["#' + safeCardName + '"]';
        }
        return '["#' + safeCardId + '" ' + safeCardName + ']';
    };

    var getCardAttachmentCount = function (card) {
        if (!card || !card.properties) {
            return 0;
        }
        var attachments = card.properties.Attachment;
        if (!Array.isArray(attachments)) {
            return 0;
        }
        return attachments.filter(function (value) { return Boolean((value || '').trim()); }).length;
    };

    var updateCardCommentCount = function (card) {
        if (!card) {
            return;
        }
        if (card.commentCountElement) {
            var count = (card.comments || []).length;
            if (count > 0) {
                card.commentCountElement.style.display = '';
                card.commentCountElement.innerHTML = '<i class="fas fa-comment" aria-hidden="true"></i> ' + count;
            } else {
                card.commentCountElement.style.display = 'none';
                card.commentCountElement.innerHTML = '';
            }
        }
        if (card.attachmentCountElement) {
            var attachmentCount = getCardAttachmentCount(card);
            if (attachmentCount > 0) {
                card.attachmentCountElement.style.display = '';
                card.attachmentCountElement.innerHTML = '<i class="fas fa-paperclip" aria-hidden="true"></i> ' + attachmentCount;
            } else {
                card.attachmentCountElement.style.display = 'none';
                card.attachmentCountElement.innerHTML = '';
            }
        }
    };
    var parseKanbanText = function (text, interpreterStartLine) {
        var parseCardHeading = function (line) {
            var match = line.match(/^====\s+(.+?)(\s+====(?:\s+([#.].+))?)?$/);
            if (!match) {
                return null;
            }

            var title = (match[1] || '').trim();
            var rawAttrs = (match[3] || '').trim();
            var id = '';
            var classNames = [];

            rawAttrs.replace(/([#.])([^#.\s]+)/g, function (_, prefix, value) {
                if (prefix === '#') {
                    id = value;
                } else if (prefix === '.') {
                    classNames.push(value);
                }
                return _;
            });

            return {
                text: title,
                id: id,
                classNames: classNames
            };
        };

        var lines = text.split(/\r?\n/);
        var columns = [];
        var currentColumn = null;
        var currentCard = null;
        var cardSection = '';
        var propertyKey = '';

        lines.forEach(function (line, lineIndex) {
            var sectionMatch = line.match(/^===\s+(.+)$/);
            if (sectionMatch) {
                currentColumn = {
                    title: sectionMatch[1].trim(),
                    lineNumber: interpreterStartLine + lineIndex,
                    cards: []
                };
                columns.push(currentColumn);
                currentCard = null;
                cardSection = '';
                propertyKey = '';
                return;
            }
            var cardSectionMatch = line.match(/^=====\s+(Property|Activity)\s*$/);
            if (cardSectionMatch && currentCard) {
                cardSection = cardSectionMatch[1];
                propertyKey = '';
                if (cardSection === 'Property') {
                    currentCard.structured = true;
                }
                return;
            }

            if (cardSection === 'Property' && currentCard) {
                var propertyHeaderMatch = line.match(/^\s\*\s+([^:]+)\s*:\s*(.+)$/);
                if (propertyHeaderMatch) {
                    var key = (propertyHeaderMatch[1] || '').trim();
                    var value = (propertyHeaderMatch[2] || '').trim();
                    currentCard.properties[key] = currentCard.properties[key] || [];
                    if (value) {
                        currentCard.properties[key].push(value);
                    }
                    propertyKey = key;
                    return;
                }
                var propertyKeyOnlyMatch = line.match(/^\s\*\s+(.+)$/);
                if (propertyKeyOnlyMatch) {
                    propertyKey = (propertyKeyOnlyMatch[1] || '').trim();
                    currentCard.properties[propertyKey] = currentCard.properties[propertyKey] || [];
                    return;
                }
                var propertyValueMatch = line.match(/^\s{2}\*\s+(.+)$/);
                if (propertyValueMatch && propertyKey) {
                    currentCard.properties[propertyKey] = currentCard.properties[propertyKey] || [];
                    currentCard.properties[propertyKey].push((propertyValueMatch[1] || '').trim());
                    return;
                }
            }

            var commentDetailMatch = line.match(/^\s{2}\*\s+(.+)$/);
            if (commentDetailMatch && currentCard && cardSection !== 'Property' && currentCard.comments.length > 0) {
                var currentComment = currentCard.comments[currentCard.comments.length - 1];
                currentComment.details = currentComment.details || [];
                currentComment.details.push(commentDetailMatch[1].trim());
                return;
            }

            var commentHeaderMatch = line.match(/^\s\*\s+(.+)$/);
            if (commentHeaderMatch && currentCard && cardSection !== 'Property') {
                currentCard.comments = currentCard.comments || [];
                currentCard.comments.push({
                    header: commentHeaderMatch[1].trim(),
                    details: []
                });
                return;
            }


            var cardHeading = parseCardHeading(line);
            if (cardHeading && currentColumn) {
                currentCard = {
                    text: cardHeading.text,
                    id: cardHeading.id,
                    classNames: cardHeading.classNames,
                    lineNumber: interpreterStartLine + lineIndex,
                    comments: [],
                    structured: false,
                    properties: {}
                };
                cardSection = '';
                propertyKey = '';
                currentColumn.cards.push(currentCard);
            }
        });

        return columns;
    };

    var openCardDetail = function () {};
    var openCardDetailById = function () {};
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
        columnElement.style.display = 'flex';
        columnElement.style.flexDirection = 'column';
        columnElement.style.maxHeight = '80vh';

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
        deleteListButton.innerHTML = '<i class="fas fa-trash-alt" aria-hidden="true"></i>';
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
        cardList.style.flex = '1 1 auto';
        cardList.style.overflowY = 'auto';

        (column.cards || []).forEach(function (card) {
            var cardElement = document.createElement('div');
            cardElement.className = 'kanban-card';
            if (card.id) {
                cardElement.id = card.id;
            }
            (card.classNames || []).forEach(function (className) {
                if (className) {
                    cardElement.classList.add(className);
                }
            });
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

            var cardIdText = document.createElement('div');
            cardIdText.style.marginBottom = '4px';
            cardIdText.textContent = card.id || '-';
            cardMeta.appendChild(cardIdText);

            var cardStat = document.createElement('div');
            card.commentCountElement = document.createElement('span');
            card.attachmentCountElement = document.createElement('span');
            card.attachmentCountElement.style.marginRight = '10px';
            card.commentCountElement.style.marginRight = '10px';
            updateCardCommentCount(card);
            var dueDateValues = (card.properties && card.properties.DueDate) || [];
            var dueDateText = dueDateValues.length > 0 ? String(dueDateValues[0]).replace(/^\[|\]$/g, '').trim() : '';
            var dueDateElement = document.createElement('span');
            if (dueDateText) {
                dueDateElement.innerHTML = '<i class="far fa-calendar-alt" aria-hidden="true"></i> ' + dueDateText;
            } else {
                dueDateElement.style.display = 'none';
            }
            cardStat.appendChild(card.attachmentCountElement);
            cardStat.appendChild(card.commentCountElement);
            cardStat.appendChild(dueDateElement);
            cardMeta.appendChild(cardStat);

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
        addCardButton.innerHTML = '<i class="fas fa-plus" aria-hidden="true"></i> Add a Card';
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
        submitCardButton.innerHTML = '<i class="fas fa-plus" aria-hidden="true"></i> Add a Card';
        submitCardButton.style.border = 'none';
        submitCardButton.style.borderRadius = '4px';
        submitCardButton.style.padding = '8px 12px';
        submitCardButton.style.background = '#0c66e4';
        submitCardButton.style.color = '#fff';
        submitCardButton.style.cursor = 'pointer';

        var cancelCardButton = document.createElement('button');
        cancelCardButton.type = 'button';
        cancelCardButton.innerHTML = '<i class="fas fa-times" aria-hidden="true"></i> Cancel';
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
                    id: '',
                    classNames: [],
                    lineNumber: cardLineStart,
                    comments: [buildCommentEntry(['Created card'])]
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

                return requestAddCard(root.getAttribute('data-page-name') || '', value, cardLineStart, newCard.comments[0].header)
                    .then(function (result) {
                        if (result && result.cardId) {
                            newCard.id = result.cardId;
                        }
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
            var dragCancelled = false;
            var draggingCard = false;
            var draggingItem = null;
            var draggingFromList = null;
            var draggingOldIndex = -1;
            var cardSortable = null;
            var restoreDraggedItem = function (item, fromList, oldIndex) {
                var restoreItem = item || draggingItem;
                var restoreFromList = fromList || draggingFromList;
                var restoreOldIndex = Number.isFinite(oldIndex) ? oldIndex : draggingOldIndex;
                if (!restoreItem || !restoreFromList || restoreOldIndex < 0) {
                    return;
                }
                var children = restoreFromList.children;
                if (restoreOldIndex >= children.length) {
                    restoreFromList.appendChild(restoreItem);
                    return;
                }
                if (children[restoreOldIndex] === restoreItem) {
                    return;
                }
                restoreFromList.insertBefore(restoreItem, children[restoreOldIndex]);
            };
            var handleDragEscape = function (evt) {
                if (!draggingCard || evt.key !== 'Escape') {
                    return;
                }
                evt.preventDefault();
                dragCancelled = true;
            };

            cardSortable = Sortable.create(cardList, {
                group: root.id || 'kanban-default',
                animation: 120,
                onStart: function (evt) {
                    draggingCard = true;
                    dragCancelled = false;
                    draggingItem = evt.item;
                    draggingFromList = evt.from;
                    draggingOldIndex = evt.oldIndex;
                    window.addEventListener('keydown', handleDragEscape, true);
                },
                onEnd: function (evt) {
                    draggingCard = false;
                    window.removeEventListener('keydown', handleDragEscape, true);

                    var hasInvalidIndex = !Number.isFinite(evt.oldIndex) || !Number.isFinite(evt.newIndex);
                    var droppedOutsideList = !(evt.to && evt.to.classList && evt.to.classList.contains('kanban-card-list'));
                    var returnedToSameSpot = evt.from === evt.to && evt.oldIndex === evt.newIndex;
                    if (dragCancelled || droppedOutsideList || hasInvalidIndex || returnedToSameSpot) {
                        restoreDraggedItem(evt.item, evt.from, evt.oldIndex);
                        console.info('[Kanban] card move canceled', {
                            reason: dragCancelled ? 'escape' : (droppedOutsideList ? 'outside-dropzone' : (hasInvalidIndex ? 'invalid-index' : 'no-change')),
                            oldIndex: evt.oldIndex,
                            newIndex: evt.newIndex
                        });
                        dragCancelled = false;
                        draggingItem = null;
                        draggingFromList = null;
                        draggingOldIndex = -1;
                        rerenderColumns();
                        return;
                    }
                    draggingItem = null;
                    draggingFromList = null;
                    draggingOldIndex = -1;

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
                        movedCard.comments.push(buildCommentEntry(["Moved from '''" + fromTitle + "''' to '''" + toTitle + "'''"]));
                        updateCardCommentCount(movedCard);
                        columns[toColumnIndex].cards.splice(evt.newIndex, 0, movedCard);
                        shiftLineNumbersAfterInsert();
                        rerenderColumns();
                        return persistColumns('card:move', {
                            cardTitle: movedCard.text || '',
                            fromOrder: String((evt.oldIndex || 0) + 1),
                            toOrder: String((evt.newIndex || 0) + 1),
                            fromList: fromTitle,
                            toList: toTitle
                        }).catch(function (error) {
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
                var lines = ['=== ' + (column.title || '')];
                (column.cards || []).forEach(function (card) {
                    var attrs = [];
                    if (card.id) {
                        attrs.push('#' + card.id);
                    }
                    (card.classNames || []).forEach(function (className) {
                        if (className) {
                            attrs.push('.' + className);
                        }
                    });
                    var attrSuffix = attrs.length > 0 ? ' ' + attrs.join('') : '';
                    lines.push('==== ' + (card.text || '') + ' ====' + attrSuffix);
                    if (card.structured) {
                        lines.push('===== Property');
                        Object.keys(card.properties || {}).forEach(function (key) {
                            var values = card.properties[key] || [];
                            if (values.length <= 1) {
                                if (values.length === 1) {
                                    lines.push(COMMENT_PREFIX + key + ': ' + values[0]);
                                } else {
                                    lines.push(COMMENT_PREFIX + key);
                                }
                            } else {
                                lines.push(COMMENT_PREFIX + key);
                                values.forEach(function (value) {
                                    lines.push(COMMENT_DETAIL_PREFIX + value);
                                });
                            }
                        });
                        lines.push('===== Activity');
                    }
                    (card.comments || []).forEach(function (comment) {
                        if (!comment || !comment.header) {
                            return;
                        }
                        lines.push(COMMENT_PREFIX + comment.header);
                        (comment.details || []).forEach(function (detail) {
                            lines.push(COMMENT_DETAIL_PREFIX + detail);
                        });
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
        addListButton.innerHTML = '<i class="fas fa-plus" aria-hidden="true"></i> Add a List';
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
        submitListButton.innerHTML = '<i class="fas fa-plus" aria-hidden="true"></i> Add a List';
        submitListButton.style.border = 'none';
        submitListButton.style.borderRadius = '4px';
        submitListButton.style.padding = '8px 12px';
        submitListButton.style.background = '#0c66e4';
        submitListButton.style.color = '#fff';

        var cancelListButton = document.createElement('button');
        cancelListButton.type = 'button';
        cancelListButton.innerHTML = '<i class="fas fa-times" aria-hidden="true"></i> Cancel';
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
                    var commentCount = (targetCard.comments || []).length;
                    if (commentCount > 0) {
                        cursor += commentCount;
                    }
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

        var scrollToKanbanForHashCard = function () {
            var rawHash = window.location.hash || '';
            if (!rawHash || rawHash.length <= 1) {
                return;
            }

            var hashId = rawHash.slice(1);
            var targetCard = board.querySelector('.kanban-card[id="' + CSS.escape(hashId) + '"]');
            if (!targetCard) {
                return;
            }

            root.scrollIntoView({ block: 'start' });
        };

        var getCardInsertLineStart = function (columnIndex) {
            var column = columns[columnIndex] || {};

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
        window.requestAnimationFrame(scrollToKanbanForHashCard);

        openCardDetail = function (card) {
            if (!card) {
                return;
            }
            var pageName = root.getAttribute('data-page-name') || '';
            var existing = document.querySelector('.kanban-card-detail-overlay');
            if (existing && existing.parentNode) {
                existing.parentNode.removeChild(existing);
            }
            setHashCardId(card.id || '');
            var overlay = document.createElement('div');
            overlay.className = 'kanban-card-detail-overlay';
            overlay.style.position = 'fixed';
            overlay.style.inset = '0';
            overlay.style.background = 'rgba(9, 30, 66, 0.5)';
            overlay.style.zIndex = '1000';
            overlay.style.display = 'flex';
            overlay.style.alignItems = 'center';
            overlay.style.justifyContent = 'center';
            overlay.style.backdropFilter = 'blur(2px)';
            var modal = document.createElement('div');
            modal.style.width = 'min(640px, calc(100vw - 24px))';
            modal.style.maxHeight = 'calc(100vh - 24px)';
            modal.style.overflowY = 'auto';
            modal.style.background = '#fff';
            modal.style.borderRadius = '14px';
            modal.style.padding = '20px';
            modal.style.boxShadow = '0 20px 48px rgba(9, 30, 66, 0.28)';
            modal.style.border = '1px solid rgba(9, 30, 66, 0.12)';

            var header = document.createElement('div');
            header.style.display = 'flex';
            header.style.alignItems = 'flex-start';
            header.style.justifyContent = 'space-between';
            header.style.gap = '12px';

            var title = document.createElement('h3');
            title.textContent = card.text || '';
            title.style.cursor = 'pointer';
            title.title = 'Click to edit title';
            title.style.margin = '0';
            title.style.fontSize = '22px';
            title.style.lineHeight = '1.35';
            title.style.color = '#172b4d';

            var cardIdLabel = document.createElement('a');
            var cardId = (card.id || '').trim();
            if (cardId) {
                cardIdLabel.href = window.location.pathname + window.location.search + '#' + encodeURIComponent(cardId);
                cardIdLabel.textContent = cardId;
            } else {
                cardIdLabel.href = '#';
                cardIdLabel.textContent = '-';
            }
            cardIdLabel.style.marginTop = '4px';
            cardIdLabel.style.fontSize = '12px';
            cardIdLabel.style.color = '#6b778c';
            cardIdLabel.style.textDecoration = 'underline';
            cardIdLabel.title = 'Copy/share this link to reopen this card popup';

            var titleWrap = document.createElement('div');
            titleWrap.style.flex = '1 1 auto';
            titleWrap.appendChild(title);
            titleWrap.appendChild(cardIdLabel);

            var titleEditor = document.createElement('input');
            titleEditor.type = 'text';
            titleEditor.value = card.text || '';
            titleEditor.style.display = 'none';
            titleEditor.style.width = '100%';
            titleEditor.style.boxSizing = 'border-box';
            titleEditor.style.margin = '0 0 12px 0';
            titleEditor.style.border = '1px solid #b6c2cf';
            titleEditor.style.borderRadius = '8px';
            titleEditor.style.padding = '10px 12px';
            titleEditor.style.fontSize = '18px';
            titleEditor.style.fontWeight = '600';
            titleEditor.style.color = '#172b4d';

            var closeButton = document.createElement('button');
            closeButton.type = 'button';
            closeButton.innerHTML = '<i class="fas fa-times" aria-hidden="true"></i>';
            closeButton.style.border = 'none';
            closeButton.style.background = 'transparent';
            closeButton.style.cursor = 'pointer';
            closeButton.style.width = '32px';
            closeButton.style.height = '32px';
            closeButton.style.borderRadius = '50%';
            closeButton.style.fontSize = '18px';
            closeButton.style.color = '#44546f';

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
                card.comments.push(buildCommentEntry([
                    'Renamed Card Title',
                    buildCardLinkText(card.id, previousCardTitle) + ' to ' + buildCardLinkText(card.id, nextTitle)
                ]));
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
            textarea.style.boxSizing = 'border-box';
            textarea.style.border = '1px solid #b6c2cf';
            textarea.style.borderRadius = '8px';
            textarea.style.padding = '10px 12px';
            textarea.style.resize = 'vertical';
            textarea.style.fontSize = '14px';
            textarea.style.marginTop = '8px';

            var actionBar = document.createElement('div');
            actionBar.style.display = 'flex';
            actionBar.style.alignItems = 'center';
            actionBar.style.gap = '8px';
            actionBar.style.marginTop = '10px';

            var submit = document.createElement('button');
            submit.type = 'button';
            submit.innerHTML = '<i class="fas fa-comment-medical" aria-hidden="true"></i> Add Comment';
            submit.style.border = 'none';
            submit.style.borderRadius = '8px';
            submit.style.padding = '8px 12px';
            submit.style.background = '#0c66e4';
            submit.style.color = '#fff';
            submit.style.cursor = 'pointer';

            var deleteCardButton = document.createElement('button');
            deleteCardButton.type = 'button';
            deleteCardButton.innerHTML = '<i class="fas fa-trash-alt" aria-hidden="true"></i> Delete Card';
            deleteCardButton.style.border = '1px solid #f1b5b5';
            deleteCardButton.style.borderRadius = '8px';
            deleteCardButton.style.padding = '8px 12px';
            deleteCardButton.style.background = '#fff5f5';
            deleteCardButton.style.color = '#ae2a19';
            deleteCardButton.style.cursor = 'pointer';

            var propertyTitle = document.createElement('div');
            propertyTitle.innerHTML = '<i class="fas fa-tags" aria-hidden="true"></i> Properties';
            propertyTitle.style.marginTop = '16px';
            propertyTitle.style.fontWeight = '600';
            propertyTitle.style.fontSize = '14px';
            propertyTitle.style.color = '#44546f';

            var propertyList = document.createElement('div');
            propertyList.style.marginTop = '8px';
            var dueDateEditor = document.createElement('div');
            dueDateEditor.style.marginTop = '10px';
            dueDateEditor.style.display = 'flex';
            dueDateEditor.style.alignItems = 'center';
            dueDateEditor.style.gap = '8px';

            var dueDateInput = document.createElement('input');
            dueDateInput.type = 'date';

            var dueDateSaveButton = document.createElement('button');
            dueDateSaveButton.type = 'button';
            dueDateSaveButton.textContent = 'Save DueDate';

            var renderProperties = function () {
                propertyList.innerHTML = '';
                var propertyEntries = Object.keys(card.properties || {}).filter(function (key) {
                    return Array.isArray(card.properties[key]) && card.properties[key].length > 0;
                });
                if (propertyEntries.length === 0) {
                    var empty = document.createElement('div');
                    empty.textContent = 'No properties';
                    empty.style.color = '#6b778c';
                    empty.style.fontSize = '13px';
                    propertyList.appendChild(empty);
                    return;
                }

                propertyEntries.forEach(function (key) {
                    var values = card.properties[key] || [];
                    var row = document.createElement('div');
                    row.style.padding = '8px 10px';
                    row.style.border = '1px solid #eceff3';
                    row.style.borderRadius = '8px';
                    row.style.background = '#fafbfc';
                    row.style.marginBottom = '8px';

                    var label = document.createElement('div');
                    label.textContent = key;
                    label.style.fontWeight = '600';
                    label.style.fontSize = '13px';
                    label.style.marginBottom = '4px';
                    row.appendChild(label);

                    values.forEach(function (value) {
                        var valueRow = document.createElement('div');
                        valueRow.textContent = value;
                        valueRow.style.fontSize = '13px';
                        valueRow.style.color = '#172b4d';
                        valueRow.style.paddingLeft = '8px';
                        row.appendChild(valueRow);

                        requestRenderInlineComment(pageName, value).then(function (html) {
                            if (html) {
                                valueRow.innerHTML = html;
                            }
                        }).catch(function (error) {
                            console.error('[Kanban] failed to render property value', error);
                        });
                    });
                    propertyList.appendChild(row);
                });
                var dueDates = (card.properties && card.properties.DueDate) || [];
                dueDateInput.value = dueDates.length > 0 ? String(dueDates[0]).replace(/^\[|\]$/g, '') : '';
            };
            dueDateInput.style.border = '1px solid #b6c2cf';
            dueDateInput.style.borderRadius = '6px';
            dueDateInput.style.padding = '6px 8px';
            dueDateInput.style.fontSize = '13px';
            dueDateInput.style.color = '#172b4d';
            dueDateSaveButton.style.border = '1px solid #d0d7de';
            dueDateSaveButton.style.borderRadius = '6px';
            dueDateSaveButton.style.padding = '6px 10px';
            dueDateSaveButton.style.background = '#f6f8fa';
            dueDateSaveButton.style.color = '#172b4d';
            dueDateSaveButton.style.cursor = 'pointer';

            var submitDueDate = function () {
                var nextDueDate = (dueDateInput.value || '').trim();
                var previousDueDates = (card.properties && card.properties.DueDate) || [];
                var previousDueDate = previousDueDates.length > 0 ? String(previousDueDates[0]).replace(/^\[|\]$/g, '') : '';
                if (nextDueDate === previousDueDate) {
                    return;
                }
                card.structured = true;
                card.properties = card.properties || {};
                if (nextDueDate) {
                    card.properties.DueDate = ['[' + nextDueDate + ']'];
                } else {
                    delete card.properties.DueDate;
                }
                card.comments = card.comments || [];
                card.comments.push(buildCommentEntry(['Updated DueDate', (previousDueDate || '(none)') + ' to ' + (nextDueDate || '(none)')]));
                updateCardCommentCount(card);
                renderColumns();
                renderProperties();
                renderComments();
                enqueueMutation(function () {
                    return persistColumns('card:property:update', {
                        cardTitle: card.text || '',
                        property: 'DueDate',
                        value: nextDueDate || ''
                    }).catch(function (error) {
                        console.error('[Kanban] failed to save due date', error);
                    });
                });
            };

            var commentsTitle = document.createElement('div');
            commentsTitle.innerHTML = '<i class="fas fa-history" aria-hidden="true"></i> Activity';
            commentsTitle.style.marginTop = '16px';
            commentsTitle.style.fontWeight = '600';
            commentsTitle.style.fontSize = '14px';
            commentsTitle.style.color = '#44546f';

            var comments = document.createElement('div');
            comments.style.marginTop = '12px';
            var renderComments = function () {
                comments.innerHTML = '';
                (card.comments || []).slice().reverse().forEach(function (entry) {
                    if (!entry || !entry.header) {
                        return;
                    }

                    var row = document.createElement('div');
                    row.style.padding = '10px 12px';
                    row.style.border = '1px solid #eceff3';
                    row.style.borderRadius = '8px';
                    row.style.background = '#fafbfc';
                    row.style.marginBottom = '8px';
                    row.style.color = '#172b4d';
                    row.style.lineHeight = '1.4';

                    var header = document.createElement('div');
                    header.style.fontWeight = '600';
                    header.style.marginBottom = '6px';
                    row.appendChild(header);

                    requestRenderInlineComment(pageName, entry.header).then(function (html) {
                        if (html) {
                            header.innerHTML = html;
                        }
                    }).catch(function (error) {
                        console.error('[Kanban] failed to render comment header', error);
                        header.textContent = entry.header;
                    });

                    (entry.details || []).forEach(function (detailLine) {
                        var detailRow = document.createElement('div');
                        detailRow.style.paddingLeft = '12px';
                        detailRow.style.marginTop = '4px';
                        detailRow.textContent = detailLine;
                        row.appendChild(detailRow);

                        requestRenderInlineComment(pageName, detailLine).then(function (html) {
                            if (html) {
                                detailRow.innerHTML = html;
                            }
                        }).catch(function (error) {
                            console.error('[Kanban] failed to render comment detail', error);
                        });
                    });

                    comments.appendChild(row);
                });
            };

            var handleClipboardImagePaste = function (evt) {
                if (!evt || !evt.clipboardData || !evt.clipboardData.items) {
                    return;
                }
                var clipboardItems = Array.prototype.slice.call(evt.clipboardData.items || []);
                var imageItem = clipboardItems.find(function (item) {
                    return item && item.kind === 'file' && /^image\//i.test(item.type || '');
                });
                if (!imageItem) {
                    return;
                }

                evt.preventDefault();
                var file = imageItem.getAsFile();
                if (!file) {
                    return;
                }

                var pageName = root.getAttribute('data-page-name') || '';
                var reader = new FileReader();
                reader.onload = function (loadEvent) {
                    var dataUrl = loadEvent && loadEvent.target ? loadEvent.target.result : '';
                    if (!dataUrl) {
                        return;
                    }

                    requestUploadClipboardImage(pageName, dataUrl).then(function (payload) {
                        var macro = payload && payload.attachmentMacro ? payload.attachmentMacro : '';
                        if (!macro) {
                            throw new Error('Missing attachment macro.');
                        }
                        var commentText = macro;
                        card.structured = true;
                        card.properties = card.properties || {};
                        card.properties.Attachment = card.properties.Attachment || [];
                        card.properties.Attachment.push(commentText);
                        var commentEntry = buildCommentEntry([commentText]);
                        card.comments = card.comments || [];
                        card.comments.push(commentEntry);
                        updateCardCommentCount(card);
                        renderComments();
                        renderProperties();
                        renderColumns();
                        enqueueMutation(function () {
                            return persistColumns('card:comment:add', { cardTitle: card.text || '', comment: commentText }).catch(function (error) {
                                console.error('[Kanban] failed to save clipboard image comment', error);
                            });
                        });
                    }).catch(function (error) {
                        console.error('[Kanban] failed to upload clipboard image', error);
                        alert('Image upload failed. ' + (error && error.message ? error.message : ''));
                    });
                };
                reader.readAsDataURL(file);
            };

            textarea.addEventListener('paste', handleClipboardImagePaste);

            submit.addEventListener('click', function () {
                var body = (textarea.value || '').trim();
                if (!body) {
                    return;
                }
                var commentEntry = buildCommentEntry([body]);
                card.comments = card.comments || [];
                card.comments.push(commentEntry);
                updateCardCommentCount(card);
                textarea.value = '';
                renderComments();
                enqueueMutation(function () {
                    return persistColumns('card:comment:add', { cardTitle: card.text || '', comment: body }).catch(function (error) {
                        console.error('[Kanban] failed to save comments', error);
                    });
                });
            });
            dueDateSaveButton.addEventListener('click', submitDueDate);
            dueDateInput.addEventListener('keydown', function (evt) {
                if (evt.key === 'Enter') {
                    evt.preventDefault();
                    submitDueDate();
                }
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
                clearHashCardId(card.id || '');

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
                clearHashCardId(card.id || '');
            });
            closeButton.addEventListener('click', function () {
                if (overlay.parentNode) {
                    overlay.parentNode.removeChild(overlay);
                }
                clearHashCardId(card.id || '');
            });
            modal.addEventListener('click', function (evt) { evt.stopPropagation(); });
            header.appendChild(titleWrap);
            header.appendChild(closeButton);
            modal.appendChild(header);
            modal.appendChild(titleEditor);
            modal.appendChild(textarea);
            actionBar.appendChild(submit);
            actionBar.appendChild(deleteCardButton);
            dueDateEditor.appendChild(dueDateInput);
            dueDateEditor.appendChild(dueDateSaveButton);
            modal.appendChild(actionBar);
            modal.appendChild(propertyTitle);
            modal.appendChild(propertyList);
            modal.appendChild(dueDateEditor);
            modal.appendChild(commentsTitle);
            modal.appendChild(comments);
            overlay.appendChild(modal);
            document.body.appendChild(overlay);
            renderProperties();
            renderComments();
        };
        openCardDetailById = function (cardId) {
            if (!cardId) {
                return false;
            }
            var targetCard = null;
            columns.some(function (column) {
                return (column.cards || []).some(function (card) {
                    if (card && card.id === cardId) {
                        targetCard = card;
                        return true;
                    }
                    return false;
                });
            });
            if (!targetCard) {
                return false;
            }
            root.scrollIntoView({ behavior: 'smooth', block: 'start' });
            openCardDetail(targetCard);
            return true;
        };
        var initialHashCardId = getHashCardId();
        if (initialHashCardId) {
            openCardDetailById(initialHashCardId);
        }

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
