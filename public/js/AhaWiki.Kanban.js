document.addEventListener('DOMContentLoaded', function () {
    var kanbanInterpreters = document.querySelectorAll('.InterpreterKanban');
    var getHashCardId = function () {
        var rawHash = (window.location.hash || '').replace(/^#/, '').trim();
        try {
            return decodeURIComponent(rawHash);
        } catch (error) {
            return rawHash;
        }
    };
    var replaceCurrentUrl = function (url) {
        if (window.history && window.history.replaceState) {
            window.history.replaceState(null, '', url);
            return;
        }
        if (window.location && window.location.replace) {
            window.location.replace(url);
        }
    };
    var setHashCardId = function (cardId) {
        if (!cardId) {
            return;
        }
        if (getHashCardId() === cardId) {
            return;
        }
        var nextHash = '#' + encodeURIComponent(cardId);
        replaceCurrentUrl(window.location.pathname + window.location.search + nextHash);
    };
    var clearHashCardId = function (cardId) {
        var currentHash = getHashCardId();
        if (!currentHash || (cardId && currentHash !== cardId)) {
            return;
        }
        replaceCurrentUrl(window.location.pathname + window.location.search);
    };
    var getCurrentPageUrl = function () {
        var path = window.location.pathname + window.location.search;
        if (window.location.origin) {
            return window.location.origin + path;
        }
        var href = String(window.location.href || '');
        if (href) {
            return href.replace(/#.*$/, '');
        }
        return path;
    };
    var buildCardUrl = function (cardId) {
        if (!cardId) {
            return getCurrentPageUrl();
        }
        return getCurrentPageUrl() + '#' + encodeURIComponent(cardId);
    };
    var fallbackCopyTextToClipboard = function (text) {
        if (!document.body || !document.createElement || !document.execCommand) {
            return Promise.resolve(false);
        }
        var textarea = document.createElement('textarea');
        textarea.value = text;
        textarea.setAttribute('readonly', 'readonly');
        textarea.style.position = 'fixed';
        textarea.style.top = '-9999px';
        textarea.style.opacity = '0';
        document.body.appendChild(textarea);
        textarea.focus();
        textarea.select();
        var copied = false;
        try {
            copied = document.execCommand('copy');
        } catch (error) {
            copied = false;
        }
        if (textarea.parentNode) {
            textarea.parentNode.removeChild(textarea);
        }
        return Promise.resolve(copied);
    };
    var copyTextToClipboard = function (text) {
        if (!text) {
            return Promise.resolve(false);
        }
        if (window.navigator && window.navigator.clipboard && window.navigator.clipboard.writeText) {
            return window.navigator.clipboard.writeText(text).then(function () {
                return true;
            }).catch(function () {
                return fallbackCopyTextToClipboard(text);
            });
        }
        return fallbackCopyTextToClipboard(text);
    };
    var parseBooleanDataAttribute = function (value, fallback) {
        if (value === 'true') {
            return true;
        }
        if (value === 'false') {
            return false;
        }
        return fallback;
    };
    var isKanbanRootWritable = function (root) {
        if (!root) {
            return true;
        }
        var rootValue = root.getAttribute('data-wiki-writable');
        if (rootValue === 'true' || rootValue === 'false') {
            return parseBooleanDataAttribute(rootValue, true);
        }
        var wikiContent = root.closest ? root.closest('[data-wiki-writable]') : null;
        return parseBooleanDataAttribute(wikiContent ? wikiContent.getAttribute('data-wiki-writable') : null, true);
    };

    var generateCardId = function () {
        return 'c' + Date.now().toString(36) + Math.random().toString(36).slice(2, 8);
    };
    var getCurrentRevision = function () {
        var node = document.querySelector('.revision a');
        var revision = Number(node ? node.textContent : 0);
        return Number.isFinite(revision) ? revision : 0;
    };
    var setCurrentRevision = function (nextRevision) {
        var node = document.querySelector('.revision a');
        if (!node || !Number.isFinite(nextRevision) || nextRevision < 0) {
            return;
        }
        node.textContent = String(nextRevision);
    };
    var fetchLatestRevision = function (pageName) {
        return fetch('/api/pageRevision/' + encodeURIComponent(pageName), { credentials: 'same-origin' })
            .then(function (response) { return response.json().catch(function () { return {}; }); })
            .then(function (payload) {
                var revision = Number(payload && payload.revision);
                return Number.isFinite(revision) ? revision : 0;
            });
    };
    var showAlert = function (message) {
        if (typeof window !== 'undefined' && typeof window.alert === 'function') {
            window.alert(message);
            return;
        }
        if (typeof alert === 'function') {
            alert(message);
        }
    };
    var showKanbanConflictToast = function () {
        try {
            var body = typeof document !== 'undefined' && document.body;
            if (!body) { return; }
            if (document.querySelector && document.querySelector('.kanban-conflict-toast')) { return; }
            var toast = document.createElement('div');
            toast.className = 'kanban-conflict-toast';
            toast.setAttribute('role', 'alert');
            var msg = document.createElement('span');
            msg.className = 'kanban-conflict-toast-message';
            msg.textContent = 'Page was modified by another session. Changes could not be saved. Reload to see the latest version.';
            var closeBtn = document.createElement('button');
            closeBtn.type = 'button';
            closeBtn.className = 'kanban-conflict-toast-close';
            closeBtn.setAttribute('aria-label', 'Close');
            closeBtn.textContent = '×';
            var timer;
            var remove = function () {
                clearTimeout(timer);
                if (toast.parentNode) { toast.parentNode.removeChild(toast); }
            };
            closeBtn.addEventListener('click', remove);
            toast.appendChild(msg);
            toast.appendChild(closeBtn);
            body.appendChild(toast);
            timer = setTimeout(remove, 15000);
        } catch (e) {
            // fail silently in test/non-DOM environments
        }
    };
    var cloneCardData = function (card) {
        if (!card) { return null; }
        try {
            return {
                id: card.id || '',
                text: card.text || '',
                classNames: (card.classNames || []).slice(),
                lineNumber: card.lineNumber || 0,
                description: JSON.parse(JSON.stringify(card.description || [])),
                comments: JSON.parse(JSON.stringify(card.comments || [])),
                properties: JSON.parse(JSON.stringify(card.properties || {}))
            };
        } catch (e) {
            return { id: card.id || '', text: card.text || '', classNames: [], lineNumber: 0, description: [], comments: [], properties: {} };
        }
    };
    var deepCloneColumns = function (cols) {
        return (cols || []).map(function (col) {
            return {
                title: col.title || '',
                lineNumber: col.lineNumber || 0,
                cards: (col.cards || []).map(cloneCardData).filter(Boolean)
            };
        });
    };
    var cardDataEqual = function (a, b) {
        // comments(Activity)는 항상 별도 병합하므로 여기서 비교하지 않음
        if (!a || !b) { return a === b; }
        if (a.id !== b.id || a.text !== b.text) { return false; }
        try {
            return JSON.stringify([a.classNames || [], a.description || [], a.properties || {}]) ===
                   JSON.stringify([b.classNames || [], b.description || [], b.properties || {}]);
        } catch (e) { return false; }
    };
    var mergeComments = function (serverComments, localComments) {
        // server와 local 댓글을 header 기준으로 유니온 병합 (순서: server 우선, 이후 local-only)
        var seen = Object.create(null);
        var result = [];
        (serverComments || []).forEach(function (c) {
            if (c && c.header && !seen[c.header]) { seen[c.header] = true; result.push(c); }
        });
        (localComments || []).forEach(function (c) {
            if (c && c.header && !seen[c.header]) { seen[c.header] = true; result.push(c); }
        });
        return result;
    };
    var findKanbanBlockInRaw = function (rawText, hintLineStart) {
        var lines = (rawText || '').split(/\r?\n/);
        var blocks = [];
        var i, j;
        for (i = 0; i < lines.length; i++) {
            var trimmed = (lines[i] || '').trim();
            var isWholePageShebang = (i === 0) && /^#!kanban(\s|$)/i.test(trimmed);
            var isEmbeddedShebang = /^\[\[\[#!kanban(\s|$)/i.test(trimmed);
            if (!isWholePageShebang && !isEmbeddedShebang) { continue; }
            var contentStart1 = i + 2;
            var blockEnd1 = lines.length + 1;
            for (j = i + 1; j < lines.length; j++) {
                if (/^\]\]\]/.test(lines[j])) { blockEnd1 = j + 1; break; }
            }
            blocks.push({
                interpreterLineStart: contentStart1,
                lineEnd: blockEnd1,
                contentText: lines.slice(contentStart1 - 1, blockEnd1 - 1).join('\n')
            });
            i = blockEnd1 - 2;
        }
        var k;
        for (k = 0; k < blocks.length; k++) {
            if (hintLineStart >= blocks[k].interpreterLineStart && hintLineStart < blocks[k].lineEnd) {
                return blocks[k];
            }
        }
        if (blocks.length > 0) {
            var closest = blocks[0], closestDist = Math.abs(blocks[0].interpreterLineStart - hintLineStart);
            for (k = 1; k < blocks.length; k++) {
                var d = Math.abs(blocks[k].interpreterLineStart - hintLineStart);
                if (d < closestDist) { closest = blocks[k]; closestDist = d; }
            }
            return closest;
        }
        return null;
    };
    var getLineCountForText = function (value) {
        if (!value) {
            return 0;
        }
        var rows = String(value).split(/\r?\n/);
        while (rows.length > 0 && rows[rows.length - 1] === '') {
            rows.pop();
        }
        return rows.length;
    };
    var getActionMetaValue = function (actionMeta, key) {
        if (!actionMeta || typeof actionMeta !== 'object') {
            return '';
        }
        var value = actionMeta[key];
        return value === null || typeof value === 'undefined' ? '' : String(value).trim();
    };
    var shortenCardCommentForRevision = function (commentText) {
        var raw = String(commentText || '').trim();
        if (!raw) {
            return '';
        }
        var firstLine = raw.split(/\r?\n/)[0].trim();
        var base = firstLine.length <= 80 ? firstLine : firstLine.slice(0, 80);
        if (base.length < firstLine.length) {
            return base + '...';
        }
        return base;
    };
    var serializePropertyValueForRevision = function (value) {
        if (Array.isArray(value)) {
            var filtered = value.map(function (item) { return String(item || '').trim(); }).filter(Boolean);
            return filtered.length > 0 ? filtered.join(', ') : '(empty)';
        }
        var text = String(value || '').trim();
        return text || '(empty)';
    };
    var truncateRevisionText = function (value, maxLength) {
        var text = String(value || '').trim();
        if (!text) {
            return '';
        }
        var limit = Number.isFinite(maxLength) && maxLength > 0 ? Math.floor(maxLength) : 80;
        if (text.length <= limit) {
            return text;
        }
        return text.slice(0, limit) + '...';
    };

    var buildKanbanSaveComment = function (actionType, actionMeta) {
        var pageName = getActionMetaValue(actionMeta, 'pageName');
        var cardId = getActionMetaValue(actionMeta, 'cardId');
        var cardTitle = getActionMetaValue(actionMeta, 'cardTitle');
        switch (actionType) {
            case 'list:add': return `Kanban - '''${truncateRevisionText(getActionMetaValue(actionMeta, 'listTitle'), 60)}''' added`;
            case 'list:rename': return `Kanban - '''${truncateRevisionText(getActionMetaValue(actionMeta, 'fromTitle'), 60)}''' renamed to '''${truncateRevisionText(getActionMetaValue(actionMeta, 'toTitle'), 60)}'''`;
            case 'list:move': return `Kanban - '''${truncateRevisionText(getActionMetaValue(actionMeta, 'listTitle'), 60)}''' moved - ${getActionMetaValue(actionMeta, 'fromOrder')} to ${getActionMetaValue(actionMeta, 'toOrder')}`;
            case 'list:delete': return `Kanban - '''${truncateRevisionText(getActionMetaValue(actionMeta, 'listTitle'), 60)}''' deleted`;
            case 'card:add': return `Kanban - ${buildCardLinkText(pageName, cardId, truncateRevisionText(cardTitle, 60))} added`;
            case 'card:rename': return `Kanban - ${buildCardLinkText(pageName, cardId, truncateRevisionText(getActionMetaValue(actionMeta, 'toTitle'), 60))} renamed - '''${truncateRevisionText(getActionMetaValue(actionMeta, 'fromTitle'), 60)}'''`;
            case 'card:move':
                if (truncateRevisionText(getActionMetaValue(actionMeta, 'fromList'), 60) && truncateRevisionText(getActionMetaValue(actionMeta, 'toList'), 60) && truncateRevisionText(getActionMetaValue(actionMeta, 'fromList'), 60) !== truncateRevisionText(getActionMetaValue(actionMeta, 'toList'), 60)) {
                    return `Kanban - ${buildCardLinkText(pageName, cardId, truncateRevisionText(cardTitle, 60))} moved - '''${truncateRevisionText(getActionMetaValue(actionMeta, 'fromList'), 60)}''' to '''${truncateRevisionText(getActionMetaValue(actionMeta, 'toList'), 60)}'''`;
                }
                return `Kanban - ${buildCardLinkText(pageName, cardId, truncateRevisionText(cardTitle, 60))} moved - ${getActionMetaValue(actionMeta, 'fromOrder')} to ${getActionMetaValue(actionMeta, 'toOrder')}`;
            case 'card:delete': return `Kanban - ${buildCardLinkText(pageName, cardId, truncateRevisionText(cardTitle, 60))} deleted`;
            case 'card:comment:add': return `Kanban - ${buildCardLinkText(pageName, cardId, truncateRevisionText(cardTitle, 60))} commented - ${shortenCardCommentForRevision(getActionMetaValue(actionMeta, 'comment'))}`;
            case 'card:property:update': return `Kanban - ${buildCardLinkText(pageName, cardId, truncateRevisionText(cardTitle, 60))} updated - ${truncateRevisionText(getActionMetaValue(actionMeta, 'property'), 40)} - ${serializePropertyValueForRevision(actionMeta && actionMeta.value)}`;
            case 'card:description:update': return `Kanban - ${buildCardLinkText(pageName, cardId, truncateRevisionText(cardTitle, 60))} updated - ${truncateRevisionText(getActionMetaValue(actionMeta, 'descriptionPreview'), 60) || '(removed)'}`;
        }
        return `Kanban - ${actionType || 'Save'}`;
    };
    if (typeof window !== 'undefined') {
        window.__AhaWikiKanbanTestHooks = Object.assign({}, window.__AhaWikiKanbanTestHooks || {}, {
            getActionMetaValue: getActionMetaValue,
            shortenCardCommentForRevision: shortenCardCommentForRevision,
            serializePropertyValueForRevision: serializePropertyValueForRevision,
            truncateRevisionText: truncateRevisionText,
            buildKanbanSaveComment: buildKanbanSaveComment,
            getHashCardId: getHashCardId,
            setHashCardId: setHashCardId,
            clearHashCardId: clearHashCardId,
            buildCardUrl: buildCardUrl,
            copyTextToClipboard: copyTextToClipboard,
            isKanbanRootWritable: isKanbanRootWritable,
            requestSaveKanban: requestSaveKanban
        });
    }

    var extractActivityDetailFromRevisionComment = function (comment) {
        var raw = String(comment || '').trim();
        var marker = ' - ';
        var first = raw.indexOf(marker);
        if (first < 0) { return raw; }
        return raw.slice(first + marker.length).trim();
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


    var requestUploadAttachmentFile = function (pageName, file) {
        if (!pageName || !file) {
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
            var formData = new FormData();
            formData.append('csrfToken', tokenValue);
            formData.append('pageName', pageName);
            formData.append('file', file, file.name || 'attachment');

            return fetch('/api/uploadAttachment', {
                method: 'POST',
                credentials: 'same-origin',
                headers: {
                    'Csrf-Token': tokenValue,
                    'X-CSRF-Token': tokenValue
                },
                body: formData
            }).then(function (response) {
                return response.json().catch(function () {
                    return {};
                }).then(function (payload) {
                    if (!response.ok) {
                        throw new Error(payload.error || payload.message || 'Failed to upload attachment.');
                    }
                    return payload;
                });
            });
        });
    };


    var attachFilesToCard = function (pageName, card, files) {
        if (!pageName || !card || !Array.isArray(files) || files.length === 0) {
            return Promise.resolve(false);
        }
        return Promise.all(files.map(function (file) {
            return requestUploadAttachmentFile(pageName, file).then(function (payload) {
                var macro = payload && payload.attachmentMacro ? payload.attachmentMacro : '';
                if (!macro) {
                    throw new Error('Missing attachment macro.');
                }
                card.properties = card.properties || {};
                card.properties.Attachment = card.properties.Attachment || [];
                card.properties.Attachment.push(macro);
                card.comments = card.comments || [];
                card.comments.unshift(buildCommentEntry([macro]));
            });
        })).then(function () { return true; });
    };

    var extractAttachmentObjectKeyFromMacro = function (macro) {
        var raw = String(macro || '').trim();
        if (!raw) {
            return '';
        }
        var match = raw.match(/^\[\[Attachment\((.+)\)\]\]$/);
        if (!match || !match[1]) {
            return '';
        }
        return match[1].trim();
    };

    var extractCardAttachmentObjectKeys = function (card) {
        if (!card || !card.properties || !Array.isArray(card.properties.Attachment)) {
            return [];
        }
        var objectKeys = card.properties.Attachment
            .map(extractAttachmentObjectKeyFromMacro)
            .filter(function (objectKey) { return Boolean(objectKey); });
        return Array.from(new Set(objectKeys));
    };

    var requestDeleteAttachmentObject = function (pageName, objectKey) {
        if (!pageName || !objectKey) {
            return Promise.resolve(false);
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
            params.append('csrfToken', tokenValue);
            params.append('pageName', pageName);
            params.append('objectKey', objectKey);

            return fetch('/api/deleteAttachment', {
                method: 'POST',
                credentials: 'same-origin',
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8',
                    'Csrf-Token': tokenValue,
                    'X-CSRF-Token': tokenValue
                },
                body: params.toString()
            }).then(function (response) {
                if (!response.ok) {
                    return response.text().then(function (bodyText) {
                        throw new Error(bodyText || ('Failed to delete attachment. status=' + response.status));
                    });
                }
                return true;
            });
        });
    };

    var removeAttachmentFromCardProperty = function (card, attachmentMacro) {
        if (!card || !card.properties || !Array.isArray(card.properties.Attachment)) {
            return false;
        }
        var nextAttachments = card.properties.Attachment.filter(function (value) {
            return String(value || '') !== String(attachmentMacro || '');
        });
        if (nextAttachments.length === card.properties.Attachment.length) {
            return false;
        }
        if (nextAttachments.length > 0) {
            card.properties.Attachment = nextAttachments;
        } else {
            delete card.properties.Attachment;
        }
        return true;
    };

    var createCardsFromFiles = function (pageName, list, files) {
        if (!pageName || !list || !Array.isArray(files) || files.length === 0) {
            return Promise.resolve([]);
        }
        return Promise.all(files.map(function (file) {
            return requestUploadAttachmentFile(pageName, file).then(function (payload) {
                var macro = payload && payload.attachmentMacro ? payload.attachmentMacro : '';
                if (!macro) {
                    throw new Error('Missing attachment macro.');
                }
                var fileName = (file && file.name ? file.name : '').trim() || 'Untitled';
                var newCard = {
                    id: generateCardId(),
                    text: fileName,
                    comments: [buildCommentEntry([macro])],
                    properties: {
                        Creator: ['[User:' + getCurrentAuthor() + ']'],
                        dateCreated: [formatKanbanDateTime(getNowIsoWithoutMillis())],
                        Attachment: [macro]
                    }
                };
                prependCardActivity(newCard, [extractActivityDetailFromRevisionComment(buildKanbanSaveComment('card:add', { eventPrefix: 'User:' + getCurrentAuthor(), cardId: newCard.id, cardTitle: newCard.text || '' }))]);
                return newCard;
            });
        })).then(function (cards) {
            list.cards = list.cards || [];
            cards.forEach(function (newCard) { list.cards.push(newCard); });
            return cards;
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
                return fetch('/api/renderAhaMark/' + encodeURIComponent(pageName), {
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


    var clampRenderedInlineImages = function (container) {
        if (!container || !container.querySelectorAll) {
            return;
        }
        Array.prototype.forEach.call(container.querySelectorAll('img'), function (img) {
            img.style.maxWidth = '100%';
            img.style.height = 'auto';
            img.style.maxHeight = '320px';
            img.style.objectFit = 'contain';
        });
    };

    var openKanbanImageLightbox = function (imageUrl, imageAlt) {
        if (!imageUrl) {
            return;
        }
        var existing = document.querySelector('.kanban-image-lightbox-overlay');
        if (existing && existing.parentNode) {
            existing.parentNode.removeChild(existing);
        }
        var overlay = document.createElement('div');
        overlay.className = 'kanban-image-lightbox-overlay';
        overlay.style.position = 'fixed';
        overlay.style.left = '0';
        overlay.style.top = '0';
        overlay.style.right = '0';
        overlay.style.bottom = '0';
        overlay.style.background = 'var(--color-overlay-bg)';
        overlay.style.zIndex = '10001';
        overlay.style.display = 'flex';
        overlay.style.alignItems = 'center';
        overlay.style.justifyContent = 'center';
        overlay.style.padding = '24px';
        overlay.style.cursor = 'zoom-out';

        var image = document.createElement('img');
        image.src = imageUrl;
        image.alt = imageAlt || '';
        image.style.maxWidth = 'min(92vw, 1400px)';
        image.style.maxHeight = '92vh';
        image.style.width = 'auto';
        image.style.height = 'auto';
        image.style.objectFit = 'contain';
        image.style.borderRadius = '8px';
        image.style.boxShadow = '0 12px 36px var(--color-shadow-soft)';

        overlay.appendChild(image);
        overlay.addEventListener('click', function () {
            if (overlay.parentNode) {
                overlay.parentNode.removeChild(overlay);
            }
        });
        document.body.appendChild(overlay);
    };

    var enableInlineImageLightbox = function (container) {
        if (!container || !container.querySelectorAll) {
            return;
        }
        Array.prototype.forEach.call(container.querySelectorAll('img'), function (img) {
            var sourceUrl = img.currentSrc || img.src || '';
            if (!sourceUrl || img.getAttribute('data-kanban-lightbox-bound') === '1') {
                return;
            }
            img.setAttribute('data-kanban-lightbox-bound', '1');
            if (!img.style.cursor) {
                img.style.cursor = 'zoom-in';
            }
            img.addEventListener('click', function (evt) {
                evt.preventDefault();
                evt.stopPropagation();
                openKanbanImageLightbox(sourceUrl, img.alt || 'Attachment');
            });
        });
    };

    var enhanceAttachmentPropertyPreview = function (container) {
        if (!container || !container.querySelectorAll) {
            return;
        }
        Array.prototype.forEach.call(container.querySelectorAll('img'), function (img) {
            var sourceUrl = img.currentSrc || img.src || '';
            img.style.width = '120px';
            img.style.maxWidth = '100%';
            img.style.height = '80px';
            img.style.maxHeight = '80px';
            img.style.objectFit = 'cover';
            img.style.borderRadius = '6px';
            img.style.border = '1px solid var(--kanban-border)';
            img.style.cursor = sourceUrl ? 'zoom-in' : 'default';
            img.style.background = 'var(--kanban-card-bg)';
            img.style.padding = '0';
            img.style.margin = '0';
        });
        container.style.display = 'grid';
        container.style.gridTemplateColumns = 'repeat(auto-fill, minmax(120px, 1fr))';
        container.style.gap = '8px';
        container.style.paddingLeft = '8px';
        enableInlineImageLightbox(container);
    };

    var requestSaveKanban = function (pageName, lineStart, lineEnd, content, actionType, actionMeta, retryCount) {
        if (!pageName) {
            return Promise.resolve(null);
        }
        var attempt = Number.isFinite(retryCount) ? retryCount : 0;
        var knownRevision = getCurrentRevision();
        if ((!Number.isFinite(knownRevision) || knownRevision <= 0) && attempt < 1) {
            return fetchLatestRevision(pageName).then(function (latestRevision) {
                setCurrentRevision(latestRevision);
                return requestSaveKanban(pageName, lineStart, lineEnd, content, actionType, actionMeta, attempt);
            });
        }
        return fetch('/api/csrf', { credentials: 'same-origin' })
            .then(function (csrfResponse) { return csrfResponse.json().catch(function () { return {}; }); })
            .then(function (csrfToken) {
                var tokenValue = csrfToken && csrfToken.value ? csrfToken.value : '';
                var actionMetaWithPageName = Object.assign({ pageName: pageName }, actionMeta || {});
                var comment = buildKanbanSaveComment(actionType, actionMetaWithPageName);
                var params = new URLSearchParams();
                params.set('revision', String(getCurrentRevision()));
                params.set('text', content || '');
                params.set('comment', comment);
                params.set('minorEdit', 'false');
                params.set('recaptcha', '');
                params.set('lineStart', String(lineStart));
                params.set('lineEnd', String(lineEnd));
                params.set('saveSenderId', getOrCreateSaveSenderId());
                return fetch('/w/' + encodeURIComponent(pageName), {
                    method: 'POST',
                    credentials: 'same-origin',
                    headers: {
                        'Content-Type': 'application/x-www-form-urlencoded; charset=UTF-8',
                        'Csrf-Token': tokenValue,
                        'X-CSRF-Token': tokenValue
                    },
                    body: params.toString()
                }).then(function (response) {
                    if (!response.ok) {
                        if (response.status === 409) {
                            if (attempt < 3) {
                                return fetchLatestRevision(pageName).then(function (latestRevision) {
                                    setCurrentRevision(latestRevision);
                                    return requestSaveKanban(pageName, lineStart, lineEnd, content, actionType, actionMeta, attempt + 1);
                                });
                            }
                            showKanbanConflictToast();
                            throw new Error('Conflict: save failed after retries.');
                        }
                        showAlert('Failed to save. (status=' + response.status + ')');
                        throw new Error('Failed to save kanban. status=' + response.status);
                    }
                    setCurrentRevision(getCurrentRevision() + 1);
                    return { lineEnd: lineStart + getLineCountForText(content || '') };
                });
            });
    };


    if (typeof window !== 'undefined' && window.__AhaWikiKanbanTestHooks) {
        window.__AhaWikiKanbanTestHooks.requestSaveKanban = requestSaveKanban;
    }

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

    var getCurrentAuthor = function () {
        return window.AhaWikiCurrentUserNickname || 'Anonymous';
    };

    var SENDER_ID_COOKIE_NAME = 'ahaWikiEditorSenderId';
    var SENDER_ID_TAB_STORAGE_KEY = 'ahaWikiEditorTabSenderId';
    var SENDER_ID_COOKIE_MAX_AGE_SECONDS = 365 * 24 * 60 * 60;

    var createSenderId = function () {
        return (window.crypto && window.crypto.randomUUID)
            ? window.crypto.randomUUID()
            : ('sender-' + Date.now() + '-' + Math.random().toString(36).slice(2));
    };

    var readCookie = function (name) {
        var escaped = String(name || '').replace(/[-/\^$*+?.()|[\]{}]/g, '\\$&');
        var matched = document.cookie.match(new RegExp('(?:^|; )' + escaped + '=([^;]*)'));
        return matched ? decodeURIComponent(matched[1]) : '';
    };

    var writeCookie = function (name, value, maxAgeSeconds) {
        document.cookie = name + '=' + encodeURIComponent(value) + '; Path=/; Max-Age=' + String(maxAgeSeconds) + '; SameSite=Lax';
    };

    var getOrCreateSaveSenderId = function () {
        try {
            var browserId = readCookie(SENDER_ID_COOKIE_NAME);
            if (!browserId) {
                browserId = createSenderId();
            }
            writeCookie(SENDER_ID_COOKIE_NAME, browserId, SENDER_ID_COOKIE_MAX_AGE_SECONDS);

            var tabId = window.sessionStorage.getItem(SENDER_ID_TAB_STORAGE_KEY);
            if (!tabId) {
                tabId = createSenderId();
                window.sessionStorage.setItem(SENDER_ID_TAB_STORAGE_KEY, tabId);
            }
            return browserId + ':' + tabId;
        } catch (e) {
            return createSenderId() + ':' + createSenderId();
        }
    };
    var toUserLinkMarkup = function (author) {
        var safeAuthor = (author || '').trim();
        if (!safeAuthor) {
            return '';
        }
        if (/^\[User:[^\]]+\]$/.test(safeAuthor)) {
            return safeAuthor;
        }
        return '[User:' + safeAuthor + ']';
    };
    var getNowIsoWithoutMillis = function () {
        return new Date().toISOString().replace(/\.\d{3}Z$/, 'Z');
    };

    var formatKanbanDateTime = function (isoDateTime) {
        var value = (isoDateTime || '').trim();
        if (!value) {
            return '';
        }
        if (value.length <= 10) {
            return '[' + value + ']';
        }
        return '[' + value.slice(0, 10) + ']' + value.slice(10);
    };


    var toClientKanbanDateTime = function (value) {
        var raw = (value || '').trim();
        var matched = raw.match(/^\[(\d{4}-\d{2}-\d{2})\]T(\d{2}:\d{2}:\d{2})(Z|[+-]\d{2}:\d{2})$/);
        if (!matched) {
            return raw;
        }
        var date = new Date(matched[1] + 'T' + matched[2] + matched[3]);
        if (Number.isNaN(date.getTime())) {
            return raw;
        }
        var pad = function (n) { return String(n).padStart(2, '0'); };
        return '[' + date.getFullYear() + '-' + pad(date.getMonth() + 1) + '-' + pad(date.getDate()) + ']T' + pad(date.getHours()) + ':' + pad(date.getMinutes()) + ':' + pad(date.getSeconds());
    };

    var toClientKanbanCommentHeader = function (header) {
        var raw = (header || '').trim();
        var matched = raw.match(/^(\[User:[^\]]+\]\s+)\[(\d{4}-\d{2}-\d{2})\]T(\d{2}:\d{2}:\d{2})(Z|[+-]\d{2}:\d{2})$/);
        if (!matched) {
            return raw;
        }
        return matched[1] + toClientKanbanDateTime('[' + matched[2] + ']T' + matched[3] + matched[4]);
    };

    // Escape newlines in comment details for safe storage
    var escapeCommentNewlines = function (text) {
        return String(text || '').replace(/\r?\n/g, '[[Br]]');
    };
    // Restore newlines from escaped comment details in UI/rendering
    var restoreCommentNewlines = function (text) {
        return String(text || '').replace(/\[\[Br\]\]/g, '\n');
    };

    var buildCommentEntry = function (details) {
        var nowIso = getNowIsoWithoutMillis();
        var author = getCurrentAuthor();
        var escapedDetails = (details || [])
            .map(function (item) { return escapeCommentNewlines(item); })
            .filter(function (item) { return Boolean((item || '').trim()); });
        return {
            header: '[User:' + author + '] ' + formatKanbanDateTime(nowIso),
            details: escapedDetails
        };
    };
    var buildCardLinkText = function (pageName, cardId, cardName) {
        var safePageName = (pageName || '').trim();
        var safeCardName = (cardName || '').trim();
        var safeCardId = (cardId || '').trim();
        if (!safeCardId) {
            return '["' + safePageName + '#" ' + safeCardName + ']';
        }
        return '["' + safePageName + '#' + safeCardId + '" ' + safePageName + '#' + safeCardName + ']';
    };

    var CARD_RELATION_PROPERTY_KEYS = ['BlockedBy', 'ParentCard', 'RelatedCard'];
    var DERIVED_CARD_RELATION_PROPERTIES = [
        { key: 'Blocks', sourceKey: 'BlockedBy' },
        { key: 'SubCards', sourceKey: 'ParentCard' }
    ];

    var isCardRelationProperty = function (key) {
        return CARD_RELATION_PROPERTY_KEYS.indexOf(key) >= 0;
    };

    var sanitizePropertyLabel = function (value) {
        return String(value || '').replace(/\r?\n/g, ' ').replace(/\]/g, '').trim();
    };

    var normalizeCardIdToken = function (value) {
        var token = String(value || '').trim().replace(/^#/, '');
        if (!token || /[#.\s\[\]]/.test(token)) {
            return '';
        }
        return token;
    };

    var parseCardReferenceValue = function (value) {
        var raw = String(value || '').trim();
        var matched;
        if (!raw) {
            return null;
        }
        matched = raw.match(/^\[\s*#([^#.\s\[\]]+)(?:\s+([^\]]+))?\s*\]$/);
        if (matched) {
            return {
                id: normalizeCardIdToken(matched[1]),
                label: sanitizePropertyLabel(matched[2] || '')
            };
        }
        matched = raw.match(/^\[\s*"#([^#.\s\[\]"]+)"(?:\s+([^\]]+))?\s*\]$/);
        if (matched) {
            return {
                id: normalizeCardIdToken(matched[1]),
                label: sanitizePropertyLabel(matched[2] || '')
            };
        }
        matched = raw.match(/^#?([^#.\s\[\]]+)$/);
        if (matched) {
            return {
                id: normalizeCardIdToken(matched[1]),
                label: ''
            };
        }
        return null;
    };

    var buildCardReferenceValue = function (cardId, label) {
        var safeCardId = normalizeCardIdToken(cardId);
        var safeLabel = sanitizePropertyLabel(label);
        if (!safeCardId) {
            return '';
        }
        return safeLabel ? '[#' + safeCardId + ' ' + safeLabel + ']' : '[#' + safeCardId + ']';
    };

    var normalizeCardReferenceValue = function (value, fallbackLabel) {
        var parsed = parseCardReferenceValue(value);
        if (!parsed || !parsed.id) {
            return '';
        }
        return buildCardReferenceValue(parsed.id, parsed.label || fallbackLabel || '');
    };

    var parseAssigneeValue = function (value) {
        var raw = String(value || '').trim();
        var matched;
        if (!raw) {
            return null;
        }
        matched = raw.match(/^\[User:([^\]]+)\]$/);
        if (matched) {
            return { name: matched[1].trim() };
        }
        matched = raw.match(/^User:(.+)$/);
        if (matched) {
            return { name: matched[1].trim() };
        }
        matched = raw.match(/^\[([^\]]+)\]$/);
        if (matched) {
            return parseAssigneeValue(matched[1]);
        }
        return { name: raw };
    };

    var normalizeAssigneeValue = function (value) {
        var parsed = parseAssigneeValue(value);
        if (!parsed || !parsed.name) {
            return '';
        }
        return toUserLinkMarkup(parsed.name);
    };

    var getAssigneeName = function (value) {
        var parsed = parseAssigneeValue(value);
        return parsed && parsed.name ? parsed.name : '';
    };

    var getCardPropertyValues = function (card, key) {
        var raw = card && card.properties ? card.properties[key] : [];
        if (Array.isArray(raw)) {
            return raw.map(function (value) { return String(value || '').trim(); }).filter(Boolean);
        }
        var text = String(raw || '').trim();
        return text ? [text] : [];
    };

    var buildCardIndex = function (columns) {
        var byId = {};
        var cards = [];
        (columns || []).forEach(function (column, columnIndex) {
            (column.cards || []).forEach(function (card, cardIndex) {
                var id = card && card.id ? String(card.id).trim() : '';
                if (!id) {
                    return;
                }
                var entry = {
                    id: id,
                    card: card,
                    column: column,
                    columnIndex: columnIndex,
                    cardIndex: cardIndex
                };
                byId[id] = entry;
                cards.push(entry);
            });
        });
        return { byId: byId, cards: cards };
    };

    var getCardAssigneeNames = function (card) {
        return getCardPropertyValues(card, 'Assignee')
            .map(getAssigneeName)
            .filter(Boolean);
    };

    var assigneeProfileImageUrlCache = {};

    var decodeHtmlAttribute = function (value) {
        return String(value || '')
            .replace(/&amp;/g, '&')
            .replace(/&quot;/g, '"')
            .replace(/&#39;/g, "'")
            .replace(/&lt;/g, '<')
            .replace(/&gt;/g, '>');
    };

    var extractUserProfileImageUrlFromHtml = function (html) {
        var raw = String(html || '');
        var imageTags = raw.match(/<img\b[^>]*>/gi) || [];
        var srcMatch;
        for (var i = 0; i < imageTags.length; i += 1) {
            if (!/\buserInlineProfileImage\b/.test(imageTags[i])) {
                continue;
            }
            srcMatch = imageTags[i].match(/\bsrc=["']([^"']+)["']/i);
            if (srcMatch) {
                return decodeHtmlAttribute(srcMatch[1]);
            }
        }
        return '';
    };

    var getAssigneeProfileImageUrl = function (pageName, assigneeName) {
        var normalizedAssignee = normalizeAssigneeValue(assigneeName);
        var cacheKey = normalizedAssignee || String(assigneeName || '').trim();
        if (!cacheKey) {
            return Promise.resolve('');
        }
        if (Object.prototype.hasOwnProperty.call(assigneeProfileImageUrlCache, cacheKey)) {
            return assigneeProfileImageUrlCache[cacheKey];
        }
        assigneeProfileImageUrlCache[cacheKey] = requestRenderInlineComment(pageName, normalizedAssignee)
            .then(extractUserProfileImageUrlFromHtml)
            .catch(function () { return ''; });
        return assigneeProfileImageUrlCache[cacheKey];
    };

    var getAssigneeInitial = function (assigneeName) {
        var safeName = String(assigneeName || '').trim();
        return safeName ? safeName.slice(0, 1).toUpperCase() : '?';
    };

    var renderCardAssigneeAvatars = function (pageName, container, assigneeNames) {
        var visibleNames = (assigneeNames || []).filter(Boolean).slice(0, 4);
        var hiddenCount = Math.max(0, (assigneeNames || []).length - visibleNames.length);
        container.innerHTML = '';
        container.title = (assigneeNames || []).join(', ');
        if (visibleNames.length === 0) {
            container.style.display = 'none';
            return;
        }
        container.style.display = '';
        visibleNames.forEach(function (assigneeName) {
            var avatar = document.createElement('span');
            avatar.className = 'kanban-card-assignee-avatar';
            avatar.title = assigneeName;
            avatar.textContent = getAssigneeInitial(assigneeName);
            container.appendChild(avatar);

            getAssigneeProfileImageUrl(pageName, assigneeName).then(function (imageUrl) {
                if (!imageUrl) {
                    return;
                }
                avatar.innerHTML = '';
                var image = document.createElement('img');
                image.src = imageUrl;
                image.alt = assigneeName;
                image.className = 'kanban-card-assignee-avatar-image';
                avatar.appendChild(image);
            });
        });
        if (hiddenCount > 0) {
            var more = document.createElement('span');
            more.className = 'kanban-card-assignee-avatar kanban-card-assignee-more';
            more.title = (assigneeNames || []).slice(visibleNames.length).join(', ');
            more.textContent = '+' + hiddenCount;
            container.appendChild(more);
        }
    };

    var computeDerivedCardRelationValues = function (columns, targetCard) {
        var targetId = targetCard && targetCard.id ? String(targetCard.id).trim() : '';
        var derived = {
            Blocks: [],
            SubCards: []
        };
        if (!targetId) {
            return derived;
        }
        (columns || []).forEach(function (column) {
            (column.cards || []).forEach(function (candidate) {
                if (!candidate || candidate === targetCard || !candidate.id) {
                    return;
                }
                DERIVED_CARD_RELATION_PROPERTIES.forEach(function (definition) {
                    getCardPropertyValues(candidate, definition.sourceKey).forEach(function (value) {
                        var parsed = parseCardReferenceValue(value);
                        if (parsed && parsed.id === targetId) {
                            derived[definition.key].push(buildCardReferenceValue(candidate.id, candidate.text || ''));
                        }
                    });
                });
            });
        });
        return derived;
    };

    if (typeof window !== 'undefined') {
        window.__AhaWikiKanbanTestHooks = Object.assign({}, window.__AhaWikiKanbanTestHooks || {}, {
            parseCardReferenceValue: parseCardReferenceValue,
            buildCardReferenceValue: buildCardReferenceValue,
            normalizeCardReferenceValue: normalizeCardReferenceValue,
            parseAssigneeValue: parseAssigneeValue,
            normalizeAssigneeValue: normalizeAssigneeValue,
            getAssigneeName: getAssigneeName,
            buildCardIndex: buildCardIndex,
            computeDerivedCardRelationValues: computeDerivedCardRelationValues
        });
    }

    var prependCardActivity = function (card, details) {
        var entry = buildCommentEntry(details);
        card.comments = card.comments || [];
        card.comments.unshift(entry);
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
        if (card.footerElement) {
            var commentCount = (card.comments || []).length;
            var attachmentCount = getCardAttachmentCount(card);
            if (commentCount > 0 || attachmentCount > 0) {
                card.footerElement.classList.add('kanban-card-footer--has-content');
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
            var cardSectionMatch = line.match(/^=====\s+(Description|Property|Activity)\s*$/);
            if (cardSectionMatch && currentCard) {
                cardSection = cardSectionMatch[1];
                propertyKey = '';
                return;
            }

            if (cardSection === 'Description' && currentCard) {
                currentCard.description = currentCard.description || [];
                currentCard.description.push(line);
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
                    description: [],
                    comments: [],
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
    var isFileDragEvent = function (evt) {
        if (!evt || !evt.dataTransfer) {
            return false;
        }
        var dt = evt.dataTransfer;
        if (dt.files && dt.files.length > 0) {
            return true;
        }
        var types = dt.types ? Array.prototype.slice.call(dt.types) : [];
        return types.indexOf('Files') >= 0;
    };
    var getOpenedCardOverlay = function () {
        return document.querySelector('.kanban-card-detail-overlay');
    };
    var closeOpenedCardOverlay = function () {
        var overlay = getOpenedCardOverlay();
        if (overlay && overlay.parentNode) {
            overlay.parentNode.removeChild(overlay);
            return true;
        }
        return false;
    };
    var createColumnElement = function (root, columns, column, index, shiftLineNumbersAfterInsert, getCardInsertLineStart, enqueueMutation, rerenderColumns, persistColumns, isWritable, boardAutoScroll) {
        var columnElement = document.createElement('div');
        var pageName = root.getAttribute('data-page-name') || '';
        boardAutoScroll = boardAutoScroll || {
            start: function () {},
            update: function () {},
            stop: function () {}
        };
        columnElement.className = 'kanban-column';
        columnElement.setAttribute('data-column-index', String(index));
        columnElement.setAttribute('data-column-line-number', String(column.lineNumber || 1));


        var titleRow = document.createElement('div');
        titleRow.className = 'kanban-column-title-row';

        var title = document.createElement('div');
        title.textContent = column.title;
        title.className = 'kanban-column-title';
        title.title = isWritable ? 'Click to edit list name' : '';

        var cardCountBadge = document.createElement('span');
        cardCountBadge.className = 'kanban-column-badge';
        cardCountBadge.textContent = String((column.cards || []).length);

        var deleteListButton = document.createElement('button');
        deleteListButton.type = 'button';
        deleteListButton.innerHTML = '<i class="fas fa-trash-alt" aria-hidden="true"></i>';
        deleteListButton.title = 'Delete list';
        deleteListButton.className = 'kanban-icon-button';

        titleRow.appendChild(title);
        titleRow.appendChild(cardCountBadge);
        if (isWritable) {
            titleRow.appendChild(deleteListButton);
        }
        var titleEditor = document.createElement('input');
        titleEditor.type = 'text';
        titleEditor.value = column.title || '';
        titleEditor.className = 'kanban-column-title-editor';
        titleEditor.classList.add('kanban-hidden');

        if (isWritable) {
            titleRow.insertBefore(titleEditor, deleteListButton);
        }
        columnElement.appendChild(titleRow);

        var closeTitleEditor = function () {
            titleEditor.classList.add('kanban-hidden');
            title.classList.remove('kanban-hidden');
            titleEditor.value = column.title || '';
        };

        var openTitleEditor = function () {
            title.classList.add('kanban-hidden');
            titleEditor.classList.remove('kanban-hidden');
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
                return persistColumns('list:rename', { eventPrefix: 'User:' + getCurrentAuthor(), fromTitle: previousTitle, toTitle: nextTitle || '' }).catch(function (error) {
                    console.error('[Kanban] failed to save list title', error);
                });
            });
        };

        if (isWritable) {
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
                    return persistColumns('list:delete', { eventPrefix: 'User:' + getCurrentAuthor(), listTitle: removedTitle }).catch(function (error) {
                        console.error('[Kanban] failed to delete list', error);
                    });
                });
            });
        }

        var cardList = document.createElement('div');
        cardList.className = 'kanban-card-list';
        cardList.setAttribute('data-column-index', String(index));

        var listDropHint = document.createElement('div');
        listDropHint.textContent = 'Drop files here to create cards';
        listDropHint.style.display = 'none';
        listDropHint.style.padding = '8px 10px';
        listDropHint.style.marginBottom = '8px';
        listDropHint.style.border = '1px dashed var(--kanban-primary)';
        listDropHint.style.borderRadius = '6px';
        listDropHint.style.background = 'var(--color-link-hover-bg)';
        listDropHint.style.color = 'var(--kanban-primary)';
        listDropHint.style.fontSize = '12px';
        listDropHint.style.fontWeight = '600';
        cardList.insertBefore(listDropHint, cardList.firstChild);

        var resetListDropFeedback = function () {
            cardList.style.removeProperty('outline');
            cardList.style.removeProperty('background');
            listDropHint.style.display = 'none';
        };
        var logDragEvent = function (scope, evt, extra) {
            var payload = Object.assign({
                scope: scope,
                type: evt && evt.type ? evt.type : '',
                hasFiles: Boolean(evt && evt.dataTransfer && evt.dataTransfer.files && evt.dataTransfer.files.length),
                fileCount: evt && evt.dataTransfer && evt.dataTransfer.files ? evt.dataTransfer.files.length : 0
            }, extra || {});
            console.debug('[Kanban][Drag]', payload);
        };

        if (isWritable) {
        cardList.addEventListener('dragenter', function (evt) {
            logDragEvent('list', evt, { columnIndex: index, overlayOpen: Boolean(getOpenedCardOverlay()) });
            if (!isFileDragEvent(evt)) {
                return;
            }
            if (getOpenedCardOverlay()) {
                return;
            }
            evt.preventDefault();
            evt.stopPropagation();
            cardList.style.setProperty('outline', '2px dashed var(--kanban-primary)', 'important');
            cardList.style.setProperty('background', 'var(--color-link-hover-bg)', 'important');
            listDropHint.style.display = '';
        });

        cardList.addEventListener('dragover', function (evt) {
            logDragEvent('list', evt, { columnIndex: index, overlayOpen: Boolean(getOpenedCardOverlay()) });
            if (!isFileDragEvent(evt)) {
                return;
            }
            if (getOpenedCardOverlay()) {
                return;
            }
            evt.preventDefault();
            evt.stopPropagation();
            if (evt.dataTransfer) {
                evt.dataTransfer.dropEffect = 'copy';
            }
            cardList.style.setProperty('outline', '2px dashed var(--kanban-primary)', 'important');
            cardList.style.setProperty('background', 'var(--color-link-hover-bg)', 'important');
            listDropHint.style.display = '';
        });

        cardList.addEventListener('dragleave', function (evt) {
            logDragEvent('list', evt, { columnIndex: index, relatedTargetInList: Boolean(evt && evt.relatedTarget && cardList.contains(evt.relatedTarget)) });
            if (!evt || (evt.relatedTarget && cardList.contains(evt.relatedTarget))) {
                return;
            }
            resetListDropFeedback();
        });

        cardList.addEventListener('drop', function (evt) {
            logDragEvent('list', evt, { columnIndex: index, overlayOpen: Boolean(getOpenedCardOverlay()) });
            if (!evt || !evt.dataTransfer || !evt.dataTransfer.files) {
                return;
            }
            var files = Array.prototype.slice.call(evt.dataTransfer.files || []).filter(function (file) { return Boolean(file); });
            if (!files.length) {
                return;
            }
            evt.preventDefault();
            evt.stopPropagation();
            resetListDropFeedback();

            createCardsFromFiles(pageName, column, files).then(function (cards) {
                if (!cards.length) {
                    return;
                }
                rerenderColumns();
                enqueueMutation(function () {
                    var firstCard = cards[0] || {};
                    var revisionCardTitle = cards.length === 1 ? (firstCard.text || '') : ((firstCard.text || '') + ' and ' + String(cards.length - 1) + ' more cards');
                    return persistColumns('card:add', { eventPrefix: 'User:' + getCurrentAuthor(), cardId: firstCard.id || '', cardTitle: revisionCardTitle }).catch(function (error) {
                        console.error('[Kanban] failed to save dropped file cards', error);
                    });
                });
            }).catch(function (error) {
                console.error('[Kanban] failed to create dropped file cards', error);
                alert('File upload failed. ' + (error && error.message ? error.message : ''));
            });
        });
        }

        (column.cards || []).forEach(function (card) {
            var cardElement = document.createElement('div');
            cardElement.className = 'kanban-card';
            if (card.id) {
                cardElement.setAttribute('data-card-id', card.id);
            }
            (card.classNames || []).forEach(function (className) {
                if (className) {
                    cardElement.classList.add(className);
                }
            });
            cardElement.setAttribute('data-line-number', String(card.lineNumber));


            var cardText = document.createElement('div');
            cardText.textContent = card.text;
            cardText.className = 'kanban-card-text';

            var cardMeta = document.createElement('div');
            cardMeta.className = 'kanban-card-meta';

            var cardStat = document.createElement('div');
            cardStat.className = 'kanban-card-stats';
            card.commentCountElement = document.createElement('span');
            card.attachmentCountElement = document.createElement('span');
            card.attachmentCountElement.className = 'kanban-card-stat';
            card.commentCountElement.className = 'kanban-card-stat';
            updateCardCommentCount(card);
            var dueDateValues = (card.properties && card.properties.DueDate) || [];
            var dueDateText = dueDateValues.length > 0 ? String(dueDateValues[0]).replace(/^\[|\]$/g, '').trim() : '';
            var dueDateElement = document.createElement('span');
            if (dueDateText) {
                dueDateElement.innerHTML = '<i class="far fa-calendar-alt" aria-hidden="true"></i> ' + dueDateText;
            } else {
                dueDateElement.style.display = 'none';
            }
            var assigneeNames = getCardAssigneeNames(card);
            var assigneeElement = document.createElement('span');
            assigneeElement.className = 'kanban-card-stat kanban-card-assignees';
            renderCardAssigneeAvatars(pageName, assigneeElement, assigneeNames);
            cardStat.appendChild(card.attachmentCountElement);
            cardStat.appendChild(card.commentCountElement);
            cardStat.appendChild(dueDateElement);
            cardMeta.appendChild(assigneeElement);
            cardMeta.appendChild(cardStat);

            var cardRelDefs = [
                { key: 'BlockedBy', label: 'Blocked by', icon: 'fa-ban', css: 'blockedby' },
                { key: 'ParentCard', label: 'Parent', icon: 'fa-level-up-alt', css: 'parent' },
                { key: 'RelatedCard', label: 'Related', icon: 'fa-link', css: 'related' }
            ];
            var cardDerivedRelDefs = [
                { key: 'Blocks', label: 'Blocks', icon: 'fa-ban', css: 'blocks' },
                { key: 'SubCards', label: 'Sub-cards', icon: 'fa-level-down-alt', css: 'subcards' }
            ];
            var cardDerivedRelData = computeDerivedCardRelationValues(columns, card);
            var relBadges = [];
            cardRelDefs.forEach(function (def) {
                var count = getCardPropertyValues(card, def.key).length;
                if (count > 0) { relBadges.push({ def: def, count: count }); }
            });
            cardDerivedRelDefs.forEach(function (def) {
                var count = (cardDerivedRelData[def.key] || []).length;
                if (count > 0) { relBadges.push({ def: def, count: count }); }
            });

            var hasFooterContent = relBadges.length > 0 ||
                assigneeNames.length > 0 ||
                getCardAttachmentCount(card) > 0 ||
                (card.comments || []).length > 0 ||
                Boolean(dueDateText);
            var cardFooter = document.createElement('div');
            cardFooter.className = 'kanban-card-footer';
            if (hasFooterContent) {
                cardFooter.classList.add('kanban-card-footer--has-content');
            }
            card.footerElement = cardFooter;

            if (relBadges.length > 0) {
                var cardRelations = document.createElement('div');
                cardRelations.className = 'kanban-card-relations';
                relBadges.forEach(function (item) {
                    var badge = document.createElement('span');
                    badge.className = 'kanban-card-relation-badge kanban-card-relation-badge--' + item.def.css;
                    badge.innerHTML = '<i class="fas ' + item.def.icon + '" aria-hidden="true"></i> ' + item.def.label + (item.count > 1 ? ' ' + item.count : '');
                    badge.title = item.def.label + ': ' + item.count + ' card' + (item.count > 1 ? 's' : '');
                    cardRelations.appendChild(badge);
                });
                cardFooter.appendChild(cardRelations);
            }
            cardFooter.appendChild(cardMeta);

            cardElement.appendChild(cardText);
            if (card.__remoteConflict || card.__remoteUpdate) {
                var remoteBadge = document.createElement('div');
                remoteBadge.className = card.__remoteConflict
                    ? 'kanban-card-remote-badge kanban-card-remote-badge--conflict'
                    : 'kanban-card-remote-badge kanban-card-remote-badge--update';
                remoteBadge.title = card.__remoteConflict
                    ? 'Updated by another user — server version applied'
                    : 'You are editing this card — remote update pending';
                remoteBadge.innerHTML = card.__remoteConflict
                    ? '<i class="fas fa-exclamation-triangle" aria-hidden="true"></i> Updated by another user'
                    : '<i class="fas fa-sync-alt" aria-hidden="true"></i> Remote update pending';
                cardElement.appendChild(remoteBadge);
            }
            cardElement.appendChild(cardFooter);
            cardElement.addEventListener('click', function () {
                openCardDetail(card);
            });
            cardList.appendChild(cardElement);
        });

        columnElement.appendChild(cardList);

        var cardComposer = document.createElement('div');
        cardComposer.className = 'kanban-card-composer';

        var addCardButton = document.createElement('button');
        addCardButton.type = 'button';
        addCardButton.innerHTML = '<i class="fas fa-plus" aria-hidden="true"></i> Add a Card';
        addCardButton.className = 'kanban-add-card-button';

        var cardEditor = document.createElement('div');
        cardEditor.className = 'kanban-card-editor kanban-hidden';

        var cardTextarea = document.createElement('textarea');
        cardTextarea.rows = 3;
        cardTextarea.placeholder = 'Enter a Title';
        cardTextarea.className = 'kanban-card-textarea';

        var cardActions = document.createElement('div');
        cardActions.className = 'kanban-inline-actions';

        var submitCardButton = document.createElement('button');
        submitCardButton.type = 'button';
        submitCardButton.innerHTML = '<i class="fas fa-plus" aria-hidden="true"></i> Add a Card';
        submitCardButton.className = 'kanban-primary-button';

        var cancelCardButton = document.createElement('button');
        cancelCardButton.type = 'button';
        cancelCardButton.innerHTML = '<i class="fas fa-times" aria-hidden="true"></i> Cancel';
        cancelCardButton.className = 'kanban-ghost-button';

        cardActions.appendChild(submitCardButton);
        cardActions.appendChild(cancelCardButton);
        cardEditor.appendChild(cardTextarea);
        cardEditor.appendChild(cardActions);
        cardComposer.appendChild(addCardButton);
        cardComposer.appendChild(cardEditor);
        if (isWritable) {
            columnElement.appendChild(cardComposer);
        }

        var closeCardEditor = function () {
            cardEditor.className = 'kanban-card-editor kanban-hidden';
            addCardButton.classList.remove('kanban-hidden');
            cardTextarea.value = '';
        };

        if (isWritable) {
        addCardButton.addEventListener('click', function () {
            addCardButton.classList.add('kanban-hidden');
            cardEditor.classList.remove('kanban-hidden');
            cardTextarea.focus();
        });

        cancelCardButton.addEventListener('click', function () {
            closeCardEditor();
        });

        var submitCard = function () {
            var value = (cardTextarea.value || '').trim();
            var titles = value ? value.split(/\r?\n/).map(function (line) { return line.trim(); }).filter(function (line) { return !!line; }) : [];
            if (titles.length === 0) {
                cardTextarea.focus();
                return;
            }
            submitCardButton.disabled = true;
            closeCardEditor();

            enqueueMutation(function () {
                column.cards = column.cards || [];

                var createdCards = titles.map(function (title) {
                    return {
                        text: title,
                        id: generateCardId(),
                        classNames: [],
                        comments: [],
                        properties: {
                            Creator: [toUserLinkMarkup(getCurrentAuthor())],
                            dateCreated: [formatKanbanDateTime(getNowIsoWithoutMillis())]
                        }
                    };
                });

                createdCards.forEach(function (newCard) {
                    column.cards.push(newCard);
                    prependCardActivity(newCard, [extractActivityDetailFromRevisionComment(buildKanbanSaveComment('card:add', { eventPrefix: 'User:' + getCurrentAuthor(), cardId: newCard.id, cardTitle: newCard.text || '' }))]);
                });

                shiftLineNumbersAfterInsert();
                rerenderColumns();

                createdCards.forEach(function (newCard, addedIndex) {
                    root.dispatchEvent(new CustomEvent('kanban:cardAdded', {
                        detail: {
                            text: newCard.text || '',
                            columnIndex: index,
                            cardIndex: column.cards.length - createdCards.length + addedIndex
                        }
                    }));
                });

                var firstCard = createdCards[0] || {};
                var revisionCardTitle = titles.length === 1 ? (firstCard.text || '') : ((firstCard.text || '') + ' and ' + String(titles.length - 1) + ' more cards');
                return persistColumns('card:add', {
                    eventPrefix: 'User:' + getCurrentAuthor(),
                    cardId: firstCard.id || '',
                    cardTitle: revisionCardTitle
                })
                    .then(function (result) {
                        rerenderColumns();
                        console.info('[Kanban] card added', result);
                    })
                    .catch(function (error) {
                        console.error('[Kanban] failed to save card add', error);
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
                return;
            }
            if (evt.key === 'Escape' || evt.key === 'Esc') {
                evt.preventDefault();
                closeCardEditor();
            }
        });
        }

        if (isWritable && window.Sortable) {
            var dragCancelled = false;
            var draggingCard = false;
            var draggingItem = null;
            var draggingFromList = null;
            var draggingOldIndex = -1;
            var draggingOriginClone = null;
            var cardSortable = null;
            var clearCardDragOrigin = function () {
                if (draggingOriginClone && draggingOriginClone.parentNode) {
                    draggingOriginClone.parentNode.removeChild(draggingOriginClone);
                }
                draggingOriginClone = null;
            };
            var createCardDragOrigin = function (item) {
                clearCardDragOrigin();
                if (!item || !item.parentElement || typeof item.cloneNode !== 'function') {
                    return;
                }
                draggingOriginClone = item.cloneNode(true);
                draggingOriginClone.id = '';
                if (draggingOriginClone.removeAttribute) {
                    draggingOriginClone.removeAttribute('id');
                    draggingOriginClone.removeAttribute('data-line-number');
                }
                if (draggingOriginClone.classList) {
                    draggingOriginClone.classList.remove('kanban-card-drag-chosen', 'kanban-card-drag-placeholder', 'kanban-card-drag-preview');
                }
                draggingOriginClone.classList.add('kanban-card-drag-origin');
                draggingOriginClone.setAttribute('aria-hidden', 'true');
                draggingOriginClone.setAttribute('data-kanban-drag-origin', 'true');
                item.parentElement.insertBefore(draggingOriginClone, item);
            };
            var clearCardDragItemClasses = function (item) {
                if (!item || !item.classList) {
                    return;
                }
                item.classList.remove('kanban-card-drag-chosen', 'kanban-card-drag-placeholder');
            };
            var setCardDragCursorState = function (active, doc) {
                if (root && root.classList) {
                    root.classList[active ? 'add' : 'remove']('kanban-card-dragging');
                }
                var targetDocument = doc || (root && root.ownerDocument ? root.ownerDocument : document);
                if (targetDocument && targetDocument.body && targetDocument.body.classList) {
                    targetDocument.body.classList[active ? 'add' : 'remove']('kanban-card-dragging-global');
                }
            };
            var restoreDraggedItem = function (item, fromList, oldIndex) {
                var restoreItem = item || draggingItem;
                var restoreFromList = fromList || draggingFromList;
                var restoreOldIndex = Number.isFinite(oldIndex) ? oldIndex : draggingOldIndex;
                if (!restoreItem || !restoreFromList || restoreOldIndex < 0) {
                    return;
                }
                var children = restoreFromList.children;
                if (draggingOriginClone && draggingOriginClone.parentElement === restoreFromList && children[restoreOldIndex] === draggingOriginClone) {
                    restoreFromList.insertBefore(restoreItem, draggingOriginClone.nextSibling || draggingOriginClone.nextElementSibling || null);
                    return;
                }
                if (restoreOldIndex >= children.length) {
                    restoreFromList.appendChild(restoreItem);
                    return;
                }
                if (children[restoreOldIndex] === restoreItem) {
                    return;
                }
                restoreFromList.insertBefore(restoreItem, children[restoreOldIndex]);
            };
            var isEscapeKey = function (evt) {
                return evt && (evt.key === 'Escape' || evt.key === 'Esc' || evt.keyCode === 27 || evt.which === 27);
            };
            var handleDragEscape = function (evt) {
                if (!draggingCard || !isEscapeKey(evt)) {
                    return;
                }
                evt.preventDefault();
                dragCancelled = true;
            };

            var findNearestCardList = function (clientX, clientY) {
                if (!Number.isFinite(clientX) || !Number.isFinite(clientY)) {
                    return null;
                }
                var candidates = root.querySelectorAll('.kanban-card-list');
                var nearest = null;
                var nearestDistance = Number.POSITIVE_INFINITY;
                Array.prototype.forEach.call(candidates, function (candidate) {
                    if (!candidate || !candidate.parentElement || !candidate.parentElement.classList.contains('kanban-column')) {
                        return;
                    }
                    var rect = candidate.getBoundingClientRect();
                    var centerX = rect.left + (rect.width / 2);
                    var centerY = rect.top + (rect.height / 2);
                    var distance = Math.hypot(clientX - centerX, clientY - centerY);
                    if (distance < nearestDistance) {
                        nearestDistance = distance;
                        nearest = candidate;
                    }
                });
                return nearest;
            };
            var isPointInsideRect = function (clientX, clientY, rect) {
                if (!rect || !Number.isFinite(clientX) || !Number.isFinite(clientY)) {
                    return false;
                }
                var left = Number.isFinite(rect.left) ? rect.left : 0;
                var top = Number.isFinite(rect.top) ? rect.top : 0;
                var width = Number.isFinite(rect.width) ? rect.width : 0;
                var height = Number.isFinite(rect.height) ? rect.height : 0;
                var right = Number.isFinite(rect.right) ? rect.right : left + width;
                var bottom = Number.isFinite(rect.bottom) ? rect.bottom : top + height;
                return clientX >= left && clientX <= right && clientY >= top && clientY <= bottom;
            };
            var findCardDropListAtPoint = function (clientX, clientY) {
                if (!Number.isFinite(clientX) || !Number.isFinite(clientY)) {
                    return null;
                }
                var columnsForDrop = root.querySelectorAll('.kanban-column');
                var targetList = null;
                Array.prototype.some.call(columnsForDrop, function (candidateColumn) {
                    if (!candidateColumn) {
                        return false;
                    }
                    var candidateList = candidateColumn.querySelector('.kanban-card-list');
                    if (!candidateList) {
                        return false;
                    }
                    if (!isPointInsideRect(clientX, clientY, candidateColumn.getBoundingClientRect())) {
                        return false;
                    }
                    targetList = candidateList;
                    return true;
                });
                return targetList;
            };
            var getChildIndex = function (parent, child) {
                if (!parent || !child || !parent.children) {
                    return -1;
                }
                return Array.prototype.indexOf.call(parent.children, child);
            };

            var clearCardDropTargetHighlight = function () {};
            var setCardDropTargetHighlight = function () {};

            var latestPointerForCardDrag = { x: null, y: null };
            var updateCardDragPointer = function (evt) {
                if (!draggingCard || !evt) {
                    return;
                }
                if (Number.isFinite(evt.clientX)) {
                    latestPointerForCardDrag.x = evt.clientX;
                }
                if (Number.isFinite(evt.clientY)) {
                    latestPointerForCardDrag.y = evt.clientY;
                }
                var activeDropList = findCardDropListAtPoint(latestPointerForCardDrag.x, latestPointerForCardDrag.y);
                var nearestList = activeDropList || findNearestCardList(latestPointerForCardDrag.x, latestPointerForCardDrag.y);
                if (!activeDropList) {
                    restoreDraggedItem();
                }
                boardAutoScroll.update(evt, nearestList);
                setCardDropTargetHighlight(activeDropList);
            };

            cardSortable = Sortable.create(cardList, {
                group: root.id || 'kanban-default',
                draggable: '.kanban-card:not(.kanban-card-drag-origin)',
                animation: 120,
                ghostClass: 'kanban-card-drag-placeholder',
                chosenClass: 'kanban-card-drag-chosen',
                dragClass: 'kanban-card-drag-placeholder',
                forceFallback: true,
                fallbackClass: 'kanban-card-drag-preview',
                fallbackOnBody: true,
                fallbackTolerance: 3,
                onClone: function (evt) {
                    var clone = evt && evt.clone ? evt.clone : null;
                    if (!clone) {
                        return;
                    }
                    clone.id = '';
                    if (clone.removeAttribute) {
                        clone.removeAttribute('id');
                        clone.removeAttribute('data-line-number');
                    }
                    if (clone.classList) {
                        clone.classList.add('kanban-card-drag-preview');
                    }
                    clone.setAttribute('aria-hidden', 'true');
                },
                onStart: function (evt) {
                    var doc = root && root.ownerDocument ? root.ownerDocument : document;
                    draggingCard = true;
                    dragCancelled = false;
                    draggingItem = evt.item;
                    draggingFromList = evt.from;
                    draggingOldIndex = evt.oldIndex;
                    createCardDragOrigin(evt.item);
                    setCardDragCursorState(true, doc);
                    window.addEventListener('keydown', handleDragEscape, true);
                    doc.addEventListener('keydown', handleDragEscape, true);
                    clearCardDropTargetHighlight();
                    latestPointerForCardDrag.x = null;
                    latestPointerForCardDrag.y = null;
                    boardAutoScroll.start();
                    boardAutoScroll.update(evt.originalEvent, evt.from);
                    window.addEventListener('dragover', updateCardDragPointer, true);
                    doc.addEventListener('dragover', updateCardDragPointer, true);
                },
                onMove: function (evt) {
                    if (dragCancelled) {
                        clearCardDropTargetHighlight();
                        return false;
                    }
                    var pointerEvent = evt && evt.originalEvent ? evt.originalEvent : null;
                    updateCardDragPointer(pointerEvent);
                    var currentList = findCardDropListAtPoint(latestPointerForCardDrag.x, latestPointerForCardDrag.y);
                    if (!currentList && pointerEvent) {
                        restoreDraggedItem();
                        return false;
                    }
                    if (currentList) {
                        boardAutoScroll.update(pointerEvent, currentList);
                        setCardDropTargetHighlight(currentList);
                    }
                    return true;
                },
                onEnd: function (evt) {
                    var doc = root && root.ownerDocument ? root.ownerDocument : document;
                    draggingCard = false;
                    window.removeEventListener('keydown', handleDragEscape, true);
                    doc.removeEventListener('keydown', handleDragEscape, true);
                    window.removeEventListener('dragover', updateCardDragPointer, true);
                    doc.removeEventListener('dragover', updateCardDragPointer, true);
                    boardAutoScroll.stop();
                    clearCardDropTargetHighlight();
                    clearCardDragOrigin();
                    clearCardDragItemClasses(evt.item);
                    setCardDragCursorState(false, doc);

                    var pointerEvent = evt.originalEvent || null;
                    var clientX = pointerEvent && Number.isFinite(pointerEvent.clientX) ? pointerEvent.clientX : latestPointerForCardDrag.x;
                    var clientY = pointerEvent && Number.isFinite(pointerEvent.clientY) ? pointerEvent.clientY : latestPointerForCardDrag.y;
                    var hasFinalPointer = Number.isFinite(clientX) && Number.isFinite(clientY);
                    var fallbackDropList = evt.to && evt.to.classList && evt.to.classList.contains('kanban-card-list') ? evt.to : null;
                    var finalDropList = hasFinalPointer ? findCardDropListAtPoint(clientX, clientY) : fallbackDropList;
                    var droppedOutsideList = !finalDropList;
                    if (!droppedOutsideList && evt.item) {
                        if (evt.item.parentElement !== finalDropList) {
                            finalDropList.appendChild(evt.item);
                        }
                        evt.to = finalDropList;
                        evt.newIndex = getChildIndex(finalDropList, evt.item);
                    }
                    var hasInvalidIndex = !Number.isFinite(evt.oldIndex) || evt.oldIndex < 0 || !Number.isFinite(evt.newIndex) || evt.newIndex < 0;
                    var returnedToSameSpot = evt.from === evt.to && evt.oldIndex === evt.newIndex;
                    if (dragCancelled || droppedOutsideList || hasInvalidIndex || returnedToSameSpot || !evt.to) {
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
                        var fromCards = fromColumn && Array.isArray(fromColumn.cards) ? fromColumn.cards : [];
                        var toCards = toColumn && Array.isArray(toColumn.cards) ? toColumn.cards : [];
                        var movedCard = fromCards.splice(Math.max(0, evt.oldIndex - 1), 1)[0];
                        if (!movedCard && Number.isFinite(movedLine)) {
                            for (var cardIndex = 0; cardIndex < fromCards.length; cardIndex += 1) {
                                var candidate = fromCards[cardIndex];
                                if (candidate && Number(candidate.lineNumber) === movedLine) {
                                    movedCard = fromCards.splice(cardIndex, 1)[0];
                                    break;
                                }
                            }
                        }
                        if (!movedCard) {
                            console.error('[Kanban] failed to resolve moved card', {
                                fromColumnIndex: fromColumnIndex,
                                toColumnIndex: toColumnIndex,
                                oldIndex: evt.oldIndex,
                                newIndex: evt.newIndex,
                                movedLine: movedLine
                            });
                            rerenderColumns();
                            return;
                        }
                        prependCardActivity(movedCard, [extractActivityDetailFromRevisionComment(buildKanbanSaveComment('card:move', { eventPrefix: 'User:' + getCurrentAuthor(), cardId: movedCard.id || '', cardTitle: movedCard.text || '', fromOrder: String(evt.oldIndex), toOrder: String(evt.newIndex), fromList: fromTitle, toList: toTitle }))]);
                        updateCardCommentCount(movedCard);
                        toCards.splice(Math.max(0, evt.newIndex - 1), 0, movedCard);
                        shiftLineNumbersAfterInsert();
                        rerenderColumns();
                        return persistColumns('card:move', {
                            eventPrefix: 'User:' + getCurrentAuthor(),
                            cardId: movedCard.id || '',
                            cardTitle: movedCard.text || '',
                            fromOrder: String(evt.oldIndex),
                            toOrder: String(evt.newIndex),
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

    var initKanban = function (root) {
        if (!root || root.getAttribute('data-kanban-initialized') === 'true') {
            return;
        }
        root.setAttribute('data-kanban-initialized', 'true');
        var pre = root.querySelector('pre[data-shebang]');
        var board = root.querySelector('.kanban-board');
        if (!pre || !board) {
            return;
        }

        var pageName = root.getAttribute('data-page-name') || '';
        var isWritable = isKanbanRootWritable(root);
        var metaWrapper = root.closest('.InterpreterRenderMetaWrapper');
        var metaLineStart = Number(metaWrapper ? metaWrapper.getAttribute('data-line-start') : 1) || 1;
        var hasShebang = Boolean(pre.getAttribute('data-shebang'));
        var interpreterStartLine = Math.max(1, metaLineStart + (hasShebang ? 1 : 0));
        var columns = parseKanbanText(pre.textContent || '', interpreterStartLine);
        var baseColumns = deepCloneColumns(columns);
        var interpreterLineEnd = Number(metaWrapper ? metaWrapper.getAttribute('data-line-end') : 1) || 1;
        var currentKanbanLineCount = getLineCountForText(pre.textContent || '');
        root.setAttribute('data-kanban-line-count', String(currentKanbanLineCount));
        root.setAttribute('data-kanban-read-only', isWritable ? 'false' : 'true');
        if (!isWritable) {
            root.classList.add('kanban-read-only');
        }
        var rerenderColumns = function () {};
        var mutationQueue = Promise.resolve();
        var pendingMutationCount = 0;
        var pendingRemoteRevision = 0;
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
                    if (card.description && card.description.length > 0) {
                        lines.push('===== Description');
                        card.description.forEach(function (descLine) { lines.push(descLine); });
                    }
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
            if (!isWritable) {
                return Promise.reject(new Error('Kanban is read-only.'));
            }
            var content = serializeColumns();
            var replacementLineCount = getLineCountForText(content);
            var requestLineEnd = interpreterStartLine + Math.max(0, currentKanbanLineCount);
            return requestSaveKanban(pageName, interpreterStartLine, requestLineEnd, content, actionType, actionMeta)
                .then(function (result) {
                    var nextLineEnd = Number(result && result.lineEnd);
                    if (metaWrapper && Number.isFinite(nextLineEnd) && nextLineEnd >= interpreterStartLine) {
                        var delta = nextLineEnd - requestLineEnd;
                        if (delta !== 0) {
                            shiftMetaLineRangeAfterInsert(metaWrapper, requestLineEnd, delta);
                        }
                        metaWrapper.setAttribute('data-line-end', String(nextLineEnd));
                    }
                    currentKanbanLineCount = replacementLineCount;
                    root.setAttribute('data-kanban-line-count', String(replacementLineCount));
                    pre.textContent = content;
                    columns.forEach(function (column) {
                        (column.cards || []).forEach(function (card) {
                            delete card.__remoteConflict;
                            delete card.__remoteUpdate;
                        });
                    });
                    baseColumns = deepCloneColumns(columns);
                    return result;
                });
        };
        var enqueueMutation = function (executor) {
            if (!isWritable) {
                return Promise.reject(new Error('Kanban is read-only.'));
            }
            pendingMutationCount++;
            mutationQueue = mutationQueue
                .then(function () {
                    return executor();
                })
                .catch(function (error) {
                    console.error('[Kanban] queued mutation failed', error);
                })
                .then(function () {
                    pendingMutationCount = Math.max(0, pendingMutationCount - 1);
                    if (pendingMutationCount === 0 && pendingRemoteRevision > 0) {
                        var r = pendingRemoteRevision;
                        pendingRemoteRevision = 0;
                        applyRemoteKanbanUpdate(r);
                    }
                });
            return mutationQueue;
        };
        var getNextListInsertLineStart = function () {
            var currentLineEnd = Number(metaWrapper ? metaWrapper.getAttribute('data-line-end') : interpreterLineEnd) || interpreterLineEnd;
            return Math.max(1, currentLineEnd - 1);
        };
        pre.classList.add('kanban-hidden');
        board.classList.add('kanban-board-ready');
        if (!isWritable) {
            var readOnlyNotice = document.createElement('div');
            readOnlyNotice.className = 'kanban-read-only-notice';
            readOnlyNotice.textContent = 'Read only';
            board.appendChild(readOnlyNotice);
        }

        var findNearestBoardCardList = function (clientX, clientY) {
            if (!Number.isFinite(clientX) || !Number.isFinite(clientY)) {
                return null;
            }
            var candidates = board.querySelectorAll('.kanban-card-list');
            var nearest = null;
            var nearestDistance = Number.POSITIVE_INFINITY;
            Array.prototype.forEach.call(candidates, function (candidate) {
                var rect = candidate.getBoundingClientRect();
                var centerX = rect.left + (rect.width / 2);
                var centerY = rect.top + (rect.height / 2);
                var distance = Math.hypot(clientX - centerX, clientY - centerY);
                if (distance < nearestDistance) {
                    nearestDistance = distance;
                    nearest = candidate;
                }
            });
            return nearest;
        };
        var boardFileDragDepth = 0;
        var setBoardFileDropFeedback = function (active) {
            if (active) {
                board.style.setProperty('outline', '2px dashed var(--kanban-primary)', 'important');
                board.style.setProperty('background', 'var(--color-link-hover-bg)', 'important');
                return;
            }
            board.style.removeProperty('outline');
            board.style.removeProperty('background');
        };

        var boardAutoScrollState = {
            active: false,
            pointerX: null,
            pointerY: null,
            verticalTarget: null,
            frameId: null
        };
        var requestAutoScrollFrame = function (callback) {
            if (window.requestAnimationFrame) {
                return window.requestAnimationFrame(callback);
            }
            return window.setTimeout(callback, 16);
        };
        var cancelAutoScrollFrame = function (frameId) {
            if (frameId === null || frameId === undefined) {
                return;
            }
            if (window.cancelAnimationFrame) {
                window.cancelAnimationFrame(frameId);
                return;
            }
            window.clearTimeout(frameId);
        };
        var stepBoardAutoScroll = function () {
            if (!boardAutoScrollState.active) {
                boardAutoScrollState.frameId = null;
                return;
            }
            var pointerX = boardAutoScrollState.pointerX;
            var pointerY = boardAutoScrollState.pointerY;
            if (Number.isFinite(pointerX) && board.scrollWidth > board.clientWidth) {
                var rect = board.getBoundingClientRect();
                var rectLeft = Number.isFinite(rect.left) ? rect.left : 0;
                var rectWidth = Number.isFinite(rect.width) ? rect.width : board.clientWidth;
                var rectRight = Number.isFinite(rect.right) ? rect.right : rectLeft + rectWidth;
                var edgeSize = Math.min(96, Math.max(48, rectWidth * 0.18));
                var maxStep = 28;
                var scrollDelta = 0;
                if (pointerX < rectLeft + edgeSize) {
                    scrollDelta = -maxStep * Math.min(1, (rectLeft + edgeSize - pointerX) / edgeSize);
                } else if (pointerX > rectRight - edgeSize) {
                    scrollDelta = maxStep * Math.min(1, (pointerX - (rectRight - edgeSize)) / edgeSize);
                }
                if (scrollDelta !== 0) {
                    board.scrollLeft += scrollDelta;
                }
            }
            var verticalTarget = boardAutoScrollState.verticalTarget;
            if (verticalTarget && Number.isFinite(pointerY) && verticalTarget.scrollHeight > verticalTarget.clientHeight) {
                var listRect = verticalTarget.getBoundingClientRect();
                var listTop = Number.isFinite(listRect.top) ? listRect.top : 0;
                var listHeight = Number.isFinite(listRect.height) ? listRect.height : verticalTarget.clientHeight;
                var listBottom = Number.isFinite(listRect.bottom) ? listRect.bottom : listTop + listHeight;
                var verticalEdgeSize = Math.min(80, Math.max(36, listHeight * 0.18));
                var maxVerticalStep = 24;
                var verticalDelta = 0;
                if (pointerY < listTop + verticalEdgeSize) {
                    verticalDelta = -maxVerticalStep * Math.min(1, (listTop + verticalEdgeSize - pointerY) / verticalEdgeSize);
                } else if (pointerY > listBottom - verticalEdgeSize) {
                    verticalDelta = maxVerticalStep * Math.min(1, (pointerY - (listBottom - verticalEdgeSize)) / verticalEdgeSize);
                }
                if (verticalDelta !== 0) {
                    verticalTarget.scrollTop += verticalDelta;
                }
            }
            boardAutoScrollState.frameId = requestAutoScrollFrame(stepBoardAutoScroll);
        };
        var startBoardAutoScroll = function () {
            if (boardAutoScrollState.active) {
                return;
            }
            boardAutoScrollState.active = true;
            boardAutoScrollState.frameId = requestAutoScrollFrame(stepBoardAutoScroll);
        };
        var updateBoardAutoScrollPointer = function (evt, verticalTarget) {
            if (!evt) {
                return;
            }
            if (Number.isFinite(evt.clientX)) {
                boardAutoScrollState.pointerX = evt.clientX;
            }
            if (Number.isFinite(evt.clientY)) {
                boardAutoScrollState.pointerY = evt.clientY;
            }
            if (verticalTarget && verticalTarget.classList && verticalTarget.classList.contains('kanban-card-list')) {
                boardAutoScrollState.verticalTarget = verticalTarget;
            }
        };
        var stopBoardAutoScroll = function () {
            boardAutoScrollState.active = false;
            boardAutoScrollState.pointerX = null;
            boardAutoScrollState.pointerY = null;
            boardAutoScrollState.verticalTarget = null;
            cancelAutoScrollFrame(boardAutoScrollState.frameId);
            boardAutoScrollState.frameId = null;
        };

        var backgroundDragState = {
            active: false,
            startX: 0,
            startScrollLeft: 0,
            moved: false
        };
        var clearBackgroundDrag = function () {
            if (!backgroundDragState.active) {
                return;
            }
            backgroundDragState.active = false;
            board.classList.remove('kanban-dragging');
            board.classList.add('kanban-draggable');
            board.style.userSelect = '';
            document.body.style.userSelect = '';
        };
        var canStartBackgroundDrag = function (event) {
            if (!event || event.button !== 0) {
                return false;
            }
            if (board.scrollWidth <= board.clientWidth) {
                return false;
            }
            return event.target === board;
        };
        board.classList.add('kanban-draggable');
        board.addEventListener('mousedown', function (event) {
            if (!canStartBackgroundDrag(event)) {
                return;
            }
            backgroundDragState.active = true;
            backgroundDragState.startX = event.clientX;
            backgroundDragState.startScrollLeft = board.scrollLeft;
            backgroundDragState.moved = false;
            board.classList.add('kanban-dragging');
            document.body.style.userSelect = 'none';
            event.preventDefault();
        });
        document.addEventListener('mousemove', function (event) {
            if (!backgroundDragState.active) {
                return;
            }
            var deltaX = event.clientX - backgroundDragState.startX;
            if (Math.abs(deltaX) > 2) {
                backgroundDragState.moved = true;
            }
            board.scrollLeft = backgroundDragState.startScrollLeft - deltaX;
        });
        document.addEventListener('mouseup', function () {
            clearBackgroundDrag();
        });
        board.addEventListener('mouseleave', function () {
            if (!backgroundDragState.active) {
                return;
            }
            board.classList.add('kanban-draggable');
        });
        board.addEventListener('dragenter', function (evt) {
            if (!isWritable) {
                return;
            }
            if (!isFileDragEvent(evt) || getOpenedCardOverlay()) {
                return;
            }
            boardFileDragDepth += 1;
            evt.preventDefault();
            evt.stopPropagation();
            setBoardFileDropFeedback(true);
        });
        board.addEventListener('dragover', function (evt) {
            if (!isWritable) {
                return;
            }
            if (!isFileDragEvent(evt) || getOpenedCardOverlay()) {
                return;
            }
            evt.preventDefault();
            evt.stopPropagation();
            if (evt.dataTransfer) {
                evt.dataTransfer.dropEffect = 'copy';
            }
            setBoardFileDropFeedback(true);
        });
        board.addEventListener('dragleave', function (evt) {
            if (!isWritable) {
                return;
            }
            if (!isFileDragEvent(evt)) {
                return;
            }
            boardFileDragDepth = Math.max(0, boardFileDragDepth - 1);
            if (boardFileDragDepth === 0) {
                setBoardFileDropFeedback(false);
            }
        });
        board.addEventListener('drop', function (evt) {
            if (!isWritable) {
                return;
            }
            if (!isFileDragEvent(evt) || getOpenedCardOverlay()) {
                return;
            }
            var files = Array.prototype.slice.call((evt.dataTransfer && evt.dataTransfer.files) || []).filter(Boolean);
            if (!files.length) {
                return;
            }
            evt.preventDefault();
            evt.stopPropagation();
            boardFileDragDepth = 0;
            setBoardFileDropFeedback(false);

            var nearestList = findNearestBoardCardList(evt.clientX, evt.clientY);
            if (!nearestList || !nearestList.parentElement) {
                return;
            }
            var targetColumnIndex = Number(nearestList.parentElement.getAttribute('data-column-index'));
            var targetColumn = columns[targetColumnIndex];
            if (!targetColumn) {
                return;
            }
            createCardsFromFiles(pageName, targetColumn, files).then(function (cards) {
                if (!cards.length) {
                    return;
                }
                rerenderColumns();
                enqueueMutation(function () {
                    var firstCard = cards[0] || {};
                    var revisionCardTitle = cards.length === 1 ? (firstCard.text || '') : ((firstCard.text || '') + ' and ' + String(cards.length - 1) + ' more cards');
                    return persistColumns('card:add', { eventPrefix: 'User:' + getCurrentAuthor(), cardId: firstCard.id || '', cardTitle: revisionCardTitle }).catch(function (error) {
                        console.error('[Kanban] failed to save dropped file cards from board', error);
                    });
                });
            }).catch(function (error) {
                console.error('[Kanban] failed to create dropped file cards from board', error);
                alert('File upload failed. ' + (error && error.message ? error.message : ''));
            });
        });


        var addListWrapper = document.createElement('div');
        addListWrapper.className = 'kanban-add-list-wrapper';

        var addListButton = document.createElement('button');
        addListButton.type = 'button';
        addListButton.innerHTML = '<i class="fas fa-plus" aria-hidden="true"></i> Add a List';
        addListButton.className = 'kanban-add-list-button';

        var addListEditor = document.createElement('div');
        addListEditor.className = 'kanban-add-list-editor kanban-hidden';

        var addListInput = document.createElement('input');
        addListInput.type = 'text';
        addListInput.placeholder = 'Enter list name';
        addListInput.className = 'kanban-add-list-input';

        var addListActions = document.createElement('div');
        addListActions.className = 'kanban-add-list-actions';

        var submitListButton = document.createElement('button');
        submitListButton.type = 'button';
        submitListButton.innerHTML = '<i class="fas fa-plus" aria-hidden="true"></i> Add a List';
        submitListButton.className = 'kanban-primary-button';

        var cancelListButton = document.createElement('button');
        cancelListButton.type = 'button';
        cancelListButton.innerHTML = '<i class="fas fa-times" aria-hidden="true"></i> Cancel';
        cancelListButton.className = 'kanban-ghost-button';

        addListActions.appendChild(submitListButton);
        addListActions.appendChild(cancelListButton);
        addListEditor.appendChild(addListInput);
        addListEditor.appendChild(addListActions);
        addListWrapper.appendChild(addListButton);
        addListWrapper.appendChild(addListEditor);


        var countSerializedCardLines = function (targetCard) {
            var total = 1; // ==== title ====
            total += 1; // ===== Property
            Object.keys((targetCard && targetCard.properties) || {}).forEach(function (key) {
                var values = (((targetCard && targetCard.properties) || {})[key]) || [];
                if (values.length <= 1) {
                    total += 1;
                } else {
                    total += 1 + values.length;
                }
            });
            total += 1; // ===== Activity
            (targetCard && targetCard.comments || []).forEach(function (comment) {
                if (!comment || !comment.header) {
                    return;
                }
                total += 1;
                total += (comment.details || []).length;
            });
            return total;
        };

        var normalizeLineNumbers = function () {
            var cursor = interpreterStartLine;
            columns.forEach(function (targetColumn) {
                targetColumn.lineNumber = cursor;
                cursor += 1;

                (targetColumn.cards || []).forEach(function (targetCard) {
                    targetCard.lineNumber = cursor;
                    cursor += countSerializedCardLines(targetCard);
                });
            });
        };

        var renderColumns = function () {
            Array.prototype.slice.call(board.querySelectorAll('.kanban-column')).forEach(function (node) {
                board.removeChild(node);
            });

            columns.forEach(function (column, index) {
                board.insertBefore(createColumnElement(root, columns, column, index, normalizeLineNumbers, getCardInsertLineStart, enqueueMutation, renderColumns, persistColumns, isWritable, {
                    start: startBoardAutoScroll,
                    update: updateBoardAutoScrollPointer,
                    stop: stopBoardAutoScroll
                }), isWritable ? addListWrapper : null);
            });
        };
        rerenderColumns = renderColumns;

        var applyRemoteKanbanUpdate = function (revision) {
            if (!pageName) { return; }
            fetch('/w/' + encodeURIComponent(pageName) + '?action=raw', { credentials: 'same-origin' })
                .then(function (response) {
                    if (!response.ok) { throw new Error('fetch raw failed: ' + response.status); }
                    return response.text();
                })
                .then(function (rawText) {
                    var block = findKanbanBlockInRaw(rawText, interpreterStartLine);
                    if (!block) {
                        console.warn('[Kanban] remote update: kanban block not found');
                        return;
                    }
                    var serverColumns = parseKanbanText(block.contentText, block.interpreterLineStart);
                    var activeOverlay = getOpenedCardOverlay();
                    var activeCardId = activeOverlay ? (activeOverlay.getAttribute('data-card-id') || '') : '';
                    var baseIdx = buildCardIndex(baseColumns);
                    var localIdx = buildCardIndex(columns);
                    var serverIdx = buildCardIndex(serverColumns);

                    var merged = deepCloneColumns(serverColumns);
                    var conflictCardIds = [];
                    var activeCardServerEntry = null;

                    // Re-insert locally-added cards not yet on server
                    localIdx.cards.forEach(function (entry) {
                        if (!serverIdx.byId[entry.id] && !baseIdx.byId[entry.id]) {
                            if (merged[entry.columnIndex]) {
                                merged[entry.columnIndex].cards.push(cloneCardData(entry.card));
                            }
                        }
                    });

                    // 3-way merge for cards present in server
                    serverIdx.cards.forEach(function (serverEntry) {
                        var id = serverEntry.id;
                        var localEntry = localIdx.byId[id];
                        var baseEntry = baseIdx.byId[id];
                        if (!localEntry) { return; }
                        // comments는 별도 union 병합하므로 데이터 필드(제목·설명·속성)만 비교
                        var localDataChanged = !baseEntry || !cardDataEqual(localEntry.card, baseEntry.card);
                        var serverDataChanged = !baseEntry || !cardDataEqual(serverEntry.card, baseEntry.card);
                        var mergedCol = merged[serverEntry.columnIndex];
                        if (!mergedCol) { return; }
                        var mergedCardIdx = -1;
                        for (var ci = 0; ci < mergedCol.cards.length; ci++) {
                            if (mergedCol.cards[ci].id === id) { mergedCardIdx = ci; break; }
                        }
                        if (mergedCardIdx < 0) { return; }

                        if (id === activeCardId) {
                            // 모달에서 열린 카드: 동일 객체 참조 유지
                            // title/description/properties 업데이트는 modal의 handleRemoteCardUpdate가 처리
                            localEntry.card.comments = mergeComments(serverEntry.card.comments, localEntry.card.comments);
                            mergedCol.cards[mergedCardIdx] = localEntry.card;
                            activeCardServerEntry = serverEntry;
                            return;
                        }

                        // 데이터 필드 병합
                        if (localDataChanged && serverDataChanged) {
                            mergedCol.cards[mergedCardIdx].__remoteConflict = true;
                            conflictCardIds.push(id);
                        } else if (localDataChanged && !serverDataChanged) {
                            mergedCol.cards[mergedCardIdx] = cloneCardData(localEntry.card);
                        }

                        // 댓글 union 병합: server ∪ local (header 기준 중복 제거)
                        mergedCol.cards[mergedCardIdx].comments = mergeComments(
                            mergedCol.cards[mergedCardIdx].comments,
                            localEntry.card.comments
                        );
                    });

                    columns = merged;
                    interpreterStartLine = block.interpreterLineStart;
                    currentKanbanLineCount = getLineCountForText(block.contentText);
                    root.setAttribute('data-kanban-line-count', String(currentKanbanLineCount));
                    if (metaWrapper) {
                        metaWrapper.setAttribute('data-line-start', String(block.interpreterLineStart - (hasShebang ? 1 : 0)));
                        metaWrapper.setAttribute('data-line-end', String(block.lineEnd));
                    }
                    pre.textContent = block.contentText;
                    setCurrentRevision(revision);
                    normalizeLineNumbers();
                    rerenderColumns();

                    // 모달에 카드 업데이트 이벤트 전달 (title/description/properties를 모달이 직접 처리)
                    if (activeCardId && activeCardServerEntry) {
                        try {
                            root.dispatchEvent(new CustomEvent('kanban:card.remoteupdated', {
                                detail: { cardId: activeCardId, serverCard: activeCardServerEntry.card }
                            }));
                        } catch (e) {}
                    }

                    if (conflictCardIds.length > 0) {
                        console.info('[Kanban] remote update applied with ' + conflictCardIds.length + ' conflict(s) — server wins', conflictCardIds);
                    } else {
                        console.info('[Kanban] remote update applied cleanly', { revision: revision });
                    }
                    try {
                        document.dispatchEvent(new CustomEvent('wiki:kanban.synced', {
                            detail: { pageName: pageName, revision: revision }
                        }));
                    } catch (e) { /* ignore */ }
                })
                .catch(function (error) {
                    console.warn('[Kanban] remote update failed', error);
                });
        };

        var remoteUpdateDebounceTimer = null;
        var currentCardUpdateHandler = null;
        var handleRemotePageUpdated = function (revision) {
            if (remoteUpdateDebounceTimer) { clearTimeout(remoteUpdateDebounceTimer); }
            remoteUpdateDebounceTimer = setTimeout(function () {
                remoteUpdateDebounceTimer = null;
                if (pendingMutationCount > 0) {
                    pendingRemoteRevision = Math.max(pendingRemoteRevision, revision);
                    return;
                }
                applyRemoteKanbanUpdate(revision);
            }, 500);
        };

        var findRenderedCardById = function (cardId) {
            if (!cardId) {
                return null;
            }
            var cards = board.querySelectorAll('.kanban-card');
            for (var i = 0; i < cards.length; i += 1) {
                if ((cards[i].getAttribute('data-card-id') || '') === cardId) {
                    return cards[i];
                }
            }
            return null;
        };
        var adjustScrollToReveal = function (container, target, axis, padding) {
            if (!container || !target) {
                return;
            }
            var containerSize = axis === 'x' ? container.clientWidth : container.clientHeight;
            var containerScrollSize = axis === 'x' ? container.scrollWidth : container.scrollHeight;
            if (!Number.isFinite(containerSize) || !Number.isFinite(containerScrollSize) || containerScrollSize <= containerSize) {
                return;
            }
            var containerRect = container.getBoundingClientRect();
            var targetRect = target.getBoundingClientRect();
            var containerStart = axis === 'x' ? containerRect.left : containerRect.top;
            var containerEnd = axis === 'x' ? containerRect.right : containerRect.bottom;
            var targetStart = axis === 'x' ? targetRect.left : targetRect.top;
            var targetEnd = axis === 'x' ? targetRect.right : targetRect.bottom;
            var currentScroll = axis === 'x' ? container.scrollLeft : container.scrollTop;
            var delta = 0;

            padding = Number.isFinite(padding) ? padding : 0;
            if (targetStart < containerStart + padding) {
                delta = targetStart - containerStart - padding;
            } else if (targetEnd > containerEnd - padding) {
                delta = targetEnd - containerEnd + padding;
            }

            if (delta === 0) {
                return;
            }
            var nextScroll = Math.max(0, currentScroll + delta);
            if (axis === 'x') {
                container.scrollLeft = nextScroll;
            } else {
                container.scrollTop = nextScroll;
            }
        };
        var scrollKanbanToCard = function (cardElement) {
            if (!cardElement) {
                return;
            }
            var targetColumn = cardElement.closest ? cardElement.closest('.kanban-column') : null;
            var targetList = cardElement.closest ? cardElement.closest('.kanban-card-list') : null;

            adjustScrollToReveal(board, targetColumn || cardElement, 'x', 14);
            adjustScrollToReveal(targetList, cardElement, 'y', 8);
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
            addListEditor.classList.add('kanban-hidden');
            addListButton.classList.remove('kanban-hidden');
            addListInput.value = '';
        };

        addListButton.addEventListener('click', function () {
            addListButton.classList.add('kanban-hidden');
            addListEditor.classList.remove('kanban-hidden');
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

                return persistColumns('list:add', { eventPrefix: 'User:' + getCurrentAuthor(), listTitle: trimmed })
                    .then(function (result) {
                        renderColumns();
                        console.info('[Kanban] list added', result);
                    })
                    .catch(function (error) {
                        console.error('[Kanban] failed to save list add', error);
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

        if (isWritable) {
            board.appendChild(addListWrapper);
        }
        renderColumns();

        document.addEventListener('wiki:page.updated', function (evt) {
            var detail = evt && evt.detail;
            if (!detail || detail.pageName !== pageName) { return; }
            handleRemotePageUpdated(detail.revision);
        });

        document.addEventListener('wiki:ws.connected', function (evt) {
            var detail = evt && evt.detail;
            if (!detail || detail.pageName !== pageName) { return; }
            // WS 재연결 시 누락된 변경이 있을 수 있으므로 최신 revision 확인 후 동기화
            fetchLatestRevision(pageName).then(function (latestRevision) {
                if (latestRevision > getCurrentRevision()) {
                    handleRemotePageUpdated(latestRevision);
                }
            }).catch(function () {});
        });

        openCardDetail = function (card) {
            if (!card) {
                return;
            }
            var pageName = root.getAttribute('data-page-name') || '';
            if (currentCardUpdateHandler) {
                root.removeEventListener('kanban:card.remoteupdated', currentCardUpdateHandler);
                currentCardUpdateHandler = null;
            }
            closeOpenedCardOverlay();
            setHashCardId(card.id || '');
            var overlay = document.createElement('div');
            overlay.className = 'kanban-card-detail-overlay';
            overlay.setAttribute('data-card-id', card.id || '');
            var modal = document.createElement('div');
            modal.className = 'kanban-modal';
            var dragDepthInModal = 0;
            var dragDepthInOverlay = 0;
            var modalDropHint = document.createElement('div');
            modalDropHint.textContent = 'Drop files here to attach to this card';
            modalDropHint.className = 'kanban-modal-drop-hint kanban-hidden';
            var setModalDropFeedback = function (active) {
                if (active) {
                    modal.classList.add('kanban-modal-drop-active');
                    modalDropHint.classList.remove('kanban-hidden');
                    return;
                }
                modal.classList.remove('kanban-modal-drop-active');
                modalDropHint.classList.add('kanban-hidden');
            };
            var logModalDragEvent = function (scope, evt, extra) {
                var payload = Object.assign({
                    scope: scope,
                    type: evt && evt.type ? evt.type : '',
                    hasFiles: Boolean(evt && evt.dataTransfer && evt.dataTransfer.files && evt.dataTransfer.files.length),
                    fileCount: evt && evt.dataTransfer && evt.dataTransfer.files ? evt.dataTransfer.files.length : 0,
                    cardId: card && card.id ? card.id : ''
                }, extra || {});
                console.debug('[Kanban][Drag]', payload);
            };

            var header = document.createElement('div');
            header.className = 'kanban-modal-header';

            var title = document.createElement('h3');
            title.textContent = card.text || '';
            title.className = 'kanban-modal-title';
            if (!isWritable) { title.classList.add('kanban-read-only'); }
            title.title = isWritable ? 'Click to edit title' : '';

            var cardIdLabel = document.createElement('a');
            var cardId = (card.id || '').trim();
            var cardUrl = buildCardUrl(cardId);
            if (cardId) {
                cardIdLabel.href = cardUrl;
                cardIdLabel.textContent = cardId;
            } else {
                cardIdLabel.href = '#';
                cardIdLabel.textContent = '-';
            }
            cardIdLabel.className = 'kanban-modal-card-id';
            cardIdLabel.title = cardId ? 'Copy card URL' : '';
            var cardIdCopyFeedback = document.createElement('span');
            cardIdCopyFeedback.className = 'kanban-card-copy-feedback kanban-hidden';
            cardIdCopyFeedback.setAttribute('aria-live', 'polite');
            var cardIdCopyFeedbackTimer = null;
            var showCardIdCopyFeedback = function (message) {
                cardIdCopyFeedback.textContent = message;
                cardIdCopyFeedback.classList.remove('kanban-hidden');
                if (cardIdCopyFeedbackTimer && window.clearTimeout) {
                    window.clearTimeout(cardIdCopyFeedbackTimer);
                }
                if (window.setTimeout) {
                    cardIdCopyFeedbackTimer = window.setTimeout(function () {
                        cardIdCopyFeedback.classList.add('kanban-hidden');
                        cardIdCopyFeedback.textContent = '';
                        cardIdCopyFeedbackTimer = null;
                    }, 1200);
                }
            };
            cardIdLabel.addEventListener('click', function (evt) {
                evt.preventDefault();
                if (evt.stopPropagation) {
                    evt.stopPropagation();
                }
                if (!cardId) {
                    return;
                }
                copyTextToClipboard(cardUrl).then(function (copied) {
                    cardIdLabel.title = copied ? 'Copied card URL' : 'Copy failed';
                    showCardIdCopyFeedback(copied ? 'Copied' : 'Copy failed');
                    if (window.setTimeout) {
                        window.setTimeout(function () {
                            cardIdLabel.title = 'Copy card URL';
                        }, 1200);
                    }
                });
            });

            var titleWrap = document.createElement('div');
            titleWrap.className = 'kanban-modal-title-wrap';

            var titleDisplay = document.createElement('div');
            titleDisplay.className = 'kanban-modal-title-display';
            titleDisplay.appendChild(title);
            titleDisplay.appendChild(cardIdLabel);
            titleDisplay.appendChild(cardIdCopyFeedback);

            var titleEditorWrap = document.createElement('div');
            titleEditorWrap.className = 'kanban-modal-title-editor-wrap kanban-hidden';

            var titleEditor = document.createElement('input');
            titleEditor.type = 'text';
            titleEditor.value = card.text || '';
            titleEditor.className = 'kanban-modal-title-editor';

            var titleSaveButton = document.createElement('button');
            titleSaveButton.type = 'button';
            titleSaveButton.textContent = 'Save';
            titleSaveButton.className = 'kanban-modal-title-save-btn';

            titleWrap.appendChild(titleDisplay);
            titleWrap.appendChild(titleEditorWrap);

            var headerActions = document.createElement('div');
            headerActions.className = 'kanban-modal-header-actions';

            var deleteCardButton = document.createElement('button');
            deleteCardButton.type = 'button';
            deleteCardButton.innerHTML = '<i class="fas fa-trash-alt" aria-hidden="true"></i>';
            deleteCardButton.title = 'Delete Card';
            deleteCardButton.className = 'kanban-modal-btn-delete';

            var closeButton = document.createElement('button');
            closeButton.type = 'button';
            closeButton.innerHTML = '<i class="fas fa-times" aria-hidden="true"></i>';
            closeButton.className = 'kanban-modal-btn-close';

            var closeTitleEditor = function () {
                titleEditorWrap.classList.add('kanban-hidden');
                titleDisplay.classList.remove('kanban-hidden');
                titleEditor.value = card.text || '';
            };

            var openTitleEditor = function () {
                titleDisplay.classList.add('kanban-hidden');
                titleEditorWrap.classList.remove('kanban-hidden');
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
                prependCardActivity(card, [extractActivityDetailFromRevisionComment(buildKanbanSaveComment('card:rename', { eventPrefix: 'User:' + getCurrentAuthor(), cardId: card.id || '', cardTitle: card.text || '', fromTitle: previousCardTitle, toTitle: nextTitle || '' }))]);
                updateCardCommentCount(card);
                title.textContent = nextTitle;
                closeTitleEditor();
                renderColumns();

                enqueueMutation(function () {
                    return persistColumns('card:rename', { eventPrefix: 'User:' + getCurrentAuthor(), cardId: card.id || '', cardTitle: card.text || '', fromTitle: previousCardTitle, toTitle: nextTitle || '' }).catch(function (error) {
                        console.error('[Kanban] failed to save card title', error);
                    });
                });
            };

            if (isWritable) {
                title.addEventListener('click', openTitleEditor);
                titleSaveButton.addEventListener('click', submitTitleEditor);
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
            }

            var descriptionTitle = document.createElement('div');
            descriptionTitle.innerHTML = '<i class="fas fa-align-left" aria-hidden="true"></i> Description';
            descriptionTitle.className = 'kanban-modal-section-title';

            var getDescriptionText = function () {
                return (card.description || []).join('\n');
            };

            var descriptionDisplay = document.createElement('div');
            descriptionDisplay.className = 'kanban-description-display';

            var descriptionEditorWrap = document.createElement('div');
            descriptionEditorWrap.className = 'kanban-description-editor-wrap kanban-hidden';

            var descriptionTextarea = document.createElement('textarea');
            descriptionTextarea.className = 'kanban-description-textarea';
            descriptionTextarea.placeholder = 'Add a description...';

            var descriptionEditorActions = document.createElement('div');
            descriptionEditorActions.className = 'kanban-description-action-bar';

            var descriptionSaveButton = document.createElement('button');
            descriptionSaveButton.type = 'button';
            descriptionSaveButton.innerHTML = '<i class="fas fa-save" aria-hidden="true"></i> Save Description';
            descriptionSaveButton.className = 'kanban-description-save-btn';

            var descriptionCancelButton = document.createElement('button');
            descriptionCancelButton.type = 'button';
            descriptionCancelButton.textContent = 'Cancel';
            descriptionCancelButton.className = 'kanban-description-cancel-btn';

            descriptionEditorActions.appendChild(descriptionSaveButton);
            descriptionEditorActions.appendChild(descriptionCancelButton);
            descriptionEditorWrap.appendChild(descriptionTextarea);
            descriptionEditorWrap.appendChild(descriptionEditorActions);

            var renderDescriptionDisplay = function () {
                var text = getDescriptionText();
                if (text) {
                    descriptionDisplay.textContent = text;
                    requestRenderInlineComment(pageName, text).then(function (html) {
                        if (html) { descriptionDisplay.innerHTML = html; }
                    }).catch(function () {});
                    descriptionDisplay.classList.remove('kanban-description-placeholder');
                } else {
                    descriptionDisplay.textContent = isWritable ? 'Add a description...' : '';
                    if (isWritable) {
                        descriptionDisplay.classList.add('kanban-description-placeholder');
                    }
                }
            };
            renderDescriptionDisplay();

            var openDescriptionEditor = function () {
                descriptionTextarea.value = getDescriptionText();
                descriptionDisplay.classList.add('kanban-hidden');
                descriptionEditorWrap.classList.remove('kanban-hidden');
                descriptionTextarea.focus();
            };

            var closeDescriptionEditor = function () {
                descriptionEditorWrap.classList.add('kanban-hidden');
                descriptionDisplay.classList.remove('kanban-hidden');
            };

            var submitDescriptionEditor = function () {
                var newText = (descriptionTextarea.value || '').trimEnd();
                var currentText = getDescriptionText();
                closeDescriptionEditor();
                if (newText === currentText) { return; }
                card.description = newText ? newText.split('\n') : [];
                var descriptionPreview = newText
                    ? truncateRevisionText((newText.split('\n')[0] || '').trim(), 60) || '(updated)'
                    : '(removed)';
                var actionMeta = {
                    eventPrefix: 'User:' + getCurrentAuthor(),
                    cardId: card.id || '',
                    cardTitle: card.text || '',
                    descriptionPreview: descriptionPreview
                };
                prependCardActivity(card, [extractActivityDetailFromRevisionComment(buildKanbanSaveComment('card:description:update', actionMeta))]);
                updateCardCommentCount(card);
                renderDescriptionDisplay();
                renderComments();
                enqueueMutation(function () {
                    return persistColumns('card:description:update', actionMeta).catch(function (error) {
                        console.error('[Kanban] failed to save description', error);
                    });
                });
            };

            if (isWritable) {
                descriptionDisplay.style.cursor = 'pointer';
                descriptionDisplay.title = 'Click to edit description';
                descriptionDisplay.addEventListener('click', openDescriptionEditor);
                descriptionSaveButton.addEventListener('click', submitDescriptionEditor);
                descriptionCancelButton.addEventListener('click', closeDescriptionEditor);
                descriptionTextarea.addEventListener('keydown', function (evt) {
                    if (evt.key === 'Escape') {
                        evt.preventDefault();
                        closeDescriptionEditor();
                    }
                });
            }

            var textarea = document.createElement('textarea');
            textarea.placeholder = 'Write a comment...';
            textarea.rows = 3;
            textarea.className = 'kanban-comment-textarea';

            var actionBar = document.createElement('div');
            actionBar.className = 'kanban-comment-action-bar';

            var submit = document.createElement('button');
            submit.type = 'button';
            submit.innerHTML = '<i class="fas fa-comment-medical" aria-hidden="true"></i> Add Comment';
            submit.className = 'kanban-comment-submit-btn';

            var propertyTitle = document.createElement('div');
            propertyTitle.innerHTML = '<i class="fas fa-tags" aria-hidden="true"></i> Properties';
            propertyTitle.className = 'kanban-modal-section-title';

            var propertyList = document.createElement('div');
            var dueDateEditor = document.createElement('div');
            dueDateEditor.className = 'kanban-duedate-row';

            var dueDateInput = document.createElement('input');
            dueDateInput.type = 'date';
            dueDateInput.className = 'kanban-duedate-input';

            var dueDateSaveButton = document.createElement('button');
            dueDateSaveButton.type = 'button';
            dueDateSaveButton.textContent = 'Save DueDate';
            dueDateSaveButton.className = 'kanban-duedate-save-btn';

            var getModalPropertyValues = function (key) {
                return getCardPropertyValues(card, key);
            };

            var arePropertyValuesEqual = function (left, right) {
                if (left.length !== right.length) {
                    return false;
                }
                for (var i = 0; i < left.length; i += 1) {
                    if (left[i] !== right[i]) {
                        return false;
                    }
                }
                return true;
            };

            var getAssigneeSuggestions = function () {
                var seen = {};
                var suggestions = [];
                var addName = function (name) {
                    var safeName = String(name || '').trim();
                    var key = safeName.toLowerCase();
                    if (!safeName || seen[key]) {
                        return;
                    }
                    seen[key] = true;
                    suggestions.push(safeName);
                };
                addName(getCurrentAuthor());
                (columns || []).forEach(function (column) {
                    (column.cards || []).forEach(function (targetCard) {
                        getCardAssigneeNames(targetCard).forEach(addName);
                    });
                });
                return suggestions.sort(function (a, b) { return a.localeCompare(b); });
            };

            var setCardPropertyValues = function (property, nextValues) {
                var normalizedValues = (nextValues || []).map(function (value) {
                    return String(value || '').trim();
                }).filter(Boolean);
                var previousValues = getModalPropertyValues(property);
                if (arePropertyValuesEqual(previousValues, normalizedValues)) {
                    return false;
                }

                card.properties = card.properties || {};
                if (normalizedValues.length > 0) {
                    card.properties[property] = normalizedValues;
                } else {
                    delete card.properties[property];
                }

                var actionMeta = {
                    eventPrefix: 'User:' + getCurrentAuthor(),
                    cardId: card.id || '',
                    cardTitle: card.text || '',
                    property: property,
                    value: normalizedValues
                };
                prependCardActivity(card, [extractActivityDetailFromRevisionComment(buildKanbanSaveComment('card:property:update', actionMeta))]);
                updateCardCommentCount(card);
                renderColumns();
                renderProperties();
                renderComments();
                enqueueMutation(function () {
                    return persistColumns('card:property:update', actionMeta).catch(function (error) {
                        console.error('[Kanban] failed to save card property', error);
                    });
                });
                return true;
            };

            var renderInlineMarkupInto = function (target, markup) {
                target.textContent = markup;
                requestRenderInlineComment(pageName, markup).then(function (html) {
                    if (html) {
                        target.innerHTML = html;
                        clampRenderedInlineImages(target);
                    }
                }).catch(function (error) {
                    console.error('[Kanban] failed to render property value', error);
                });
            };

            var appendPropertyEmpty = function (container, text) {
                var empty = document.createElement('div');
                empty.className = 'kanban-property-empty';
                empty.textContent = text || 'None';
                container.appendChild(empty);
            };

            var createRelationChip = function (property, value, derived) {
                var parsed = parseCardReferenceValue(value);
                var cardIndex = buildCardIndex(columns);
                var entry = parsed && parsed.id ? cardIndex.byId[parsed.id] : null;
                var chip = document.createElement('span');
                chip.className = 'kanban-property-chip kanban-relation-chip';
                if (!entry) {
                    chip.classList.add('kanban-property-chip-unresolved');
                }

                var main = document.createElement('button');
                main.type = 'button';
                main.className = 'kanban-property-chip-main';
                main.disabled = !entry;
                main.title = entry ? 'Open related card' : 'Card not found in this board';

                var title = document.createElement('span');
                title.className = 'kanban-property-chip-title';
                title.textContent = entry ? (entry.card.text || parsed.id) : ((parsed && (parsed.label || parsed.id)) || String(value || '').trim());
                main.appendChild(title);

                if (parsed && parsed.id) {
                    var id = document.createElement('span');
                    id.className = 'kanban-property-chip-id';
                    id.textContent = '#' + parsed.id;
                    main.appendChild(id);
                }

                if (entry) {
                    main.addEventListener('click', function (evt) {
                        evt.preventDefault();
                        evt.stopPropagation();
                        openCardDetailById(parsed.id);
                    });
                }
                chip.appendChild(main);

                if (isWritable && !derived && property) {
                    var remove = document.createElement('button');
                    remove.type = 'button';
                    remove.className = 'kanban-property-chip-remove';
                    remove.innerHTML = '<i class="fas fa-times" aria-hidden="true"></i>';
                    remove.title = 'Remove relation';
                    remove.addEventListener('click', function (evt) {
                        evt.preventDefault();
                        evt.stopPropagation();
                        var nextValues = getModalPropertyValues(property).filter(function (candidate) {
                            var candidateParsed = parseCardReferenceValue(candidate);
                            if (candidateParsed && parsed) {
                                return candidateParsed.id !== parsed.id;
                            }
                            return candidate !== value;
                        });
                        setCardPropertyValues(property, nextValues);
                    });
                    chip.appendChild(remove);
                }
                return chip;
            };

            var createAssigneeChip = function (value) {
                var normalizedValue = normalizeAssigneeValue(value);
                var assigneeName = getAssigneeName(normalizedValue);
                var chip = document.createElement('span');
                chip.className = 'kanban-property-chip kanban-assignee-chip';

                var main = document.createElement('span');
                main.className = 'kanban-property-chip-main';
                if (normalizedValue) {
                    renderInlineMarkupInto(main, normalizedValue);
                } else {
                    main.textContent = value || '';
                }
                chip.appendChild(main);

                if (isWritable && assigneeName) {
                    var remove = document.createElement('button');
                    remove.type = 'button';
                    remove.className = 'kanban-property-chip-remove';
                    remove.innerHTML = '<i class="fas fa-times" aria-hidden="true"></i>';
                    remove.title = 'Remove assignee';
                    remove.addEventListener('click', function (evt) {
                        evt.preventDefault();
                        evt.stopPropagation();
                        var nextValues = getModalPropertyValues('Assignee').filter(function (candidate) {
                            return getAssigneeName(candidate).toLowerCase() !== assigneeName.toLowerCase();
                        });
                        setCardPropertyValues('Assignee', nextValues);
                    });
                    chip.appendChild(remove);
                }
                return chip;
            };

            var appendRelationEditor = function (container, property, currentValues) {
                if (!isWritable) {
                    return;
                }
                var cardIndex = buildCardIndex(columns);
                var currentIds = {};
                (currentValues || []).forEach(function (value) {
                    var parsed = parseCardReferenceValue(value);
                    if (parsed && parsed.id) {
                        currentIds[parsed.id] = true;
                    }
                });

                var editor = document.createElement('div');
                editor.className = 'kanban-property-editor kanban-relation-editor';

                var select = document.createElement('select');
                select.className = 'kanban-property-select';
                var placeholder = document.createElement('option');
                placeholder.value = '';
                placeholder.textContent = 'Select card...';
                select.appendChild(placeholder);
                (columns || []).forEach(function (column) {
                    var group = document.createElement('optgroup');
                    var hasOptions = false;
                    group.label = column && column.title ? column.title : 'Untitled';
                    (column && column.cards || []).forEach(function (targetCard) {
                        if (!targetCard || !targetCard.id || targetCard.id === card.id || currentIds[targetCard.id]) {
                            return;
                        }
                        var option = document.createElement('option');
                        option.value = targetCard.id;
                        option.textContent = targetCard.text || targetCard.id;
                        option.title = '#' + targetCard.id;
                        group.appendChild(option);
                        hasOptions = true;
                    });
                    if (hasOptions) {
                        select.appendChild(group);
                    }
                });

                var input = document.createElement('input');
                input.type = 'text';
                input.className = 'kanban-property-input';
                input.placeholder = '#cardId or [#cardId label]';

                var add = document.createElement('button');
                add.type = 'button';
                add.className = 'kanban-property-add-btn';
                add.textContent = 'Add';

                var submit = function () {
                    var raw = (input.value || '').trim() || (select.value || '').trim();
                    var parsed = parseCardReferenceValue(raw);
                    var targetEntry = parsed && parsed.id ? cardIndex.byId[parsed.id] : null;
                    var normalizedValue;
                    if (!parsed || !parsed.id) {
                        input.focus();
                        return;
                    }
                    if (parsed.id === card.id) {
                        showAlert('A card cannot reference itself.');
                        return;
                    }
                    if (!targetEntry) {
                        var shouldKeepUnresolved = window.confirm ? window.confirm('Card #' + parsed.id + ' was not found in this board. Save unresolved relation?') : true;
                        if (!shouldKeepUnresolved) {
                            return;
                        }
                    }
                    if (currentIds[parsed.id]) {
                        input.value = '';
                        select.value = '';
                        return;
                    }
                    normalizedValue = buildCardReferenceValue(parsed.id, parsed.label || (targetEntry && targetEntry.card ? targetEntry.card.text : ''));
                    setCardPropertyValues(property, getModalPropertyValues(property).concat([normalizedValue]));
                };

                add.addEventListener('click', submit);
                input.addEventListener('keydown', function (evt) {
                    if (evt.key === 'Enter') {
                        evt.preventDefault();
                        submit();
                    }
                });

                editor.appendChild(select);
                editor.appendChild(input);
                editor.appendChild(add);
                container.appendChild(editor);
            };

            var appendAssigneeEditor = function (container) {
                if (!isWritable) {
                    return;
                }
                var editor = document.createElement('div');
                editor.className = 'kanban-property-editor kanban-assignee-editor';

                var input = document.createElement('input');
                input.type = 'text';
                input.className = 'kanban-property-input';
                input.placeholder = 'User nickname or [User:Name]';

                var datalist = document.createElement('datalist');
                datalist.id = 'kanban-assignee-options-' + generateCardId();
                getAssigneeSuggestions().forEach(function (name) {
                    var option = document.createElement('option');
                    option.value = name;
                    datalist.appendChild(option);
                });
                input.setAttribute('list', datalist.id);

                var add = document.createElement('button');
                add.type = 'button';
                add.className = 'kanban-property-add-btn';
                add.textContent = 'Add';

                var submit = function () {
                    var normalizedValue = normalizeAssigneeValue(input.value || '');
                    var assigneeName = getAssigneeName(normalizedValue);
                    var existingNames = getModalPropertyValues('Assignee').map(function (value) {
                        return getAssigneeName(value).toLowerCase();
                    });
                    if (!normalizedValue || !assigneeName) {
                        input.focus();
                        return;
                    }
                    if (existingNames.indexOf(assigneeName.toLowerCase()) >= 0) {
                        input.value = '';
                        return;
                    }
                    setCardPropertyValues('Assignee', getModalPropertyValues('Assignee').concat([normalizedValue]));
                };

                add.addEventListener('click', submit);
                input.addEventListener('keydown', function (evt) {
                    if (evt.key === 'Enter') {
                        evt.preventDefault();
                        submit();
                    }
                });

                editor.appendChild(input);
                editor.appendChild(datalist);
                editor.appendChild(add);
                container.appendChild(editor);
            };

            var renderRelationPropertyRow = function (row, label, key, values, derived) {
                row.classList.add('kanban-relation-row');
                if (key) {
                    row.setAttribute('data-relation-key', key);
                }
                if (derived) {
                    row.classList.add('kanban-property-row-derived');
                }
                row.appendChild(label);

                var valueCell = document.createElement('div');
                valueCell.className = 'kanban-property-value kanban-property-composite';

                var chipList = document.createElement('div');
                chipList.className = 'kanban-property-chip-list';
                (values || []).forEach(function (value) {
                    chipList.appendChild(createRelationChip(derived ? '' : key, value, derived));
                });
                if (chipList.children.length === 0) {
                    appendPropertyEmpty(chipList, 'No cards linked');
                }
                valueCell.appendChild(chipList);
                if (!derived) {
                    appendRelationEditor(valueCell, key, values || []);
                }
                row.appendChild(valueCell);
            };

            var renderAssigneePropertyRow = function (row, label, values) {
                row.appendChild(label);

                var valueCell = document.createElement('div');
                valueCell.className = 'kanban-property-value kanban-property-composite';

                var chipList = document.createElement('div');
                chipList.className = 'kanban-property-chip-list';
                (values || []).forEach(function (value) {
                    var normalizedValue = normalizeAssigneeValue(value);
                    if (normalizedValue) {
                        chipList.appendChild(createAssigneeChip(normalizedValue));
                    }
                });
                if (chipList.children.length === 0) {
                    appendPropertyEmpty(chipList, 'No assignees');
                }
                valueCell.appendChild(chipList);
                appendAssigneeEditor(valueCell);
                row.appendChild(valueCell);
            };

            var renderProperties = function () {
                propertyList.innerHTML = '';
                var propertyKeys = Object.keys(card.properties || {});
                var ensurePropertyKey = function (key) {
                    if (propertyKeys.indexOf(key) < 0) {
                        propertyKeys.push(key);
                    }
                };
                var propertySortRank = {
                    Creator: 0,
                    Assignee: 10,
                    DueDate: 20,
                    BlockedBy: 30,
                    ParentCard: 31,
                    RelatedCard: 32,
                    Attachment: 90
                };
                if (isWritable) {
                    ensurePropertyKey('Assignee');
                    ensurePropertyKey('DueDate');
                    CARD_RELATION_PROPERTY_KEYS.forEach(ensurePropertyKey);
                }
                var derivedRelations = computeDerivedCardRelationValues(columns, card);
                var propertyEntries = propertyKeys.filter(function (key) {
                    if (isWritable && (key === 'DueDate' || key === 'Assignee' || isCardRelationProperty(key))) { return true; }
                    var value = (card.properties || {})[key];
                    if (Array.isArray(value)) {
                        return value.length > 0;
                    }
                    return value !== undefined && value !== null && String(value).trim() !== '';
                }).sort(function (a, b) {
                    var rankA = Object.prototype.hasOwnProperty.call(propertySortRank, a) ? propertySortRank[a] : 50;
                    var rankB = Object.prototype.hasOwnProperty.call(propertySortRank, b) ? propertySortRank[b] : 50;
                    if (rankA !== rankB) {
                        return rankA - rankB;
                    }
                    return a.localeCompare(b);
                });
                var hasDerivedRelations = DERIVED_CARD_RELATION_PROPERTIES.some(function (definition) {
                    return (derivedRelations[definition.key] || []).length > 0;
                });
                if (propertyEntries.length === 0 && !hasDerivedRelations) {
                    var empty = document.createElement('div');
                    empty.textContent = 'No properties';
                    empty.className = 'kanban-property-empty';
                    propertyList.appendChild(empty);
                    return;
                }

                propertyEntries.forEach(function (key) {
                    var values = getModalPropertyValues(key);
                    var row = document.createElement('div');
                    row.className = 'kanban-property-row';

                    var label = document.createElement('div');
                    label.textContent = key;
                    label.className = 'kanban-property-key';

                    if (key === 'Assignee') {
                        renderAssigneePropertyRow(row, label, values);
                        propertyList.appendChild(row);
                        return;
                    }

                    if (isCardRelationProperty(key)) {
                        renderRelationPropertyRow(row, label, key, values, false);
                        propertyList.appendChild(row);
                        return;
                    }

                    if (isWritable && key === 'DueDate') {
                        var dueDates = getModalPropertyValues('DueDate');
                        dueDateInput.value = dueDates.length > 0 ? String(dueDates[0]).replace(/^\[|\]$/g, '') : '';
                        var dueDateValueCell = document.createElement('div');
                        dueDateValueCell.className = 'kanban-duedate-inline';
                        dueDateValueCell.appendChild(dueDateInput);
                        dueDateValueCell.appendChild(dueDateSaveButton);
                        row.appendChild(label);
                        row.appendChild(dueDateValueCell);
                        propertyList.appendChild(row);
                        return;
                    }

                    if (Array.isArray(values)) {
                        row.appendChild(label);
                        var attachmentGrid = null;
                        if (key === 'Attachment') {
                            attachmentGrid = document.createElement('div');
                            row.appendChild(attachmentGrid);
                        }
                        values.forEach(function (value) {
                            var rawValue = value;
                            var displayValue = value;
                            if (key === 'Creator') {
                                displayValue = toUserLinkMarkup(value);
                            } else if (key === 'dateCreated') {
                                displayValue = toClientKanbanDateTime(value);
                            }
                            var valueRow = document.createElement('div');
                            valueRow.textContent = displayValue;
                            valueRow.className = 'kanban-property-value';
                            var attachmentItem = null;
                            if (attachmentGrid) {
                                attachmentItem = document.createElement('div');
                                attachmentItem.style.position = 'relative';
                                attachmentItem.style.display = 'inline-block';
                                attachmentItem.style.marginRight = '12px';
                                attachmentItem.style.marginBottom = '12px';
                                valueRow.style.paddingLeft = '0';
                                attachmentItem.appendChild(valueRow);
                                attachmentGrid.appendChild(attachmentItem);
                            } else {
                                row.appendChild(valueRow);
                            }
                            if (isWritable && key === 'Attachment') {
                                var removeButton = document.createElement('button');
                                removeButton.type = 'button';
                                removeButton.innerHTML = '<i class="fas fa-trash-alt" aria-hidden="true"></i>';
                                removeButton.title = '첨부파일 삭제';
                                removeButton.setAttribute('aria-label', '첨부파일 삭제');
                                removeButton.className = 'kanban-attachment-remove-btn';
                                removeButton.addEventListener('click', function () {
                                    if (!window.confirm('이 첨부파일을 삭제할까요?')) {
                                        return;
                                    }
                                    var attachmentObjectKey = extractAttachmentObjectKeyFromMacro(rawValue);
                                    requestDeleteAttachmentObject(pageName, attachmentObjectKey).then(function () {
                                        if (removeAttachmentFromCardProperty(card, rawValue)) {
                                            renderProperties();
                                            updateCardCommentCount(card);
                                            renderColumns();
                                            enqueueMutation(function () {
                                                return persistColumns('card:property:update', {
                                                    eventPrefix: 'User:' + getCurrentAuthor(),
                                                    cardId: card.id || '',
                                                    cardTitle: card.text || '',
                                                    property: 'Attachment',
                                                    value: (card.properties && card.properties.Attachment) || []
                                                }).catch(function (error) {
                                                    console.error('[Kanban] failed to persist attachment property after delete', error);
                                                });
                                            });
                                        }
                                    }).catch(function (error) {
                                        console.error('[Kanban] failed to delete attachment from detail popup', error);
                                        alert('Attachment delete failed. ' + (error && error.message ? error.message : ''));
                                    });
                                });
                                if (attachmentItem) {
                                    attachmentItem.appendChild(removeButton);
                                } else {
                                    row.appendChild(removeButton);
                                }
                            }

                            if (key === 'Attachment') {
                                requestRenderInlineComment(pageName, displayValue).then(function (html) {
                                    if (html) {
                                        valueRow.innerHTML = html;
                                        clampRenderedInlineImages(valueRow);
                                        enhanceAttachmentPropertyPreview(attachmentGrid || valueRow);
                                    }
                                }).catch(function (error) {
                                    console.error('[Kanban] failed to render property value', error);
                                });
                            } else {
                                renderInlineMarkupInto(valueRow, displayValue);
                            }
                        });
                    } else {
                        row.style.display = 'flex';
                        row.style.alignItems = 'center';
                        row.style.justifyContent = 'space-between';
                        row.style.gap = '12px';
                        row.appendChild(label);
                        var displayValue = values;
                        if (key === 'Creator') {
                            displayValue = toUserLinkMarkup(values);
                        } else if (key === 'dateCreated') {
                            displayValue = toClientKanbanDateTime(values);
                        }
                        var valueRow = document.createElement('div');
                        valueRow.textContent = displayValue;
                        valueRow.className = 'kanban-property-value';
                        valueRow.style.textAlign = 'left';
                        valueRow.style.wordBreak = 'break-word';
                        row.appendChild(valueRow);

                        renderInlineMarkupInto(valueRow, displayValue);
                    }
                    propertyList.appendChild(row);
                });
                DERIVED_CARD_RELATION_PROPERTIES.forEach(function (definition) {
                    var values = derivedRelations[definition.key] || [];
                    if (values.length === 0) {
                        return;
                    }
                    var row = document.createElement('div');
                    row.className = 'kanban-property-row';
                    var label = document.createElement('div');
                    label.textContent = definition.key;
                    label.className = 'kanban-property-key';
                    renderRelationPropertyRow(row, label, definition.key, values, true);
                    propertyList.appendChild(row);
                });
            };

            var submitDueDate = function () {
                var nextDueDate = (dueDateInput.value || '').trim();
                var previousDueDates = (card.properties && card.properties.DueDate) || [];
                var previousDueDate = previousDueDates.length > 0 ? String(previousDueDates[0]).replace(/^\[|\]$/g, '') : '';
                if (nextDueDate === previousDueDate) {
                    return;
                }
                card.properties = card.properties || {};
                if (nextDueDate) {
                    card.properties.DueDate = ['[' + nextDueDate + ']'];
                } else {
                    delete card.properties.DueDate;
                }
                prependCardActivity(card, [extractActivityDetailFromRevisionComment(buildKanbanSaveComment('card:property:update', { eventPrefix: 'User:' + getCurrentAuthor(), cardId: card.id || '', cardTitle: card.text || '', property: 'DueDate', value: nextDueDate || '' }))]);
                updateCardCommentCount(card);
                renderColumns();
                renderProperties();
                renderComments();
                enqueueMutation(function () {
                    return persistColumns('card:property:update', {
                        eventPrefix: 'User:' + getCurrentAuthor(),
                        cardId: card.id || '',
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
            commentsTitle.className = 'kanban-modal-section-title';

            var comments = document.createElement('div');
            var renderComments = function () {
                comments.innerHTML = '';
                (card.comments || []).forEach(function (entry) {
                    if (!entry || !entry.header) {
                        return;
                    }

                    var row = document.createElement('div');
                    row.className = 'kanban-activity-row';

                    var header = document.createElement('div');
                    header.className = 'kanban-activity-header';
                    row.appendChild(header);

                    var displayHeader = toClientKanbanCommentHeader(entry.header);
                    requestRenderInlineComment(pageName, displayHeader).then(function (html) {
                        if (html) {
                            header.innerHTML = html;
                            clampRenderedInlineImages(header);
                            enableInlineImageLightbox(header);
                        }
                    }).catch(function (error) {
                        console.error('[Kanban] failed to render comment header', error);
                        header.textContent = displayHeader;
                    });

                    (entry.details || []).forEach(function (detailLine) {
                         var detailRow = document.createElement('div');
                         detailRow.className = 'kanban-activity-detail';
                         var restoredDetailLine = restoreCommentNewlines(detailLine);
                         detailRow.textContent = restoredDetailLine;
                         row.appendChild(detailRow);

                         requestRenderInlineComment(pageName, restoredDetailLine).then(function (html) {
                             if (html) {
                                 detailRow.innerHTML = html;
                                 clampRenderedInlineImages(detailRow);
                                 enableInlineImageLightbox(detailRow);
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
                        card.properties = card.properties || {};
                        card.properties.Attachment = card.properties.Attachment || [];
                        card.properties.Attachment.push(commentText);
                        var commentEntry = buildCommentEntry([commentText]);
                        card.comments = card.comments || [];
                        card.comments.unshift(commentEntry);
                        updateCardCommentCount(card);
                        renderComments();
                        renderProperties();
                        renderColumns();
                        enqueueMutation(function () {
                            return persistColumns('card:comment:add', { eventPrefix: 'User:' + getCurrentAuthor(), cardId: card.id || '', cardTitle: card.text || '', comment: commentText }).catch(function (error) {
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

            if (isWritable) {
                textarea.addEventListener('paste', handleClipboardImagePaste);
            }

            overlay.addEventListener('dragenter', function (evt) {
                if (!isWritable) {
                    return;
                }
                logModalDragEvent('overlay', evt, { cardId: card.id || '' });
                if (!isFileDragEvent(evt)) {
                    return;
                }
                dragDepthInOverlay += 1;
                evt.preventDefault();
                evt.stopPropagation();
                setModalDropFeedback(true);
            });

            overlay.addEventListener('dragover', function (evt) {
                if (!isWritable) {
                    return;
                }
                logModalDragEvent('overlay', evt, { cardId: card.id || '' });
                if (!isFileDragEvent(evt)) {
                    return;
                }
                evt.preventDefault();
                evt.stopPropagation();
                if (evt.dataTransfer) {
                    evt.dataTransfer.dropEffect = 'copy';
                }
                setModalDropFeedback(true);
            });

            overlay.addEventListener('dragleave', function (evt) {
                if (!isWritable) {
                    return;
                }
                logModalDragEvent('overlay', evt, { cardId: card.id || '' });
                if (!evt || (evt.relatedTarget && overlay.contains(evt.relatedTarget))) {
                    return;
                }
                dragDepthInOverlay = 0;
                if (dragDepthInOverlay === 0 && dragDepthInModal === 0) {
                    setModalDropFeedback(false);
                }
            });

            modal.addEventListener('dragenter', function (evt) {
                if (!isWritable) {
                    return;
                }
                logModalDragEvent('modal', evt, { cardId: card.id || '' });
                if (!isFileDragEvent(evt)) {
                    return;
                }
                dragDepthInModal += 1;
                evt.preventDefault();
                evt.stopPropagation();
                setModalDropFeedback(true);
            });

            modal.addEventListener('dragover', function (evt) {
                if (!isWritable) {
                    return;
                }
                logModalDragEvent('modal', evt, { cardId: card.id || '' });
                if (!isFileDragEvent(evt)) {
                    return;
                }
                evt.preventDefault();
                evt.stopPropagation();
                if (evt.dataTransfer) {
                    evt.dataTransfer.dropEffect = 'copy';
                }
            });

            modal.addEventListener('dragleave', function (evt) {
                if (!isWritable) {
                    return;
                }
                logModalDragEvent('modal', evt, { cardId: card.id || '' });
                if (!evt || (evt.relatedTarget && modal.contains(evt.relatedTarget))) {
                    return;
                }
                dragDepthInModal = 0;
                if (dragDepthInModal === 0 && dragDepthInOverlay === 0) {
                    setModalDropFeedback(false);
                }
            });

            var handleModalFileDrop = function (evt, source) {
                if (!isWritable) {
                    return;
                }
                logModalDragEvent(source, evt, { cardId: card.id || '' });
                if (!evt || !evt.dataTransfer || !evt.dataTransfer.files) {
                    return;
                }
                var files = Array.prototype.slice.call(evt.dataTransfer.files || []).filter(function (file) { return Boolean(file); });
                if (!files.length) {
                    return;
                }
                evt.preventDefault();
                evt.stopPropagation();
                dragDepthInModal = 0;
                dragDepthInOverlay = 0;
                setModalDropFeedback(false);

                attachFilesToCard(pageName, card, files).then(function (updated) {
                    if (!updated) {
                        return;
                    }
                    updateCardCommentCount(card);
                    renderComments();
                    renderProperties();
                    renderColumns();
                    enqueueMutation(function () {
                        return persistColumns('card:comment:add', { eventPrefix: 'User:' + getCurrentAuthor(), cardId: card.id || '', cardTitle: card.text || '', comment: '[Attachment Drop x' + files.length + ']' }).catch(function (error) {
                            console.error('[Kanban] failed to save dropped attachments', error);
                        });
                    });
                }).catch(function (error) {
                    console.error('[Kanban] failed to attach dropped files', error);
                    alert('File upload failed. ' + (error && error.message ? error.message : ''));
                });
            };

            overlay.addEventListener('drop', function (evt) {
                handleModalFileDrop(evt, 'overlay');
            });

            modal.addEventListener('drop', function (evt) {
                handleModalFileDrop(evt, 'modal');
            });

            if (isWritable) {
                submit.addEventListener('click', function () {
                    var body = (textarea.value || '').trim();
                    if (!body) {
                        return;
                    }
                    var commentEntry = buildCommentEntry([body]);
                    card.comments = card.comments || [];
                    card.comments.unshift(commentEntry);
                    updateCardCommentCount(card);
                    textarea.value = '';
                    renderComments();
                    enqueueMutation(function () {
                        return persistColumns('card:comment:add', { eventPrefix: 'User:' + getCurrentAuthor(), cardId: card.id || '', cardTitle: card.text || '', comment: body }).catch(function (error) {
                            console.error('[Kanban] failed to save comments', error);
                        });
                    });
                });
                textarea.addEventListener('keydown', function (evt) {
                    if ((evt.altKey || evt.ctrlKey) && evt.key === 'Enter') {
                        evt.preventDefault();
                        submit.click();
                    }
                });
                dueDateSaveButton.addEventListener('click', submitDueDate);
                dueDateInput.addEventListener('keydown', function (evt) {
                    if (evt.key === 'Enter') {
                        evt.preventDefault();
                        submitDueDate();
                    }
                });
            }

            if (isWritable) {
            deleteCardButton.addEventListener('click', function () {
                var shouldDelete = window.confirm('Delete card "' + (card.text || '') + '"?');
                if (!shouldDelete) {
                    return;
                }
                var cardAttachmentObjectKeys = extractCardAttachmentObjectKeys(card);

                columns.forEach(function (targetColumn) {
                    var cardIndex = (targetColumn.cards || []).indexOf(card);
                    if (cardIndex >= 0) {
                        targetColumn.cards.splice(cardIndex, 1);
                    }
                });
                renderColumns();
                cleanupCardListener();
                closeOpenedCardOverlay();
                clearHashCardId(card.id || '');

                enqueueMutation(function () {
                    return Promise.allSettled(cardAttachmentObjectKeys.map(function (objectKey) {
                        return requestDeleteAttachmentObject(pageName, objectKey);
                    })).then(function (results) {
                        results.forEach(function (result, index) {
                            if (result.status !== 'fulfilled') {
                                console.error('[Kanban] failed to delete attachment for deleted card', {
                                    objectKey: cardAttachmentObjectKeys[index],
                                    error: result.reason
                                });
                            }
                        });
                    }).then(function () {
                        return persistColumns('card:delete', { eventPrefix: 'User:' + getCurrentAuthor(), cardId: card.id || '', cardTitle: card.text || '' });
                    }).catch(function (error) {
                        console.error('[Kanban] failed to delete card', error);
                    });
                });
            });
            }
            var cleanupCardListener = function () {
                if (currentCardUpdateHandler) {
                    root.removeEventListener('kanban:card.remoteupdated', currentCardUpdateHandler);
                    currentCardUpdateHandler = null;
                }
            };
            overlay.addEventListener('click', function () {
                cleanupCardListener();
                closeOpenedCardOverlay();
                clearHashCardId(card.id || '');
            });
            closeButton.addEventListener('click', function () {
                cleanupCardListener();
                closeOpenedCardOverlay();
                clearHashCardId(card.id || '');
            });
            modal.addEventListener('click', function (evt) { evt.stopPropagation(); });
            if (isWritable) {
                headerActions.appendChild(deleteCardButton);
            }
            headerActions.appendChild(closeButton);
            header.appendChild(titleWrap);
            header.appendChild(headerActions);
            modal.appendChild(header);
            if (isWritable) {
                modal.appendChild(modalDropHint);
                titleEditorWrap.appendChild(titleEditor);
                titleEditorWrap.appendChild(titleSaveButton);
                actionBar.appendChild(submit);
                var submitHint = document.createElement('span');
                submitHint.textContent = 'Ctrl+Enter / Alt+Enter';
                submitHint.className = 'kanban-comment-hint';
                actionBar.appendChild(submitHint);
                dueDateEditor.appendChild(dueDateInput);
                dueDateEditor.appendChild(dueDateSaveButton);
            }
            modal.appendChild(descriptionTitle);
            modal.appendChild(descriptionDisplay);
            if (isWritable) {
                modal.appendChild(descriptionEditorWrap);
            }
            modal.appendChild(propertyTitle);
            modal.appendChild(propertyList);
            modal.appendChild(commentsTitle);
            if (isWritable) {
                modal.appendChild(textarea);
                modal.appendChild(actionBar);
            }
            modal.appendChild(comments);
            overlay.appendChild(modal);
            document.body.appendChild(overlay);
            renderProperties();
            renderComments();

            // 원격 업데이트 핸들러: 편집 중 여부에 따라 각 필드를 선택적으로 in-place 갱신
            var handleRemoteCardUpdate = function (evt) {
                var detail = evt && evt.detail;
                if (!detail || detail.cardId !== (card.id || '')) { return; }
                var serverCard = detail.serverCard;
                if (!serverCard) { return; }

                var titleEditing = !titleEditorWrap.classList.contains('kanban-hidden');
                var descEditing = !descriptionEditorWrap.classList.contains('kanban-hidden');
                var propFocused = Boolean(modal.querySelector &&
                    modal.querySelector('.kanban-property-input:focus, .kanban-duedate-input:focus, .kanban-property-select:focus'));
                var needsBanner = false;

                // 제목
                if (serverCard.text && serverCard.text !== card.text) {
                    if (!titleEditing) {
                        card.text = serverCard.text;
                        title.textContent = serverCard.text;
                        titleEditor.value = serverCard.text;
                    } else { needsBanner = true; }
                }

                // 설명
                var serverDescText = (serverCard.description || []).join('\n');
                if (serverDescText !== getDescriptionText()) {
                    if (!descEditing) {
                        card.description = (serverCard.description || []).slice();
                        renderDescriptionDisplay();
                    } else { needsBanner = true; }
                }

                // 속성 (이미 저장된 값이므로 입력 포커스 없으면 즉시 반영)
                try {
                    var spJson = JSON.stringify(serverCard.properties || {});
                    if (spJson !== JSON.stringify(card.properties || {})) {
                        if (!propFocused) {
                            card.properties = JSON.parse(spJson);
                            renderProperties();
                        } else { needsBanner = true; }
                    }
                } catch (e) {}

                // classNames
                card.classNames = (serverCard.classNames || []).slice();

                // 댓글 섹션 재렌더링 (comments는 merge 완료 상태)
                renderComments();
                rerenderColumns();

                // 배너 관리
                var existingBanner = overlay.querySelector('.kanban-modal-remote-update-banner');
                if (needsBanner) {
                    if (!existingBanner) {
                        var remoteBanner = document.createElement('div');
                        remoteBanner.className = 'kanban-modal-remote-update-banner';
                        remoteBanner.textContent = 'This card was updated by another user while you are editing. Your unsaved edits may conflict.';
                        modal.insertBefore(remoteBanner, modal.firstChild);
                    }
                } else if (existingBanner && existingBanner.parentNode) {
                    existingBanner.parentNode.removeChild(existingBanner);
                }
            };
            currentCardUpdateHandler = handleRemoteCardUpdate;
            root.addEventListener('kanban:card.remoteupdated', handleRemoteCardUpdate);
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
            scrollKanbanToCard(findRenderedCardById(cardId));
            openCardDetail(targetCard);
            return true;
        };
        var initialHashCardId = getHashCardId();
        if (initialHashCardId) {
            openCardDetailById(initialHashCardId);
        }

        if (isWritable && window.Sortable) {
            var listDragOriginClone = null;
            var clearListDragOrigin = function () {
                if (listDragOriginClone && listDragOriginClone.parentNode) {
                    listDragOriginClone.parentNode.removeChild(listDragOriginClone);
                }
                listDragOriginClone = null;
            };
            var removeListDragClasses = function (item) {
                if (!item || !item.classList) {
                    return;
                }
                item.classList.remove('kanban-list-drag-chosen', 'kanban-list-drag-placeholder', 'kanban-list-drag-preview');
            };
            var clearListCloneAttributes = function (item) {
                if (!item || !item.removeAttribute) {
                    return;
                }
                item.removeAttribute('data-column-index');
                item.removeAttribute('data-column-line-number');
            };
            var createListDragOrigin = function (item) {
                clearListDragOrigin();
                if (!item || !item.parentElement || typeof item.cloneNode !== 'function') {
                    return;
                }
                listDragOriginClone = item.cloneNode(true);
                clearListCloneAttributes(listDragOriginClone);
                removeListDragClasses(listDragOriginClone);
                listDragOriginClone.classList.add('kanban-list-drag-origin');
                listDragOriginClone.setAttribute('aria-hidden', 'true');
                listDragOriginClone.setAttribute('data-kanban-list-drag-origin', 'true');
                item.parentElement.insertBefore(listDragOriginClone, item);
            };
            var setListDragCursorState = function (active) {
                if (root && root.classList) {
                    root.classList[active ? 'add' : 'remove']('kanban-list-dragging');
                }
                if (document && document.body && document.body.classList) {
                    document.body.classList[active ? 'add' : 'remove']('kanban-list-dragging-global');
                }
            };
            var getBoardColumnIndex = function (item) {
                if (!item) {
                    return -1;
                }
                var draggableColumns = Array.prototype.filter.call(board.children || [], function (candidate) {
                    return candidate && candidate.classList && candidate.classList.contains('kanban-column') && !candidate.classList.contains('kanban-list-drag-origin');
                });
                return draggableColumns.indexOf(item);
            };

            Sortable.create(board, {
                draggable: '.kanban-column:not(.kanban-list-drag-origin)',
                animation: 120,
                ghostClass: 'kanban-list-drag-placeholder',
                chosenClass: 'kanban-list-drag-chosen',
                dragClass: 'kanban-list-drag-placeholder',
                forceFallback: true,
                fallbackClass: 'kanban-list-drag-preview',
                fallbackOnBody: true,
                fallbackTolerance: 3,
                onClone: function (evt) {
                    var clone = evt && evt.clone ? evt.clone : null;
                    if (!clone) {
                        return;
                    }
                    clearListCloneAttributes(clone);
                    removeListDragClasses(clone);
                    if (clone.classList) {
                        clone.classList.add('kanban-list-drag-preview');
                    }
                    clone.setAttribute('aria-hidden', 'true');
                },
                onStart: function (evt) {
                    createListDragOrigin(evt.item);
                    setListDragCursorState(true);
                },
                onMove: function () {
                    return true;
                },
                onEnd: function (evt) {
                    var oldIndex = Number.isFinite(evt.oldDraggableIndex) ? evt.oldDraggableIndex : evt.oldIndex;
                    clearListDragOrigin();
                    removeListDragClasses(evt.item);
                    setListDragCursorState(false);

                    var newIndex = getBoardColumnIndex(evt.item);
                    if (oldIndex === newIndex || oldIndex < 0 || newIndex < 0 || oldIndex >= columns.length || newIndex >= columns.length) {
                        return;
                    }

                    var movedColumn = columns.splice(oldIndex, 1)[0];
                    if (!movedColumn) {
                        renderColumns();
                        return;
                    }
                    columns.splice(newIndex, 0, movedColumn);
                    normalizeLineNumbers();
                    renderColumns();

                    root.dispatchEvent(new CustomEvent('kanban:listMoved', {
                        detail: {
                            oldIndex: oldIndex,
                            newIndex: newIndex
                        }
                    }));

                    enqueueMutation(function () {
                        return persistColumns('list:move', { eventPrefix: 'User:' + getCurrentAuthor(), listTitle: movedColumn.title || '', fromOrder: String((oldIndex || 0) + 1), toOrder: String((newIndex || 0) + 1) }).catch(function (error) {
                            console.error('[Kanban] failed to save list order', error);
                        });
                    });
                }
            });
        }
    };
    kanbanInterpreters.forEach(initKanban);

    var initAll = function (container) {
        var scope = container || document;
        Array.prototype.forEach.call(scope.querySelectorAll('.InterpreterKanban'), initKanban);
    };

    window.AhaWiki = window.AhaWiki || {};
    window.AhaWiki.Kanban = { initAll: initAll };
});
