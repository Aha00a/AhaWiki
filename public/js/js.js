// https://stackoverflow.com/a/28132596/3751968
location.params = function(params, preventReload) {
    var obj = {}, i, parts, len, key, value;

    if (typeof params === 'string') {
        value = location.search.match(new RegExp('[?&]' + params + '=?([^&]*)[&#$]?'));
        return value ? value[1] : undefined;
    }

    var _params = location.search.substr(1).split('&');

    for (i = 0, len = _params.length; i < len; i++) {
        parts = _params[i].split('=');
        if (! parts[0]) {continue;}
        obj[parts[0]] = parts.splice(1).join('=') || true;
    }

    if (typeof params !== 'object') {return obj;}

    for (key in params) {
        value = params[key];
        if (typeof value === 'undefined') {
            delete obj[key];
        } else {
            obj[key] = value;
        }
    }

    parts = [];
    for (key in obj) {
        parts.push(key + (obj[key] === true ? '' : '=' + obj[key]));
    }

    var search = parts.join('&');
    if(!preventReload) {
        location.search = search;
    } else {
        history.replaceState({}, document.title, '?' + search);
    }
};

document.addEventListener('DOMContentLoaded', function () {
    var hasMixed = document.querySelector('.paperContent .page.landscape, .paperContent .page.portrait');
    var isLandscapeDoc = document.querySelector('.paperContent.landscape');
    if (hasMixed) {
        var style = document.createElement('style');
        if (isLandscapeDoc) {
            // 기본=가로, 세로 페이지만 named page 할당 → 가로 페이지는 default @page 사용
            style.textContent = [
                '@page { size: A4 landscape; }',
                '@page portrait-page { size: A4 portrait; }',
                '.paperContent.landscape .page.portrait { page: portrait-page; }',
            ].join('\n');
        } else {
            // 기본=세로, 가로 페이지만 named page 할당 → 세로 페이지는 default @page 사용
            style.textContent = [
                '@page { size: A4 portrait; }',
                '@page landscape-page { size: A4 landscape; }',
                '.paperContent .page.landscape { page: landscape-page; }',
            ].join('\n');
        }
        document.head.appendChild(style);
    } else if (isLandscapeDoc) {
        var landscapeStyle = document.createElement('style');
        landscapeStyle.textContent = '@page { size: A4 landscape; }';
        document.head.appendChild(landscapeStyle);
    }
});

document.addEventListener('DOMContentLoaded', function () {
    var wrappedTableAncestorSelector = [
        '.wikiTableSimpleScroll',
        '.macro-recent-changes-table-wrap',
        '.MacroThemesTableWrapper',
        '.InterpreterRenderMetaWrapper.Table'
    ].join(',');

    document.querySelectorAll('table.wikiTableSimple').forEach(function (table) {
        if (table.classList.contains('MacroCalendar') || table.closest(wrappedTableAncestorSelector) || !table.parentElement) {
            return;
        }

        var wrapper = document.createElement('div');
        wrapper.className = 'wikiTableSimpleScroll';
        table.parentElement.insertBefore(wrapper, table);
        wrapper.appendChild(table);
    });
});

document.addEventListener('DOMContentLoaded', function () {
    var wrappers = document.querySelectorAll('.InterpreterRenderMetaWrapper');
    var foldControllers = [];

    wrappers.forEach(function (wrapper) {
        var content = wrapper.querySelector('.InterpreterRenderContent');
        if (!content) {
            return;
        }

        wrapper.style.pointerEvents = 'none';
        content.style.pointerEvents = 'none';

        var contentAnchorTarget = content.firstElementChild || content;
        contentAnchorTarget.style.pointerEvents = 'auto';

        var href = wrapper.getAttribute('data-edit-link');
        var title = wrapper.getAttribute('data-edit-title');
        var heading = content.querySelector('h2, h3, h4, h5, h6');
        var interpreterCodeBlock = content.querySelector('.Interpreter.Text, .Interpreter.Vim');
        var editLink = null;
        var copyButton = null;
        if (href) {
            editLink = wrapper.querySelector('.InterpreterRenderEditLink');
            if (!editLink) {
                editLink = document.createElement('a');
                editLink.className = 'InterpreterRenderEditLink';
                editLink.rel = 'nofollow';
                editLink.innerHTML = '<i class="fas fa-edit"></i>&nbsp;Edit';
                editLink.style.position = 'absolute';
                editLink.style.top = '6px';
                editLink.style.right = '8px';
                editLink.style.display = 'inline-flex';
                editLink.style.alignItems = 'center';
                editLink.style.justifyContent = 'center';
                editLink.style.width = '50px';
                editLink.style.height = '20px';
                editLink.style.borderRadius = '50px';
                editLink.style.border = '1px solid var(--color-input-border)';
                editLink.style.background = 'var(--color-surface)';
                editLink.style.color = 'var(--color-text)';
                editLink.style.textDecoration = 'none';
                editLink.style.opacity = '.4';
                editLink.style.fontSize = '12px';
                editLink.style.transition = 'opacity .15s ease, color .15s ease, background-color .15s ease, border-color .15s ease';
                editLink.style.pointerEvents = 'auto';
                editLink.addEventListener('mouseenter', function () {
                    editLink.style.opacity = '1';
                });
                editLink.addEventListener('mouseleave', function () {
                    editLink.style.opacity = '.4';
                });
                editLink.addEventListener('focus', function () {
                    editLink.style.opacity = '1';
                });
                editLink.addEventListener('blur', function () {
                    editLink.style.opacity = '.4';
                });
                wrapper.appendChild(editLink);
            }

            editLink.href = href;
            if (title) {
                editLink.title = title;
            }
        }

        if (interpreterCodeBlock) {
            copyButton = wrapper.querySelector('.InterpreterRenderCopyButton');
            if (!copyButton) {
                copyButton = document.createElement('button');
                copyButton.type = 'button';
                copyButton.className = 'InterpreterRenderCopyButton';
                copyButton.innerHTML = '<i class="fas fa-copy"></i>&nbsp;Copy';
                copyButton.style.position = 'absolute';
                copyButton.style.top = '6px';
                copyButton.style.right = '8px';
                copyButton.style.display = 'inline-flex';
                copyButton.style.alignItems = 'center';
                copyButton.style.justifyContent = 'center';
                copyButton.style.width = '64px';
                copyButton.style.whiteSpace = 'nowrap';
                copyButton.style.height = '22px';
                copyButton.style.borderRadius = '50px';
                copyButton.style.border = '1px solid var(--color-input-border)';
                copyButton.style.background = 'var(--color-surface)';
                copyButton.style.color = 'var(--color-text)';
                copyButton.style.textDecoration = 'none';
                copyButton.style.opacity = '.4';
                copyButton.style.fontSize = '12px';
                copyButton.style.transition = 'opacity .15s ease, color .15s ease, background-color .15s ease, border-color .15s ease';
                copyButton.style.pointerEvents = 'auto';
                copyButton.style.cursor = 'pointer';
                copyButton.addEventListener('mouseenter', function () {
                    copyButton.style.opacity = '1';
                });
                copyButton.addEventListener('mouseleave', function () {
                    copyButton.style.opacity = '.4';
                });
                copyButton.addEventListener('focus', function () {
                    copyButton.style.opacity = '1';
                });
                copyButton.addEventListener('blur', function () {
                    copyButton.style.opacity = '.4';
                });
                copyButton.addEventListener('click', function () {
                    var text = interpreterCodeBlock.innerText || '';
                    if (navigator.clipboard && navigator.clipboard.writeText) {
                        navigator.clipboard.writeText(text);
                    } else {
                        var fallback = document.createElement('textarea');
                        fallback.value = text;
                        fallback.style.position = 'fixed';
                        fallback.style.opacity = '0';
                        document.body.appendChild(fallback);
                        fallback.focus();
                        fallback.select();
                        document.execCommand('copy');
                        document.body.removeChild(fallback);
                    }
                });
                wrapper.appendChild(copyButton);
            }
        }

        var foldToggle = null;
        if (heading && !wrapper.closest('.paperContent')) {
            foldToggle = wrapper.querySelector('.InterpreterRenderFoldToggle');
            if (!foldToggle) {
                foldToggle = document.createElement('button');
                foldToggle.type = 'button';
                foldToggle.className = 'InterpreterRenderFoldToggle';
                foldToggle.style.position = 'static';
                foldToggle.style.display = 'inline-flex';
                foldToggle.style.alignItems = 'center';
                foldToggle.style.justifyContent = 'center';
                foldToggle.style.borderRadius = '50px';
                foldToggle.style.border = 'none';
                foldToggle.style.background = 'var(--color-surface)';
                foldToggle.style.color = 'var(--color-text)';
                foldToggle.style.textDecoration = 'none';
                foldToggle.style.opacity = '.4';
                foldToggle.style.fontSize = '0.5em';
                foldToggle.style.lineHeight = '1';
                foldToggle.style.marginRight = '0px';
                foldToggle.style.verticalAlign = 'middle';
                foldToggle.style.transition = 'opacity .15s ease, color .15s ease, background-color .15s ease, border-color .15s ease';
                foldToggle.style.pointerEvents = 'auto';
                foldToggle.style.cursor = 'pointer';
                foldToggle.addEventListener('mouseenter', function () {
                    foldToggle.style.opacity = '1';
                });
                foldToggle.addEventListener('mouseleave', function () {
                    foldToggle.style.opacity = '.4';
                });
                foldToggle.addEventListener('focus', function () {
                    foldToggle.style.opacity = '1';
                });
                foldToggle.addEventListener('blur', function () {
                    foldToggle.style.opacity = '.4';
                });
                heading.insertBefore(foldToggle, heading.firstChild);
            }

            var schemaContainer = heading.closest('.InterpreterSchema');
            var isSchemaHeading = !!(schemaContainer && heading.classList.contains('schemaClassTitle'));
            var schemaHeadingRoot = null;
            var schemaFoldTargets = [];
            if (isSchemaHeading) {
                schemaHeadingRoot = heading;
                while (schemaHeadingRoot.parentElement && schemaHeadingRoot.parentElement !== schemaContainer) {
                    schemaHeadingRoot = schemaHeadingRoot.parentElement;
                }

                var schemaNode = heading.nextElementSibling;
                while (schemaNode) {
                    schemaFoldTargets.push(schemaNode);
                    schemaNode = schemaNode.nextElementSibling;
                }

                Array.prototype.forEach.call(schemaContainer.children, function (child) {
                    if (child !== schemaHeadingRoot) {
                        schemaFoldTargets.push(child);
                    }
                });
            }
            var section = wrapper.parentElement;
            var headingLevel = parseInt(heading.tagName.replace('H', ''), 10);
            var getSectionHeadingLevel = function (sectionElement) {
                var sectionHeading = sectionElement.querySelector(':scope > .InterpreterRenderMetaWrapper .InterpreterRenderContent h2, :scope > .InterpreterRenderMetaWrapper .InterpreterRenderContent h3, :scope > .InterpreterRenderMetaWrapper .InterpreterRenderContent h4, :scope > .InterpreterRenderMetaWrapper .InterpreterRenderContent h5, :scope > .InterpreterRenderMetaWrapper .InterpreterRenderContent h6');
                if (!sectionHeading) {
                    return null;
                }
                return parseInt(sectionHeading.tagName.replace('H', ''), 10);
            };
            var getChildSections = function () {
                var childSections = [];
                var sibling = section.nextElementSibling;
                while (sibling) {
                    var siblingHeadingLevel = getSectionHeadingLevel(sibling);
                    if (siblingHeadingLevel !== null && siblingHeadingLevel <= headingLevel) {
                        break;
                    }
                    childSections.push(sibling);
                    sibling = sibling.nextElementSibling;
                }
                return childSections;
            };
            var updateFoldState = function (isCollapsed) {
                if (isSchemaHeading) {
                    schemaContainer.classList.toggle('sectionCollapsed', isCollapsed);
                    schemaFoldTargets.forEach(function (target) {
                        target.style.display = isCollapsed ? 'none' : '';
                    });
                } else {
                    section.classList.toggle('sectionCollapsed', isCollapsed);
                    Array.prototype.forEach.call(section.children, function (child) {
                        if (child !== wrapper) {
                            child.style.display = isCollapsed ? 'none' : '';
                        }
                    });
                    var childSections = getChildSections();
                    if (isCollapsed) {
                        childSections.forEach(function (childSection) {
                            childSection.style.display = 'none';
                        });
                    } else if (section.style.display !== 'none') {
                        var blockedLevel = null;
                        childSections.forEach(function (childSection) {
                            var childHeadingLevel = getSectionHeadingLevel(childSection);
                            if (childHeadingLevel !== null && blockedLevel !== null && childHeadingLevel <= blockedLevel) {
                                blockedLevel = null;
                            }

                            if (blockedLevel !== null) {
                                childSection.style.display = 'none';
                                return;
                            }

                            childSection.style.display = '';
                            if (childHeadingLevel !== null && childSection.classList.contains('sectionCollapsed')) {
                                blockedLevel = childHeadingLevel;
                            }
                        });
                    }
                }
                heading.style.opacity = isCollapsed ? '0.5' : '';
                foldToggle.innerHTML = isCollapsed ? '<i class="fas fa-chevron-right fa-fw"></i>' : '<i class="fas fa-chevron-down fa-fw"></i>';
                foldToggle.setAttribute('aria-expanded', (!isCollapsed).toString());
            };
            var isInitiallyCollapsed = isSchemaHeading
                ? schemaContainer.classList.contains('sectionCollapsed')
                : section.classList.contains('sectionCollapsed');
            updateFoldState(isInitiallyCollapsed);

            heading.style.cursor = 'pointer';
            heading.setAttribute('role', 'button');
            heading.setAttribute('tabindex', '0');
            heading.setAttribute('aria-expanded', (!isInitiallyCollapsed).toString());

            var setSectionFold = function (isCollapsed) {
                updateFoldState(isCollapsed);
                heading.setAttribute('aria-expanded', (!isCollapsed).toString());
                schedulePositionEditLink();
            };

            var toggleSectionFold = function () {
                var nextCollapsed = isSchemaHeading
                    ? !schemaContainer.classList.contains('sectionCollapsed')
                    : !section.classList.contains('sectionCollapsed');
                setSectionFold(nextCollapsed);
            };

            heading.addEventListener('click', function (event) {
                if (event.target.closest('a, button')) {
                    return;
                }
                toggleSectionFold();
            });

            heading.addEventListener('keydown', function (event) {
                if (event.key === 'Enter' || event.key === ' ') {
                    event.preventDefault();
                    toggleSectionFold();
                }
            });

            foldToggle.onclick = function (event) {
                event.stopPropagation();
                toggleSectionFold();
            };
        }

        var positionEditLink = function () {
            if (!editLink) {
                return;
            }
            var targetRect = contentAnchorTarget.getBoundingClientRect();
            var wrapperRect = wrapper.getBoundingClientRect();
            var top = targetRect.top - wrapperRect.top + 4;
            var rightOffset = 8;
            var left = targetRect.right - wrapperRect.left - editLink.offsetWidth - rightOffset;
            editLink.style.top = top + 'px';
            editLink.style.left = (left < 0 ? 0 : left) + 'px';
            editLink.style.right = 'auto';
            if (copyButton) {
                rightOffset += editLink.offsetWidth + 4;
                copyButton.style.top = top + 'px';
                copyButton.style.left = (targetRect.right - wrapperRect.left - copyButton.offsetWidth - rightOffset) + 'px';
                copyButton.style.right = 'auto';
            }
        };

        var schedulePositionEditLink = function () {
            if (window.requestAnimationFrame) {
                window.requestAnimationFrame(positionEditLink);
            } else {
                positionEditLink();
            }
        };

        window.addEventListener('resize', schedulePositionEditLink);
        window.addEventListener('scroll', schedulePositionEditLink, { passive: true });

        if (window.ResizeObserver) {
            var resizeObserver = new ResizeObserver(schedulePositionEditLink);
            resizeObserver.observe(wrapper);
            resizeObserver.observe(contentAnchorTarget);
        }

        if (window.MutationObserver) {
            document.querySelectorAll('.wikiContent .toc').forEach(function (toc) {
                var mutationObserver = new MutationObserver(schedulePositionEditLink);
                mutationObserver.observe(toc, {
                    attributes: true,
                    attributeFilter: ['class', 'style']
                });
            });
        }

        setTimeout(schedulePositionEditLink, 100);

        if (heading && foldToggle) {
            foldControllers.push({
                heading: heading,
                section: section,
                level: headingLevel,
                isSchemaHeading: isSchemaHeading,
                foldRoot: isSchemaHeading ? schemaContainer : section,
                setCollapsed: setSectionFold
            });
        }
    });

    var getHashTarget = function () {
        var rawHash = (window.location.hash || '').replace(/^#/, '');
        var decodedHash;
        if (!rawHash) {
            return null;
        }

        var target = document.getElementById(rawHash);
        if (target) {
            return target;
        }

        try {
            decodedHash = decodeURIComponent(rawHash);
        } catch (e) {
            decodedHash = rawHash;
        }

        if (decodedHash && decodedHash !== rawHash) {
            target = document.getElementById(decodedHash);
        }

        return target || null;
    };

    var getControllerForElement = function (element) {
        var match = null;
        foldControllers.forEach(function (controller) {
            if (!controller.isSchemaHeading && controller.section === element) {
                match = controller;
                return;
            }

            if (controller.heading === element || (controller.foldRoot && controller.foldRoot.contains(element))) {
                match = controller;
            }
        });
        return match;
    };

    var getClosestHeadingSection = function (element) {
        var node = element;
        while (node && node !== document.body) {
            if (node.classList && Array.prototype.some.call(node.classList, function (className) {
                return className.indexOf('HeadingWrapper') === 0;
            })) {
                return node;
            }
            node = node.parentElement;
        }
        return null;
    };

    var getControllerForHashTarget = function (target) {
        var directController = getControllerForElement(target);
        if (directController) {
            return directController;
        }

        var section = getClosestHeadingSection(target);
        return section ? getControllerForElement(section) : null;
    };

    var expandFoldedHashTarget = function () {
        var target = getHashTarget();
        if (!target) {
            return;
        }

        var targetController = getControllerForHashTarget(target);
        var controllersToExpand = [];
        var ancestorControllers = {};
        var ancestorLevels = [];

        foldControllers.forEach(function (controller) {
            if (controller === targetController) {
                ancestorLevels.sort(function (a, b) {
                    return a - b;
                });
                ancestorLevels.forEach(function (level) {
                    controllersToExpand.push(ancestorControllers[level]);
                });
                controllersToExpand.push(controller);
                return;
            }

            if (!targetController || controller.isSchemaHeading || targetController.isSchemaHeading) {
                if (controller.foldRoot && controller.foldRoot.contains(target)) {
                    controllersToExpand.push(controller);
                }
                return;
            }

            // Heading sections are siblings, so keep the current document-outline
            // ancestors while walking toward the hash target section.
            if (controller.level >= targetController.level) {
                return;
            }

            Object.keys(ancestorControllers).forEach(function (level) {
                if (parseInt(level, 10) >= controller.level) {
                    delete ancestorControllers[level];
                    ancestorLevels = ancestorLevels.filter(function (storedLevel) {
                        return storedLevel !== parseInt(level, 10);
                    });
                }
            });

            ancestorControllers[controller.level] = controller;
            if (ancestorLevels.indexOf(controller.level) === -1) {
                ancestorLevels.push(controller.level);
            }
        });

        controllersToExpand.forEach(function (controller) {
            controller.setCollapsed(false);
        });

        if (controllersToExpand.length) {
            if (window.requestAnimationFrame) {
                window.requestAnimationFrame(function () {
                    target.scrollIntoView();
                });
            } else {
                target.scrollIntoView();
            }
        }
    };

    expandFoldedHashTarget();
    window.addEventListener('hashchange', expandFoldedHashTarget);
});
