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
    var wrappers = document.querySelectorAll('.InterpreterRenderMetaWrapper');

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
        if (!href) {
            return;
        }

        var editLink = wrapper.querySelector('.InterpreterRenderEditLink');
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
            editLink.style.border = '1px solid #888';
            editLink.style.background = '#fff';
            editLink.style.color = '#000';
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

        var foldToggle = null;
        if (heading) {
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
                foldToggle.style.background = '#fff';
                foldToggle.style.color = '#000';
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
                    if (siblingHeadingLevel !== null && siblingHeadingLevel > headingLevel) {
                        childSections.push(sibling);
                    }
                    sibling = sibling.nextElementSibling;
                }
                return childSections;
            };
            var updateFoldState = function (isCollapsed) {
                section.classList.toggle('sectionCollapsed', isCollapsed);
                Array.prototype.forEach.call(section.children, function (child) {
                    if (child !== wrapper) {
                        child.style.display = isCollapsed ? 'none' : '';
                    }
                });
                var childSections = getChildSections();
                childSections.forEach(function (childSection) {
                    childSection.style.display = isCollapsed ? 'none' : '';
                });
                foldToggle.innerHTML = isCollapsed ? '<i class="fas fa-chevron-right fa-fw"></i>' : '<i class="fas fa-chevron-down fa-fw"></i>';
                foldToggle.setAttribute('aria-expanded', (!isCollapsed).toString());
            };
            updateFoldState(section.classList.contains('sectionCollapsed'));

            heading.style.cursor = 'pointer';
            heading.setAttribute('role', 'button');
            heading.setAttribute('tabindex', '0');
            heading.setAttribute('aria-expanded', (!section.classList.contains('sectionCollapsed')).toString());

            var toggleSectionFold = function () {
                var nextCollapsed = !section.classList.contains('sectionCollapsed');
                updateFoldState(nextCollapsed);
                heading.setAttribute('aria-expanded', (!nextCollapsed).toString());
                schedulePositionEditLink();
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
            var targetRect = contentAnchorTarget.getBoundingClientRect();
            var wrapperRect = wrapper.getBoundingClientRect();
            var top = targetRect.top - wrapperRect.top + 4;
            var left = targetRect.right - wrapperRect.left - editLink.offsetWidth - 8;
            editLink.style.top = top + 'px';
            editLink.style.left = (left < 0 ? 0 : left) + 'px';
            editLink.style.right = 'auto';
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
    });
});
