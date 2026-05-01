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
