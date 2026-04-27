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
    var wrappers = document.querySelectorAll('.InterpreterRenderMetaWrapper[data-edit-link]');

    wrappers.forEach(function (wrapper) {
        var content = wrapper.querySelector('.InterpreterRenderContent');
        if (!content) {
            return;
        }

        wrapper.style.pointerEvents = 'none';
        content.style.pointerEvents = 'none';

        var contentAnchorTarget = content.firstElementChild || content;
        contentAnchorTarget.style.pointerEvents = 'auto';
        var editLink = wrapper.querySelector('.InterpreterRenderEditLink');
        if (!editLink) {
            editLink = document.createElement('a');
            editLink.className = 'InterpreterRenderEditLink';
            editLink.rel = 'nofollow';
            editLink.innerHTML = '<i class="fas fa-edit"></i>';
            editLink.style.position = 'absolute';
            editLink.style.top = '6px';
            editLink.style.right = '8px';
            editLink.style.display = 'inline-flex';
            editLink.style.alignItems = 'center';
            editLink.style.justifyContent = 'center';
            editLink.style.width = '26px';
            editLink.style.height = '26px';
            editLink.style.borderRadius = '50%';
            editLink.style.border = '1px solid #ddd';
            editLink.style.background = 'rgba(255,255,255,.92)';
            editLink.style.color = '#555';
            editLink.style.textDecoration = 'none';
            editLink.style.opacity = '1';
            editLink.style.pointerEvents = 'auto';
            editLink.style.zIndex = '1';
            wrapper.appendChild(editLink);
        }

        var href = wrapper.getAttribute('data-edit-link');
        var title = wrapper.getAttribute('data-edit-title');
        if (href) {
            editLink.href = href;
        }
        if (title) {
            editLink.title = title;
        }

        var positionEditLink = function () {
            var targetRect = contentAnchorTarget.getBoundingClientRect();
            var wrapperRect = wrapper.getBoundingClientRect();
            var top = targetRect.top - wrapperRect.top + 6;
            var left = targetRect.right - wrapperRect.left - editLink.offsetWidth - 8;
            editLink.style.top = top + 'px';
            editLink.style.left = (left < 0 ? 0 : left) + 'px';
            editLink.style.right = 'auto';
        };

        positionEditLink();
        window.addEventListener('resize', positionEditLink);
        window.addEventListener('scroll', positionEditLink, { passive: true });
    });
});
