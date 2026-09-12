(function () {
    /*
     * A reCAPTCHA v3 token for a page save.
     *
     * A web save (POST /w/...) needs a token whenever reCAPTCHA is on, and the server refuses one
     * without it (Wiki.save; the save section of the wiki page Dev Editor). "On" means both keys
     * are configured, and only then does the page carry <meta name="ahawiki-recaptcha-site-key">.
     * No meta tag: no token to get, and the server asks for none.
     *
     * A token passes one check and lasts two minutes, so every save asks for its own, retries
     * included. Fetching one ahead of time is how the editor used to send stale or spent ones.
     * Google's script is loaded on the first request, or by preload() on a page that will save.
     */
    if (typeof window.AhaWiki === "undefined") {
        window.AhaWiki = {};
    }

    // Long enough for a slow connection to fetch the script, short enough that a save blocked
    // by something that swallows the script without failing does not hang with no answer.
    var TokenTimeoutMs = 15000;

    var scriptPromise = null;

    var siteKey = function () {
        var meta = document.querySelector('meta[name="ahawiki-recaptcha-site-key"]');
        return meta ? (meta.getAttribute('content') || '') : '';
    };

    var load = function (key) {
        if (window.grecaptcha && typeof window.grecaptcha.execute === 'function') {
            return Promise.resolve(window.grecaptcha);
        }
        if (!scriptPromise) {
            scriptPromise = new Promise(function (resolve, reject) {
                var script = document.createElement('script');
                script.src = 'https://www.google.com/recaptcha/api.js?render=' + encodeURIComponent(key);
                script.async = true;
                script.onload = function () {
                    if (window.grecaptcha) {
                        resolve(window.grecaptcha);
                        return;
                    }
                    scriptPromise = null;
                    reject(new Error('reCAPTCHA script loaded without grecaptcha'));
                };
                script.onerror = function () {
                    // Forgotten, so the next save tries again instead of failing for good.
                    scriptPromise = null;
                    reject(new Error('reCAPTCHA script failed to load'));
                };
                document.head.appendChild(script);
            });
        }
        return scriptPromise;
    };

    var withTimeout = function (promise, ms) {
        return new Promise(function (resolve, reject) {
            var timer = setTimeout(function () {
                reject(new Error('reCAPTCHA did not answer in time'));
            }, ms);
            promise.then(function (value) {
                clearTimeout(timer);
                resolve(value);
            }, function (error) {
                clearTimeout(timer);
                reject(error);
            });
        });
    };

    window.AhaWiki.ReCaptcha = {
        isEnabled: function () {
            return siteKey() !== '';
        },

        preload: function () {
            var key = siteKey();
            return key ? load(key).then(function () {}, function () {}) : Promise.resolve();
        },

        // Resolves to '' when reCAPTCHA is off and to a fresh token when it is on. Rejects when
        // no token can be had, the script blocked for one, so the caller can say so instead of
        // sending an empty token into a 403.
        token: function (action) {
            var key = siteKey();
            if (!key) {
                return Promise.resolve('');
            }
            return withTimeout(load(key).then(function (grecaptcha) {
                return new Promise(function (resolve, reject) {
                    grecaptcha.ready(function () {
                        grecaptcha.execute(key, { action: action || 'save' }).then(resolve, reject);
                    });
                });
            }), TokenTimeoutMs);
        }
    };
})();
