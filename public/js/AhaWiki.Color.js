// Colour arithmetic in OKLCH, whose lightness follows what the eye sees where HSL's does not, and
// the two things the wiki does with it (2026-11-16). Plain ES5 maths that hands the browser
// ordinary #rrggbb or a number, so it asks nothing of the browser's own CSS colour support.
//
// 1. The theme's hue. The stored hue (slider, site default) is an HSL hue; the theme's tokens in
//    less.less are OKLCH and turn by --hue-ok, the same hue on OKLCH's circle. hueFromHsl gives it.
//
// 2. A page's own colour (its #!Schema themeColor, logics.PageColor) as it can be drawn on the
//    background it lands on. The colour as written is kept for its hue; the lightness moves until
//    the stripe or dot stands at 3:1 against the background -- what WCAG 1.4.11 asks of a mark that
//    is not text. Measured on aha00a.com's language pages, GitHub's JavaScript yellow stood at
//    1.35:1 on the light theme's white and C's grey at 2.31:1 on the dark one, so no single
//    lightness range serves both themes: the answer depends on the background. An HSL clamp to
//    35-65%, the first version, left that yellow at 65% and as bright as ever. Chroma is lowered
//    only as far as the moved colour needs to stay inside sRGB. The infobox, the adjacent-pages
//    graph and the link preview all go through here, so the three cannot disagree about a colour.
//
// The reasons are on the wiki pages InterpreterSchema «페이지 색» and Dev Theme Support.
(function () {
    if (typeof window.AhaWiki === 'undefined') {
        window.AhaWiki = {};
    }

    var targetRatio = 3;

    function toLinear(c) {
        c /= 255;
        return c <= 0.04045 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4);
    }

    function fromLinear(c) {
        var v = c <= 0.0031308 ? 12.92 * c : 1.055 * Math.pow(c, 1 / 2.4) - 0.055;
        return v * 255;
    }

    function luminance(rgb) {
        return 0.2126 * toLinear(rgb[0]) + 0.7152 * toLinear(rgb[1]) + 0.0722 * toLinear(rgb[2]);
    }

    function contrast(a, b) {
        var x = luminance(a);
        var y = luminance(b);
        return (Math.max(x, y) + 0.05) / (Math.min(x, y) + 0.05);
    }

    function parseHex(hex) {
        var m = /^#([0-9a-f]{6})$/i.exec(hex || '');
        if (!m) {
            return null;
        }
        var v = parseInt(m[1], 16);
        return [(v >> 16) & 255, (v >> 8) & 255, v & 255];
    }

    function toHex(rgb) {
        return '#' + rgb.map(function (c) {
            return ('0' + Math.round(c).toString(16)).slice(-2);
        }).join('');
    }

    // sRGB <-> OKLab, as Björn Ottosson published it.
    function toOklch(rgb) {
        var r = toLinear(rgb[0]), g = toLinear(rgb[1]), b = toLinear(rgb[2]);
        var l = Math.cbrt(0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b);
        var m = Math.cbrt(0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b);
        var s = Math.cbrt(0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b);
        var L = 0.2104542553 * l + 0.7936177850 * m - 0.0040720468 * s;
        var A = 1.9779984951 * l - 2.4285922050 * m + 0.4505937099 * s;
        var B = 0.0259040371 * l + 0.7827717662 * m - 0.8086757660 * s;
        return [L, Math.sqrt(A * A + B * B), Math.atan2(B, A)];
    }

    // Linear sRGB, which may fall outside 0..1 when the colour is outside the gamut.
    function oklchToLinear(L, C, h) {
        var A = C * Math.cos(h), B = C * Math.sin(h);
        var l = Math.pow(L + 0.3963377774 * A + 0.2158037573 * B, 3);
        var m = Math.pow(L - 0.1055613458 * A - 0.0638541728 * B, 3);
        var s = Math.pow(L - 0.0894841775 * A - 1.2914855480 * B, 3);
        return [
            4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s,
            -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s,
            -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s
        ];
    }

    function inGamut(linear) {
        return linear.every(function (c) {
            return c >= -1e-4 && c <= 1 + 1e-4;
        });
    }

    // The colour at lightness L with this hue, as much of the chroma as sRGB holds there.
    function fromOklch(L, C, h) {
        var linear = oklchToLinear(L, C, h);
        if (!inGamut(linear)) {
            var lo = 0, hi = C;
            for (var i = 0; i < 16; i++) {
                var mid = (lo + hi) / 2;
                if (inGamut(oklchToLinear(L, mid, h))) {
                    lo = mid;
                } else {
                    hi = mid;
                }
            }
            linear = oklchToLinear(L, lo, h);
        }
        // Rounded here, so what is checked against 3:1 is the colour that will be drawn.
        return linear.map(function (c) {
            return Math.round(fromLinear(Math.min(1, Math.max(0, c))));
        });
    }

    /**
     * The hue on OKLCH's circle, 0 to 360, of the HSL hue `hue` -- the hue of the fully saturated
     * HSL colour. Not linear: HSL's 90 to 150 are all greens and land within 16 degrees of each other.
     */
    function hueFromHsl(hue) {
        var h = ((Number(hue) % 360) + 360) % 360;
        var f = function (n) {
            var k = (n + h / 30) % 12;
            return 0.5 - 0.5 * Math.max(-1, Math.min(k - 3, 9 - k, 1));
        };
        var lch = toOklch([f(0) * 255, f(8) * 255, f(4) * 255]);
        var degrees = lch[2] * 180 / Math.PI;
        return Math.round((degrees < 0 ? degrees + 360 : degrees) * 10) / 10;
    }

    /**
     * [r, g, b] of CSS `oklch(L C h)` (L 0..1, h in degrees) as browsers draw it when sRGB cannot
     * hold it: each channel clipped, not the chroma reduced. What the theme's tokens look like at a
     * hue whose chroma they overshoot; test/theme.oklch-contrast.test.mjs reads them through this.
     */
    function cssOklch(L, C, hueDegrees) {
        return oklchToLinear(L, C, hueDegrees * Math.PI / 180).map(function (c) {
            return Math.round(fromLinear(Math.min(1, Math.max(0, c))));
        });
    }

    function standsOnAll(rgb, backgrounds) {
        return backgrounds.every(function (bg) {
            return contrast(rgb, bg) >= targetRatio;
        });
    }

    /**
     * `#rrggbb`, as close to `hex` as stands at 3:1 on every one of `backgrounds` ([r, g, b] each).
     * Unchanged when it already does. Darker on a light background, lighter on a dark one; when the
     * backgrounds straddle the middle and nothing reaches 3:1 on all, the end of the range is taken.
     * Empty for anything that is not `#rrggbb`.
     */
    function forBackgrounds(hex, backgrounds) {
        var rgb = parseHex(hex);
        if (!rgb) {
            return '';
        }
        backgrounds = (backgrounds || []).filter(Boolean);
        if (!backgrounds.length || standsOnAll(rgb, backgrounds)) {
            return hex.toLowerCase();
        }
        var meanLuminance = backgrounds.reduce(function (sum, bg) {
            return sum + luminance(bg);
        }, 0) / backgrounds.length;
        // 0.18 is where black and white stand at the same contrast with a background.
        var darken = meanLuminance > 0.18;
        var lch = toOklch(rgb);
        var near = lch[0];
        var far = darken ? 0 : 1;
        if (!standsOnAll(fromOklch(far, lch[1], lch[2]), backgrounds)) {
            return toHex(fromOklch(far, lch[1], lch[2]));
        }
        // `near` fails and `far` stands: find the lightness nearest the original that stands.
        for (var i = 0; i < 24; i++) {
            var mid = (near + far) / 2;
            if (standsOnAll(fromOklch(mid, lch[1], lch[2]), backgrounds)) {
                far = mid;
            } else {
                near = mid;
            }
        }
        return toHex(fromOklch(far, lch[1], lch[2]));
    }

    // Any CSS colour the browser understands -- a custom property's `hsl(220, 16%, 11%)`, a computed
    // `rgba(…)` -- as [r, g, b], or null when it is transparent or not a colour.
    var probe = null;
    function resolveColor(css) {
        var m = /^rgba?\(\s*([\d.]+)[,\s]+([\d.]+)[,\s]+([\d.]+)(?:\s*[,\/]\s*([\d.]+%?))?\s*\)$/i.exec((css || '').trim());
        if (!m && typeof document !== 'undefined') {
            probe = probe || document.createElement('canvas').getContext('2d');
            if (probe) {
                probe.fillStyle = '#010203';
                probe.fillStyle = css;
                var resolved = probe.fillStyle;
                if (/^#[0-9a-f]{6}$/i.test(resolved) && !(resolved === '#010203' && css.trim() !== '#010203')) {
                    return parseHex(resolved);
                }
                m = /^rgba?\(\s*([\d.]+)[,\s]+([\d.]+)[,\s]+([\d.]+)(?:\s*[,\/]\s*([\d.]+%?))?\s*\)$/i.exec(resolved);
            }
        }
        if (!m) {
            return null;
        }
        if (m[4] !== undefined && parseFloat(m[4]) === 0) {
            return null;
        }
        return [parseFloat(m[1]), parseFloat(m[2]), parseFloat(m[3])];
    }

    // What shows behind an element: its own background, or the nearest ancestor's that is not
    // transparent.
    function backgroundBehind(element) {
        for (var e = element; e && e.nodeType === 1; e = e.parentElement) {
            var rgb = resolveColor(getComputedStyle(e).backgroundColor);
            if (rgb) {
                return rgb;
            }
        }
        return [255, 255, 255];
    }

    /**
     * Sets `--page-color` on an element carrying `data-page-color`: a stripe along its edge sits
     * between the element's own background and what is behind it, so it has to stand on both.
     */
    function applyPageColor(element) {
        var raw = element.getAttribute('data-page-color');
        var backgrounds = [backgroundBehind(element)];
        if (element.parentElement) {
            backgrounds.push(backgroundBehind(element.parentElement));
        }
        var shown = forBackgrounds(raw, backgrounds);
        if (shown) {
            element.style.setProperty('--page-color', shown);
        }
    }

    function applyPageColors(root) {
        Array.prototype.forEach.call((root || document).querySelectorAll('[data-page-color]'), applyPageColor);
    }

    window.AhaWiki.Color = {
        hueFromHsl: hueFromHsl,
        cssOklch: cssOklch,
        forBackgrounds: forBackgrounds,
        contrast: contrast,
        resolveColor: resolveColor,
        applyPageColor: applyPageColor,
        applyPageColors: applyPageColors
    };

    if (typeof document !== 'undefined' && document.addEventListener) {
        document.addEventListener('DOMContentLoaded', function () {
            applyPageColors(document);
        });
        // _base.scala.html announces a theme switch through jQuery once the classes have changed.
        if (window.jQuery) {
            window.jQuery(document).on('ahawiki:themeChanged', function () {
                applyPageColors(document);
            });
        }
    }
})();
