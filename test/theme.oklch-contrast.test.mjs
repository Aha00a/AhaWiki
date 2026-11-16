// The theme's colours at every hue the slider can reach. They are OKLCH turned by --hue-ok
// (less.less has why): until 2026-11-16 they were HSL, and with the slider between yellow and cyan
// the light theme's links stood at 1.5:1 on white. This reads the tokens out of less.less, draws
// each pair the way a browser does -- an oklch() sRGB cannot hold has its channels clipped -- and
// checks the contrast at every degree of the slider: 4.5:1 for text, 3:1 for a mark that is not
// text (WCAG 1.4.3, 1.4.11). The wiki page Dev Theme Support has the reasons.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const sandbox = { window: {} };
vm.createContext(sandbox);
vm.runInContext(fs.readFileSync('public/js/AhaWiki.Color.js', 'utf8').replace(/^﻿/, ''), sandbox);
const { hueFromHsl, cssOklch, contrast } = sandbox.window.AhaWiki.Color;
const defaultHue = Number(/val DefaultHue: Int = (\d+)/.exec(fs.readFileSync('app/logics/SiteThemeLogic.scala', 'utf8'))[1]);

const less = fs.readFileSync('app/assets/less.less', 'utf8');

function block(selector) {
    const start = less.indexOf(selector);
    assert.ok(start >= 0, `less.less no longer has ${selector}`);
    const open = less.indexOf('{', start);
    const tokens = {};
    for (const line of less.slice(open + 1, less.indexOf('\n}', open)).split('\n')) {
        const m = /^\s*(--[\w-]+):\s*(.*);\s*$/.exec(line);
        if (m) tokens[m[1]] = m[2];
    }
    return tokens;
}

const light = block(':root,\nhtml.theme-ahawiki-light,\n.hueScope {');
// A dark token not given falls through to the light one, as it does in the page.
const dark = { ...light, ...block('html.theme-ahawiki-dark,\n\thtml.theme-ahawiki-dark .hueScope {') };

function colour(tokens, name, hue) {
    const value = tokens[name];
    assert.ok(value, `${name} is not a token`);
    const ok = /^~"oklch\(([\d.]+)% ([\d.]+) var\(--hue\)\)"$/.exec(value);
    if (ok) return cssOklch(ok[1] / 100, Number(ok[2]), hue);
    const hex = /^#([0-9a-f]{6})$/i.exec(value);
    if (hex) return [0, 2, 4].map(i => parseInt(hex[1].slice(i, i + 2), 16));
    assert.fail(`${name}: ${value} is neither an opaque oklch() token nor #rrggbb`);
}

const text = 4.5;
const mark = 3;
const pairs = {
    light: [
        ['--color-link', '--color-bg', text],
        ['--color-link', '--color-link-hover-bg', text],
        ['--color-link-visited', '--color-bg', text],
        ['--color-link-schema', '--color-bg', text],
        ['--color-code-link', '--color-code-bg', text],
        ['--color-code-text', '--color-code-bg', text],
        ['--color-kanban-text', '--color-kanban-card-bg', text],
        ['--color-adjacent-hud-text', '--color-bg', text],
        ['--color-focus-ring', '--color-bg', mark],
        ['--color-kanban-primary', '--color-kanban-card-bg', mark],
        ['--color-adjacent-hud-action-active-bg', '--color-bg', mark],
    ],
    dark: [
        ['--color-link', '--color-bg', text],
        ['--color-link', '--color-surface', text],
        ['--color-link', '--color-link-hover-bg', text],
        ['--color-link-visited', '--color-surface', text],
        ['--color-muted', '--color-surface', text],
        ['--color-muted-strong', '--color-surface', text],
        ['--color-code-text', '--color-code-bg', text],
        ['--color-kanban-text', '--color-kanban-card-bg', text],
        ['--color-adjacent-hud-action-active-text', '--color-adjacent-hud-action-active-bg', text],
        ['--color-focus-ring', '--color-surface', mark],
        ['--color-kanban-primary', '--color-kanban-card-bg', mark],
        ['--color-adjacent-dot-hop1', '--color-surface', mark],
    ],
};

for (const [theme, tokens] of [['light', light], ['dark', dark]]) {
    test(`the ${theme} theme reads at every hue of the slider`, () => {
        const failures = [];
        for (const [fg, bg, need] of pairs[theme]) {
            for (let hue = 0; hue < 360; hue++) {
                const ratio = contrast(colour(tokens, fg, hue), colour(tokens, bg, hue));
                if (ratio < need) {
                    failures.push(`${fg} on ${bg} at hue ${hue}: ${ratio.toFixed(2)} < ${need}`);
                    break;
                }
            }
        }
        assert.deepEqual(failures, []);
    });
}

test('no theme token is left in HSL', () => {
    const code = file => fs.readFileSync(`app/assets/${file}`, 'utf8').split('\n').filter(line => !line.trim().startsWith('//')).join('\n');
    const left = ['less.less', '_wiki_table.less'].flatMap(file => code(file).match(/hsla?\(var\(--hue\)[^"]*/g) || []);
    assert.deepEqual(left, []);
});

test('the default hue is the same in SiteThemeLogic and in less.less, which paints before any script', () => {
    assert.match(less, new RegExp(`--hue: ${defaultHue};`));
});

test("the default is the old HSL default 220 on OKLCH's circle, as a stored HSL hue is converted", () => {
    assert.equal(Math.round(hueFromHsl(220)), defaultHue);
});
