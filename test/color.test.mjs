// A page's own colour, as AhaWiki.Color.js draws it: moved in lightness until it stands at 3:1
// on the background. The colours are the eleven GitHub language colours put on aha00a.com's language
// pages on 2026-11-16, where the first version (an HSL clamp to 35-65%) left JavaScript's yellow at
// 1.35:1 on the light theme and C's grey at 2.31:1 on the dark one. The backgrounds are the two
// themes' --color-surface. The reasons are on the wiki page InterpreterSchema «페이지 색».
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync('public/js/AhaWiki.Color.js', 'utf8').replace(/^﻿/, '');
const sandbox = { window: {} };
vm.createContext(sandbox);
vm.runInContext(source, sandbox);
const { forBackgrounds, contrast, resolveColor } = sandbox.window.AhaWiki.Color;

const languages = {
    Go: '#00add8', Perl: '#0298c3', PHP: '#4f5d95', C: '#555555', Haskell: '#5e5086', J: '#9eedff',
    Java: '#b07219', Scala: '#c22d40', Rust: '#dea584', JavaScript: '#f1e05a', 'C++': '#f34b7d',
};
const light = [255, 255, 255];
const dark = [0x18, 0x1b, 0x21]; // hsl(220, 16%, 11%)

const rgb = hex => [1, 3, 5].map(i => parseInt(hex.slice(i, i + 2), 16));

for (const [name, theme] of [['light', light], ['dark', dark]]) {
    test(`every language colour stands at 3:1 on the ${name} theme`, () => {
        for (const [language, hex] of Object.entries(languages)) {
            const shown = forBackgrounds(hex, [theme]);
            assert.match(shown, /^#[0-9a-f]{6}$/, language);
            assert.ok(contrast(rgb(shown), theme) >= 3, `${language} ${hex} -> ${shown}: ${contrast(rgb(shown), theme).toFixed(2)}`);
        }
    });
}

test('a colour that already stands is left as written', () => {
    assert.equal(forBackgrounds('#c22d40', [light]), '#c22d40');
    assert.equal(forBackgrounds('#F1E05A', [dark]), '#f1e05a');
});

test('the hue survives the move: yellow darkens to an olive, not a brown or a grey', () => {
    const [r, g, b] = rgb(forBackgrounds('#f1e05a', [light]));
    assert.ok(r > b + 40 && g > b + 40, `${r},${g},${b}`);
    assert.ok(Math.abs(r - g) < 40, `${r},${g},${b}`);
});

test('the move is as small as it can be: just past 3:1, not far beyond', () => {
    for (const hex of Object.values(languages)) {
        for (const theme of [light, dark]) {
            if (contrast(rgb(hex), theme) >= 3) continue;
            assert.ok(contrast(rgb(forBackgrounds(hex, [theme])), theme) < 3.2, hex);
        }
    }
});

test('a stripe between two backgrounds stands on both', () => {
    const shown = forBackgrounds('#f1e05a', [light, [0xee, 0xf1, 0xf6]]);
    assert.ok(contrast(rgb(shown), light) >= 3);
    assert.ok(contrast(rgb(shown), [0xee, 0xf1, 0xf6]) >= 3);
});

test('what is not #rrggbb is not drawn', () => {
    assert.equal(forBackgrounds('red', [light]), '');
    assert.equal(forBackgrounds('#e60012; x', [light]), '');
    assert.equal(forBackgrounds('', [light]), '');
});

test('a computed background reads as a colour, a transparent one as none', () => {
    assert.deepEqual([...resolveColor('rgb(24, 27, 33)')], [24, 27, 33]);
    assert.deepEqual([...resolveColor('rgba(24, 27, 33, 0.5)')], [24, 27, 33]);
    assert.equal(resolveColor('rgba(0, 0, 0, 0)'), null);
});
