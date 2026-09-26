// Every CSS and JS file of the wiki's own that a template names goes through
// logics.PublicAsset.url, which puts a digest of the file in the address.
//
// /public/ is served with max-age=3600. A file named at its bare address keeps that address across
// a deploy, so a browser that fetched it in the hour before draws the new HTML with the old file --
// which is how the old-revision notice showed unstyled after the 2026-09-26 deploy. One template
// naming one file the old way brings that hour back for that file, and nothing else would notice.
// PublicAssetVersionSpec checks what a rendered page ends up with; this checks every template,
// including the ones a spec cannot easily reach. The wiki page Dev Deploying has the reasons.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import { rootDir } from '../scripts/lib/ahawiki.net.mjs';

const viewsDir = path.join(rootDir, 'app', 'views');
const templates = fs.readdirSync(viewsDir, { recursive: true })
    .filter(file => file.endsWith('.scala.html'))
    .map(file => ({
        file: `app/views/${file.split(path.sep).join('/')}`,
        text: fs.readFileSync(path.join(viewsDir, file), 'utf8'),
    }));

test('no template names its own CSS or JS at a bare address', () => {
    const bare = templates.flatMap(({ file, text }) =>
        [...text.matchAll(/(?:href|src)\s*=\s*["'](\/(?:public|assets)\/[^"'?]+\.(?:css|js))["'?]/g)]
            .map(match => `${file}: ${match[1]}`));
    assert.deepEqual(bare, [], `Name these through logics.PublicAsset.url instead:\n  ${bare.join('\n  ')}`);
});

// sbt-web serves both public/ and app/assets/ under /public/; the compiled .css live in the second.
// A misspelt name would still render, at a bare address that answers 404.
test('every file a template names through PublicAsset.url is one the build serves', () => {
    const named = templates.flatMap(({ file, text }) =>
        [...text.matchAll(/PublicAsset\.url\("([^"]+)"\)/g)].map(match => ({ file, name: match[1] })));
    assert.ok(named.length > 0, 'the templates name their files the way this test looks for');

    const missing = named
        .filter(({ name }) => !['public', 'app/assets'].some(root => fs.existsSync(path.join(rootDir, root, name))))
        .map(({ file, name }) => `${file}: ${name}`);
    assert.deepEqual(missing, [], `No such file under public/ or app/assets/:\n  ${missing.join('\n  ')}`);
});
