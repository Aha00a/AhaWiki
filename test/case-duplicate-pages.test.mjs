// The move is two writes, and the dangerous half is the one that turns a page into a redirect:
// get the order or the heading wrong and the content is the thing pointing at a pointer.
import test from 'node:test';
import assert from 'node:assert/strict';
import { retitle, redirectTo } from '../scripts/case-duplicate-pages.mjs';

test('the heading moves with the page', () => {
    const before = '= Css\nCascading Style Sheet\n * [wiki:CssNakedDay]\n';
    const after = retitle(before, [/^= Css$/m, '= CSS']);
    assert.equal(after.ok, true);
    assert.equal(after.content, '= CSS\nCascading Style Sheet\n * [wiki:CssNakedDay]\n');
});

test('only the heading, never a mention of the old name in the body', () => {
    // CssNakedDay and CssPreprocessor are other pages; renaming them here would break real links.
    const after = retitle('= Css\n * [CssPreprocessor]\n * [wiki:CssTools]\n', [/^= Css$/m, '= CSS']);
    assert.ok(after.content.includes('[CssPreprocessor]'));
    assert.ok(after.content.includes('[wiki:CssTools]'));
});

test('a page whose heading is not what was expected is refused, not guessed at', () => {
    // The heading is how the script knows it is looking at the page it planned for.
    const after = retitle('= Cascading Style Sheets\nbody\n', [/^= Css$/m, '= CSS']);
    assert.equal(after.ok, false);
    assert.match(after.reason, /no heading/);
});

test('a redirect is the directive alone, and ends with a newline', () => {
    // #!redirect is read from the page header. Trailing text on the line would go to the parser
    // as part of the target name.
    assert.equal(redirectTo('CSS'), '#!redirect CSS\n');
    assert.equal(redirectTo('CSS').trim().split('\n').length, 1);
});
