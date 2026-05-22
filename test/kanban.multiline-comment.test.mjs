import { test } from 'node:test';
import assert from 'node:assert';

// Mock functions from AhaWiki.Kanban.js that we added
const escapeCommentNewlines = (text) => {
    return String(text || '').replace(/\r?\n/g, '[[Br]]');
};

const restoreCommentNewlines = (text) => {
    return String(text || '').replace(/\[\[Br\]\]/g, '\n');
};

test('should escape newlines when storing comment', () => {
    const multilineComment = 'Line 1\nLine 2\nLine 3';
    const escaped = escapeCommentNewlines(multilineComment);
    assert.strictEqual(escaped, 'Line 1[[Br]]Line 2[[Br]]Line 3');
});

test('should restore newlines when displaying comment', () => {
    const escapedComment = 'Line 1[[Br]]Line 2[[Br]]Line 3';
    const restored = restoreCommentNewlines(escapedComment);
    assert.strictEqual(restored, 'Line 1\nLine 2\nLine 3');
});

test('should handle Windows-style newlines (CRLF)', () => {
    const multilineComment = 'Line 1\r\nLine 2\r\nLine 3';
    const escaped = escapeCommentNewlines(multilineComment);
    assert.strictEqual(escaped, 'Line 1[[Br]]Line 2[[Br]]Line 3');
});

test('should roundtrip: escape then restore should match original', () => {
    const original = 'First line\nSecond line\nThird line';
    const escaped = escapeCommentNewlines(original);
    const restored = restoreCommentNewlines(escaped);
    assert.strictEqual(restored, original);
});

test('should handle empty string', () => {
    assert.strictEqual(escapeCommentNewlines(''), '');
    assert.strictEqual(restoreCommentNewlines(''), '');
});

test('should handle string with only newlines', () => {
    const onlyNewlines = '\n\n\n';
    const escaped = escapeCommentNewlines(onlyNewlines);
    assert.strictEqual(escaped, '[[Br]][[Br]][[Br]]');
});

test('should handle multiline with special characters', () => {
    const complex = 'Code: int x = 5;\nComment: // this is a comment\nAnother line';
    const escaped = escapeCommentNewlines(complex);
    const restored = restoreCommentNewlines(escaped);
    assert.strictEqual(restored, complex);
});





