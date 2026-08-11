import assert from 'node:assert/strict';
import fs from 'node:fs';
import test from 'node:test';
import vm from 'node:vm';

/**
 * Tab in the editor, and who gets it when something else wants it.
 *
 * AhaWiki.Editor turns Tab into a list indent: one space at the start of the line, cursor moved
 * along with it. The autocomplete popup wants Tab too, to accept the highlighted suggestion.
 * Both used to run — the indent first, shifting every character of the line one to the right,
 * and then the completion, replacing the range it had measured before the shift. Typing
 * ` * [JIH` with `JIH0` offered and pressing Tab produced `  * JIH0H]`.
 */

function loadEditor() {
    const sandbox = { window: {}, setTimeout, clearTimeout };
    vm.runInNewContext(fs.readFileSync('public/js/AhaWiki.Editor.js', 'utf8'), sandbox);
    return sandbox.window.AhaWiki.Editor;
}

/** Just enough CodeMirror for AhaWiki.Editor: one line, offsets in and out. */
function fakeCodeMirror(text, cursor) {
    const state = { value: text, from: cursor, to: cursor };
    const handlers = [];
    const doc = {
        getValue: () => state.value,
        getCursor: (which) => (which === 'to' ? state.to : state.from),
        setValue: (next) => { state.value = next; },
        setSelection: (from, to) => { state.from = from; state.to = to; },
    };
    return {
        state,
        handlers,
        getDoc: () => doc,
        indexFromPos: (pos) => pos,
        posFromIndex: (index) => index,
        focus: () => {},
        on: (type, handler) => { if (type === 'keydown') handlers.push(handler); },
        press: (event) => handlers.forEach((handler) => handler(cmSelf, event)),
    };
}

let cmSelf = null;
function cmFor(text, cursor) {
    cmSelf = fakeCodeMirror(text, cursor);
    return cmSelf;
}

function keydown(overrides) {
    return {
        key: 'Tab',
        code: 'Tab',
        shiftKey: false,
        isComposing: false,
        defaultPrevented: false,
        preventDefault() { this.defaultPrevented = true; },
        ...overrides,
    };
}

test('Tab on a list line indents it', () => {
    const editor = loadEditor();
    const cm = cmFor(' * [JIH]', 7);
    editor.addCodeMirrorEventListener(cm);

    cm.press(keydown());

    assert.equal(cm.state.value, '  * [JIH]');
    assert.equal(cm.state.from, 8);
});

test('Tab a popup already took is left alone', () => {
    const editor = loadEditor();
    const cm = cmFor(' * [JIH]', 7);
    editor.addCodeMirrorEventListener(cm);

    // 자동완성이 먼저 받아 처리한 상태. 여기서 또 편집하면 자동완성이 재 둔 좌표가 어긋난다.
    cm.press(keydown({ defaultPrevented: true }));

    assert.equal(cm.state.value, ' * [JIH]');
    assert.equal(cm.state.from, 7);
});

test('a jQuery event that has been prevented counts as taken', () => {
    const editor = loadEditor();
    const cm = cmFor(' * [JIH]', 7);
    editor.addCodeMirrorEventListener(cm);

    // jQuery.Event 는 defaultPrevented 대신 isDefaultPrevented() 를 준다.
    cm.press(keydown({ isDefaultPrevented: () => true }));

    assert.equal(cm.state.value, ' * [JIH]');
});

test('Tab still indents when the popup ignored it', () => {
    const editor = loadEditor();
    const cm = cmFor(' * [JIH]', 7);
    editor.addCodeMirrorEventListener(cm);

    cm.press(keydown({ isDefaultPrevented: () => false }));

    assert.equal(cm.state.value, '  * [JIH]');
});
