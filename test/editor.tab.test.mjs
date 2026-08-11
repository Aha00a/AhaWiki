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

test('Enter on a list line continues the list', () => {
    const editor = loadEditor();
    const cm = cmFor(' * [JIH]', 7);
    editor.addCodeMirrorEventListener(cm);

    cm.press(keydown({ key: 'Enter', code: 'Enter' }));

    assert.equal(cm.state.value, ' * [JIH\n * ]');
});

/**
 * The page says which keys the completion popup owns, so the answer does not depend on which
 * handler was registered first. Registration order decided it before, and Enter went the other
 * way from Tab: the list rule split the line inside the brackets, the completion then wrote over
 * coordinates the split had moved, and ` * [JIH` came out as ` * [JIH0` above a line of ` * ]`.
 */
test('the editor keeps its hands off keys the popup owns', () => {
    const editor = loadEditor();
    const cm = cmFor(' * [JIH]', 7);
    const ownedByPopup = (e) => ['Tab', 'Enter'].indexOf(e.key) !== -1;
    editor.addCodeMirrorEventListener(cm, { skipWhen: ownedByPopup });

    cm.press(keydown({ key: 'Enter', code: 'Enter' }));
    cm.press(keydown());

    assert.equal(cm.state.value, ' * [JIH]');
    assert.equal(cm.state.from, 7);
});

test('it declines even when it was registered first', () => {
    const editor = loadEditor();
    const cm = cmFor(' * [JIH]', 7);
    let popupVisible = true;
    editor.addCodeMirrorEventListener(cm, { skipWhen: () => popupVisible });

    cm.press(keydown({ key: 'Enter', code: 'Enter' }));
    assert.equal(cm.state.value, ' * [JIH]');

    // 자동완성이 키를 사양하면 먼저 팝업을 닫으므로, 그 뒤로는 원래 동작이 돌아온다.
    popupVisible = false;
    cm.press(keydown({ key: 'Enter', code: 'Enter' }));
    assert.equal(cm.state.value, ' * [JIH\n * ]');
});

test('other keys are untouched while the popup is up', () => {
    const editor = loadEditor();
    const cm = cmFor(' * [JIH]', 7);
    editor.addCodeMirrorEventListener(cm, { skipWhen: (e) => ['Tab', 'Enter'].indexOf(e.key) !== -1 });

    cm.press(keydown({ key: '(', code: 'Digit9' }));

    assert.equal(cm.state.value, ' * [JIH()]');
});
