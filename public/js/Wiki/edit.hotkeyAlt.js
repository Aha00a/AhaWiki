$(function () {
    $(".edit").keydown(function (e) {
        if (!e.altKey)
            return;

        if (e.key === 'Alt')
            return;

        // Only single letters name a hint, and anything else would be a broken selector.
        if (!/^[a-z]$/i.test(e.key))
            return;

        // Search inside the editor, not the document. The navigation bar renders the same
        // kbd[data-hotkey-alt] for its single-key hints, and a document-wide query clicked those
        // too: Alt+S saved and opened the search box, Alt+R followed /r as well as
        // ?action=rename, and Alt+H, Alt+B and Alt+E left for History, Blame and Edit with the
        // text unsaved. test/manual/editor-hotkeys/ presses the keys.
        this.querySelectorAll(`kbd[data-hotkey-alt=${e.key.toUpperCase()}]`).forEach(kbd => {
            kbd.click();
        });
    });
});
