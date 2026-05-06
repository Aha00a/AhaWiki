$(function () {
    $(".edit").keydown(function (e) {
        if (!e.altKey)
            return;

        if (e.key === 'Alt')
            return;

        document.querySelectorAll(`kbd[data-hotkey-alt=${e.key.toUpperCase()}]`).forEach(kbd => {
            kbd.click();
            return false;
        });
    });
});
