(function(window, $) {
    if(window.AhaWikiD3CanvasControls) return;

    window.AhaWikiD3CanvasControls = {
        bindRangeControl: function(selector, onChange) {
            $(selector).off('input change').on('input change', function() {
                onChange.call(this);
            });
        },
        bindActionControl: function(selector, action) {
            $(selector).off('click').on('click', function() {
                action();
                return false;
            });
        }
    };

    // A graph's Control and Metrics panels open and close from their button. Delegated, so it is
    // bound once for every graph on the page, whichever script draws it.
    var setOpen = function(toggle, open) {
        var body = document.getElementById(toggle.getAttribute('aria-controls'));
        if(!body) return;
        body.hidden = !open;
        toggle.setAttribute('aria-expanded', String(open));
    };
    $(document).on('click', '.adjacentPagesHud .hudToggle', function() {
        var open = this.getAttribute('aria-expanded') !== 'true';
        // In a narrow graph -- a phone, or a #!Graph in a table cell -- the two panels together are
        // wider than the graph and would lie on each other, so opening one closes the other. 520px
        // is the width below which adjacent.less shows the HUD's buttons as icons.
        var $hud = $(this).closest('.adjacentPagesHud');
        if(open && $hud.width() < 520) {
            var self = this;
            $hud.find('.hudToggle').each(function() {
                if(this !== self) setOpen(this, false);
            });
        }
        setOpen(this, open);
    });
})(window, jQuery);
