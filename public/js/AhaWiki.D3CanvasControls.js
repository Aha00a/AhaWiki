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
    // In a narrow graph only one of the two is shown at a time. That is adjacent.less's doing, which
    // takes the other's button away while one is open; until 2026-11-26 this handler closed the other
    // itself, below a width it had to know as well as the stylesheet did.
    $(document).on('click', '.adjacentPagesHud .hudToggle', function() {
        var body = document.getElementById(this.getAttribute('aria-controls'));
        if(!body) return;
        var open = this.getAttribute('aria-expanded') !== 'true';
        body.hidden = !open;
        this.setAttribute('aria-expanded', String(open));
    });
})(window, jQuery);
