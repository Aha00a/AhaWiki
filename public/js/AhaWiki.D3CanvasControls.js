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
})(window, jQuery);
