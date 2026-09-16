// Screen presentation for InterpreterSlide. The server renders a `.slideDeck` of `.slide` sections
// (see InterpreterSlide.scala); this shows them one at a time with keyboard/click navigation and
// fullscreen. The index math is a handful of pure functions on window.AhaWiki.Slide so it is unit
// tested directly (test/ahawiki.slide.test.mjs); the DOM wiring below runs only in a browser.
//
// Presentation only: without this script the deck degrades to the slides stacked and readable
// (_slide.less), and print always shows every slide as a static handout regardless of this file.
(function () {
    window.AhaWiki = window.AhaWiki || {};

    var Slide = {
        // Keep an index inside [0, total - 1]; an empty deck stays at 0.
        clampIndex: function (index, total) {
            if (total <= 0) return 0;
            if (index < 0) return 0;
            if (index > total - 1) return total - 1;
            return index;
        },
        nextIndex: function (current, total, delta) {
            return Slide.clampIndex(current + delta, total);
        },
        // "#/3" -> zero-based 2, clamped to the deck. null when the hash is not a slide reference, so
        // an ordinary "#Section" anchor is left alone.
        parseHashIndex: function (hash, total) {
            var m = /^#\/(\d+)$/.exec(hash || '');
            if (!m) return null;
            return Slide.clampIndex(parseInt(m[1], 10) - 1, total);
        },
        hashForIndex: function (index) {
            return '#/' + (index + 1);
        }
    };

    window.AhaWiki.Slide = Slide;

    // Loaded into a vm context by the unit test, where there is no document: expose the pure helpers
    // and stop before touching the DOM.
    if (typeof document === 'undefined') {
        return;
    }

    function initDeck(deck) {
        var slides = Array.prototype.slice.call(deck.querySelectorAll('.slide'));
        var total = slides.length;
        if (total === 0) {
            return;
        }

        var counterEl = deck.querySelector('.slideCurrent');
        // The deep-link (#/n) and hash updates make sense for one deck per page; with several decks a
        // shared hash would fight, so only a lone deck reads and writes it.
        var lone = document.querySelectorAll('.slideDeck').length === 1;
        var index = 0;

        // Present view is primary: switch from the stacked fallback to one-at-a-time.
        deck.classList.add('presenting');

        function paint() {
            for (var i = 0; i < slides.length; i++) {
                slides[i].classList.toggle('current', i === index);
            }
            if (counterEl) {
                counterEl.textContent = String(index + 1);
            }
        }

        function to(i) {
            index = Slide.clampIndex(i, total);
            paint();
            if (lone) {
                try {
                    history.replaceState(null, '', Slide.hashForIndex(index));
                } catch (e) {
                    location.hash = Slide.hashForIndex(index);
                }
            }
        }

        function go(delta) {
            to(Slide.nextIndex(index, total, delta));
        }

        function toggleFullscreen() {
            if (document.fullscreenElement === deck) {
                if (document.exitFullscreen) {
                    document.exitFullscreen();
                }
            } else if (deck.requestFullscreen) {
                deck.requestFullscreen();
            }
        }

        function handleKey(e) {
            switch (e.key) {
                case 'ArrowRight':
                case 'PageDown':
                case ' ':
                case 'Spacebar':
                    go(1); e.preventDefault(); break;
                case 'ArrowLeft':
                case 'PageUp':
                    go(-1); e.preventDefault(); break;
                case 'Home':
                    to(0); e.preventDefault(); break;
                case 'End':
                    to(total - 1); e.preventDefault(); break;
                case 'f':
                case 'F':
                    toggleFullscreen(); e.preventDefault(); break;
                default:
                    break;
            }
        }

        // Start on the deep-linked slide when this is the only deck; otherwise the first.
        if (lone) {
            var fromHash = Slide.parseHashIndex(location.hash, total);
            if (fromHash !== null) {
                index = fromHash;
            }
        }
        paint();

        // Click the slide area to advance; clicks on the chrome are handled by its own buttons.
        deck.addEventListener('click', function (e) {
            if (e.target.closest && e.target.closest('.slideChrome')) {
                return;
            }
            go(1);
        });

        var prev = deck.querySelector('.slidePrev');
        var next = deck.querySelector('.slideNext');
        var full = deck.querySelector('.slideFullscreen');
        if (prev) { prev.addEventListener('click', function (e) { e.stopPropagation(); go(-1); }); }
        if (next) { next.addEventListener('click', function (e) { e.stopPropagation(); go(1); }); }
        if (full) { full.addEventListener('click', function (e) { e.stopPropagation(); toggleFullscreen(); }); }

        // One document-level listener per deck, guarded so only the active deck responds: the
        // fullscreen deck while fullscreen, otherwise the deck that holds focus (a click focuses it,
        // since the deck carries tabindex="0"). This avoids double handling and scopes keys when
        // several decks share a page.
        document.addEventListener('keydown', function (e) {
            var active = document.fullscreenElement
                ? (document.fullscreenElement === deck)
                : deck.contains(document.activeElement);
            if (active) {
                handleKey(e);
            }
        });
    }

    document.addEventListener('DOMContentLoaded', function () {
        Array.prototype.slice.call(document.querySelectorAll('.slideDeck')).forEach(initDeck);
    });
})();
