// Screen presentation for InterpreterSlide. The server renders a `.slideDeck` of `.slide` sections
// (see InterpreterSlide.scala); this shows them one at a time with keyboard/click navigation,
// fullscreen, an overview grid and a filmstrip rail. The index math is a handful of pure functions
// on window.AhaWiki.Slide so it is unit tested directly (test/ahawiki.slide.test.mjs); the DOM
// wiring below runs only in a browser.
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

    // Thumbnails are rendered at this fixed design size and scaled down with transform, so a slide's
    // fixed-px spacing shrinks with the thumbnail rather than overflowing it.
    var DESIGN_W = 1280;
    var DESIGN_H = 720;

    function initDeck(deck) {
        var slides = Array.prototype.slice.call(deck.querySelectorAll('.slide'));
        var total = slides.length;
        if (total === 0) {
            return;
        }

        var counterEl = deck.querySelector('.slideCurrent');
        var filmBtn = deck.querySelector('.slideFilmstrip');
        var overBtn = deck.querySelector('.slideOverview');
        var fullBtn = deck.querySelector('.slideFullscreen');
        // The deep-link (#/n) and hash updates make sense for one deck per page; with several decks a
        // shared hash would fight, so only a lone deck reads and writes it.
        var lone = document.querySelectorAll('.slideDeck').length === 1;
        var index = 0;
        var thumbs = null; // thumbnail container (clones), built lazily and shared by both modes

        // Present view is primary: switch from the stacked fallback to one-at-a-time.
        deck.classList.add('presenting');

        function setPressed(btn, on) {
            if (btn) {
                btn.setAttribute('aria-pressed', on ? 'true' : 'false');
            }
        }

        function paint() {
            for (var i = 0; i < slides.length; i++) {
                slides[i].classList.toggle('current', i === index);
            }
            if (counterEl) {
                counterEl.textContent = String(index + 1);
            }
            if (thumbs) {
                var items = thumbs.querySelectorAll('.slideThumbItem');
                for (var j = 0; j < items.length; j++) {
                    items[j].classList.toggle('current', j === index);
                }
            }
            setPressed(filmBtn, deck.classList.contains('filmstrip'));
            setPressed(overBtn, deck.classList.contains('overview'));
            setPressed(fullBtn, document.fullscreenElement === deck);
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

        // One clone per slide, shared by the overview grid and the filmstrip rail. Built once.
        function buildThumbs() {
            if (thumbs) {
                return;
            }
            thumbs = document.createElement('div');
            thumbs.className = 'slideThumbs';
            slides.forEach(function (slide, i) {
                var item = document.createElement('button');
                item.type = 'button';
                item.className = 'slideThumbItem';
                item.setAttribute('data-index', String(i));
                var clone = slide.cloneNode(true);
                clone.classList.remove('current');
                clone.removeAttribute('id');
                clone.removeAttribute('data-index');
                var withId = clone.querySelectorAll('[id]');
                for (var k = 0; k < withId.length; k++) {
                    withId[k].removeAttribute('id');
                }
                item.appendChild(clone);
                item.addEventListener('click', function (e) {
                    e.stopPropagation();
                    var wasOverview = deck.classList.contains('overview');
                    to(i);
                    if (wasOverview) {
                        deck.classList.remove('overview'); // a pick in overview jumps and returns to the slide
                        paint();
                    }
                });
                thumbs.appendChild(item);
            });
            deck.insertBefore(thumbs, deck.firstChild);
        }

        // Scale each clone to fill its item: render at the fixed design size, then transform:scale.
        // Recomputed on resize because the grid's item width changes with it.
        function sizeThumbs() {
            if (!thumbs) {
                return;
            }
            var items = thumbs.querySelectorAll('.slideThumbItem');
            for (var i = 0; i < items.length; i++) {
                var w = items[i].clientWidth;
                if (!w) {
                    continue;
                }
                var scale = w / DESIGN_W;
                items[i].style.height = Math.round(DESIGN_H * scale) + 'px';
                var clone = items[i].querySelector('.slide');
                if (clone) {
                    clone.style.width = DESIGN_W + 'px';
                    clone.style.height = DESIGN_H + 'px';
                    clone.style.transform = 'scale(' + scale + ')';
                }
            }
        }

        function toggleOverview() {
            if (deck.classList.contains('overview')) {
                deck.classList.remove('overview');
                paint();
                return;
            }
            deck.classList.remove('filmstrip'); // overview and filmstrip are separate views
            buildThumbs();
            deck.classList.add('overview');
            sizeThumbs();
            paint();
        }

        function toggleFilmstrip() {
            if (deck.classList.contains('filmstrip')) {
                deck.classList.remove('filmstrip');
                paint();
                return;
            }
            deck.classList.remove('overview');
            buildThumbs();
            deck.classList.add('filmstrip');
            sizeThumbs();
            paint();
            var current = thumbs.querySelector('.slideThumbItem.current');
            if (current) {
                current.scrollIntoView({ block: 'nearest' });
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
                case 'o':
                case 'O':
                    toggleOverview(); e.preventDefault(); break;
                case 'l':
                case 'L':
                    toggleFilmstrip(); e.preventDefault(); break;
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

        // Click the stage to advance; chrome buttons and thumbnail items handle their own clicks.
        deck.addEventListener('click', function (e) {
            if (e.target.closest && (e.target.closest('.slideChrome') || e.target.closest('.slideThumbs'))) {
                return;
            }
            go(1);
        });

        if (filmBtn) { filmBtn.addEventListener('click', function (e) { e.stopPropagation(); toggleFilmstrip(); }); }
        if (overBtn) { overBtn.addEventListener('click', function (e) { e.stopPropagation(); toggleOverview(); }); }
        if (fullBtn) { fullBtn.addEventListener('click', function (e) { e.stopPropagation(); toggleFullscreen(); }); }
        var prev = deck.querySelector('.slidePrev');
        var next = deck.querySelector('.slideNext');
        if (prev) { prev.addEventListener('click', function (e) { e.stopPropagation(); go(-1); }); }
        if (next) { next.addEventListener('click', function (e) { e.stopPropagation(); go(1); }); }

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

        // Keep thumbnails scaled correctly when the grid reflows on resize.
        window.addEventListener('resize', function () {
            if (deck.classList.contains('overview') || deck.classList.contains('filmstrip')) {
                sizeThumbs();
            }
        });

        // The fullscreen button's pressed state follows the actual fullscreen element.
        document.addEventListener('fullscreenchange', function () {
            setPressed(fullBtn, document.fullscreenElement === deck);
        });
    }

    document.addEventListener('DOMContentLoaded', function () {
        Array.prototype.slice.call(document.querySelectorAll('.slideDeck')).forEach(initDeck);
    });
})();
