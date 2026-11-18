// Screen presentation for InterpreterSlide. The server renders a `.slideDeck` of `.slide` sections
// (see InterpreterSlide.scala); this shows them one at a time with keyboard/click navigation,
// fullscreen, an overview grid and a filmstrip rail.
//
// Every surface renders the slide at ONE fixed design size (1280x720) and scales it to fit its box
// via a CSS variable the script sets (_slide.less reads it in `transform`). Same design width
// everywhere means a thumbnail wraps and spaces exactly like the stage -- a true miniature. The
// scale rides a CSS variable rather than an inline transform so it never leaks into @media print.
//
// The index math is a handful of pure functions on window.AhaWiki.Slide, unit tested directly
// (test/ahawiki.slide.test.mjs); the DOM wiring below runs only in a browser.
(function () {
    window.AhaWiki = window.AhaWiki || {};

    var Slide = {
        clampIndex: function (index, total) {
            if (total <= 0) return 0;
            if (index < 0) return 0;
            if (index > total - 1) return total - 1;
            return index;
        },
        nextIndex: function (current, total, delta) {
            return Slide.clampIndex(current + delta, total);
        },
        parseHashIndex: function (hash, total) {
            var m = /^#\/(\d+)$/.exec(hash || '');
            if (!m) return null;
            return Slide.clampIndex(parseInt(m[1], 10) - 1, total);
        },
        hashForIndex: function (index) {
            return '#/' + (index + 1);
        },
        // The presenter window and the deck it follows talk on one BroadcastChannel per deck: the
        // page and the deck's position on it, so two decks on a page, or two pages, never cross.
        channelName: function (pathname, deckIndex) {
            return 'AhaWiki.Slide:' + pathname + ':' + deckIndex;
        },
        // A window opened with this name is a presenter window: notes, next slide and a timer.
        presenterWindowName: function (deckIndex) {
            return 'AhaWikiSlidePresenter:' + deckIndex;
        },
        isPresenterWindowName: function (name, deckIndex) {
            return name === Slide.presenterWindowName(deckIndex);
        },
        // Elapsed time as m:ss, or h:mm:ss past the hour.
        formatElapsed: function (ms) {
            var total = Math.max(0, Math.floor(ms / 1000));
            var h = Math.floor(total / 3600);
            var m = Math.floor((total % 3600) / 60);
            var s = total % 60;
            var ss = (s < 10 ? '0' : '') + s;
            if (h > 0) {
                return h + ':' + (m < 10 ? '0' : '') + m + ':' + ss;
            }
            return m + ':' + ss;
        }
    };

    window.AhaWiki.Slide = Slide;

    if (typeof document === 'undefined') {
        return;
    }

    var DESIGN_W = 1280;
    var DESIGN_H = 720;

    function initDeck(deck, deckIndex) {
        var slides = Array.prototype.slice.call(deck.querySelectorAll('.slide'));
        var total = slides.length;
        if (total === 0) {
            return;
        }
        var hasNotes = !!deck.querySelector('.slideNotes');
        var isPresenter = hasNotes && Slide.isPresenterWindowName(window.name, deckIndex);

        // Wrap the real slides in a stage; it is the 16:9 box the current slide is scaled into.
        var stageEl = document.createElement('div');
        stageEl.className = 'slideStage';
        deck.insertBefore(stageEl, slides[0]);
        slides.forEach(function (s) { stageEl.appendChild(s); });

        var counterEl = deck.querySelector('.slideCurrent');
        var filmBtn = deck.querySelector('.slideFilmstrip');
        var overBtn = deck.querySelector('.slideOverview');
        var fullBtn = deck.querySelector('.slideFullscreen');
        var notesBtn = deck.querySelector('.slideNotesToggle');
        var presenterBtn = deck.querySelector('.slidePresenter');
        var timerEl = deck.querySelector('.slideTimer');
        var lone = document.querySelectorAll('.slideDeck').length === 1;
        var index = 0;
        var thumbs = null; // thumbnail container (clones), built lazily and shared by both modes

        // The speaker-notes panel under the stage. The notes themselves stay in each slide's
        // <aside>, hidden; the panel shows a copy of the current one, and the next slide's title.
        var notesPanel = null;
        var notesBody = null;
        var notesNext = null;
        if (hasNotes) {
            notesPanel = document.createElement('div');
            notesPanel.className = 'slideNotesPanel';
            notesNext = document.createElement('div');
            notesNext.className = 'slideNotesNext';
            notesBody = document.createElement('div');
            notesBody.className = 'slideNotesBody';
            notesPanel.appendChild(notesNext);
            notesPanel.appendChild(notesBody);
            deck.insertBefore(notesPanel, deck.querySelector('.slideChrome'));
        }

        // Keeps a presenter window and the deck it was opened from on the same slide. Either side
        // may move; a window that has just opened asks where the other is.
        var channel = null;
        if (hasNotes && typeof BroadcastChannel !== 'undefined') {
            channel = new BroadcastChannel(Slide.channelName(location.pathname, deckIndex));
        }

        deck.classList.add('presenting');
        if (isPresenter) {
            deck.classList.add('notes');
            deck.classList.add('presenterView');
        }

        function setPressed(btn, on) {
            if (btn) {
                btn.setAttribute('aria-pressed', on ? 'true' : 'false');
            }
        }

        // Scale the current slide to fit the stage box (fit + center, so a non-16:9 box letterboxes),
        // scale each thumbnail clone to its item, and keep the filmstrip rail no taller than the stage.
        function layout() {
            var bw = stageEl.clientWidth;
            var bh = stageEl.clientHeight;
            if (bw && bh) {
                var s = Math.min(bw / DESIGN_W, bh / DESIGN_H);
                for (var i = 0; i < slides.length; i++) {
                    slides[i].style.setProperty('--slide-scale', s);
                }
            }
            if (thumbs) {
                var items = thumbs.querySelectorAll('.slideThumbItem');
                for (var j = 0; j < items.length; j++) {
                    var w = items[j].clientWidth;
                    if (!w) { continue; }
                    var ts = w / DESIGN_W;
                    items[j].style.height = Math.round(DESIGN_H * ts) + 'px';
                    var clone = items[j].querySelector('.slide');
                    if (clone) { clone.style.setProperty('--thumb-scale', ts); }
                }
                thumbs.style.maxHeight = deck.classList.contains('filmstrip')
                    ? stageEl.clientHeight + 'px'
                    : '';
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
            if (notesBody) {
                var aside = slides[index].querySelector('.slideNotes');
                notesBody.innerHTML = aside ? aside.innerHTML : '';
                notesBody.classList.toggle('empty', !aside);
                notesBody.scrollTop = 0;
                var nextSlide = slides[index + 1];
                var heading = nextSlide && nextSlide.querySelector('.slideContent h1, .slideContent h2, .slideContent h3');
                notesNext.textContent = nextSlide
                    ? '→ ' + (index + 2) + '. ' + (heading ? heading.textContent.trim() : '')
                    : '→ (end)';
            }
            setPressed(filmBtn, deck.classList.contains('filmstrip'));
            setPressed(overBtn, deck.classList.contains('overview'));
            setPressed(notesBtn, deck.classList.contains('notes'));
            setPressed(fullBtn, document.fullscreenElement === deck);
        }

        function to(i, fromChannel) {
            index = Slide.clampIndex(i, total);
            paint();
            if (channel && !fromChannel) {
                channel.postMessage({ type: 'index', index: index });
            }
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
                        deck.classList.remove('overview');
                        paint();
                        layout();
                    }
                });
                thumbs.appendChild(item);
            });
            deck.insertBefore(thumbs, deck.firstChild);
        }

        // The notes panel, the overview and the filmstrip each lay the deck out differently, so only
        // one is on at a time.
        function toggleNotes() {
            if (!notesPanel) {
                return;
            }
            if (deck.classList.contains('notes')) {
                deck.classList.remove('notes');
            } else {
                deck.classList.remove('overview');
                deck.classList.remove('filmstrip');
                deck.classList.add('notes');
            }
            paint();
            layout();
        }

        function openPresenter() {
            if (!hasNotes) {
                return;
            }
            var url = location.href.split('#')[0] + Slide.hashForIndex(index);
            window.open(url, Slide.presenterWindowName(deckIndex), 'width=1100,height=800');
        }

        function toggleOverview() {
            if (deck.classList.contains('overview')) {
                deck.classList.remove('overview');
            } else {
                deck.classList.remove('filmstrip');
                deck.classList.remove('notes');
                buildThumbs();
                deck.classList.add('overview');
            }
            paint();
            layout();
        }

        function toggleFilmstrip() {
            if (deck.classList.contains('filmstrip')) {
                deck.classList.remove('filmstrip');
                paint();
                layout();
                return;
            }
            deck.classList.remove('overview');
            deck.classList.remove('notes');
            buildThumbs();
            deck.classList.add('filmstrip');
            paint();
            layout();
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
                case 'n':
                case 'N':
                    if (hasNotes) { toggleNotes(); e.preventDefault(); }
                    break;
                case 'p':
                case 'P':
                    if (hasNotes) { openPresenter(); e.preventDefault(); }
                    break;
                default:
                    break;
            }
        }

        if (lone) {
            var fromHash = Slide.parseHashIndex(location.hash, total);
            if (fromHash !== null) {
                index = fromHash;
            }
        }
        paint();
        layout();

        deck.addEventListener('click', function (e) {
            if (e.target.closest && (e.target.closest('.slideChrome') || e.target.closest('.slideThumbs') || e.target.closest('.slideNotesPanel'))) {
                return;
            }
            go(1);
        });

        if (notesBtn) { notesBtn.addEventListener('click', function (e) { e.stopPropagation(); toggleNotes(); }); }
        if (presenterBtn) { presenterBtn.addEventListener('click', function (e) { e.stopPropagation(); openPresenter(); }); }

        if (channel) {
            channel.addEventListener('message', function (e) {
                var data = e.data || {};
                if (data.type === 'index' && typeof data.index === 'number') {
                    to(data.index, true);
                } else if (data.type === 'hello') {
                    channel.postMessage({ type: 'index', index: index });
                }
            });
            if (isPresenter) {
                channel.postMessage({ type: 'hello' });
            }
        }

        // The presenter's clock: time since the window opened; clicking it starts it again.
        if (isPresenter && timerEl) {
            var started = Date.now();
            timerEl.hidden = false;
            timerEl.title = 'Elapsed (click to reset)';
            var tick = function () { timerEl.textContent = Slide.formatElapsed(Date.now() - started); };
            tick();
            setInterval(tick, 1000);
            timerEl.addEventListener('click', function (e) { e.stopPropagation(); started = Date.now(); tick(); });
        }
        if (isPresenter) {
            deck.focus();
        }

        if (filmBtn) { filmBtn.addEventListener('click', function (e) { e.stopPropagation(); toggleFilmstrip(); }); }
        if (overBtn) { overBtn.addEventListener('click', function (e) { e.stopPropagation(); toggleOverview(); }); }
        if (fullBtn) { fullBtn.addEventListener('click', function (e) { e.stopPropagation(); toggleFullscreen(); }); }
        var prev = deck.querySelector('.slidePrev');
        var next = deck.querySelector('.slideNext');
        if (prev) { prev.addEventListener('click', function (e) { e.stopPropagation(); go(-1); }); }
        if (next) { next.addEventListener('click', function (e) { e.stopPropagation(); go(1); }); }

        document.addEventListener('keydown', function (e) {
            var active = document.fullscreenElement
                ? (document.fullscreenElement === deck)
                : deck.contains(document.activeElement);
            if (active) {
                handleKey(e);
            }
        });

        // Re-fit the slide and thumbnails whenever their boxes change size.
        window.addEventListener('resize', layout);
        document.addEventListener('fullscreenchange', function () {
            setPressed(fullBtn, document.fullscreenElement === deck);
            layout();
        });
    }

    document.addEventListener('DOMContentLoaded', function () {
        Array.prototype.slice.call(document.querySelectorAll('.slideDeck')).forEach(function (deck, i) {
            initDeck(deck, i);
        });
    });
})();
