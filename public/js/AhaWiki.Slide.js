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
        // The largest whole number from lo to hi for which fits() holds, given that it holds for
        // every number below one that it holds for; lo when it holds for none. What sizes a `big`
        // title: fits(px) draws the title at px and says whether it stayed inside the slide.
        largestFitting: function (lo, hi, fits) {
            if (!fits(lo)) return lo;
            if (fits(hi)) return hi;
            while (hi - lo > 1) {
                var mid = Math.floor((lo + hi) / 2);
                if (fits(mid)) {
                    lo = mid;
                } else {
                    hi = mid;
                }
            }
            return lo;
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

    // The size a `big` slide's titles take (2026-11-18, the Takahashi method): one size for all of
    // them, the largest at which none is wider than the slide -- a word that does not fit makes its
    // title wider, since they do not break inside a word -- and the slide's content is no taller
    // than the slide. The slide must be drawn at the design size, 1280x720, which is what every
    // presenting surface draws it at; transform scales what is seen, not what is measured. Null
    // when there is nothing to size, or the slide is not drawn.
    var FIT_MIN = 16;
    var FIT_MAX = 480;

    // A heading's words, without the number the wiki puts in front of it -- hidden in a deck, but
    // still in its text: «→ 7. 1. 통합 대시보드» (2026-11-18).
    function headingText(heading) {
        var copy = heading.cloneNode(true);
        var numbers = copy.querySelectorAll('.headingNumber');
        for (var i = 0; i < numbers.length; i++) {
            numbers[i].parentNode.removeChild(numbers[i]);
        }
        return copy.textContent.trim();
    }

    function fitTitles(slide) {
        var content = slide.querySelector('.slideContent');
        var titles = content ? content.querySelectorAll('h1, h2') : [];
        if (!titles.length || !slide.getClientRects().length) {
            return null;
        }
        var style = getComputedStyle(slide);
        var height = slide.clientHeight - parseFloat(style.paddingTop) - parseFloat(style.paddingBottom);
        var fits = function (px) {
            slide.style.setProperty('--slide-fit', px + 'px');
            for (var i = 0; i < titles.length; i++) {
                if (titles[i].scrollWidth > titles[i].clientWidth + 1) {
                    return false;
                }
            }
            return content.scrollHeight <= height + 1;
        };
        var px = Slide.largestFitting(FIT_MIN, FIT_MAX, fits);
        slide.style.setProperty('--slide-fit', px + 'px');
        return px;
    }

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
        var keysBtn = deck.querySelector('.slideKeys');
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
        // A deck with notes opens with the panel under the slide (owner's call, 2026-11-19): on a
        // page the reader is usually the speaker rehearsing. notesWanted is that choice, which only
        // N changes; the overview, the filmstrip and fullscreen (the audience's view) hide the panel
        // while they are on, and restoreNotes() gives it back when they go. Under 768px -- a phone --
        // the panel would take what is left of the screen under a small slide, so it opens folded
        // (same day); N still opens it.
        var notesWanted = hasNotes && (isPresenter || !window.matchMedia
            || window.matchMedia('(min-width: 768px)').matches);
        if (notesWanted) {
            deck.classList.add('notes');
        }
        if (isPresenter) {
            deck.classList.add('presenterView');
        }

        function restoreNotes() {
            var audience = document.fullscreenElement === deck && !isPresenter;
            if (notesWanted && !audience
                    && !deck.classList.contains('overview') && !deck.classList.contains('filmstrip')) {
                deck.classList.add('notes');
            }
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
            fitBig();
        }

        // Size the `big` slides' titles, each slide and its thumbnail alike. A slide is measured
        // where it can be: on the stage, shown for the measuring if it is not the current one, or,
        // with the stage hidden (overview), as its thumbnail. One that could be measured nowhere --
        // the deck itself hidden -- waits for the next layout(). Measured again when a font
        // arrives: the slide was sized with whatever drew it until then.
        var bigSlides = slides.filter(function (s) {
            return s.classList.contains('big') && s.querySelector('.slideContent h1, .slideContent h2');
        });
        var fitPending = bigSlides.length > 0;

        function fitBig() {
            if (!fitPending) {
                return;
            }
            var pending = false;
            bigSlides.forEach(function (slide) {
                var i = slides.indexOf(slide);
                var clone = thumbs ? thumbs.children[i].querySelector('.slide') : null;
                var px = null;
                if (stageEl.getClientRects().length) {
                    var display = slide.style.display;
                    slide.style.display = 'flex';
                    px = fitTitles(slide);
                    slide.style.display = display;
                } else if (clone) {
                    px = fitTitles(clone);
                }
                if (px === null) {
                    pending = true;
                    return;
                }
                slide.style.setProperty('--slide-fit', px + 'px');
                if (clone) {
                    clone.style.setProperty('--slide-fit', px + 'px');
                }
            });
            fitPending = pending;
        }

        if (bigSlides.length && document.fonts) {
            var refit = function () {
                fitPending = true;
                fitBig();
            };
            if (document.fonts.ready) {
                document.fonts.ready.then(refit);
            }
            if (document.fonts.addEventListener) {
                document.fonts.addEventListener('loadingdone', refit);
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
                railFollow();
            }
            if (notesBody) {
                var aside = slides[index].querySelector('.slideNotes');
                notesBody.innerHTML = aside ? aside.innerHTML : '';
                notesBody.classList.toggle('empty', !aside);
                notesBody.scrollTop = 0;
                var nextSlide = slides[index + 1];
                // A slide that is only a picture has no heading of its own; its notes may.
                var heading = nextSlide && (nextSlide.querySelector('.slideContent h1, .slideContent h2, .slideContent h3')
                    || nextSlide.querySelector('.slideNotes h1, .slideNotes h2, .slideNotes h3'));
                notesNext.textContent = nextSlide
                    ? '→ ' + (index + 2) + '. ' + (heading ? headingText(heading) : '')
                    : '→ (end)';
            }
            setPressed(filmBtn, deck.classList.contains('filmstrip'));
            setPressed(overBtn, deck.classList.contains('overview'));
            setPressed(notesBtn, deck.classList.contains('notes'));
            setPressed(fullBtn, document.fullscreenElement === deck);
        }

        // Scroll the filmstrip rail, and only the rail, so the current slide's thumbnail is in view.
        // Every move, not only when the rail opens: in a deck longer than the rail the current one
        // went out of sight (slide 64 of 83, 2026-11-18). Not scrollIntoView, which scrolls the page
        // as well when the deck is not wholly on screen.
        function railFollow() {
            if (!thumbs || !deck.classList.contains('filmstrip')) {
                return;
            }
            var item = thumbs.children[index];
            if (!item) {
                return;
            }
            var rail = thumbs.getBoundingClientRect();
            var box = item.getBoundingClientRect();
            var pad = parseFloat(getComputedStyle(thumbs).paddingTop) || 0;
            if (box.top < rail.top + pad) {
                thumbs.scrollTop += box.top - rail.top - pad;
            } else if (box.bottom > rail.bottom - pad) {
                thumbs.scrollTop += box.bottom - rail.bottom + pad;
            }
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
                if (!isPresenter && deck.classList.contains('notes')) {
                    deck.classList.remove('notes');
                    paint();
                }
                var asked = deck.requestFullscreen();
                // Refused (no user gesture, a frame that forbids it): no change event follows.
                if (asked && asked.catch) {
                    asked.catch(function () {
                        restoreNotes();
                        paint();
                        layout();
                    });
                }
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
                        restoreNotes();
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
                notesWanted = false;
            } else {
                deck.classList.remove('overview');
                deck.classList.remove('filmstrip');
                deck.classList.add('notes');
                notesWanted = true;
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
                restoreNotes();
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
                restoreNotes();
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
            // Again now that layout() has given the rail its height.
            railFollow();
        }

        function handleKey(e) {
            if (e.key === 'Escape' && keyHelp && !keyHelp.hidden) {
                showKeyHelp(false);
                e.preventDefault();
                return;
            }
            var key = e.key.length === 1 ? e.key.toLowerCase() : e.key;
            for (var i = 0; i < KEYS.length; i++) {
                if (KEYS[i].keys.indexOf(key) >= 0 && (!KEYS[i].when || KEYS[i].when())) {
                    KEYS[i].run();
                    e.preventDefault();
                    return;
                }
            }
        }

        // The deck's keys, once: handleKey() reads them, and the help `?` opens lists them. Inside
        // a deck the wiki's own hotkeys do not reach (F, L), so its `?` help would have named
        // keys that do something else here (2026-11-19).
        var KEYS = [
            { keys: ['ArrowRight', 'PageDown', ' ', 'Spacebar'], shown: '→ Space', label: 'Next slide', run: function () { go(1); } },
            { keys: ['ArrowLeft', 'PageUp'], shown: '←', label: 'Previous slide', run: function () { go(-1); } },
            { keys: ['Home'], shown: 'Home', label: 'First slide', run: function () { to(0); } },
            { keys: ['End'], shown: 'End', label: 'Last slide', run: function () { to(total - 1); } },
            { keys: ['f'], shown: 'F', label: 'Fullscreen', run: function () { toggleFullscreen(); } },
            { keys: ['o'], shown: 'O', label: 'Overview', run: function () { toggleOverview(); } },
            { keys: ['l'], shown: 'L', label: 'Filmstrip', run: function () { toggleFilmstrip(); } },
            { keys: ['n'], shown: 'N', label: 'Speaker notes', when: function () { return hasNotes; }, run: function () { toggleNotes(); } },
            { keys: ['p'], shown: 'P', label: 'Presenter window', when: function () { return hasNotes; }, run: function () { openPresenter(); } },
            { keys: ['?'], shown: '?', label: 'These keys', run: function () { toggleKeyHelp(); } }
        ];

        // Inside the deck, so it shows in fullscreen too. Shaped and behaving as the wiki's own `?`
        // list (hotkey.scala.html) is: a title, a line on closing it, a backdrop that closes it, and
        // a click on the list itself leaves it open. Neither click moves the deck on.
        var keyHelp = null;
        var keyHelpBg = null;
        function toggleKeyHelp() {
            if (!keyHelp) {
                keyHelpBg = document.createElement('div');
                keyHelpBg.className = 'slideKeyHelpBg';
                keyHelpBg.hidden = true;
                keyHelpBg.addEventListener('click', function (e) { e.stopPropagation(); showKeyHelp(false); });
                deck.appendChild(keyHelpBg);
                keyHelp = document.createElement('div');
                keyHelp.className = 'slideKeyHelp';
                keyHelp.hidden = true;
                var title = document.createElement('h2');
                title.textContent = 'Slide hotkeys';
                keyHelp.appendChild(title);
                var description = document.createElement('p');
                description.className = 'description';
                description.textContent = 'Press ESC or click outside to close.';
                keyHelp.appendChild(description);
                var table = document.createElement('table');
                KEYS.forEach(function (k) {
                    if (k.when && !k.when()) { return; }
                    var row = table.insertRow();
                    var th = document.createElement('th');
                    th.textContent = k.shown;
                    row.appendChild(th);
                    row.insertCell().textContent = k.label;
                });
                keyHelp.appendChild(table);
                keyHelp.addEventListener('click', function (e) { e.stopPropagation(); });
                deck.appendChild(keyHelp);
            }
            showKeyHelp(keyHelp.hidden);
        }
        function showKeyHelp(open) {
            keyHelp.hidden = !open;
            keyHelpBg.hidden = !open;
            setPressed(keysBtn, open);
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
        if (keysBtn) { keysBtn.addEventListener('click', function (e) { e.stopPropagation(); toggleKeyHelp(); }); }
        var prev = deck.querySelector('.slidePrev');
        var next = deck.querySelector('.slideNext');
        if (prev) { prev.addEventListener('click', function (e) { e.stopPropagation(); go(-1); }); }
        if (next) { next.addEventListener('click', function (e) { e.stopPropagation(); go(1); }); }

        // On window, capturing, so the deck sees its keys before the wiki's own hotkeys do
        // (hotkey.scala.html binds them on document): F went to the front page instead of
        // fullscreen, and L to PageList instead of the filmstrip, until 2026-11-19. A key the deck
        // used goes no further. With a modifier it is the browser's (Ctrl+F) and is left alone.
        window.addEventListener('keydown', function (e) {
            if (e.ctrlKey || e.metaKey || e.altKey) {
                return;
            }
            var active = document.fullscreenElement
                ? (document.fullscreenElement === deck)
                : deck.contains(document.activeElement);
            if (active) {
                handleKey(e);
                if (e.defaultPrevented) {
                    e.stopPropagation();
                }
            }
        }, true);

        // Re-fit the slide and thumbnails whenever their boxes change size -- the window's, and the
        // stage's own, which also changes when a deck that was hidden (a folded section) is shown.
        window.addEventListener('resize', layout);
        if (typeof ResizeObserver !== 'undefined') {
            new ResizeObserver(function () { layout(); }).observe(stageEl);
        }
        document.addEventListener('fullscreenchange', function () {
            restoreNotes();
            paint();
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
