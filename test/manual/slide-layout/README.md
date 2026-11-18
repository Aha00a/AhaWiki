# slide-layout

Draws the slide layouts `InterpreterSlide` marks up -- a `dark cover`, `dark cards flow`,
`dark cards stats`, a screen slide with a label over its title, a line under it and a 16:9 picture
given a width (as `[[Attachment(x.webp, 93%)]]` leaves it once `InterpreterSlide` has made the
width a max-width; left as `width: 93%`, the picture's box loses its proportions and the check says
so), a tall picture inside a link, and a plain slide -- with the real `app/assets/wiki.css`,
`public/js/js.js` and `public/js/AhaWiki.Slide.js`, and checks what the stylesheet is for: the dark
colours whatever the theme, the cards in one row of equal width with arrows between them, a figure
that stays on one line in the narrowest card, a picture that stays inside its slide in its own
proportions, the label on its title, the line between the title and the picture -- under half the
title, muted, one line -- and headings without a number, an underline or a fold button. And the
filmstrip rail when it holds more slides than it is tall: the page cuts the rail short, since its
few slides would fit, and checks that the rail scrolls and the thumbnails stay 16:9 rather than
squeezed flat, which is how the EWS deck's 83 drew -- and that moving to the last slide and back
scrolls the rail to keep the current thumbnail in view, without scrolling the page.

Under it, a `keynote` deck of seven, with the Pretendard stylesheet `InterpreterSlide` links for
one: a cover with a picture behind it (`backdrop`), cards with arrows, three `big` slides -- a
short title, a long one that has to wrap between words, one word in an h1 -- a screenshot under
a label and a line, and figures. The checks there: the black slide and Pretendard whatever the
theme, the font arrived, the picture behind the content across the whole slide, the line under the
screenshot's title in the values the Chrome tab settled, a `big` title at the size the script found,
inside the slide, a pixel more not, no word broken, the thumbnail at the stage's size -- and that a
font arriving later has the titles sized again (the page draws one in serif, then lets a font
arrive).

The markup is what `SlideUnit` pins on the server side; change one and change the other.

Serve the repository root and open the page:

```bash
python -m http.server 9997
```

then `http://localhost:9997/test/manual/slide-layout/index.html`. The results are printed under the
decks. `?theme=dark` draws the page in the dark theme; `?stacked` leaves `AhaWiki.Slide.js` out, to
check the slides as they stand without it. `?at=3` leaves the first deck on its third slide, and
`?deck=keynote&at=3` the keynote deck, with the first put away -- for `--screenshot`.

Headless, the three of them:

```bash
chrome --headless=new --window-size=1400,1000 --virtual-time-budget=8000 --dump-dom "http://localhost:9997/test/manual/slide-layout/index.html?theme=dark"
```

and read the `<pre id="result">` in what it prints. Run it at more than one window width -- 1280,
1920 and 3840 at least. Until 2026-11-18 a slide's padding followed the window, and a 1280px window
is the one width at which that looked right; the page prints the padding and type size it measured.

A run in which both pictures measure about 60px high and three times as wide is one in which
`http.server` dropped the pictures' requests; they drew as broken images. Run it again -- two in
about thirty runs did that on 2026-11-18, and the same page passed each time it was repeated. Its
fonts go the same way: such a run also says no slice of Pretendard loaded.

Give each run a new, empty `--user-data-dir`: `http.server` sends no cache headers, so a profile
kept between runs can go on running yesterday's
`js.js` against today's edit -- a fix that had already worked once read as failing that way.

The deck sits in the wrapper a `[[[#!Slide]]]` block gets on a page. The first deck on the
InterpreterSlide page got a fold button on its first `==` title from that wrapper, which the
first version of this page, with the deck bare, could not see.
