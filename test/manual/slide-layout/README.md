# slide-layout

Draws the slide layouts `InterpreterSlide` marks up -- a `dark cover`, `dark cards flow`,
`dark cards stats`, a screen slide with a label over its title and a 16:9 picture given a width (as
`[[Attachment(x.webp, 93%)]]` leaves it once `InterpreterSlide` has made the width a max-width;
left as `width: 93%`, the picture's box loses its proportions and the check says so), a tall picture
inside a link, and a plain slide -- with the real `app/assets/wiki.css`, `public/js/js.js` and
`public/js/AhaWiki.Slide.js`, and checks what the stylesheet is for: the dark colours whatever the
theme, the cards in one row of equal width with arrows between them, a figure that stays on one
line in the narrowest card, a picture that stays inside its slide in its own proportions, the label
on its title, and headings without a number, an underline or a fold button.

The markup is what `SlideUnit` pins on the server side; change one and change the other.

Serve the repository root and open the page:

```bash
python -m http.server 9997
```

then `http://localhost:9997/test/manual/slide-layout/index.html`. The results are printed under the
deck. `?theme=dark` draws the page in the dark theme; `?stacked` leaves `AhaWiki.Slide.js` out, to
check the slides as they stand without it.

Headless, the three of them:

```bash
chrome --headless=new --window-size=1400,1000 --virtual-time-budget=5000 --dump-dom "http://localhost:9997/test/manual/slide-layout/index.html?theme=dark"
```

and read the `<pre id="result">` in what it prints. Give each run a new, empty `--user-data-dir`:
`http.server` sends no cache headers, so a profile kept between runs can go on running yesterday's
`js.js` against today's edit -- a fix that had already worked once read as failing that way.

The deck sits in the wrapper a `[[[#!Slide]]]` block gets on a page. The first deck on the
InterpreterSlide page got a fold button on its first `==` title from that wrapper, which the
first version of this page, with the deck bare, could not see.
