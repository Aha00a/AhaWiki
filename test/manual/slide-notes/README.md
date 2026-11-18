# slide-notes

Drives the real `public/js/AhaWiki.Slide.js` against the real `app/assets/wiki.css` on a deck shaped
like what `InterpreterSlide` writes, and checks the speaker notes: hidden on the stage and in
thumbnails, the panel `N` opens under the slide, and the presenter window `P` opens, which follows
the deck and which the deck follows. Inside the page's content, as on a wiki page, a heading in the
notes is drawn as a slide's is -- no number, no underline -- and the line naming the next slide
leaves out the number the wiki puts in front of a heading; until 2026-11-18 the notes drew `0.1.`
and the line read «→ 2. 1. 둘째 장».

Serve the repository root and open the page:

```bash
python -m http.server 9998
```

then `http://localhost:9998/test/manual/slide-notes/index.html`. The results are printed under the
deck. `?keep` leaves the presenter window open to look at.

The presenter checks need a browser that opens popups. The Claude desktop app's browser pane does
not -- `window.open` there replaces the page -- so run it in Chrome, or headless with
`--disable-popup-blocking` driven over the DevTools protocol. Not with `--dump-dom
--virtual-time-budget`: the two windows' messages do not cross under virtual time, and «the
presenter follows the deck» and «the deck follows the presenter» fail there with nothing wrong.
