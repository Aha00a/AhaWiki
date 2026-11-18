# slide-notes

Drives the real `public/js/AhaWiki.Slide.js` against the real `app/assets/wiki.css` on a deck shaped
like what `InterpreterSlide` writes, and checks the speaker notes: hidden on the stage and in
thumbnails, the panel `N` opens under the slide, and the presenter window `P` opens, which follows
the deck and which the deck follows.

Serve the repository root and open the page:

```bash
python -m http.server 9998
```

then `http://localhost:9998/test/manual/slide-notes/index.html`. The results are printed under the
deck. `?keep` leaves the presenter window open to look at.

The presenter checks need a browser that opens popups. The Claude desktop app's browser pane does
not -- `window.open` there replaces the page -- so run it in Chrome, or headless with
`--disable-popup-blocking`.
