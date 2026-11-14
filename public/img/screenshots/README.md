# Screenshots

The pictures at the top of the repository's `README.md` and on the wiki's
[AhaWiki](https://ahawiki.net/w/AhaWiki) page. They are served from here (`/public/img/screenshots/…`
on every site), so the two show the same files and there is one set to replace.

| File | Page on ahawiki.net | Crop of the 1280×860 shot |
|---|---|---|
| `board.png` | 예시 작업 보드 | top 1280×720 |
| `graph.png` | 예시 지식 지도 | x 0–1010, y 280–850: the graph, without the table of contents |
| `project.png` | 예시 프로젝트 홈 | top 1280×800 |

## Taking them again

Headless Chrome, **not logged in** — a logged-in screen carries the account's name in the navigation
bar and, on the edit screen, the permission summary with e-mail addresses in it. That is also why
there is no picture of the editor: it needs a login.

```bash
chrome --headless=new --disable-gpu --hide-scrollbars --user-data-dir=<a scratch folder> \
  --lang=ko-KR --window-size=1280,860 --virtual-time-budget=10000 \
  --screenshot=board.png "https://ahawiki.net/w/%EC%98%88%EC%8B%9C%20%EC%9E%91%EC%97%85%20%EB%B3%B4%EB%93%9C"
```

`--virtual-time-budget` lets the board and the graph finish drawing before the shot. Then crop as
in the table and reduce to 256 colours (Pillow: `quantize(colors=256, dither=NONE)`,
`save(optimize=True)`), which takes each from about 200 KB to about 50 KB without visible loss on
these flat screens.

Look at each one before committing it. The first graph shot showed the graph's METRICS box lying
over the page's table of contents at 1280px — a layout fault of the page, not of the picture, which
is why `graph.png` is cropped to the graph. The page was fixed the same day (the graph now starts
below the contents); the crop still frames the graph best.
