# Admin vendor bundle harness

Loads the real `public/js/babel/admin.js` against the real `public/js/babel/vendor/` files, through
the same import map `_baseAdmin.scala.html` uses, with the API answered by canned JSON in the page.
No server, no login, no database, and no CDN — which is the point: the admin UI used to get React,
Mantine, recharts and the router from esm.sh, and the day esm.sh ran out of disk (2026-10-31) every
admin screen stopped at its loading message. `scripts/admin.mjs --vendor` now builds those
packages into `vendor/`, and this page is where you see that the bundle alone is enough.

What it checks, and prints as `PASS`/`FAIL` every second:

 * **No script error.** The first version of the vendor build re-exported React with
   `export *`, which compiles but leaves React's names to run time, and the browser stopped at
   `does not provide an export named 'createContext'` before drawing anything. The page looked
   like the esm.sh outage it was meant to cure.
 * **Nothing fetched off this origin.** Any address not on `localhost` is a dependency the
   bundle was supposed to remove.
 * **The loading message is gone**, and on the dashboard **the two site-activity tables are
   drawn** from the canned `/api/Admin/SiteActivity` (the fixture mirrors
   `ApiAdminSiteActivitySpec`: a busy site and an idle one).

What you have to look at: that Mantine is styled (cards with borders, a sidebar with the section
labels), that the chart draws, and that the DataTable on **User** shows its "No records" overlay
inside the table rather than across the page — that overlay is what went wrong the last time a
stylesheet silently failed to arrive. The navigation works; the dashboard checks switch themselves
off on the other pages.

## Running it

The Browser pane refuses `file://`, so serve the repository root:

```bash
python -m http.server 9998
```

and open `http://localhost:9998/test/manual/admin-vendor/`. `.claude/launch.json` has this as
`static`. The page rewrites its address to `/Admin` so the app's router has a route to match; the
static server never sees that address.

Keep the import map here the same as the template's, minus the `?v=` digests. A specifier in one
and not the other is exactly what this page exists to catch.
