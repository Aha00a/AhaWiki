// The admin UI's colour scheme is the wiki's theme: one choice, stored under the key the wiki's
// own toggle writes, so switching it here or in the wiki's navigation bar is the same switch and
// the next page agrees either way. Mantine's default manager would keep a second value under a
// key of its own, and the two would drift.
//
// The key and its values are also spelled out in _themeClass.scala.html, which has to run inline
// before the first paint, and in _base.scala.html's toggle. This is a module inside a bundle and
// cannot share a constant with either.
const storageKey = "ahawiki-theme";
const themeOfScheme = {light: "ahawiki-light", dark: "ahawiki-dark"};

function schemeOfTheme(theme) {
    return theme === themeOfScheme.dark ? "dark" : "light";
}

/** A MantineColorSchemeManager over the wiki's theme. The parameters are for the test. */
export function createWikiThemeManager({getStorage = () => window.localStorage, events = window, root = document.documentElement} = {}) {
    let onStorage = null;
    return {
        get(defaultValue) {
            try {
                return schemeOfTheme(getStorage().getItem(storageKey));
            } catch {
                return defaultValue;
            }
        },
        set(scheme) {
            const theme = themeOfScheme[scheme] ?? themeOfScheme.light;
            // _themeClass.scala.html put the class on <html> at load; it should not go stale
            // under a toggle. Mantine keeps its own attribute right by itself.
            for (const name of Object.values(themeOfScheme)) root.classList.toggle(`theme-${name}`, name === theme);
            try {
                getStorage().setItem(storageKey, theme);
            } catch {
                // Blocked storage: the scheme still applies to this page, as in the wiki's toggle.
            }
        },
        subscribe(onUpdate) {
            onStorage = (event) => {
                if (event.key === storageKey) onUpdate(schemeOfTheme(event.newValue));
            };
            events.addEventListener("storage", onStorage);
        },
        unsubscribe() {
            events.removeEventListener("storage", onStorage);
        },
        clear() {
            try {
                getStorage().removeItem(storageKey);
            } catch {
                // Nothing stored to clear.
            }
        },
    };
}
