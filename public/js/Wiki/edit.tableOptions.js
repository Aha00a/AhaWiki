/**
 * What the editor's autocomplete offers after `[[[#!table `.
 *
 * The first line of a table block is `tsv|csv [thRow] [thColumn] [option=value | class ...]`
 * (InterpreterTable.regexShebang), so what fits depends on how many words are already written.
 * One flat list used to be offered at every word: after `[[[#!table tsv 1 ` it offered `tsv` and
 * `tsv 1 bar=all` again, and picking one wrote `tsv 1 tsv 1 bar=all`.
 *
 *  - first word: the format, or a whole first line to start from;
 *  - while only numbers follow the format, one more header count (`1`), up to two of them;
 *  - after that, options and the one class the stylesheet gives a meaning to, minus those
 *    already written.
 *
 * `options` has to name every key InterpreterTable.parseRules reads; test/editor.table-options
 * fails when the two drift apart.
 */
(function (root) {
    const presets = ['tsv', 'csv', 'tsv 1', 'tsv 1 1', 'tsv 1 bar=all', 'tsv 1 1 heat=all', 'tsv 1 compact'];
    const options = ['bar=all', 'heat=all', 'compact'];

    /**
     * `left` is the text before the cursor. Returns null outside a table shebang, otherwise where
     * the word being typed starts, the word so far, and the names that fit at that position.
     */
    function suggest(left) {
        const match = left.match(/\[\[\[#!table\s+([^\]\n]*)$/i);
        if (!match)
            return null;
        const words = match[1].split(/\s+/);
        const prefix = words.pop();
        const written = words.filter(Boolean);

        let names;
        if (written.length === 0) {
            names = presets;
        } else {
            const countsSoFar = written.slice(1);
            const anotherCount = countsSoFar.length < 2 && countsSoFar.every(word => /^\d+$/.test(word)) ? ['1'] : [];
            const writtenLower = written.map(word => word.toLowerCase());
            names = anotherCount.concat(options.filter(option => !writtenLower.includes(option)));
        }
        return { start: left.length - prefix.length, prefix: prefix, names: names };
    }

    root.AhaWiki = root.AhaWiki || {};
    root.AhaWiki.TableOptions = { suggest: suggest, presets: presets, options: options };
})(window);
