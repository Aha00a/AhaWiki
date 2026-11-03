// The header of a numeric #!Table column names tablesorter's digit parser (`sorter-digit`), so
// every spelling the server reads as a number is sorted in the browser by what that parser makes
// of it. The two have to agree: when they did not, "-(1,234)" was a number to the server and
// text to tablesorter. This runs the real vendored tablesorter, not a copy of its regular
// expressions -- a copy would be a third statement of the same fact. One direction only: the
// spellings the server accepts, from the fixture InterpreterBlockUnit reads too.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const spellings = fs.readFileSync('test/table-number-spellings.tsv', 'utf8')
    .split(/\r?\n/)
    .filter(line => line !== '' && !line.startsWith('//'))
    .map(line => {
        const [spelling, value] = line.split('\t');
        return { spelling, value: value === 'none' ? null : Number(value) };
    });

// Just enough jQuery for the file to define itself: it hangs the plugin on `fn`, and the digit
// parser trims what it cannot read as a number.
function loadTablesorter() {
    const jQuery = { fn: {}, trim: s => String(s).trim() };
    const sandbox = { jQuery, window: {} };
    vm.createContext(sandbox);
    vm.runInContext(fs.readFileSync('public/vendor/tablesorter/jquery.tablesorter.min.js', 'utf8'), sandbox);
    return jQuery.tablesorter;
}

test('the fixture has numbers and non-numbers, and every value is a number or none', () => {
    assert.ok(spellings.some(s => s.value !== null));
    assert.ok(spellings.some(s => s.value === null));
    for (const { spelling, value } of spellings) {
        assert.ok(value === null || Number.isFinite(value), `${JSON.stringify(spelling)} has no readable value`);
    }
});

test("tablesorter's digit parser reads every number spelling as the server does", () => {
    const tablesorter = loadTablesorter();
    const digit = tablesorter.getParserById('digit');
    const table = { config: { ...tablesorter.defaults } };
    const disagree = spellings
        .filter(s => s.value !== null)
        .map(s => ({ ...s, browser: digit.format(s.spelling, table) }))
        .filter(s => s.browser !== s.value);
    assert.deepEqual(disagree, [], 'tablesorter reads these differently from the server:\n' +
        disagree.map(s => `  ${JSON.stringify(s.spelling)}: server ${s.value}, tablesorter ${JSON.stringify(s.browser)}`).join('\n'));
});
