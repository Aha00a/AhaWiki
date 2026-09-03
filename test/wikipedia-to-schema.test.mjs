// The infobox importer turns Wikipedia row labels into schema.org properties. A wrong entry
// in that table is worse than a missing one: an unmapped label passes through unchanged and
// the author sees it, while a mislabelled one looks correct. So the table is checked against
// the vocabulary the application itself ships, and the conversions that have a right answer
// are pinned.
//
// The real file is loaded and run, not reimplemented — it is a browser IIFE that hangs its
// API off `window`, so the only shim needed is an empty `window`.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';
import vm from 'node:vm';
import { rootDir } from '../scripts/lib/ahawiki.net.mjs';

function loadWikipediaToSchema() {
    const source = fs.readFileSync(path.join(rootDir, 'public/js/AhaWiki.WikipediaToSchema.js'), 'utf8');
    const context = {window: {}, console};
    vm.runInNewContext(source, context, {filename: 'AhaWiki.WikipediaToSchema.js'});
    return context.window.AhaWiki.WikipediaToSchema;
}

/** Property ids from the vocabulary the app renders with — the same file SchemaOrg.scala reads. */
function schemaOrgProperties() {
    const vocabulary = JSON.parse(fs.readFileSync(path.join(rootDir, 'public/schema.org/26.0/schemaorg-current-https.jsonld'), 'utf8'));
    return new Set(vocabulary.graph.filter(node => node.type === 'Property').map(node => node.id));
}

const wikipediaToSchema = loadWikipediaToSchema();
const {WikipediaToSchemaProperty, convertProperty, parseCoordinates, convertWikipediaToSchemaOrg} = wikipediaToSchema;

// The module runs in a vm context, so objects it returns carry that realm's prototype and
// strict deepEqual rejects them. Spreading brings the fields back into this realm.
const coordinates = text => ({...parseCoordinates(text)});

test('every mapping points at a property schema.org actually has', () => {
    const properties = schemaOrgProperties();
    const unknown = Object.entries(WikipediaToSchemaProperty)
        .filter(([, target]) => !properties.has(target))
        .map(([label, target]) => `${label} -> ${target}`);

    assert.deepEqual(unknown, [], '\nThese targets are not properties in public/schema.org/26.0. ' +
        'Leaving a label unmapped is better than mapping it to a name that does not exist.');
    assert.ok(Object.keys(WikipediaToSchemaProperty).length > 100, 'the table should not have been emptied');
});

test('ownership is left unmapped on purpose', () => {
    // schema.org has no property for "this place is owned by X". `owns` is the inverse — its
    // domain is Person/Organization and its range is Product. Mapping Owner to it would read
    // as correct while meaning the opposite, so the label passes through instead.
    for (const label of ['Owner', 'Owners', '소유자', '소유주']) {
        assert.equal(WikipediaToSchemaProperty[label], undefined, `${label} should not be mapped`);
        assert.equal(convertProperty(label), label, `${label} should pass through unchanged`);
    }
    assert.equal(schemaOrgProperties().has('owner'), false, 'if schema.org ever adds `owner`, revisit this');
});

test('address labels that unambiguously mean a place map to the address parts', () => {
    assert.equal(convertProperty('Address'), 'address');
    assert.equal(convertProperty('Street address'), 'streetAddress');
    assert.equal(convertProperty('Postal code'), 'postalCode');
    assert.equal(convertProperty('소재지'), 'address');
    assert.equal(convertProperty('우편번호'), 'postalCode');
    assert.equal(convertProperty('도시'), 'addressLocality');

    // Country is deliberately still countryOfOrigin: the table sees only the row label, and
    // film and television infoboxes — where that is the right answer — dominate.
    assert.equal(convertProperty('Country'), 'countryOfOrigin');
});

test('the labels ko.wikipedia actually uses are mapped, not the ones I assumed', () => {
    // Surveying real articles found the building infobox says 지리 좌표계 rather than 좌표, so
    // 롯데월드타워 and 63빌딩 kept an unsplit coordinate even after the coordinate parser worked.
    assert.equal(convertProperty('지리 좌표계'), 'geo');

    // foundingDate's range is Date, so an organisation keeps its whole date where a building
    // mapped to yearBuilt can only keep the year.
    assert.equal(convertProperty('설립일'), 'foundingDate');

    // Ownership stays out, whichever word the article uses.
    for (const label of ['소유주', '소유기관'])
        assert.equal(convertProperty(label), label);
});

test('parseCoordinates reads the forms Wikipedia writes', () => {
    assert.deepEqual(coordinates('37°33′36″N 126°58′41″E'), {latitude: 37.56, longitude: 126.978056});
    assert.deepEqual(coordinates("37°33'36\"N 126°58'41\"E"), {latitude: 37.56, longitude: 126.978056});
    assert.deepEqual(coordinates('37°33′N 126°58′E'), {latitude: 37.55, longitude: 126.966667});
    assert.deepEqual(coordinates('37.5665, 126.978'), {latitude: 37.5665, longitude: 126.978});

    // Southern and western hemispheres are negative.
    assert.deepEqual(coordinates('33°52′S 151°12′E'), {latitude: -33.866667, longitude: 151.2});

    assert.equal(parseCoordinates('somewhere near the river'), null);
    assert.equal(parseCoordinates('37°33′36″N'), null, 'one axis alone is not a coordinate');
});

test('ko.wikipedia writes the hemisphere in front, in Korean', () => {
    // The real 세종문화회관 cell. Reading only a trailing N/S/E/W left every Korean article's
    // coordinate unsplit, which is what shipping and then looking at one found.
    const cell = '북위 37° 34′ 21″ 동경 126° 58′ 32″ / 북위 37.5725° 동경 126.9756° / 37.5725; 126.9756';
    assert.deepEqual(coordinates(cell), {latitude: 37.5725, longitude: 126.975556});

    assert.deepEqual(coordinates('남위 33° 52′ 서경 151° 12′'), {latitude: -33.866667, longitude: -151.2});
});

test('a decimal pair is read even when it trails other text', () => {
    // Wikipedia often ends the cell with a clean pair after the sexagesimal forms. Requiring the
    // whole cell to be that pair threw it away.
    assert.deepEqual(coordinates('좌표: 어딘가 / 37.5725; 126.9756'), {latitude: 37.5725, longitude: 126.9756});
});

test('coordinates become latitude and longitude, because geo wants an object', () => {
    const output = convertWikipediaToSchemaOrg([['Coordinates', '37°33′36″N 126°58′41″E']], true);
    assert.equal(output.split('\n').filter(Boolean).join('|'), 'latitude\t37.56|longitude\t126.978056');
});

test('a coordinate that cannot be read stays as geo rather than being invented', () => {
    const output = convertWikipediaToSchemaOrg([['Coordinates', 'on the third floor']], true);
    assert.match(output, /^geo\ton the third floor$/m);
});

test('yearBuilt gets a year, since its range is Number', () => {
    assert.match(convertWikipediaToSchemaOrg([['Completed', '10 May 1988']], true), /^yearBuilt\t1988$/m);
    assert.match(convertWikipediaToSchemaOrg([['완공', '1988년 5월 10일']], true), /^yearBuilt\t1988$/m);
    assert.match(convertWikipediaToSchemaOrg([['Year built', '1988']], true), /^yearBuilt\t1988$/m);
    // No year in the value: keep what Wikipedia said rather than guess.
    assert.match(convertWikipediaToSchemaOrg([['Completed', 'under construction']], true), /^yearBuilt\tunder construction$/m);

    // The row arrives in pieces because Wikipedia split "April 11, 1931; 95 years ago (...)" on
    // its comma. Looking at one value at a time produced "yearBuilt<TAB>April 11<TAB>1931".
    const split = convertWikipediaToSchemaOrg([['Completed', 'April 11', '1931; 95 years ago (1931-04-11)']], true);
    assert.match(split, /^yearBuilt\t1931$/m);
});

test('an unmapped label keeps its own name and its value', () => {
    const output = convertWikipediaToSchemaOrg([['Owner', 'Seoul Metropolitan Government']], true);
    assert.match(output, /^Owner\tSeoul Metropolitan Government$/m);
});
