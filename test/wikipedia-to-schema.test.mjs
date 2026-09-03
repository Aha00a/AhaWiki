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
});

test('an unmapped label keeps its own name and its value', () => {
    const output = convertWikipediaToSchemaOrg([['Owner', 'Seoul Metropolitan Government']], true);
    assert.match(output, /^Owner\tSeoul Metropolitan Government$/m);
});
