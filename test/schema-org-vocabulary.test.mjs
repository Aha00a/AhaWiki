// The vocabulary files under public/schema.org/ are not what schema.org publishes. They have been
// through SchemaOrgTransform, which unwraps `{"@id": x}` to `x` and strips the `@`, `rdf:`,
// `rdfs:` and `schema:` prefixes from every key and leaf. That is why the committed files say
// `graph`, `id`, `type` and `Class` where the release says `@graph`, `@id`, `@type` and
// `rdfs:Class` — see docs/ahawiki.net/'Dev SchemaOrgVocabulary'.
//
// SchemaOrg.scala reads the transformed shape directly, with `.as[String]`. Drop a release file in
// untransformed and nothing here says so: the failure is a JsResultException at runtime, on the
// first page that renders a Schema block. These tests fail by name instead.
import test from 'node:test';
import assert from 'node:assert/strict';
import { readVocabularyFile, schemaOrgTerms, schemaOrgVersion } from '../scripts/lib/schema-org.mjs';

const vocabulary = readVocabularyFile('schemaorg-current-https.jsonld');
const graph = vocabulary.graph;
const typesOf = (node) => [].concat(node.type ?? []);
const countOf = (nodes, type) => nodes.filter((node) => typesOf(node).includes(type)).length;

test('the vocabulary is in the transformed shape SchemaOrg.scala reads', () => {
    assert.ok(Array.isArray(graph), 'top level must be `graph` — an untransformed release says `@graph`');
    assert.equal(vocabulary['@graph'], undefined, '`@graph` means this file never went through SchemaOrgTransform');

    for (const key of ['id', 'type']) {
        assert.ok(graph.some((node) => node[key] !== undefined), `no node has \`${key}\``);
        assert.ok(!graph.some((node) => node[`@${key}`] !== undefined), `some node still has \`@${key}\``);
    }

    // getSeqString(v \ "type").find(v => v == "Class" || v == "Property") — the prefixed spellings
    // a release carries match neither, so every node would land with an empty schemaType.
    const types = new Set(graph.flatMap(typesOf));
    assert.ok(types.has('Class') && types.has('Property'), 'type values must be bare Class/Property');
    assert.ok(!types.has('rdfs:Class') && !types.has('rdf:Property'), 'prefixes were not stripped');
});

test('single-key @id objects were unwrapped to plain strings', () => {
    // `(v \ "id").as[String]` and getSeqString both assume strings. A release writes
    // subClassOf as {"@id": "schema:CreativeWork"}.
    const movie = graph.find((node) => node.id === 'Movie');
    assert.ok(movie, 'Movie is missing from the vocabulary');
    assert.equal(movie.subClassOf, 'CreativeWork');
    assert.equal(typeof movie.comment, 'string');

    // A localised term writes {language, value} instead, which the Scala reads with
    // `(v \ "comment" \ "value")` before falling back to the plain string. Both shapes are here.
    assert.ok(graph.some((node) => typeof node.comment === 'object' && node.comment !== null));
});

test('the counts match what SchemaOrgUnit pins, so the two cannot drift apart', () => {
    // test/com/aha00a/tests/unit/SchemaOrgUnit.scala asserts these three against the loaded maps.
    // They are the counts AFTER CalculatedSchemaOrg.isSchemaOrgTerm, which is what the maps hold.
    const kept = schemaOrgTerms(graph);
    assert.equal(kept.length, 3023, 'mapAll');
    assert.equal(countOf(kept, 'Class'), 939, 'mapClass');
    assert.equal(countOf(kept, 'Property'), 1538, 'mapProperty');
});

test('the terms that are not schema.org are the ones the filter drops', () => {
    // From 27.0 the release bundles other vocabularies, and the transform strips only schema.org's
    // prefixes, so these arrive namespaced. Without the filter they would be offered by the class
    // browser and the property suggestions as if they were ours.
    const foreign = graph.filter((node) => String(node.id ?? '').includes(':'));
    assert.equal(foreign.length, 231);
    assert.deepEqual([...new Set(foreign.map((node) => node.id.split(':')[0]))].sort().slice(0, 6),
        ['bibo', 'cmns-cls', 'cmns-col', 'cmns-dt', 'cmns-ge', 'cmns-id']);

    // The transform has to strip `rdf:` and `rdfs:` — `rdfs:Class` is how a class says it is one —
    // and the side effect is that rdf:type and rdfs:label arrive as bare `type` and `label`,
    // looking like schema.org properties. They are the only terms with no rdfs:comment, which is
    // what tells them apart.
    const commentless = graph.filter((node) => !String(node.id ?? '').includes(':') && node.comment === undefined);
    assert.deepEqual(commentless.map((node) => node.id).sort(), ['label', 'type']);

    assert.equal(schemaOrgTerms(graph).length, graph.length - foreign.length - commentless.length);
});

test('tree.pruned.jsonld holds only what getHtmlTree walks', () => {
    // getHtmlTree reads `id` and `children` and nothing else; the pruning step exists to drop the
    // rest. A tree.jsonld copied in by mistake still renders, just far heavier than it needs.
    const seen = new Set();
    const walk = (node) => {
        Object.keys(node).forEach((key) => seen.add(key));
        (node.children ?? []).forEach(walk);
    };
    walk(readVocabularyFile('tree.pruned.jsonld'));
    assert.deepEqual([...seen].sort(), ['children', 'id']);
});

test('the tree names no term the maps have thrown away, bar the one known stray', () => {
    // The two files come from different places — the release for the vocabulary, schema.org's
    // /docs/tree.jsonld for the tree — so they can be fetched a version apart with nothing saying
    // so. This checks they still describe the same terms.
    //
    // Membership, not class-ness: the tree also lists enumeration values, which the vocabulary
    // types as their enumeration (Dermatology is a MedicalSpecialty, not a Class). Those belong
    // in the tree and the class browser renders them.
    const known = new Set(schemaOrgTerms(graph).map((node) => node.id));
    const missing = [];
    const walk = (node) => {
        if (!known.has(node.id)) missing.push(node.id);
        (node.children ?? []).forEach(walk);
    };
    walk(readVocabularyFile('tree.pruned.jsonld'));

    // schema.org's own test fixture, which its docs tree lists and no layer of the vocabulary
    // defines. Pinned rather than filtered, so a change in the disagreement fails rather than the
    // disagreement itself. On 26.0 this list also held ProductReturnEnumeration and
    // ProductReturnPolicy, retired to the attic; 30.1's tree no longer names them.
    assert.deepEqual(missing.sort(), ['StupidType']);
});

test('the vocabulary directory the tests read is the one SchemaOrg.scala loads', () => {
    // schemaOrgVersion() parses the constant out of the Scala rather than repeating it, so a
    // version raised in one place and not the other cannot pass quietly. This asserts the parse
    // still finds something plausible — a rename there would otherwise throw far from the cause.
    assert.match(schemaOrgVersion(), /^\d+\.\d+$/);
});
