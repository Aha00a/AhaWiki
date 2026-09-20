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
import fs from 'node:fs';
import path from 'node:path';
import { rootDir } from '../scripts/lib/ahawiki.net.mjs';

/** The one version the application loads. SchemaOrg.scala names it in two places. */
const version = '26.0';
const dir = path.join(rootDir, 'public', 'schema.org', version);
const read = (file) => JSON.parse(fs.readFileSync(path.join(dir, file), 'utf8'));

const vocabulary = read('schemaorg-current-https.jsonld');
const graph = vocabulary.graph;
const typesOf = (node) => [].concat(node.type ?? []);

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
});

test('the counts match what SchemaOrgUnit pins, so the two cannot drift apart', () => {
    // test/com/aha00a/tests/unit/SchemaOrgUnit.scala asserts these same three numbers against the
    // loaded maps. Kept here too because this suite runs on every `npm test`, and a vocabulary
    // swap that changes them should fail in the place that explains why.
    assert.equal(graph.length, 2853, 'mapAll');
    assert.equal(graph.filter((node) => typesOf(node).includes('Class')).length, 906, 'mapClass');
    assert.equal(graph.filter((node) => typesOf(node).includes('Property')).length, 1469, 'mapProperty');
});

test('no term carries a foreign namespace', () => {
    // 26.0 is schema.org only. From 27.0 the release bundles terms from other vocabularies —
    // bibo:, cmns-*:, fibo-*:, gs1:, unece:, eli: — and SchemaOrgTransform strips only the
    // schema.org prefixes, so those keep their namespace and would land in mapClass/mapProperty,
    // reaching the class browser and the property suggestions. 30.1 brings 77 classes and 154
    // properties of them. Decide what to do with those before raising the version.
    const foreign = graph.map((node) => String(node.id ?? '')).filter((id) => id.includes(':'));
    assert.deepEqual(foreign, [], `foreign-namespace terms reached the maps: ${foreign.slice(0, 10).join(', ')}`);
});

test('tree.pruned.jsonld holds only what getHtmlTree walks', () => {
    // getHtmlTree reads `id` and `children` and nothing else; the pruning step exists to drop the
    // rest. A tree.jsonld copied in by mistake still renders, just far heavier than it needs.
    const seen = new Set();
    const walk = (node) => {
        Object.keys(node).forEach((key) => seen.add(key));
        (node.children ?? []).forEach(walk);
    };
    walk(read('tree.pruned.jsonld'));
    assert.deepEqual([...seen].sort(), ['children', 'id']);
});

test('every term the tree names exists in the vocabulary, bar the three known strays', () => {
    // The two files come from different places — the release for the vocabulary, schema.org's
    // /docs/tree.jsonld for the tree — so they can be fetched a version apart with nothing saying
    // so. This checks they still describe the same terms.
    //
    // Membership, not class-ness: the tree also lists enumeration values, which the vocabulary
    // types as their enumeration (Dermatology is a MedicalSpecialty, not a Class). Those belong
    // in the tree and the class browser renders them.
    const known = new Set(graph.map((node) => node.id));
    const missing = [];
    const walk = (node) => {
        if (!known.has(node.id)) missing.push(node.id);
        (node.children ?? []).forEach(walk);
    };
    walk(read('tree.pruned.jsonld'));

    // These three sit in the docs tree and in no layer of the current vocabulary: the two
    // ProductReturn terms were retired to the attic, and StupidType is schema.org's own test
    // fixture. The class browser therefore offers three terms that are not really there. Pinned
    // rather than filtered, so the list failing means the disagreement changed, not that it exists.
    assert.deepEqual(missing.sort(), ['ProductReturnEnumeration', 'ProductReturnPolicy', 'StupidType']);
});
