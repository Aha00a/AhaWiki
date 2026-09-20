// Which schema.org vocabulary the application reads, and how to load it.
//
// The version is not written here. `CalculatedSchemaOrg` in app/logics/SchemaOrg.scala is what
// actually loads the files, so its constant is the fact and this reads it back. Writing the
// number in both places is how a test ends up checking a vocabulary the application does not use.
//
// The files under public/schema.org/ are not what schema.org publishes — a separate repository
// transforms them. docs/ahawiki.net/'Dev SchemaOrgVocabulary' has the whole pipeline.
import fs from 'node:fs';
import path from 'node:path';
import { rootDir } from './ahawiki.net.mjs';

const scalaFile = path.join(rootDir, 'app', 'logics', 'SchemaOrg.scala');

/** The version SchemaOrg.scala loads, read from its own declaration. */
export function schemaOrgVersion() {
    const source = fs.readFileSync(scalaFile, 'utf8');
    const found = /private\s+val\s+version\s*=\s*"([^"]+)"/.exec(source);
    if (!found) throw new Error(`no \`private val version\` in ${scalaFile}; did the constant move or get renamed?`);
    return found[1];
}

export function vocabularyDir() {
    return path.join(rootDir, 'public', 'schema.org', schemaOrgVersion());
}

/** One of the three files in that directory, parsed. */
export function readVocabularyFile(name) {
    return JSON.parse(fs.readFileSync(path.join(vocabularyDir(), name), 'utf8'));
}

/**
 * The classes this wiki defines because schema.org does not. Beside the version directories
 * rather than inside one: it is ours, and does not change when the vocabulary is raised.
 */
export function readCustomVocabulary() {
    return JSON.parse(fs.readFileSync(path.join(rootDir, 'public', 'schema.org', 'custom.jsonld'), 'utf8'));
}

/**
 * The terms the application keeps, applying the same two filters as
 * `CalculatedSchemaOrg.isSchemaOrgTerm`: no namespaced id, and a comment present. Duplicated as
 * behaviour rather than as a constant, because a JS test cannot call the Scala — the tests below
 * assert the counts agree with SchemaOrgUnit.scala, which is what catches the two drifting apart.
 */
export function schemaOrgTerms(graph) {
    return graph.filter((node) => {
        const id = String(node.id ?? '');
        const comment = typeof node.comment === 'object' && node.comment !== null ? node.comment.value : node.comment;
        return !id.includes(':') && comment !== undefined && comment !== '';
    });
}
