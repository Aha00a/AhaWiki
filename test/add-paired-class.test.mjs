// The migration rewrites block headers on live pages, so what it may and may not touch is pinned
// against the vocabulary the app actually ships.
import test from 'node:test';
import assert from 'node:assert/strict';
import {loadVocabulary, planFor} from '../scripts/add-paired-class.mjs';

const vocabulary = loadVocabulary();
const block = (header, ...lines) => [`[[[#!Schema ${header}`, ...lines, ']]]'].join('\n');

test('a library typed as source that names an application category gains SoftwareApplication', () => {
    const plan = planFor(block('SoftwareSourceCode', 'name\tMocha', 'programmingLanguage\tJavaScript', 'applicationCategory\tTest framework'), vocabulary);
    assert.equal(plan.kind, 'rewrite');
    assert.deepEqual(plan.changes[0].to, ['SoftwareSourceCode', 'SoftwareApplication']);
    assert.deepEqual(plan.changes[0].because, ['applicationCategory']);
    assert.match(plan.next, /^\[\[\[#!Schema SoftwareSourceCode SoftwareApplication$/m);
});

test('a program typed as an application that names its repository gains SoftwareSourceCode', () => {
    const plan = planFor(block('SoftwareApplication', 'name\tncdu', 'operatingSystem\tLinux', 'codeRepository\tgithub.com/x', 'programmingLanguage\tC'), vocabulary);
    assert.equal(plan.kind, 'rewrite');
    assert.deepEqual(plan.changes[0].to, ['SoftwareApplication', 'SoftwareSourceCode']);
    assert.deepEqual(plan.changes[0].because.sort(), ['codeRepository', 'programmingLanguage']);
});

test('a block that already names both is left alone', () => {
    assert.equal(planFor(block('SoftwareApplication SoftwareSourceCode', 'codeRepository\tx', 'operatingSystem\ty'), vocabulary).kind, 'already-fits');
});

test('a block whose properties all fit is left alone', () => {
    // React: every property is SoftwareSourceCode's or CreativeWork's.
    assert.equal(planFor(block('SoftwareSourceCode', 'author\tJordan Walke', 'codeRepository\tx', 'programmingLanguage\tJavaScript', 'license\tMIT'), vocabulary).kind, 'already-fits');
});

test('the order the author wrote is kept, and the pair goes after', () => {
    const plan = planFor(block('WebApplication', 'applicationCategory\tx', 'codeRepository\ty'), vocabulary);
    assert.deepEqual(plan.changes[0].to, ['WebApplication', 'SoftwareSourceCode']);
});

test('a misfit the pair does not explain is reported, not papered over', () => {
    // industry belongs to JobPosting; no software class makes it fit.
    const plan = planFor(block('SoftwareApplication', 'codeRepository\tx', 'industry\tSoftware'), vocabulary);
    assert.equal(plan.kind, 'unresolved');
    assert.deepEqual(plan.unresolved[0].misfits, ['industry']);
});

test('a property the vocabulary does not know cannot cause a change', () => {
    assert.equal(planFor(block('SoftwareSourceCode', 'programmingLanguage\tC', 'Type of site\tx'), vocabulary).kind, 'already-fits');
});

test('only the block that needs it changes; comments and other blocks stay', () => {
    const page = [
        block('SoftwareSourceCode', '# Category\tTest framework', 'applicationCategory\tTest framework', 'programmingLanguage\tJS'),
        '',
        block('Rating', 'ratingValue\t5'),
    ].join('\n');
    const plan = planFor(page, vocabulary);
    assert.equal(plan.kind, 'rewrite');
    assert.equal(plan.changes.length, 1);
    assert.match(plan.next, /^\[\[\[#!Schema Rating$/m);
    assert.match(plan.next, /^# Category\tTest framework$/m);
});

test('a class outside the software pair is never widened', () => {
    // duration does not fit TVSeries, but there is no pair for it; leave that for a human.
    assert.equal(planFor(block('TVSeries', 'name\tx', 'duration\tPT1H'), vocabulary).kind, 'unresolved');
});

test('logo does not hold a page back, because it is drawn whatever the class', () => {
    // logo's domain is Organization/Brand/Product; the interpreter renders it as a picture on any
    // block (imageKeys). Twelve library pages carry one. It must neither block the pair nor be
    // reported as unresolved.
    const needsPair = planFor(block('SoftwareSourceCode', 'logo\thttps://x/logo.svg', 'applicationCategory\tFramework', 'programmingLanguage\tJS'), vocabulary);
    assert.equal(needsPair.kind, 'rewrite');
    assert.deepEqual(needsPair.changes[0].because, ['applicationCategory']);

    const fine = planFor(block('SoftwareSourceCode', 'logo\thttps://x/logo.svg', 'programmingLanguage\tJS'), vocabulary);
    assert.equal(fine.kind, 'already-fits');
});

test('a line copied from a sibling block is not a reason to widen the class', () => {
    // Graphviz and Homebrew give the program its own SoftwareApplication block and the codebase a
    // SoftwareSourceCode one, then repeat applicationCategory in the second, value and all. Adding
    // the class there would make the page declare the same application twice. Three independent
    // reviewers were asked whether the change was right; one saw this and the planner had not.
    const page = [
        block('SoftwareApplication', 'name\tGraphviz', 'operatingSystem\tLinux', 'applicationCategory\tGraph Visualization'),
        '',
        block('SoftwareSourceCode', 'programmingLanguage\tC', 'applicationCategory\tGraph Visualization'),
    ].join('\n');
    const plan = planFor(page, vocabulary);
    assert.equal(plan.kind, 'duplicate-of-sibling');
    assert.deepEqual(plan.duplicates[0].lines.map(l => l.line), ['applicationCategory\tGraph Visualization']);
});

test('a sibling block with a different value is a real second fact, so the class widens', () => {
    // Only an identical line is a copy. Two different categories are two claims, and the block
    // making the second one does need the class that defines the property.
    const page = [
        block('SoftwareApplication', 'applicationCategory\tGraph Visualization'),
        '',
        block('SoftwareSourceCode', 'programmingLanguage\tC', 'applicationCategory\tDeveloper Tools'),
    ].join('\n');
    assert.equal(planFor(page, vocabulary).kind, 'rewrite');
});

test('a lone block is widened even when the page name mentions the other class', () => {
    // netcat and npm each have one block. Two of their three reviewers died on a session limit and
    // the run recorded that as refusal; reading the pages showed one block and no sibling.
    assert.equal(planFor(block('SoftwareApplication', 'operatingSystem\tPOSIX', 'codeRepository\tx'), vocabulary).kind, 'rewrite');
    assert.equal(planFor(block('SoftwareSourceCode', 'programmingLanguage\tJS', 'softwareVersion\t1.0', 'applicationCategory\tPackage Manager'), vocabulary).kind, 'rewrite');
});

test('a page without a Schema block is reported as such', () => {
    assert.equal(planFor('= Title\nsome prose', vocabulary).kind, 'no-schema-block');
});
