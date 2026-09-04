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

test('a page without a Schema block is reported as such', () => {
    assert.equal(planFor('= Title\nsome prose', vocabulary).kind, 'no-schema-block');
});
