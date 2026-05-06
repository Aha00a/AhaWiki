import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync('public/js/AhaWiki.Kanban.js', 'utf8');

const loadHooks = () => {
  let onReady = null;
  const windowObj = {
    location: { hash: '', pathname: '/w/Test', search: '' },
    history: { pushState: () => {}, replaceState: () => {} },
    requestAnimationFrame: (fn) => fn(),
    addEventListener: () => {}
  };
  const documentObj = {
    addEventListener: (evt, cb) => { if (evt === 'DOMContentLoaded') onReady = cb; },
    querySelectorAll: () => [],
    querySelector: () => null
  };
  const sandbox = {
    window: windowObj,
    document: documentObj,
    console,
    fetch: async () => ({ ok: true, json: async () => ({}) }),
    CustomEvent: class {},
    CSS: { escape: (v) => String(v) }
  };
  vm.createContext(sandbox);
  vm.runInContext(source, sandbox);
  if (onReady) onReady();
  return sandbox.window.__AhaWikiKanbanTestHooks;
};

const hooks = loadHooks();
const { getActionMetaValue, shortenCardCommentForRevision, truncateRevisionText, serializePropertyValueForRevision } = hooks;

test('card:comment:add 첫 줄만 사용', () => {
  assert.equal(shortenCardCommentForRevision('first\nsecond'), 'first');
});

test('card:comment:add 80자 이하는 유지', () => {
  const t = 'a'.repeat(80);
  assert.equal(shortenCardCommentForRevision(t), t);
});

test('card:comment:add 80자 초과는 ... 처리', () => {
  const t = 'b'.repeat(81);
  assert.equal(shortenCardCommentForRevision(t), `${'b'.repeat(80)}...`);
});

test('card:comment:add 앞뒤 공백 정리', () => {
  assert.equal(shortenCardCommentForRevision('   hello world   '), 'hello world');
});

test('card:comment:add 이모지/한글 포함 첫 줄 유지', () => {
  assert.equal(shortenCardCommentForRevision('한글��� 테스트\n다음줄'), '한글��� 테스트');
});

test('card:property:update 단일값 직렬화', () => {
  assert.equal(serializePropertyValueForRevision(' DueDate:2026-12-31 '), 'DueDate:2026-12-31');
});

test('card:property:update 다중값 직렬화', () => {
  assert.equal(serializePropertyValueForRevision([' Alice ', 'Bob', '']), 'Alice, Bob');
});

test('card:property:update 빈 배열은 (empty)', () => {
  assert.equal(serializePropertyValueForRevision(['', '   ']), '(empty)');
});

test('card:property:update 빈 문자열은 (empty)', () => {
  assert.equal(serializePropertyValueForRevision('   '), '(empty)');
});

test('액션 메타 값 추출 null/undefined 처리', () => {
  assert.equal(getActionMetaValue({ x: null }, 'x'), '');
  assert.equal(getActionMetaValue({ x: undefined }, 'x'), '');
  assert.equal(getActionMetaValue({ x: 12 }, 'x'), '12');
});

test('리비전 가독성: 긴 리스트/카드 제목 요약', () => {
  const long = 'x'.repeat(100);
  assert.equal(truncateRevisionText(long, 60).length, 63);
  assert.equal(truncateRevisionText(long, 60).endsWith('...'), true);
});

test('리비전 가독성: 긴 속성명 요약', () => {
  const longKey = 'property-name-'.repeat(6);
  const shortened = truncateRevisionText(longKey, 40);
  assert.equal(shortened.endsWith('...'), true);
  assert.equal(shortened.length, 43);
});
