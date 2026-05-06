import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import path from 'node:path';

const COMMENT_PREFIX = ' * ';
const COMMENT_DETAIL_PREFIX = '  * ';

const parseKanbanText = (text, interpreterStartLine = 1) => {
  const parseCardHeading = (line) => {
    const match = line.match(/^====\s+(.+?)(\s+====(?:\s+([#.].+))?)?$/);
    if (!match) return null;
    const title = (match[1] || '').trim();
    const rawAttrs = (match[3] || '').trim();
    let id = '';
    const classNames = [];
    rawAttrs.replace(/([#.])([^#.\s]+)/g, (_, prefix, value) => {
      if (prefix === '#') id = value;
      else if (prefix === '.') classNames.push(value);
      return _;
    });
    return { text: title, id, classNames };
  };

  const lines = text.split(/\r?\n/);
  const columns = [];
  let currentColumn = null;
  let currentCard = null;
  let cardSection = '';
  let propertyKey = '';

  lines.forEach((line, lineIndex) => {
    const sectionMatch = line.match(/^===\s+(.+)$/);
    if (sectionMatch) {
      currentColumn = { title: sectionMatch[1].trim(), lineNumber: interpreterStartLine + lineIndex, cards: [] };
      columns.push(currentColumn);
      currentCard = null; cardSection = ''; propertyKey = '';
      return;
    }
    const cardSectionMatch = line.match(/^=====\s+(Property|Activity)\s*$/);
    if (cardSectionMatch && currentCard) { cardSection = cardSectionMatch[1]; propertyKey = ''; return; }

    if (cardSection === 'Property' && currentCard) {
      const propertyHeaderMatch = line.match(/^\s\*\s+([^:]+)\s*:\s*(.+)$/);
      if (propertyHeaderMatch) {
        const key = (propertyHeaderMatch[1] || '').trim();
        const value = (propertyHeaderMatch[2] || '').trim();
        currentCard.properties[key] = currentCard.properties[key] || [];
        if (value) currentCard.properties[key].push(value);
        propertyKey = key;
        return;
      }
      const propertyKeyOnlyMatch = line.match(/^\s\*\s+(.+)$/);
      if (propertyKeyOnlyMatch) {
        propertyKey = (propertyKeyOnlyMatch[1] || '').trim();
        currentCard.properties[propertyKey] = currentCard.properties[propertyKey] || [];
        return;
      }
      const propertyValueMatch = line.match(/^\s{2}\*\s+(.+)$/);
      if (propertyValueMatch && propertyKey) {
        currentCard.properties[propertyKey] = currentCard.properties[propertyKey] || [];
        currentCard.properties[propertyKey].push((propertyValueMatch[1] || '').trim());
        return;
      }
    }

    const commentDetailMatch = line.match(/^\s{2}\*\s+(.+)$/);
    if (commentDetailMatch && currentCard && cardSection !== 'Property' && currentCard.comments.length > 0) {
      const currentComment = currentCard.comments[currentCard.comments.length - 1];
      currentComment.details = currentComment.details || [];
      currentComment.details.push(commentDetailMatch[1].trim());
      return;
    }

    const commentHeaderMatch = line.match(/^\s\*\s+(.+)$/);
    if (commentHeaderMatch && currentCard && cardSection !== 'Property') {
      currentCard.comments = currentCard.comments || [];
      currentCard.comments.push({ header: commentHeaderMatch[1].trim(), details: [] });
      return;
    }

    const cardHeading = parseCardHeading(line);
    if (cardHeading && currentColumn) {
      currentCard = { text: cardHeading.text, id: cardHeading.id, classNames: cardHeading.classNames, lineNumber: interpreterStartLine + lineIndex, comments: [], properties: {} };
      cardSection = ''; propertyKey = '';
      currentColumn.cards.push(currentCard);
    }
  });

  return columns;
};

const serializeColumns = (columns) => columns.map((column) => {
  const lines = [`=== ${column.title || ''}`];
  (column.cards || []).forEach((card) => {
    const attrs = [];
    if (card.id) attrs.push(`#${card.id}`);
    (card.classNames || []).forEach((className) => className && attrs.push(`.${className}`));
    const attrSuffix = attrs.length > 0 ? ` ${attrs.join('')}` : '';
    lines.push(`==== ${card.text || ''} ====${attrSuffix}`);
    lines.push('===== Property');
    Object.keys(card.properties || {}).forEach((key) => {
      const values = card.properties[key] || [];
      if (values.length <= 1) {
        lines.push(values.length === 1 ? `${COMMENT_PREFIX}${key}: ${values[0]}` : `${COMMENT_PREFIX}${key}`);
      } else {
        lines.push(`${COMMENT_PREFIX}${key}`);
        values.forEach((value) => lines.push(`${COMMENT_DETAIL_PREFIX}${value}`));
      }
    });
    lines.push('===== Activity');
    (card.comments || []).forEach((comment) => {
      if (!comment || !comment.header) return;
      lines.push(`${COMMENT_PREFIX}${comment.header}`);
      (comment.details || []).forEach((detail) => lines.push(`${COMMENT_DETAIL_PREFIX}${detail}`));
    });
  });
  return lines.join('\n');
}).join('\n');

const fixtureRoot = path.join(process.cwd(), 'docs', 'kanban-fixtures');
const fixtureNames = ['01-basic', '02-properties', '03-comments', '04-multicolumn', '05-outside-text'];

const extractKanbanBlock = (text) => {
  const lines = text.split(/\r?\n/);
  const start = lines.findIndex((line) => /^===\s+/.test(line));
  if (start < 0) return { before: text, block: '', after: '' };
  const isKanbanLine = (line) => /^===\s+/.test(line) || /^====\s+/.test(line) || /^=====\s+/.test(line) || /^\s\*\s+/.test(line) || /^\s{2}\*\s+/.test(line) || line.trim() === '';
  let end = start;
  for (let i = start; i < lines.length; i += 1) {
    if (!isKanbanLine(lines[i])) break;
    end = i;
  }
  return {
    before: lines.slice(0, start).join('\n'),
    block: lines.slice(start, end + 1).join('\n').trimEnd(),
    after: lines.slice(end + 1).join('\n')
  };
};

for (const name of fixtureNames) {
  test(`kanban fixture roundtrip: ${name}`, () => {
    const input = fs.readFileSync(path.join(fixtureRoot, name, 'input.wiki'), 'utf8').trimEnd();
    const golden = fs.readFileSync(path.join(fixtureRoot, name, 'golden.wiki'), 'utf8').trimEnd();

    const extracted = name === '05-outside-text' ? extractKanbanBlock(input) : { before: '', block: input, after: '' };
    const columns = parseKanbanText(extracted.block, 1);
    const actualBlock = serializeColumns(columns).trimEnd();

    // 구조적 의미 보존(컬럼/카드/활동/속성 데이터) 검증
    const expectedColumns = parseKanbanText(extracted.block, 1);
    assert.deepEqual(columns, expectedColumns);

    // 정규화 직렬화 기준점 검증: golden도 동일 직렬화로 정규화되어야 함
    const normalizedGolden = serializeColumns(parseKanbanText(name === '05-outside-text' ? extractKanbanBlock(golden).block : golden, 1)).trimEnd();
    assert.equal(actualBlock, normalizedGolden);

    // Kanban 블록 외부 텍스트는 교체 후에도 동일해야 함
    if (name === '05-outside-text') {
      const replaced = [extracted.before, actualBlock, extracted.after].filter((v) => v !== '').join('\n');
      assert.match(replaced, /^도입부 일반 위키 텍스트\./);
      assert.match(replaced, /마무리 일반 위키 텍스트\.$/);
    }

    // 카드 ID 유지 최소 보장
    const ids = [...extracted.block.matchAll(/#([A-Za-z0-9_-]+)/g)].map((m) => m[1]);
    ids.forEach((id) => assert.match(actualBlock, new RegExp(`#${id}(\\s|$)`)));
  });
}
