import test from 'node:test';
import assert from 'node:assert/strict';
import { bootBoard as boot, cardIds, setColumnRects } from './lib/kanban-board.mjs';

const threeListContent = [
  '=== List 1',
  '==== One ==== #c1',
  '===== Property',
  '===== Activity',
  '=== List 2',
  '==== Two ==== #c2',
  '===== Property',
  '===== Activity',
  '=== List 3',
  '==== Three ==== #c3',
  '===== Property',
  '===== Activity'
].join('\n');

const longThirdListContent = [
  '=== List 1',
  '==== One ==== #c1',
  '===== Property',
  '===== Activity',
  '=== List 2',
  '==== Two ==== #c2',
  '===== Property',
  '===== Activity',
  '=== List 3',
  '==== Three 1 ==== #c3a',
  '==== Three 2 ==== #c3b',
  '==== Three 3 ==== #c3c',
  '==== Three 4 ==== #c3d',
  '==== Three 5 ==== #c3e',
  '==== Target ==== #c-target',
  '===== Property',
  '===== Activity'
].join('\n');

const assigneeContent = [
  '=== ToDo',
  '==== Card ==== #c1',
  '===== Property',
  ' * Assignee',
  '  * [User:Alice]',
  '  * [User:Bob]',
  '===== Activity'
].join('\n');

const assigneeHighlightContent = [
  '=== ToDo',
  '==== Assigned ==== #assigned',
  '===== Property',
  ' * Assignee',
  '  * [User:Aha00a]',
  '===== Activity',
  '==== Created ==== #created',
  '===== Property',
  ' * Creator',
  '  * [User:Aha00a]',
  '===== Activity'
].join('\n');

const relationSelectContent = [
  '=== Backlog',
  '==== Current ==== #current',
  '===== Property',
  '===== Activity',
  '==== Blocker ==== #blocker',
  '===== Property',
  '===== Activity',
  '=== Done',
  '==== Done Card ==== #done-card',
  '===== Property',
  '===== Activity'
].join('\n');

test('read-only Kanban hides mutation controls and does not create Sortable instances', () => {
  const { root, board, sortableCreateCount } = boot({ writable: false });

  assert.equal(root.getAttribute('data-kanban-read-only'), 'true');
  assert.equal(board.querySelectorAll('.kanban-read-only-notice').length, 1);
  assert.equal(board.querySelectorAll('.kanban-add-list-button').length, 0);
  assert.equal(board.querySelectorAll('.kanban-add-card-button').length, 0);
  assert.equal(board.querySelectorAll('.kanban-icon-button').length, 0);
  assert.equal(board.classList.contains('kanban-draggable'), true);
  assert.equal(sortableCreateCount, 0);
});

test('writable Kanban keeps mutation controls and Sortable instances', () => {
  const { root, board, sortableCreateCount } = boot({ writable: true });

  assert.equal(root.getAttribute('data-kanban-read-only'), 'false');
  assert.equal(board.querySelectorAll('.kanban-read-only-notice').length, 0);
  assert.equal(board.querySelectorAll('.kanban-add-list-button').length, 1);
  assert.equal(board.querySelectorAll('.kanban-add-card-button').length, 1);
  assert.equal(board.querySelectorAll('.kanban-icon-button').length, 1);
  assert.equal(board.classList.contains('kanban-draggable'), true);
  assert.equal(sortableCreateCount, 2);
});

test('card surface shows assignee avatars where the card id used to be', () => {
  const { board } = boot({ writable: true, content: assigneeContent });
  const card = board.querySelector('.kanban-card');
  const meta = card.querySelector('.kanban-card-meta');
  const assignees = card.querySelector('.kanban-card-assignees');
  const avatars = assignees.querySelectorAll('.kanban-card-assignee-avatar');

  assert.equal(card.querySelector('.kanban-card-id'), null);
  assert.equal(meta.children[0], assignees);
  assert.equal(meta.children[1].classList.contains('kanban-card-stats'), true);
  assert.equal(assignees.title, 'Alice, Bob');
  assert.equal(avatars.length, 2);
  assert.deepEqual(avatars.map((avatar) => avatar.textContent), ['A', 'B']);
});

test('card surface highlights cards assigned to the current user only', () => {
  const { board } = boot({ writable: true, content: assigneeHighlightContent, currentUserNickname: 'Aha00a' });
  const assigned = board.querySelectorAll('.kanban-card').find((card) => card.getAttribute('data-card-id') === 'assigned');
  const created = board.querySelectorAll('.kanban-card').find((card) => card.getAttribute('data-card-id') === 'created');

  assert.equal(assigned.classList.contains('kanban-card-assigned-to-me'), true);
  assert.equal(created.classList.contains('kanban-card-assigned-to-me'), false);
});

test('card id click copies the card URL without following the hash link', async () => {
  const { board, body, copiedTexts } = boot({ writable: true });
  const card = board.querySelector('.kanban-card');

  card.dispatchEvent({ type: 'click' });

  const cardIdLabel = body.querySelector('.kanban-modal-card-id');
  let prevented = false;
  let stopped = false;
  cardIdLabel.dispatchEvent({
    type: 'click',
    preventDefault: () => { prevented = true; },
    stopPropagation: () => { stopped = true; }
  });
  for (let i = 0; i < 5; i += 1) {
    await Promise.resolve();
  }

  assert.equal(prevented, true);
  assert.equal(stopped, true);
  assert.deepEqual(copiedTexts, ['https://example.test/w/Page#c1']);
  const feedback = body.querySelector('.kanban-card-copy-feedback');
  assert.equal(feedback.textContent, 'Copied');
  assert.equal(feedback.classList.contains('kanban-hidden'), false);
});

test('relation card select groups options by list and shows card names only', () => {
  const { board, body } = boot({ writable: true, content: relationSelectContent });
  const card = board.querySelector('.kanban-card');

  card.dispatchEvent({ type: 'click' });

  // kanban-property-select is a styling class, shared by the relation, assignee and list
  // selects. It was unique when this test was written; the popup redesign added the list one
  // ahead of the relation one, and an unscoped query has picked the wrong select ever since.
  const relationEditor = body.querySelector('.kanban-relation-editor');
  const relationSelect = relationEditor.querySelector('.kanban-property-select');
  const groups = relationSelect.children.slice(1);

  assert.equal(relationSelect.children[0].tagName, 'OPTION');
  assert.deepEqual(groups.map((group) => group.tagName), ['OPTGROUP', 'OPTGROUP']);
  assert.deepEqual(groups.map((group) => group.label), ['Backlog', 'Done']);
  assert.deepEqual(groups[0].children.map((option) => option.textContent), ['Blocker']);
  assert.deepEqual(groups[1].children.map((option) => option.textContent), ['Done Card']);
  assert.equal(groups[0].children[0].value, 'blocker');
  assert.equal(groups[0].children[0].title, '#blocker');
});

test('initial card hash scrolls inside the Kanban board and card list', () => {
  const { root, board, body } = boot({ writable: true, content: longThirdListContent, hash: '#c-target' });
  const columns = board.querySelectorAll('.kanban-column');
  const targetList = columns[2].querySelector('.kanban-card-list');
  const targetCard = targetList.querySelectorAll('.kanban-card').find((card) => card.getAttribute('data-card-id') === 'c-target');
  const overlay = body.querySelector('.kanban-card-detail-overlay');

  assert.equal(root.scrollIntoViewCalls.length, 0);
  assert.equal(targetCard.id, '');
  assert.ok(board.scrollLeft > 0);
  assert.ok(targetList.scrollTop > 0);
  assert.equal(overlay.getAttribute('data-card-id'), 'c-target');
});

test('card Sortable uses Trello-like drag preview classes', () => {
  const { board, sortableCreates } = boot({ writable: true });
  const list = board.querySelector('.kanban-card-list');
  const cardSortable = sortableCreates.find((entry) => entry.element === list);

  assert.ok(cardSortable);
  assert.equal(cardSortable.options.draggable, '.kanban-card:not(.kanban-card-drag-origin)');
  assert.equal(cardSortable.options.ghostClass, 'kanban-card-drag-placeholder');
  assert.equal(cardSortable.options.chosenClass, 'kanban-card-drag-chosen');
  assert.equal(cardSortable.options.dragClass, 'kanban-card-drag-placeholder');
  assert.equal(cardSortable.options.forceFallback, true);
  assert.equal(cardSortable.options.fallbackClass, 'kanban-card-drag-preview');
  assert.equal(cardSortable.options.fallbackOnBody, true);

  const previewClone = list.querySelector('.kanban-card').cloneNode(true);
  cardSortable.options.onClone({ clone: previewClone });
  assert.equal(previewClone.id, '');
  assert.equal(previewClone.getAttribute('data-line-number'), null);
  assert.equal(previewClone.getAttribute('aria-hidden'), 'true');
  assert.equal(previewClone.classList.contains('kanban-card-drag-preview'), true);
});

test('card drag keeps an origin clone only while dragging', () => {
  const { root, board, body, sortableCreates } = boot({ writable: true });
  const columns = board.querySelectorAll('.kanban-column');
  const lists = board.querySelectorAll('.kanban-card-list');
  setColumnRects(columns);

  const dragged = lists[0].querySelectorAll('.kanban-card')[0];
  dragged.classList.add('kanban-card-drag-placeholder', 'kanban-card-drag-chosen');
  const sourceSortable = sortableCreates.find((entry) => entry.element === lists[0]);
  sourceSortable.options.onStart({
    item: dragged,
    from: lists[0],
    oldIndex: 1,
    originalEvent: { clientX: 50, clientY: 180 }
  });

  assert.equal(root.classList.contains('kanban-card-dragging'), true);
  assert.equal(body.classList.contains('kanban-card-dragging-global'), true);

  const originClone = lists[0].querySelector('.kanban-card-drag-origin');
  assert.ok(originClone);
  assert.equal(originClone.id, '');
  assert.equal(originClone.getAttribute('aria-hidden'), 'true');
  assert.equal(originClone.parentElement, lists[0]);
  assert.equal(originClone.classList.contains('kanban-card-drag-origin'), true);
  assert.equal(originClone.classList.contains('kanban-card-drag-placeholder'), false);
  assert.equal(originClone.classList.contains('kanban-card-drag-chosen'), false);
  assert.equal(lists[0].children.indexOf(originClone), 1);
  assert.equal(lists[0].children.indexOf(dragged), 2);

  sourceSortable.options.onMove({
    item: dragged,
    from: lists[0],
    to: lists[0],
    oldIndex: 1,
    newIndex: 2,
    originalEvent: { clientX: 50, clientY: 180 }
  });

  assert.equal(lists[0].style.values.outline, undefined);
  assert.equal(lists[0].style.values.background, undefined);

  sourceSortable.options.onMove({
    item: dragged,
    from: lists[0],
    to: lists[0],
    oldIndex: 1,
    newIndex: 2,
    originalEvent: { clientX: -50, clientY: -50 }
  });

  assert.equal(lists[0].children.indexOf(originClone), 1);
  assert.equal(lists[0].children.indexOf(dragged), 2);

  sourceSortable.options.onEnd({
    item: dragged,
    from: lists[0],
    to: lists[0],
    oldIndex: 1,
    newIndex: 1,
    originalEvent: { clientX: 50, clientY: 180 }
  });

  assert.equal(board.querySelectorAll('.kanban-card-drag-origin').length, 0);
  assert.equal(root.classList.contains('kanban-card-dragging'), false);
  assert.equal(body.classList.contains('kanban-card-dragging-global'), false);
  assert.equal(dragged.classList.contains('kanban-card-drag-placeholder'), false);
  assert.equal(dragged.classList.contains('kanban-card-drag-chosen'), false);
});

test('list Sortable uses Trello-like drag preview classes', () => {
  const { board, sortableCreates } = boot({ writable: true });
  const boardSortable = sortableCreates.find((entry) => entry.element === board);

  assert.ok(boardSortable);
  assert.equal(boardSortable.options.draggable, '.kanban-column:not(.kanban-list-drag-origin)');
  assert.equal(boardSortable.options.ghostClass, 'kanban-list-drag-placeholder');
  assert.equal(boardSortable.options.chosenClass, 'kanban-list-drag-chosen');
  assert.equal(boardSortable.options.dragClass, 'kanban-list-drag-placeholder');
  assert.equal(boardSortable.options.forceFallback, true);
  assert.equal(boardSortable.options.fallbackClass, 'kanban-list-drag-preview');
  assert.equal(boardSortable.options.fallbackOnBody, true);

  const previewClone = board.querySelector('.kanban-column').cloneNode(true);
  previewClone.classList.add('kanban-list-drag-placeholder', 'kanban-list-drag-chosen');
  boardSortable.options.onClone({ clone: previewClone });
  assert.equal(previewClone.getAttribute('data-column-index'), null);
  assert.equal(previewClone.getAttribute('data-column-line-number'), null);
  assert.equal(previewClone.getAttribute('aria-hidden'), 'true');
  assert.equal(previewClone.classList.contains('kanban-list-drag-preview'), true);
  assert.equal(previewClone.classList.contains('kanban-list-drag-placeholder'), false);
  assert.equal(previewClone.classList.contains('kanban-list-drag-chosen'), false);
});

test('list drag keeps an origin clone only while dragging', () => {
  const { root, board, body, sortableCreates } = boot({ writable: true, content: threeListContent });
  const columns = board.querySelectorAll('.kanban-column');
  const dragged = columns[0];
  dragged.classList.add('kanban-list-drag-placeholder', 'kanban-list-drag-chosen');
  const boardSortable = sortableCreates.find((entry) => entry.element === board);

  boardSortable.options.onStart({
    item: dragged,
    from: board,
    oldIndex: 0,
    oldDraggableIndex: 0,
    originalEvent: { clientX: 50, clientY: 180 }
  });

  const originClone = board.querySelector('.kanban-list-drag-origin');
  assert.ok(originClone);
  assert.equal(originClone.getAttribute('aria-hidden'), 'true');
  assert.equal(originClone.parentElement, board);
  assert.equal(originClone.classList.contains('kanban-list-drag-origin'), true);
  assert.equal(originClone.classList.contains('kanban-list-drag-placeholder'), false);
  assert.equal(originClone.classList.contains('kanban-list-drag-chosen'), false);
  assert.equal(board.children.indexOf(originClone), 0);
  assert.equal(board.children.indexOf(dragged), 1);
  assert.equal(root.classList.contains('kanban-list-dragging'), true);
  assert.equal(body.classList.contains('kanban-list-dragging-global'), true);

  boardSortable.options.onEnd({
    item: dragged,
    from: board,
    to: board,
    oldIndex: 0,
    oldDraggableIndex: 0,
    newIndex: 0,
    newDraggableIndex: 0,
    originalEvent: { clientX: 50, clientY: 180 }
  });

  assert.equal(board.querySelectorAll('.kanban-list-drag-origin').length, 0);
  assert.equal(root.classList.contains('kanban-list-dragging'), false);
  assert.equal(body.classList.contains('kanban-list-dragging-global'), false);
  assert.equal(dragged.classList.contains('kanban-list-drag-placeholder'), false);
  assert.equal(dragged.classList.contains('kanban-list-drag-chosen'), false);
});

test('list drag computes drop index without the origin clone', () => {
  const { board, sortableCreates } = boot({ writable: true, content: threeListContent });
  const columns = board.querySelectorAll('.kanban-column');
  const dragged = columns[0];
  const boardSortable = sortableCreates.find((entry) => entry.element === board);

  boardSortable.options.onStart({
    item: dragged,
    from: board,
    oldIndex: 0,
    oldDraggableIndex: 0,
    originalEvent: { clientX: 50, clientY: 180 }
  });
  board.insertBefore(dragged, columns[2]);

  boardSortable.options.onEnd({
    item: dragged,
    from: board,
    to: board,
    oldIndex: 0,
    oldDraggableIndex: 0,
    newIndex: 2,
    newDraggableIndex: 1,
    originalEvent: { clientX: 180, clientY: 180 }
  });

  const titles = board.querySelectorAll('.kanban-column-title').map((title) => title.textContent);
  assert.deepEqual(titles, ['List 2', 'List 1', 'List 3']);
});

test('card drag outside expanded drop targets cancels stale list move', () => {
  const { board, sortableCreates, getFetchCalls } = boot({ writable: true, content: threeListContent });
  const columns = board.querySelectorAll('.kanban-column');
  const lists = board.querySelectorAll('.kanban-card-list');
  setColumnRects(columns);

  const dragged = lists[2].querySelectorAll('.kanban-card')[0];
  lists[1].appendChild(dragged);

  const sourceSortable = sortableCreates.find((entry) => entry.element === lists[2]);
  sourceSortable.options.onEnd({
    item: dragged,
    from: lists[2],
    to: lists[1],
    oldIndex: 1,
    newIndex: 2,
    originalEvent: { clientX: 110, clientY: 180 }
  });

  const refreshedLists = board.querySelectorAll('.kanban-card-list');
  assert.deepEqual(cardIds(refreshedLists[0]), ['c1']);
  assert.deepEqual(cardIds(refreshedLists[1]), ['c2']);
  assert.deepEqual(cardIds(refreshedLists[2]), ['c3']);
  assert.equal(getFetchCalls(), 0);
});

test('card drag uses the column as the expanded drop target', () => {
  const { board, sortableCreates } = boot({ writable: true, content: threeListContent });
  const columns = board.querySelectorAll('.kanban-column');
  const lists = board.querySelectorAll('.kanban-card-list');
  setColumnRects(columns);

  const dragged = lists[2].querySelectorAll('.kanban-card')[0];
  lists[1].appendChild(dragged);

  const sourceSortable = sortableCreates.find((entry) => entry.element === lists[2]);
  sourceSortable.options.onEnd({
    item: dragged,
    from: lists[2],
    to: lists[1],
    oldIndex: 1,
    newIndex: 2,
    originalEvent: { clientX: 50, clientY: 340 }
  });

  assert.equal(dragged.parentElement, lists[0]);
  assert.deepEqual(cardIds(lists[0]), ['c1', 'c3']);
  assert.deepEqual(cardIds(lists[1]), ['c2']);
});
