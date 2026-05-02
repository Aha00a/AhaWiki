document.addEventListener('DOMContentLoaded', function () {
    var kanbanInterpreters = document.querySelectorAll('.InterpreterKanban');

    var parseKanbanText = function (text) {
        var lines = text.split(/\r?\n/);
        var columns = [];
        var currentColumn = null;

        lines.forEach(function (line) {
            var sectionMatch = line.match(/^==\s+(.+)$/);
            if (sectionMatch) {
                currentColumn = {
                    title: sectionMatch[1].trim(),
                    cards: []
                };
                columns.push(currentColumn);
                return;
            }

            var cardMatch = line.match(/^\s*\*\s+(.+)$/);
            if (cardMatch && currentColumn) {
                currentColumn.cards.push(cardMatch[1].trim());
            }
        });

        return columns;
    };

    kanbanInterpreters.forEach(function (root) {
        var pre = root.querySelector('pre[data-shebang]');
        var board = root.querySelector('.kanban-board');
        if (!pre || !board) {
            return;
        }

        var columns = parseKanbanText(pre.textContent || '');
        pre.style.display = 'none';
        board.style.display = 'flex';
        board.style.gap = '12px';
        board.style.alignItems = 'flex-start';
        board.style.overflowX = 'auto';

        columns.forEach(function (column, index) {
            var columnElement = document.createElement('div');
            columnElement.className = 'kanban-column';
            columnElement.setAttribute('data-column-index', String(index));
            columnElement.style.minWidth = '220px';
            columnElement.style.background = '#f6f7f9';
            columnElement.style.border = '1px solid #d7dce2';
            columnElement.style.borderRadius = '6px';
            columnElement.style.padding = '10px';
            columnElement.style.boxSizing = 'border-box';

            var title = document.createElement('div');
            title.textContent = column.title;
            title.style.fontWeight = 'bold';
            title.style.marginBottom = '8px';
            columnElement.appendChild(title);

            var cardList = document.createElement('div');
            cardList.className = 'kanban-card-list';
            cardList.style.minHeight = '20px';

            column.cards.forEach(function (card) {
                var cardElement = document.createElement('div');
                cardElement.className = 'kanban-card';
                cardElement.textContent = card;
                cardElement.style.background = '#fff';
                cardElement.style.border = '1px solid #cfd5dd';
                cardElement.style.borderRadius = '6px';
                cardElement.style.padding = '8px';
                cardElement.style.marginBottom = '8px';
                cardList.appendChild(cardElement);
            });

            columnElement.appendChild(cardList);
            board.appendChild(columnElement);

            if (window.Sortable) {
                Sortable.create(cardList, {
                    group: root.id || 'kanban-default',
                    animation: 120
                });
            }
        });
    });
});
