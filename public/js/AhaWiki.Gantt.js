(function () {
    'use strict';

    // ── 날짜 유틸 ─────────────────────────────────────────────────────────────
    var DATE_RE = /^(\d{4})-(\d{2})-(\d{2})$/;

    function parseDate(str) {
        var m = DATE_RE.exec(String(str || '').trim());
        if (!m) return null;
        return new Date(+m[1], +m[2] - 1, +m[3]);
    }

    function pad2(n) { return n < 10 ? '0' + n : '' + n; }

    function isWeekend(d) { var w = d.getDay(); return w === 0 || w === 6; }

    function addBusinessDays(d, n) {
        var r = new Date(d);
        var step = n >= 0 ? 1 : -1;
        var remaining = Math.abs(n);
        while (remaining > 0) {
            r.setDate(r.getDate() + step);
            if (!isWeekend(r)) remaining--;
        }
        return r;
    }

    function calcEnd(start, est) {
        return addBusinessDays(start, Math.max(1, est) - 1);
    }

    function toBusinessDay(d) {
        var r = new Date(d);
        while (isWeekend(r)) r.setDate(r.getDate() + 1);
        return r;
    }

    function nextBizDay(d) {
        var r = new Date(d);
        do { r.setDate(r.getDate() + 1); } while (isWeekend(r));
        return r;
    }

    function diffDays(a, b) {
        return Math.round((b.getTime() - a.getTime()) / 86400000);
    }

    function countBizDays(start, end) {
        var count = 0;
        var d = new Date(start);
        while (d <= end) {
            if (!isWeekend(d)) count++;
            d.setDate(d.getDate() + 1);
        }
        return count;
    }

    function parseDefaultEst(value) {
        var s = String(value || '').trim();
        if (/^\d+$/.test(s)) {
            var n = parseInt(s, 10);
            if (n > 0) return n;
        }
        return 1;
    }

    // ── TSV 파싱 ──────────────────────────────────────────────────────────────
    function asRef(s) { return s.charAt(0) === '#' ? s.slice(1) : s; }

    function parseLine(line) {
        var cols = line.split('\t');

        // 1) 이름 컬럼을 왼쪽에서 먼저 확정
        var depth = -1, name = '', nameIdx = -1;
        for (var i = 0; i < cols.length; i++) {
            if (cols[i].trim() !== '') { depth = i; name = cols[i].trim(); nameIdx = i; break; }
        }
        if (depth === -1 || !name) return null;

        // 2) 이름 오른쪽 컬럼만 추출 (빈 칸 제거)
        var rest = [];
        for (var j = nameIdx + 1; j < cols.length; j++) {
            var v = cols[j].trim();
            if (v) rest.push(v);
        }

        // 3) 오른쪽에서 스캔: est → (날짜 or 참조)
        var est = null, startDate = null, afterRef = null;
        if (rest.length > 0 && /^\d+$/.test(rest[rest.length - 1])) {
            est = parseInt(rest.pop(), 10);
        }
        if (rest.length > 0) {
            var cand = rest[rest.length - 1];
            if (DATE_RE.test(cand)) startDate = parseDate(cand);
            else                    afterRef = asRef(cand);
        }

        // 4) 작업명 끝의 #id 추출 (공백 없는 경우만)
        var nodeLabel = null;
        var lm = name.match(/^([\s\S]*[^\s])#(\w+)$/);
        if (lm) { name = lm[1]; nodeLabel = lm[2]; }

        return { depth: depth, name: name, label: nodeLabel, startDate: startDate, est: est, afterRef: afterRef };
    }

    function buildTree(rows) {
        var root = { depth: -1, name: '__root__', children: [] };
        var stack = [root];
        rows.forEach(function (r) {
            var node = { id: 'g' + Math.random().toString(36).slice(2), depth: r.depth, name: r.name,
                label: r.label, explicitStart: r.startDate, explicitEst: r.est, afterRef: r.afterRef,
                children: [], collapsed: false };
            while (stack.length > 1 && stack[stack.length - 1].depth >= r.depth) stack.pop();
            stack[stack.length - 1].children.push(node);
            stack.push(node);
        });
        return root.children;
    }

    function collectLabels(nodes, map) {
        nodes.forEach(function (node) {
            map[node.name] = node;             // 이름 자체가 암묵적 레이블
            if (node.label) map[node.label] = node;  // #id 명시 시 override
            if (node.children.length) collectLabels(node.children, map);
        });
    }

    function parseGantt(raw) {
        var rows = [];
        raw.split(/\r?\n/).forEach(function (line) {
            if (!line.trim()) return;
            var r = parseLine(line);
            if (r) rows.push(r);
        });
        return buildTree(rows);
    }

    // ── 날짜 계산 ─────────────────────────────────────────────────────────────
    function calcDates(nodes, parentStart, prevSiblingEnd, labelMap, defaultEst) {
        var cursor = prevSiblingEnd;
        nodes.forEach(function (node) {
            var start;
            if (node.afterRef && labelMap[node.afterRef] && labelMap[node.afterRef].end) {
                start = nextBizDay(labelMap[node.afterRef].end);
            } else if (node.explicitStart) {
                start = toBusinessDay(node.explicitStart);
            } else if (cursor) {
                start = nextBizDay(cursor);
            } else if (parentStart) {
                start = parentStart;
            } else {
                start = toBusinessDay(new Date());
            }

            if (node.children.length === 0) {
                var est = node.explicitEst || defaultEst;
                node.start = start; node.est = est; node.end = calcEnd(start, est);
                cursor = node.end;
            } else {
                calcDates(node.children, start, null, labelMap, defaultEst);
                node.start = node.children[0].start;
                node.end   = node.children[node.children.length - 1].end;
                node.est   = countBizDays(node.start, node.end);
                cursor = node.end;
            }
        });
    }

    function flatten(nodes, result, collapsedIds) {
        nodes.forEach(function (node) {
            result.push(node);
            if (!collapsedIds[node.id] && node.children.length) flatten(node.children, result, collapsedIds);
        });
    }

    // ── 상수 ──────────────────────────────────────────────────────────────────
    var LABEL_W    = 360;
    var DAYS_COL_W = 36;  // 소요일 컬럼 (우측 끝)
    var DAY_W_DEF  = 20;
    var DAY_W_MIN  = 8;
    var DAY_W_MAX  = 60;
    var DAY_W_STEP = 4;
    var ROW_H      = 24;
    var HDR_H      = 46;
    var HDR_R1_Y   = 15;
    var HDR_DIV_Y  = 22;
    var HDR_R2_Y   = 37;
    var INDENT     = 16;
    var TRI_W      = 8;
    var TRI_OFFSET = 12;

    var ROOT_COLORS = ['#4c8bf5','#e05a5a','#34a853','#fbbc04','#9c27b0','#00897b','#f57c00','#546e7a'];

    function xmlEsc(s) {
        return String(s || '').replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;').replace(/"/g,'&quot;');
    }

    function fmtDate(d) {
        return d.getFullYear() + '-' + pad2(d.getMonth() + 1) + '-' + pad2(d.getDate());
    }

    function assignColors(nodes, idx) {
        nodes.forEach(function (n, i) {
            n.colorIdx = (idx !== undefined) ? idx : i;
            if (n.children.length) assignColors(n.children, n.colorIdx);
        });
    }

    function triPoints(lx, cy, collapsed) {
        if (collapsed)
            return lx + ',' + (cy - 4) + ' ' + (lx + TRI_W) + ',' + cy + ' ' + lx + ',' + (cy + 4);
        return lx + ',' + (cy - 3) + ' ' + (lx + TRI_W) + ',' + (cy - 3) + ' ' + (lx + TRI_W / 2) + ',' + (cy + 4);
    }

    function infoStr(node) {
        return fmtDate(node.start) + ' ~ ' + fmtDate(node.end) + '  ' + node.est + 'd';
    }

    // ── SVG 빌더: Corner (좌상) ───────────────────────────────────────────────
    var CORNER_SPLIT = 155; // Date + Days 컬럼 합계 너비 (오른쪽)

    function buildCornerSvg() {
        var nameColW = LABEL_W - CORNER_SPLIT;
        var daysX    = LABEL_W - DAYS_COL_W;
        var o = [];
        o.push('<svg class="gantt-svg" width="' + LABEL_W + '" height="' + HDR_H + '" xmlns="http://www.w3.org/2000/svg">');
        o.push('<rect width="100%" height="100%" class="gantt-corner-hdr-bg"/>');
        // 세로 구분선: Task | Date | Days
        o.push('<line x1="' + nameColW + '" y1="0" x2="' + nameColW + '" y2="' + HDR_H + '" class="gantt-hdr-divider"/>');
        o.push('<line x1="' + daysX    + '" y1="0" x2="' + daysX    + '" y2="' + HDR_H + '" class="gantt-hdr-divider"/>');
        // 가로 구분선
        o.push('<line x1="0" y1="' + HDR_DIV_Y + '" x2="' + LABEL_W + '" y2="' + HDR_DIV_Y + '" class="gantt-hdr-divider"/>');
        // 컬럼 헤더
        o.push('<text x="8" y="' + HDR_R2_Y + '" text-anchor="start" class="gantt-corner-hdr-text">Task</text>');
        o.push('<text x="' + ((nameColW + daysX) / 2) + '" y="' + HDR_R2_Y + '" text-anchor="middle" class="gantt-corner-hdr-text">Period</text>');
        o.push('<text x="' + (daysX + DAYS_COL_W / 2) + '" y="' + HDR_R2_Y + '" text-anchor="middle" class="gantt-corner-hdr-text">Est</text>');
        o.push('</svg>');
        return o.join('');
    }

    // ── SVG 빌더: Header (우상) ───────────────────────────────────────────────
    function buildHeaderSvg(minDate, maxDate, totalDays, dayW) {
        var svgW = totalDays * dayW;
        var showAllDays = dayW >= 15;
        var today = new Date(); today.setHours(0, 0, 0, 0);
        var todayOff = diffDays(minDate, today);

        var o = [];
        o.push('<svg class="gantt-svg gantt-header-svg" width="' + svgW + '" height="' + HDR_H + '" xmlns="http://www.w3.org/2000/svg">');
        o.push('<rect width="100%" height="100%" class="gantt-header-bg"/>');

        var cur = new Date(minDate), prevMonth = -1;
        while (cur <= maxDate) {
            var off = diffDays(minDate, cur);
            var x   = off * dayW;
            var dow = cur.getDay();
            var mo  = cur.getMonth();

            if (isWeekend(cur))
                o.push('<rect x="' + x + '" y="0" width="' + dayW + '" height="' + HDR_H + '" class="gantt-weekend"/>');

            if (mo !== prevMonth) {
                if (prevMonth !== -1)
                    o.push('<line x1="' + x + '" y1="0" x2="' + x + '" y2="' + HDR_H + '" class="gantt-grid-month"/>');
                o.push('<text x="' + (x + 4) + '" y="' + HDR_R1_Y + '" class="gantt-hdr-month">' +
                    cur.getFullYear() + '-' + pad2(mo + 1) + '</text>');
                prevMonth = mo;
            }
            if (dow === 1)
                o.push('<line x1="' + x + '" y1="' + HDR_DIV_Y + '" x2="' + x + '" y2="' + HDR_H + '" class="gantt-grid-week"/>');
            if (showAllDays || dow === 1) {
                var cx = x + Math.round(dayW / 2);
                o.push('<text x="' + cx + '" y="' + HDR_R2_Y + '" text-anchor="middle" class="gantt-hdr-day">' + pad2(cur.getDate()) + '</text>');
            }
            cur.setDate(cur.getDate() + 1);
        }

        o.push('<line x1="0" y1="' + HDR_DIV_Y + '" x2="' + svgW + '" y2="' + HDR_DIV_Y + '" class="gantt-hdr-divider"/>');

        // 오늘 표시: 행2 배경 하이라이트
        if (todayOff >= 0 && todayOff <= totalDays) {
            var tx = todayOff * dayW;
            o.push('<rect x="' + tx + '" y="' + HDR_DIV_Y + '" width="' + dayW + '" height="' + (HDR_H - HDR_DIV_Y) + '" class="gantt-today-hdr"/>');
            var tcx = tx + Math.round(dayW / 2);
            o.push('<text x="' + tcx + '" y="' + HDR_R2_Y + '" text-anchor="middle" class="gantt-today-hdr-label">' + pad2(today.getDate()) + '</text>');
        }

        o.push('</svg>');
        return o.join('');
    }

    // ── SVG 빌더: Labels (좌하) ───────────────────────────────────────────────
    function buildLabelsSvg(visible, collapsedIds, bodyH) {
        var clipId = 'gc' + (Math.random() * 1e9 | 0);
        var o = [];
        o.push('<svg class="gantt-svg gantt-labels-svg" width="' + LABEL_W + '" height="' + bodyH + '" xmlns="http://www.w3.org/2000/svg" overflow="hidden">');
        o.push('<defs><clipPath id="' + clipId + '"><rect width="' + LABEL_W + '" height="' + bodyH + '"/></clipPath></defs>');
        o.push('<rect width="100%" height="100%" class="gantt-label-bg"/>');

        visible.forEach(function (node, i) {
            var y = i * ROW_H;
            o.push('<rect x="0" y="' + y + '" width="' + LABEL_W + '" height="' + ROW_H +
                '" class="gantt-row-' + (i % 2 === 0 ? 'even' : 'odd') + '"/>');
        });

        // 컬럼 구분선
        var nameColW = LABEL_W - CORNER_SPLIT;
        var daysColX = LABEL_W - DAYS_COL_W;
        o.push('<line x1="' + nameColW + '" y1="0" x2="' + nameColW + '" y2="' + bodyH + '" class="gantt-col-divider"/>');
        o.push('<line x1="' + daysColX + '" y1="0" x2="' + daysColX + '" y2="' + bodyH + '" class="gantt-col-divider"/>');

        o.push('<g clip-path="url(#' + clipId + ')">');
        visible.forEach(function (node, i) {
            var y       = i * ROW_H;
            var color   = ROOT_COLORS[node.colorIdx % ROOT_COLORS.length];
            var hasChld = node.children.length > 0;
            var cy      = y + Math.round(ROW_H / 2);
            var ly      = y + ROW_H / 2 + 4;
            var lx      = 8 + node.depth * INDENT;

            o.push('<rect x="0" y="' + y + '" width="3" height="' + ROW_H + '" fill="' + color + '"/>');

            var daysX = LABEL_W - DAYS_COL_W;
            var info =
                '<text x="' + (daysX - 4) + '" y="' + ly + '" text-anchor="end" class="gantt-label-date">' +
                    fmtDate(node.start) + ' ~ ' + fmtDate(node.end) +
                '</text>' +
                '<text x="' + (LABEL_W - 4) + '" y="' + ly + '" text-anchor="end" class="gantt-label-days">' +
                    node.est + 'd' +
                '</text>';

            if (hasChld) {
                var collapsed = !!collapsedIds[node.id];
                o.push('<g class="gantt-toggle" data-id="' + node.id + '">');
                o.push('<rect x="3" y="' + y + '" width="' + (LABEL_W - 3) + '" height="' + ROW_H + '" fill="rgba(0,0,0,0.001)"/>');
                o.push('<polygon points="' + triPoints(lx, cy, collapsed) + '" class="gantt-tri"/>');
                o.push('<text x="' + (lx + TRI_OFFSET) + '" y="' + ly + '" class="gantt-label">' + xmlEsc(node.name) + '</text>');
                o.push(info);
                o.push('</g>');
            } else {
                o.push('<rect x="' + (lx + 2) + '" y="' + (cy - 2) + '" width="4" height="4" rx="1" class="gantt-leaf-dot"/>');
                o.push('<text x="' + (lx + TRI_OFFSET) + '" y="' + ly + '" class="gantt-label">' + xmlEsc(node.name) + '</text>');
                o.push(info);
            }
        });
        o.push('</g>');
        o.push('</svg>');
        return o.join('');
    }

    // ── 의존성 화살표 ─────────────────────────────────────────────────────────
    function buildDepArrows(visible, minDate, dayW, labelMap) {
        var rowOf = {};
        visible.forEach(function (n, i) { rowOf[n.id] = i; });

        var o = [];
        visible.forEach(function (node, i) {
            if (!node.afterRef) return;
            var refNode = labelMap[node.afterRef];
            if (!refNode || rowOf[refNode.id] === undefined) return;

            var pi = rowOf[refNode.id];
            var color = ROOT_COLORS[refNode.colorIdx % ROOT_COLORS.length];

            // 전임 바 오른쪽 끝 → 현재 바 왼쪽 끝
            var px  = (diffDays(minDate, refNode.end) + 1) * dayW;
            var py  = pi * ROW_H + Math.round(ROW_H / 2);
            var sx  = diffDays(minDate, node.start) * dayW;
            var sy  = i  * ROW_H + Math.round(ROW_H / 2);
            o.push('<path d="M' + px + ' ' + py +
                   ' V' + sy + ' H' + sx +
                   '" stroke="' + color + '" class="gantt-dep-line"/>');
            o.push('<polygon points="' +
                   (sx - 5) + ' ' + (sy - 3) + ' ' +
                   sx + ' ' + sy + ' ' +
                   (sx - 5) + ' ' + (sy + 3) +
                   '" fill="' + color + '" class="gantt-dep-arrow"/>');
        });
        return o.join('');
    }

    // ── SVG 빌더: Body (우하) ─────────────────────────────────────────────────
    function buildBodySvg(visible, minDate, maxDate, totalDays, dayW, bodyH, labelMap) {
        var svgW = totalDays * dayW;
        var today = new Date(); today.setHours(0, 0, 0, 0);
        var todayOff = diffDays(minDate, today);

        var o = [];
        o.push('<svg class="gantt-svg gantt-body-svg" width="' + svgW + '" height="' + bodyH + '" xmlns="http://www.w3.org/2000/svg">');
        o.push('<rect width="100%" height="100%" class="gantt-bg"/>');

        // 행 배경 교대색
        visible.forEach(function (node, i) {
            var y = i * ROW_H;
            o.push('<rect x="0" y="' + y + '" width="' + svgW + '" height="' + ROW_H +
                '" class="gantt-row-' + (i % 2 === 0 ? 'even' : 'odd') + '"/>');
        });

        // 그리드 (주말 + 주/월 선)
        var cur = new Date(minDate), prevMonth = -1;
        while (cur <= maxDate) {
            var off = diffDays(minDate, cur);
            var x   = off * dayW;
            var dow = cur.getDay();
            var mo  = cur.getMonth();

            if (isWeekend(cur))
                o.push('<rect x="' + x + '" y="0" width="' + dayW + '" height="' + bodyH + '" class="gantt-weekend"/>');
            if (dow === 1)
                o.push('<line x1="' + x + '" y1="0" x2="' + x + '" y2="' + bodyH + '" class="gantt-grid-week"/>');
            if (mo !== prevMonth) {
                if (prevMonth !== -1)
                    o.push('<line x1="' + x + '" y1="0" x2="' + x + '" y2="' + bodyH + '" class="gantt-grid-month"/>');
                prevMonth = mo;
            }
            cur.setDate(cur.getDate() + 1);
        }

        // 바
        visible.forEach(function (node, i) {
            var y      = i * ROW_H;
            var isLeaf = node.children.length === 0;
            var color  = ROOT_COLORS[node.colorIdx % ROOT_COLORS.length];
            var bx     = diffDays(minDate, node.start) * dayW;
            var bw     = Math.max((diffDays(node.start, node.end) + 1) * dayW - 2, dayW - 2);

            var labelY    = y + Math.round(ROW_H / 2) + 4;
            var startLabel = fmtDate(node.start);
            o.push('<text x="' + (bx - 4) + '" y="' + labelY + '" class="gantt-bar-start-label">' + startLabel + '</text>');
            o.push('<text x="' + (bx + bw + 4) + '" y="' + labelY + '" class="gantt-bar-outer-label">' +
                xmlEsc(node.name) + ' ' + node.est + 'd</text>');
            if (isLeaf) {
                var by = y + 5, bh = ROW_H - 10;
                o.push('<rect x="' + bx + '" y="' + by + '" width="' + bw + '" height="' + bh +
                    '" rx="3" fill="' + color + '" class="gantt-bar-leaf" opacity="0.85"/>');
                if (bw > 40)
                    o.push('<text x="' + (bx + bw / 2) + '" y="' + (by + bh / 2 + 4) + '" class="gantt-bar-label">' + node.est + 'd</text>');
            } else {
                var mby = y + ROW_H / 2 - 3, mbh = 6;
                o.push('<rect x="' + bx + '" y="' + mby + '" width="' + bw + '" height="' + mbh +
                    '" rx="2" fill="' + color + '" class="gantt-bar-parent" opacity="0.45"/>');
                o.push('<rect x="' + bx + '" y="' + (y + 6) + '" width="3" height="' + (ROW_H - 12) + '" fill="' + color + '" opacity="0.7"/>');
                o.push('<rect x="' + (bx + bw - 3) + '" y="' + (y + 6) + '" width="3" height="' + (ROW_H - 12) + '" fill="' + color + '" opacity="0.7"/>');
            }
        });

        // 의존성 화살표
        if (labelMap) o.push(buildDepArrows(visible, minDate, dayW, labelMap));

        // 오늘 선
        if (todayOff >= 0 && todayOff <= totalDays) {
            var tx = todayOff * dayW + Math.round(dayW / 2);
            o.push('<line x1="' + tx + '" y1="0" x2="' + tx + '" y2="' + bodyH + '" class="gantt-today-line"/>');
        }

        o.push('</svg>');
        return o.join('');
    }

    // ── 전체 레이아웃 조립 ────────────────────────────────────────────────────
    function buildAll(rootNodes, collapsedIds, dayW, defaultEst) {
        dayW = dayW || DAY_W_DEF;
        if (!rootNodes.length) return { html: '<p class="gantt-empty">데이터가 없습니다.</p>', totalDays: 0 };

        assignColors(rootNodes, undefined);
        var labelMap = {};
        collectLabels(rootNodes, labelMap);
        calcDates(rootNodes, null, null, labelMap, defaultEst);

        var visible = [];
        flatten(rootNodes, visible, collapsedIds);

        var minDate = visible.reduce(function (m, n) { return n.start < m ? n.start : m; }, visible[0].start);
        var maxDate = visible.reduce(function (m, n) { return n.end   > m ? n.end   : m; }, visible[0].end);
        minDate = new Date(minDate); minDate.setDate(minDate.getDate() - 7);
        maxDate = new Date(maxDate); maxDate.setDate(maxDate.getDate() + 7);

        var totalDays = diffDays(minDate, maxDate);
        var bodyH     = visible.length * ROW_H + 4;

        var html = [
            '<div class="gantt-layout">',
            '<div class="gantt-corner-cell">' + buildCornerSvg() + '</div>',
            '<div class="gantt-hdr-cell"><div class="gantt-hdr-inner">' + buildHeaderSvg(minDate, maxDate, totalDays, dayW) + '</div></div>',
            '<div class="gantt-lbl-cell"><div class="gantt-lbl-inner">' + buildLabelsSvg(visible, collapsedIds, bodyH) + '</div></div>',
            '<div class="gantt-body-cell"><div class="gantt-body-inner">' + buildBodySvg(visible, minDate, maxDate, totalDays, dayW, bodyH, labelMap) + '</div></div>',
            '</div>'
        ].join('\n');

        return { html: html, totalDays: totalDays };
    }

    // ── 인스턴스 관리 ─────────────────────────────────────────────────────────
    function initInstance(el) {
        if (el.getAttribute('data-gantt-init')) return;
        el.setAttribute('data-gantt-init', '1');

        var preEl     = el.querySelector('.gantt-source');
        var container = el.querySelector('.gantt-container');
        if (!preEl || !container) return;

        var raw = preEl.textContent || '';
        var defaultEst = parseDefaultEst(el.getAttribute('data-default-est'));
        var rootNodes = parseGantt(raw);
        if (!rootNodes.length) return;

        var collapsedIds  = {};
        var currentDayW   = DAY_W_DEF;
        var lastTotalDays = 0;

        // 툴바
        var toolbar = document.createElement('div');
        toolbar.className = 'gantt-toolbar';
        toolbar.innerHTML = [
            '<div class="gantt-btn-group">',
            '<button class="gantt-btn gantt-zoom-out" title="축소">−</button>',
            '<button class="gantt-btn gantt-zoom-100" title="기본 배율 (1일=1칸)">100%</button>',
            '<button class="gantt-btn gantt-zoom-in"  title="확대">+</button>',
            '</div>',
            '<button class="gantt-btn gantt-fit" title="화면 폭에 맞춤">맞춤</button>',
        ].join('');
        el.insertBefore(toolbar, container);

        function render() {
            var result = buildAll(rootNodes, collapsedIds, currentDayW, defaultEst);
            lastTotalDays = result.totalDays;
            container.innerHTML = result.html;

            var hdrInner  = container.querySelector('.gantt-hdr-inner');
            var lblInner  = container.querySelector('.gantt-lbl-inner');
            var bodyCell  = container.querySelector('.gantt-body-cell');
            var lblSvg    = container.querySelector('.gantt-labels-svg');

            if (bodyCell) {
                bodyCell.addEventListener('scroll', function () {
                    if (hdrInner) hdrInner.style.transform = 'translateX(-' + bodyCell.scrollLeft + 'px)';
                    if (lblInner) lblInner.style.transform = 'translateY(-' + bodyCell.scrollTop  + 'px)';
                });
            }

            if (lblSvg) {
                lblSvg.addEventListener('click', function (e) {
                    var t = e.target.closest ? e.target.closest('.gantt-toggle') : null;
                    if (!t) { t = (e.target.classList && e.target.classList.contains('gantt-toggle')) ? e.target : null; }
                    if (!t) return;
                    var id = t.getAttribute('data-id');
                    if (!id) return;
                    collapsedIds[id] = !collapsedIds[id];
                    render();
                });
            }
        }

        toolbar.querySelector('.gantt-zoom-in').addEventListener('click', function () {
            currentDayW = Math.min(DAY_W_MAX, currentDayW + DAY_W_STEP);
            render();
        });
        toolbar.querySelector('.gantt-zoom-out').addEventListener('click', function () {
            currentDayW = Math.max(DAY_W_MIN, currentDayW - DAY_W_STEP);
            render();
        });
        toolbar.querySelector('.gantt-zoom-100').addEventListener('click', function () {
            currentDayW = DAY_W_DEF;
            render();
        });
        toolbar.querySelector('.gantt-fit').addEventListener('click', function () {
            if (lastTotalDays > 0) {
                var bodyCell = container.querySelector('.gantt-body-cell');
                var cw = bodyCell ? bodyCell.clientWidth : (container.clientWidth - LABEL_W);
                currentDayW = Math.max(DAY_W_MIN, Math.floor(cw / lastTotalDays));
                render();
            }
        });

        render();
    }

    // ── 공개 API ──────────────────────────────────────────────────────────────
    function initAll(root) {
        var els = (root || document).querySelectorAll('.InterpreterGantt');
        for (var i = 0; i < els.length; i++) initInstance(els[i]);
    }

    if (typeof window !== 'undefined') {
        window.AhaWiki = window.AhaWiki || {};
        window.AhaWiki.Gantt = { initAll: initAll };
    }

    document.addEventListener('DOMContentLoaded', function () { initAll(document); });

})();
