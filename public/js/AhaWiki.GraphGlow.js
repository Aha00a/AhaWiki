/*
 * The nebula glow under the wiki's canvas graphs ([[PageMap]], [[AdjacentPages]], #!Graph).
 *
 * Nodes that are linked to each other often form a cluster; each cluster gets a soft radial gradient
 * laid under its nodes -- no edge, centre to nothing -- and its colour goes on the nodes' rings. The
 * glow breathes: its brightness swells and ebbs about every seven seconds, each cluster out of step
 * with the rest.
 *
 * Ported from the same effect in another project's matchup graph. The idea carries over; the code does
 * not, because both graphs here draw in screen coordinates on every simulation tick, and the other
 * one drew in world coordinates on its own frames. The wiki page Dev GraphGlow has the reasons behind
 * the numbers below. Short version of the ones that were learned the hard way:
 *
 *  - Clusters are found only from 12 nodes up, and a cluster under 4 gets no glow.
 *  - They are recomputed at most every 2.5 s, and a node's own cluster gets a 20% bonus. Without both,
 *    a node on a border changes cluster from one pass to the next and its colour flickers.
 *  - A node with no cluster yet still has a colour. The palette is indexed with a modulo that stays
 *    positive, because JavaScript's `%` of a negative number is negative, and an undefined colour
 *    throws in the middle of draw() and leaves the frame half painted.
 *  - It keeps asking for frames while it breathes, because the simulation may be stopped. It stops
 *    when the canvas is off screen or the tab is hidden.
 *  - It does not look at prefers-reduced-motion. Nothing here moves -- only the brightness changes,
 *    which is not what that setting is for -- and the nodes' movement belongs to the simulation.
 */
(function(window) {
    if(window.AhaWikiGraphGlow) return;

    // Six hues that read on both the light and the dark theme. Amber is left out: it is the root
    // node's ring colour, and a cluster the colour of the root would look like the root.
    var GROUP_RGB = [
        [92, 107, 230],
        [20, 160, 150],
        [50, 130, 240],
        [70, 170, 90],
        [210, 80, 160],
        [120, 135, 165]
    ];

    var MIN_NODES = 12;
    var MIN_GROUP = 4;
    var REGROUP_MS = 2500;
    var PROPAGATIONS = 8;
    var INERTIA = 1.2;
    var CENTER_ALPHA = 0.13;
    var MID_ALPHA = 0.05;
    var BREATH_DEPTH = 0.32;
    var BREATH_RATE = 0.9;
    var BREATH_PHASE = 1.9;
    var SPREAD = 1.25;
    // The other graph's links were 250 apart and its glow reached 70 past the spread of the nodes; here
    // they are 80 apart.
    var PAD = 55;
    // The same alpha shows less on a dark page than on a light one, so the dark theme gets more of it.
    // Measured by eye on both: at 1 the clouds on the dark theme were barely there.
    var DARK_BOOST = 1.6;
    var RING_ALPHA = 0.85;
    var RING_EASE = 0.1;
    var FRAME_MS = 33;

    function colorOf(group) {
        var n = GROUP_RGB.length;
        return GROUP_RGB[((((group | 0) % n) + n) % n)];
    }

    function groupOf(node) {
        return node.glowGroup >= 0 ? node.glowGroup : -1;
    }

    function create(options) {
        var canvas = options.canvas;
        var redraw = options.redraw;
        var groupSeq = 1;
        var groupAt = -Infinity;
        var signature = '';
        var nodeCount = 0;
        var onScreen = true;
        var breathing = false;
        var fading = false;
        var lastPaintAt = 0;
        var loop = 0;

        function now() {
            return performance.now();
        }

        function isDark() {
            return !!(document.documentElement && document.documentElement.classList.contains('theme-ahawiki-dark'));
        }

        // Label propagation over the links. The earlier result is the seed: node.glowGroup stays on
        // the node, so a graph that grows keeps its cluster numbers, and with them its colours.
        function regroup(nodes, links, at) {
            var sig = nodes.length + ':' + links.length;
            if(sig === signature || nodes.length < MIN_NODES || at - groupAt < REGROUP_MS) return;
            signature = sig;
            groupAt = at;

            var byId = new Map();
            var adjacent = new Map();
            nodes.forEach(function(n) {
                byId.set(n.id, n);
                adjacent.set(n, []);
                if(!(n.glowGroup >= 0)) n.glowGroup = groupSeq++;
            });
            function end(e) {
                return (e !== null && typeof e === 'object') ? e : byId.get(e);
            }
            links.forEach(function(l) {
                var a = end(l.source);
                var b = end(l.target);
                if(!a || !b || !adjacent.has(a) || !adjacent.has(b)) return;
                adjacent.get(a).push(b);
                adjacent.get(b).push(a);
            });

            for(var pass = 0; pass < PROPAGATIONS; pass++) {
                var changed = 0;
                nodes.forEach(function(n) {
                    var votes = new Map();
                    adjacent.get(n).forEach(function(o) {
                        votes.set(o.glowGroup, (votes.get(o.glowGroup) || 0) + 1);
                    });
                    votes.set(n.glowGroup, (votes.get(n.glowGroup) || 0) * INERTIA + 0.01);
                    var best = n.glowGroup;
                    var most = -1;
                    votes.forEach(function(w, g) {
                        if(w > most) { best = g; most = w; }
                    });
                    if(best !== n.glowGroup) { n.glowGroup = best; changed++; }
                });
                if(!changed) break;
            }
        }

        function ensureLoop() {
            if(loop) return;
            loop = requestAnimationFrame(frame);
        }

        function frame() {
            loop = 0;
            if(!(breathing || fading) || !onScreen || document.hidden) return;
            if(now() - lastPaintAt >= FRAME_MS) redraw();
            ensureLoop();
        }

        // Lay the glow under the nodes. Call it after the background and before the links.
        //   nodes -- those being drawn, each with x and y in world coordinates
        //   links -- {source, target}, ends either the node objects or their ids; those whose ends
        //            are not both among `nodes` are ignored
        //   view  -- {toScreen(x, y) -> {x, y}, scale}
        function paint(ctx, nodes, links, view) {
            var at = now();
            lastPaintAt = at;
            breathing = false;
            fading = false;
            nodeCount = nodes.length;
            if(nodeCount < MIN_NODES) return;

            regroup(nodes, links, at);

            var boost = isDark() ? DARK_BOOST : 1;
            var sums = new Map();
            nodes.forEach(function(n) {
                var g = groupOf(n);
                if(g < 0) return;
                var q = sums.get(g);
                if(!q) { q = { x: 0, y: 0, n: 0 }; sums.set(g, q); }
                q.x += n.x || 0;
                q.y += n.y || 0;
                q.n++;
            });
            sums.forEach(function(q, g) {
                if(q.n < MIN_GROUP) return;
                var cx = q.x / q.n;
                var cy = q.y / q.n;
                var spread = 0;
                nodes.forEach(function(n) {
                    if(groupOf(n) === g) spread += Math.pow((n.x || 0) - cx, 2) + Math.pow((n.y || 0) - cy, 2);
                });
                var radius = (Math.sqrt(spread / q.n) * SPREAD + PAD) * view.scale;
                var c = view.toScreen(cx, cy);
                var rgb = colorOf(g).join(', ');
                var breath = 1 + BREATH_DEPTH * Math.sin(at / 1000 * BREATH_RATE + g * BREATH_PHASE);
                var gradient = ctx.createRadialGradient(c.x, c.y, 0, c.x, c.y, radius);
                gradient.addColorStop(0, 'rgba(' + rgb + ', ' + (CENTER_ALPHA * breath * boost) + ')');
                gradient.addColorStop(0.6, 'rgba(' + rgb + ', ' + (MID_ALPHA * breath * boost) + ')');
                gradient.addColorStop(1, 'rgba(' + rgb + ', 0)');
                ctx.globalAlpha = 1;
                ctx.fillStyle = gradient;
                ctx.beginPath();
                ctx.arc(c.x, c.y, radius, 0, Math.PI * 2);
                ctx.fill();
                breathing = true;
            });
            ensureLoop();
        }

        // The colour of a node's ring, or null while the graph is too small for clusters. It moves a tenth
        // of the way to the node's cluster colour on each call -- one call per node per frame -- so a
        // node that changes cluster drifts to the new colour instead of jumping.
        function ringColor(node) {
            if(nodeCount < MIN_NODES) return null;
            var target = colorOf(groupOf(node));
            var shown = node.glowRgb || (node.glowRgb = target.slice());
            for(var i = 0; i < 3; i++) {
                var d = target[i] - shown[i];
                if(Math.abs(d) > 1) { shown[i] += d * RING_EASE; fading = true; } else shown[i] = target[i];
            }
            return 'rgba(' + shown.map(Math.round).join(', ') + ', ' + RING_ALPHA + ')';
        }

        // A breathing glow nobody can see is a canvas redrawn thirty times a second for nothing.
        if(typeof IntersectionObserver === 'function' && canvas) {
            new IntersectionObserver(function(entries) {
                onScreen = entries[entries.length - 1].isIntersecting;
                if(onScreen) redraw();
            }).observe(canvas);
        }
        document.addEventListener('visibilitychange', function() {
            if(!document.hidden) redraw();
        });

        return { paint: paint, ringColor: ringColor };
    }

    window.AhaWikiGraphGlow = { create: create };
})(window);
