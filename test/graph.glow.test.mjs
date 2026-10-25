// The nebula glow under the canvas graphs. These run the real file against a recording canvas and a
// clock the test moves by hand -- nothing here can be seen by looking, so what was learned the hard
// way in the project this was ported from is asserted instead: the cluster colour of a node with no
// cluster yet, and clusters that do not reshuffle every frame. The reasons are on the wiki page
// Dev GraphGlow.
import test from 'node:test';
import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';

const source = fs.readFileSync('public/js/AhaWiki.GraphGlow.js', 'utf8').replace(/^﻿/, '');

function load() {
    const clock = { t: 10000 };
    const frames = [];
    const sandbox = {
        window: {},
        document: { hidden: false, addEventListener() {} },
        performance: { now: () => clock.t },
        requestAnimationFrame: fn => { frames.push(fn); return frames.length; },
    };
    vm.createContext(sandbox);
    vm.runInContext(source, sandbox);
    return { api: sandbox.window.AhaWikiGraphGlow, clock, frames, document: sandbox.document };
}

function recordingCtx() {
    const gradients = [];
    return {
        gradients,
        globalAlpha: 1,
        createRadialGradient(x, y, r0, x1, y1, r1) {
            const g = { at: [x, y], radius: r1, stops: [], addColorStop(o, c) { g.stops.push([o, c]); } };
            gradients.push(g);
            return g;
        },
        beginPath() {}, arc() {}, fill() {},
    };
}

const view = { toScreen: (x, y) => ({ x, y }), scale: 1 };

// Cluster `size` nodes that all link to each other, placed around (cx, 0).
function clique(prefix, size, cx) {
    const nodes = Array.from({ length: size }, (_, i) => ({ id: `${prefix}${i}`, x: cx + i * 3, y: i * 2 }));
    const links = [];
    for (let i = 0; i < size; i++) for (let j = i + 1; j < size; j++) links.push({ source: nodes[i].id, target: nodes[j].id });
    return { nodes, links };
}

function graphOf(...parts) {
    return { nodes: parts.flatMap(p => p.nodes), links: parts.flatMap(p => p.links) };
}

const alphaOf = stop => Number(/, ([\d.]+)\)$/.exec(stop)[1]);

test('under 12 nodes there is no glow, no ring colour and no frame loop', () => {
    const { api, frames } = load();
    const glow = api.create({ canvas: null, redraw() {} });
    const ctx = recordingCtx();
    const g = clique('a', 11, 0);
    glow.paint(ctx, g.nodes, g.links, view);
    assert.equal(ctx.gradients.length, 0);
    assert.equal(glow.ringColor(g.nodes[0]), null);
    assert.equal(frames.length, 0, 'nothing is asked for until there is something to animate');
});

test('each cluster of 4 or more gets one glow; a smaller one gets none', () => {
    const { api } = load();
    const glow = api.create({ canvas: null, redraw() {} });
    const ctx = recordingCtx();
    const g = graphOf(clique('a', 6, 0), clique('b', 4, 500), clique('c', 2, 1000));
    glow.paint(ctx, g.nodes, g.links, view);
    assert.equal(ctx.gradients.length, 2);
});

test('the glow has no edge: centre, 60% and zero at the rim', () => {
    const { api } = load();
    const glow = api.create({ canvas: null, redraw() {} });
    const ctx = recordingCtx();
    const g = graphOf(clique('a', 6, 0), clique('c', 6, 800));
    glow.paint(ctx, g.nodes, g.links, view);
    const stops = ctx.gradients[0].stops;
    assert.deepEqual(stops.map(s => s[0]), [0, 0.6, 1]);
    assert.equal(alphaOf(stops[2][1]), 0);
    assert.ok(alphaOf(stops[0][1]) > alphaOf(stops[1][1]) && alphaOf(stops[1][1]) > 0);
});

test('the brightness stays within 32% of 0.13, and clusters are out of step', () => {
    const { api, clock } = load();
    const glow = api.create({ canvas: null, redraw() {} });
    const g = graphOf(clique('a', 6, 0), clique('b', 6, 800));
    const seen = [];
    for (let i = 0; i < 80; i++) {
        clock.t = 10000 + i * 100;
        const ctx = recordingCtx();
        glow.paint(ctx, g.nodes, g.links, view);
        seen.push(ctx.gradients.map(d => alphaOf(d.stops[0][1])));
    }
    const all = seen.flat();
    assert.ok(Math.min(...all) >= 0.13 * 0.68 - 1e-9 && Math.max(...all) <= 0.13 * 1.32 + 1e-9);
    assert.ok(Math.max(...all) - Math.min(...all) > 0.03, 'it actually swells and ebbs');
    assert.ok(seen.some(([a, b]) => Math.abs(a - b) > 0.01), 'two clusters are not in the same phase');
});

test('the dark theme gets a brighter glow than the light one, from the same clock', () => {
    const peak = dark => {
        const { api, document } = load();
        document.documentElement = { classList: { contains: name => dark && name === 'theme-ahawiki-dark' } };
        const glow = api.create({ canvas: null, redraw() {} });
        const ctx = recordingCtx();
        const g = graphOf(clique('a', 6, 0), clique('b', 6, 800));
        glow.paint(ctx, g.nodes, g.links, view);
        return alphaOf(ctx.gradients[0].stops[0][1]);
    };
    assert.ok(peak(true) > peak(false) * 1.5);
});

test('a node with no cluster yet has a colour, and painting does not throw', () => {
    const { api, clock } = load();
    const glow = api.create({ canvas: null, redraw() {} });
    const g = graphOf(clique('a', 6, 0), clique('b', 6, 800));
    glow.paint(recordingCtx(), g.nodes, g.links, view);
    // Added after the clusters were found, within the 2.5 s before they are found again.
    const late = { id: 'late', x: 5, y: 5 };
    clock.t += 500;
    assert.doesNotThrow(() => glow.paint(recordingCtx(), [...g.nodes, late], g.links, view));
    assert.match(glow.ringColor(late), /^rgba\(\d+, \d+, \d+, 0\.85\)$/);
    // The same through ringColor alone, for a node that was never painted at all.
    assert.match(glow.ringColor({ id: 'never' }), /^rgba\(\d+, \d+, \d+, 0\.85\)$/);
});

test('clusters are recomputed at most every 2.5 s', () => {
    const { api, clock } = load();
    const glow = api.create({ canvas: null, redraw() {} });
    // 6 + 4 + 4 nodes. Every node of the four-clique b is then linked to all six of a: each has 3
    // votes for its own cluster (3.6 with the bonus) and 6 for a's, so b should be absorbed -- but not
    // before the recompute is due. (Two cliques of six joined would not merge at all: 6.01 against 6.)
    const three = graphOf(clique('a', 6, 0), clique('b', 4, 800), clique('c', 4, 1600));
    glow.paint(recordingCtx(), three.nodes, three.links, view);
    const before = three.nodes.map(n => n.glowGroup);
    assert.notEqual(before[0], before[6], 'separate cliques are separate clusters');

    const bridge = [];
    for (const a of three.nodes.slice(0, 6)) for (const b of three.nodes.slice(6, 10)) bridge.push({ source: a.id, target: b.id });
    const joined = [...three.links, ...bridge];
    clock.t += 1000;
    glow.paint(recordingCtx(), three.nodes, joined, view);
    assert.deepEqual(three.nodes.map(n => n.glowGroup), before, 'only a second has passed');

    clock.t += 1600;
    glow.paint(recordingCtx(), three.nodes, joined, view);
    assert.equal(three.nodes[0].glowGroup, three.nodes[6].glowGroup, 'then b has joined a');
    assert.notEqual(three.nodes[0].glowGroup, three.nodes[10].glowGroup, 'and c, which was not linked, has not');
});

test('a node on the border stays in its cluster when the votes are close', () => {
    const { api, clock } = load();
    const glow = api.create({ canvas: null, redraw() {} });
    const g = graphOf(clique('a', 6, 0), clique('b', 6, 800));
    glow.paint(recordingCtx(), g.nodes, g.links, view);
    const home = g.nodes[0].glowGroup;
    // a0 links to every node of b as well as to its own five: 6 votes each way. Its own cluster gets the 20% bonus.
    const extra = g.nodes.slice(6).map(b => ({ source: 'a0', target: b.id }));
    clock.t += 3000;
    glow.paint(recordingCtx(), g.nodes, [...g.links, ...extra], view);
    assert.equal(g.nodes[0].glowGroup, home);
});

test('a graph that grows keeps the numbers, and so the colours, of the clusters it had', () => {
    const { api, clock } = load();
    const glow = api.create({ canvas: null, redraw() {} });
    const g = graphOf(clique('a', 6, 0), clique('b', 6, 800));
    glow.paint(recordingCtx(), g.nodes, g.links, view);
    const before = g.nodes.map(n => n.glowGroup);

    const added = ['n1', 'n2', 'n3'].map((id, i) => ({ id, x: i, y: 1 }));
    const links = [...g.links, ...added.flatMap(n => g.nodes.slice(0, 4).map(a => ({ source: n.id, target: a.id })))];
    clock.t += 3000;
    glow.paint(recordingCtx(), [...g.nodes, ...added], links, view);
    assert.deepEqual(g.nodes.map(n => n.glowGroup), before);
    assert.ok(added.every(n => n.glowGroup === before[0]), 'the newcomers join the cluster they link to');
});

test('a ring colour moves a tenth of the way per frame, and the loop runs until it arrives', () => {
    const { api, clock, frames } = load();
    const glow = api.create({ canvas: null, redraw() {} });
    const g = graphOf(clique('a', 6, 0), clique('b', 6, 800));
    glow.paint(recordingCtx(), g.nodes, g.links, view);
    const node = g.nodes[0];
    glow.ringColor(node);
    node.glowRgb = [255, 255, 255];                // as if it had just changed cluster
    const first = glow.ringColor(node);
    const second = glow.ringColor(node);
    const channel = s => Number(/rgba\((\d+)/.exec(s)[1]);
    assert.ok(channel(first) < 255 && channel(second) < channel(first), 'drifting, not jumping');
    assert.ok(frames.length > 0);
    for (let i = 0; i < 80; i++) glow.ringColor(node);
    const arrived = glow.ringColor(node);
    assert.equal(arrived, glow.ringColor(node), 'and it settles');
    void clock;
});

test('the frame loop redraws only when nothing has drawn lately, and stops when hidden or off screen', () => {
    const { api, clock, frames, document } = load();
    let redraws = 0;
    const glow = api.create({ canvas: null, redraw() { redraws++; glow.paint(recordingCtx(), g.nodes, g.links, view); } });
    const g = graphOf(clique('a', 6, 0), clique('b', 6, 800));
    glow.paint(recordingCtx(), g.nodes, g.links, view);
    assert.equal(frames.length, 1, 'breathing asks for the next frame');

    // The simulation drew a moment ago: the loop leaves it alone.
    clock.t += 10;
    frames.shift()();
    assert.equal(redraws, 0);

    // Nothing has drawn for a while (the simulation is stopped): the loop draws.
    clock.t += 100;
    frames.shift()();
    assert.equal(redraws, 1);
    assert.ok(frames.length >= 1, 'and keeps going');

    // A hidden tab stops it.
    document.hidden = true;
    clock.t += 100;
    while (frames.length) frames.shift()();
    const stopped = redraws;
    clock.t += 100;
    assert.equal(frames.length, 0);
    assert.equal(redraws, stopped);
});

test('nothing here consults prefers-reduced-motion', () => {
    assert.ok(!/reduced-motion|matchMedia/.test(source.replace(/\/\*[\s\S]*?\*\//, '')), 'the breath is brightness, not motion');
});
