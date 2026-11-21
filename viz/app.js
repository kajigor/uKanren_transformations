/* uKanren specialization visualizer — plain DOM/SVG, no build step.
 * Data contract: viz/SCHEMA.md. Data dir: data/ (default) or ?data=<dir>. */
'use strict';

/* ======================================================================
 * Utilities
 * ==================================================================== */

const SVGNS = 'http://www.w3.org/2000/svg';

/** Append children (strings become text nodes; null/false are skipped). */
function append(el, kids) {
  for (const k of kids.flat(Infinity)) {
    if (k === null || k === undefined || k === false) continue;
    el.appendChild(k instanceof Node ? k : document.createTextNode(String(k)));
  }
  return el;
}

function setAttrs(el, attrs, isSvg) {
  if (!attrs) return el;
  for (const [key, v] of Object.entries(attrs)) {
    if (v === null || v === undefined || v === false) continue;
    if (key.startsWith('on') && typeof v === 'function') el.addEventListener(key.slice(2), v);
    else if (key === 'class') isSvg ? el.setAttribute('class', v) : (el.className = v);
    else if (key === 'style' && typeof v === 'object') Object.assign(el.style, v);
    else if (key === 'text') el.textContent = v;
    else if (!isSvg && (key === 'value' || key === 'checked' || key === 'selected' || key === 'disabled')) el[key] = v;
    else el.setAttribute(key, v === true ? '' : v);
  }
  return el;
}

/** HTML element builder. Never uses innerHTML, so all text is escaped by construction. */
function h(tag, attrs, ...kids) { return append(setAttrs(document.createElement(tag), attrs, false), kids); }
/** SVG element builder. */
function s(tag, attrs, ...kids) { return append(setAttrs(document.createElementNS(SVGNS, tag), attrs, true), kids); }

const fmtInt = n => (n === null || n === undefined || !isFinite(n)) ? '—' : Math.round(n).toLocaleString('en-US');
function fmtCompact(n) {
  if (!isFinite(n)) return '∞';
  const a = Math.abs(n);
  if (a >= 1e6) return (n / 1e6).toFixed(a >= 1e7 ? 0 : 1).replace(/\.0$/, '') + 'M';
  if (a >= 1e3) return (n / 1e3).toFixed(a >= 1e4 ? 0 : 1).replace(/\.0$/, '') + 'k';
  return String(Math.round(n * 100) / 100);
}
function fmtRatioNum(v) {
  if (!isFinite(v)) return '∞';
  if (v >= 100) return Math.round(v).toLocaleString('en-US');
  if (v >= 10) return v.toFixed(1);
  if (v >= 0.1) return v.toFixed(2);
  return v.toPrecision(2);
}
const clamp = (x, lo, hi) => Math.max(lo, Math.min(hi, x));
const str = v => (v === null || v === undefined) ? '' : String(v);

function debounce(fn, ms) { let t; return (...a) => { clearTimeout(t); t = setTimeout(() => fn(...a), ms); }; }

/* ---- Tooltip (single floating div) ---- */
const tipEl = document.getElementById('tip');
/** Attach a hover tooltip. fn() returns {title, rows: [string | {text, mono}]}. */
function tip(el, fn) {
  el.addEventListener('mouseenter', e => showTip(fn(e), e));
  el.addEventListener('mousemove', e => { if (el.dataset.tipLive) showTip(fn(e), e); else moveTip(e); });
  el.addEventListener('mouseleave', hideTip);
  return el;
}
function showTip(content, e) {
  if (!content) { hideTip(); return; }
  tipEl.replaceChildren();
  if (content.title) tipEl.appendChild(h('div', { class: 't-title' }, content.title));
  for (const r of content.rows || []) {
    const o = typeof r === 'string' ? { text: r } : r;
    tipEl.appendChild(h('div', { class: 't-row' + (o.mono ? ' mono' : '') }, o.text));
  }
  tipEl.hidden = false;
  moveTip(e);
}
function moveTip(e) {
  if (tipEl.hidden) return;
  const pad = 14, w = tipEl.offsetWidth, ht = tipEl.offsetHeight;
  let x = e.clientX + pad, y = e.clientY + pad;
  if (x + w > window.innerWidth - 8) x = e.clientX - w - pad;
  if (y + ht > window.innerHeight - 8) y = e.clientY - ht - pad;
  tipEl.style.left = Math.max(4, x) + 'px';
  tipEl.style.top = Math.max(4, y) + 'px';
}
function hideTip() { tipEl.hidden = true; }

/* ======================================================================
 * Data loading
 * ==================================================================== */

const params = new URLSearchParams(location.search);
const DATA_DIR = (params.get('data') || 'data').replace(/[^\w\-./]/g, '').replace(/\.\.+/g, '').replace(/\/+$/, '') + '/';

async function fetchJSON(url) {
  const r = await fetch(url, { cache: 'no-cache' });
  if (!r.ok) throw new Error(`${url}: HTTP ${r.status}`);
  return r.json();
}

const state = {
  index: null,
  indexError: null,
  benchMeta: new Map(),   // name -> index entry
  benches: new Map(),     // name -> Promise<bench>
  loaded: new Map(),      // name -> bench | {error}
  route: { bench: null, tab: 'overview' },
  ui: {},                 // per-bench UI selections (reset on bench change)
  prefs: { ovMode: 'first', ovVariants: true, barLog: null, tlLog: null },
  cleanup: [],            // functions to run before re-rendering the main view (timers etc.)
};

/* View state survives reloads (the dev server reloads the page when a new build is promoted):
 * prefs go to localStorage, the per-bench selections and the scroll position to sessionStorage. */
const PREFS_KEY = 'viz:prefs', SESSION_KEY = 'viz:session';
function saveViewState() {
  try {
    localStorage.setItem(PREFS_KEY, JSON.stringify(state.prefs));
    const ui = { ...state.ui };
    if (ui.gSel) ui.gSel = { id: ui.gSel.id };
    if (ui.tl) ui.tl = Object.fromEntries(Object.entries(ui.tl).map(([k, m]) => [k, [...m]]));
    sessionStorage.setItem(SESSION_KEY, JSON.stringify({ bench: state.route.bench, ui, scroll: mainEl.scrollTop }));
  } catch (e) { /* storage unavailable */ }
}
let pendingScroll = null;
function restoreViewState() {
  try { Object.assign(state.prefs, JSON.parse(localStorage.getItem(PREFS_KEY)) || {}); } catch (e) { /* ignore */ }
  let s = null;
  try { s = JSON.parse(sessionStorage.getItem(SESSION_KEY)); } catch (e) { /* ignore */ }
  if (!s || s.bench !== parseHash().bench) return;
  const ui = s.ui || {};
  if (ui.tl) ui.tl = Object.fromEntries(Object.entries(ui.tl).map(([k, e]) => [k, new Map(e)]));
  state.ui = ui;
  state.route.bench = s.bench;   // so the first onRoute() keeps state.ui
  pendingScroll = s.scroll || 0;
}
function applyPendingScroll() {
  if (pendingScroll === null) return;
  const y = pendingScroll; pendingScroll = null;
  requestAnimationFrame(() => { mainEl.scrollTop = y; });
}
window.addEventListener('pagehide', saveViewState);

function loadBench(name) {
  if (!state.benches.has(name)) {
    const meta = state.benchMeta.get(name);
    const file = meta && meta.file ? meta.file : name + '.json';
    const p = fetchJSON(DATA_DIR + encodeURIComponent(file).replace(/%2F/g, '/'))
      .then(b => { const nb = normalizeBench(b, meta); state.loaded.set(name, nb); return nb; })
      .catch(err => { state.loaded.set(name, { error: String(err.message || err) }); throw err; });
    p.catch(() => {});
    state.benches.set(name, p);
  }
  return state.benches.get(name);
}

const VARIANT_ORDER = ['', '+opt', '+sched', '+opt+sched'];
function splitSpec(name) {
  const i = name.indexOf('+');
  return i < 0 ? [name, ''] : [name.slice(0, i), name.slice(i)];
}
/** Order spec names: original first, then by base (first-seen order), then variant. */
function orderSpecs(names) {
  const uniq = [...new Set(names)];
  const baseOrder = [];
  for (const n of uniq) { const b = splitSpec(n)[0]; if (!baseOrder.includes(b)) baseOrder.push(b); }
  const vr = v => { const i = VARIANT_ORDER.indexOf(v); return i < 0 ? 99 : i; };
  return uniq.sort((a, b) => {
    if (a === 'original') return -1;
    if (b === 'original') return 1;
    const [ba, va] = splitSpec(a), [bb, vb] = splitSpec(b);
    if (ba !== bb) return baseOrder.indexOf(ba) - baseOrder.indexOf(bb);
    return vr(va) - vr(vb) || va.localeCompare(vb);
  });
}

/** Make a bench object safe to use: defaults for missing fields + lookup maps. */
function normalizeBench(b, meta) {
  b = b && typeof b === 'object' ? b : {};
  b.name = str(b.name || (meta && meta.name));
  b.group = str(b.group || (meta && meta.group) || 'other');
  b.source = str(b.source);
  b.runsSource = str(b.runsSource);
  b.specQuery = str(b.specQuery);
  b.specializers = Array.isArray(b.specializers) ? b.specializers.filter(x => x && x.name) : [];
  b.runs = Array.isArray(b.runs) ? b.runs.filter(x => x && x.spec) : [];
  b.queries = Array.isArray(b.queries) ? b.queries.filter(x => x && x.id) : [];
  if (!b.queries.length) {
    const ids = [...new Set(b.runs.map(r => r.query))];
    b.queries = ids.map(id => ({ id, goal: '' }));
  }
  for (const r of b.runs) {
    r.steps = Number(r.steps ?? ((r.unifs || 0) + (r.calls || 0)));
    r.answers = Array.isArray(r.answers) ? r.answers : [];
    r.answerSteps = Array.isArray(r.answerSteps) ? r.answerSteps : [];
    r.profile = Array.isArray(r.profile) ? r.profile : [];
    r.trace = Array.isArray(r.trace) ? r.trace : [];
    r.outcome = str(r.outcome);
    r.check = str(r.check || '-');
  }
  b._spec = new Map(b.specializers.map(x => [x.name, x]));
  b._runs = new Map(b.runs.map(r => [r.spec + '\u0000' + r.query, r]));
  b._specOrder = orderSpecs([...b.specializers.map(x => x.name), ...b.runs.map(r => r.spec)]);
  return b;
}
const getRun = (b, spec, q) => b._runs.get(spec + '\u0000' + q) || null;

/* ---- Run classification ---- */
const isFuel = r => r && r.outcome === 'OutOfFuel';
const isSolved = r => r && (r.outcome === 'Finished' || r.outcome === 'GotEnough');
const isErr = r => r && !isFuel(r) && !isSolved(r);
const badCheck = c => c === 'MISMATCH' || /^unverified/.test(c || '');

/** Ratio of a run's steps vs the original's. kind: exact | upper (orig ran out of fuel) | inf | bothfuel | na */
function ratioOf(run, orig) {
  if (!run || !orig || isErr(run) || isErr(orig)) return { kind: 'na', v: null };
  if (isFuel(run) && isFuel(orig)) return { kind: 'bothfuel', v: null };
  if (isFuel(run)) return { kind: 'inf', v: Infinity };
  const v = orig.steps > 0 ? run.steps / orig.steps : (run.steps > 0 ? Infinity : 1);
  return { kind: isFuel(orig) ? 'upper' : 'exact', v };
}
function ratioText(r) {
  switch (r.kind) {
    case 'exact': return '×' + fmtRatioNum(r.v);
    case 'upper': return '≤×' + fmtRatioNum(r.v);
    case 'inf': return '∞';
    case 'bothfuel': return 'fuel/fuel';
    default: return '—';
  }
}
/** Diverging background for a ratio: blue = fewer steps than original, red = more. */
function ratioBg(v) {
  if (v === null || !isFinite(v) || v <= 0) return '';
  const l = Math.log10(v);
  if (Math.abs(l) < 0.02) return '';
  const a = (0.12 + 0.68 * Math.min(1, Math.abs(l) / 2.5)).toFixed(2);
  return `rgba(var(${l < 0 ? '--div-good' : '--div-bad'}), ${a})`;
}
const ratioInk = v => (v !== null && isFinite(v) && v > 0 && Math.abs(Math.log10(v)) > 1.6) ? '#fff' : '';

/** Best solved non-original run for a query (fewest steps, check not failing). */
function bestRun(b, q, exclude = 'original') {
  let best = null;
  for (const name of b._specOrder) {
    if (name === exclude) continue;
    const r = getRun(b, name, q);
    if (!isSolved(r) || badCheck(r.check)) continue;
    if (!best || r.steps < best.steps) best = r;
  }
  return best;
}

/* ======================================================================
 * Routing & top-level rendering
 * ==================================================================== */

const TABS = [['program', 'Program'], ['spec', 'Specialization'], ['runs', 'Runs & comparison']];

function parseHash() {
  const parts = location.hash.replace(/^#\/?/, '').split('/').filter(Boolean).map(decodeURIComponent);
  if (!parts.length) return { bench: null, tab: 'overview' };
  const tab = TABS.some(t => t[0] === parts[1]) ? parts[1] : 'program';
  return { bench: parts[0], tab };
}
const hrefFor = (bench, tab) => '#/' + (bench ? encodeURIComponent(bench) + '/' + (tab || 'program') : '');

function onRoute() {
  const r = parseHash();
  if (r.bench !== state.route.bench) state.ui = {};
  state.route = r;
  renderSidebar();
  renderMain();
}

const mainEl = document.getElementById('main');

function runCleanup() { for (const f of state.cleanup.splice(0)) { try { f(); } catch (e) { console.warn(e); } } }

function renderMain() {
  runCleanup();
  hideTip();
  mainEl.replaceChildren();
  if (state.indexError) {
    mainEl.appendChild(h('div', { class: 'panel' },
      h('h3', null, 'Could not load the data index'),
      h('div', { class: 'msg error' }, state.indexError),
      h('p', null, 'Generate it with ', h('code', null, 'viz-export'), ' (writes ', h('code', null, 'viz/data/'),
        '), or open the bundled ', h('a', { href: '?data=sample' }, 'sample data'), '.')));
    return;
  }
  if (!state.index) { mainEl.appendChild(h('div', { class: 'msg' }, 'Loading index…')); return; }
  const { bench, tab } = state.route;
  if (!bench) { renderOverview(mainEl); applyPendingScroll(); return; }
  if (!state.benchMeta.has(bench)) {
    mainEl.appendChild(h('div', { class: 'msg error' }, `Unknown benchmark “${bench}”.`));
    return;
  }
  const meta = state.benchMeta.get(bench);
  const head = h('div', { class: 'page-head' }, h('h2', null, bench), h('span', { class: 'group' }, meta.group || ''));
  const tabs = h('nav', { class: 'tabs' }, TABS.map(([id, label]) =>
    h('a', { class: 'tab' + (id === tab ? ' active' : ''), href: hrefFor(bench, id) }, label)));
  const body = h('div', null, h('div', { class: 'msg' }, 'Loading ' + bench + '…'));
  append(mainEl, [head, tabs, body]);
  const token = {};
  state.renderToken = token;
  loadBench(bench).then(b => {
    if (state.renderToken !== token) return;
    body.replaceChildren();
    try {
      if (tab === 'program') renderProgram(body, b);
      else if (tab === 'spec') renderSpec(body, b);
      else renderRuns(body, b);
      applyPendingScroll();
    } catch (e) {
      console.error(e);
      body.replaceChildren(h('div', { class: 'msg error' }, 'Render error: ' + (e.message || e)));
    }
  }, err => {
    if (state.renderToken !== token) return;
    body.replaceChildren(h('div', { class: 'msg error' }, 'Could not load benchmark: ' + (err.message || err)));
  });
}

/* ---- Sidebar ---- */
const filterEl = document.getElementById('filter');
const listEl = document.getElementById('bench-list');

function renderSidebar() {
  listEl.replaceChildren();
  if (!state.index) return;
  const f = filterEl.value.trim().toLowerCase();
  listEl.appendChild(h('a', { class: 'nav-item' + (!state.route.bench ? ' active' : ''), href: '#/' }, 'Overview (all benchmarks)'));
  const groups = new Map();
  for (const b of state.index.benches) {
    if (f && !(b.name.toLowerCase().includes(f) || str(b.group).toLowerCase().includes(f))) continue;
    const g = b.group || 'other';
    if (!groups.has(g)) groups.set(g, []);
    groups.get(g).push(b);
  }
  for (const [g, items] of groups) {
    const sec = h('div', { class: 'nav-group' }, h('div', { class: 'nav-group-title' }, g));
    for (const b of items) {
      sec.appendChild(h('a', {
        class: 'nav-item' + (b.name === state.route.bench ? ' active' : ''),
        href: hrefFor(b.name, state.route.bench ? state.route.tab : 'program'),
        title: b.name,
      }, h('span', null, b.name), h('span', { class: 'count' }, b.queries ? b.queries + 'q' : '')));
    }
    listEl.appendChild(sec);
  }
  if (!groups.size) listEl.appendChild(h('div', { class: 'hint', style: { padding: '8px' } }, 'No match.'));
}
filterEl.addEventListener('input', renderSidebar);

/* ======================================================================
 * Code highlighting (miniKanren surface syntax of the .mk files)
 * ==================================================================== */

const TOKEN_RE = /(--.*$)|\b(fresh|in|let|where)\b|(=\/=|==|::|&|\||\?|=|↦|≠)|\b([A-Z][\w']*)\b|\b([a-z_][\w'.]*)\b/g;

/** Render source text as highlighted <pre> with line numbers. */
function codeBlock(text, opts = {}) {
  const pre = h('pre', { class: 'code' });
  if (opts.maxHeight) pre.style.maxHeight = opts.maxHeight;
  const lines = str(text).replace(/\s+$/, '').split('\n');
  const defs = new Set();
  for (const ln of lines) { const m = /^([a-z_][\w']*)\s/.exec(ln); if (m) defs.add(m[1]); }
  for (const ln of lines) {
    const line = h('span', { class: 'ln' });
    let last = 0, m;
    TOKEN_RE.lastIndex = 0;
    while ((m = TOKEN_RE.exec(ln))) {
      if (m.index > last) line.appendChild(document.createTextNode(ln.slice(last, m.index)));
      let cls = 'tk-var';
      if (m[1]) cls = 'tk-comment';
      else if (m[2]) cls = 'tk-kw';
      else if (m[3]) cls = 'tk-op';
      else if (m[4]) cls = 'tk-ctor';
      else if (m[5]) cls = (m.index === 0 && defs.has(m[5])) ? 'tk-def' : defs.has(m[5]) ? 'tk-rel' : 'tk-var';
      line.appendChild(h('span', { class: cls }, m[0]));
      last = m.index + m[0].length;
      if (m[0].length === 0) TOKEN_RE.lastIndex++;
    }
    if (last < ln.length) line.appendChild(document.createTextNode(ln.slice(last)));
    if (!ln.length) line.appendChild(document.createTextNode(' '));
    pre.appendChild(line);
  }
  return pre;
}

/** A goal string, conjunctions broken onto separate lines. */
function goalEl(goal, split = true) {
  const g = str(goal);
  return h('div', { class: 'goal' }, split ? g.split(' & ').join('\n& ') : g);
}

/* ======================================================================
 * Overview: bench × specializer matrix
 * ==================================================================== */

let overviewLoadStarted = false;
const rerenderOverviewSoon = debounce(() => { if (!state.route.bench) renderMain(); }, 120);

function startLoadAll() {
  if (overviewLoadStarted) return;
  overviewLoadStarted = true;
  const names = state.index.benches.map(b => b.name);
  let i = 0;
  const worker = () => {
    if (i >= names.length) return Promise.resolve();
    const n = names[i++];
    return loadBench(n).catch(() => {}).then(() => { rerenderOverviewSoon(); return worker(); });
  };
  for (let k = 0; k < 6; k++) worker();
}

/** Cell for (bench, spec): {text, title, bg, ink, cls}. */
function overviewCell(b, spec, mode) {
  const sp = b._spec.get(spec);
  if (!sp && !b.runs.some(r => r.spec === spec)) return { text: '', cls: 'none', title: 'not measured' };
  if (sp && sp.error) return { text: 'err', cls: 'err', title: 'specialization failed: ' + sp.error };
  const qs = mode === 'first' ? b.queries.slice(0, 1) : b.queries;
  const ratios = [], notes = [];
  let flag = false;
  for (const q of qs) {
    const r = getRun(b, spec, q.id), o = getRun(b, 'original', q.id);
    if (!r) { notes.push(q.id + ': no run'); continue; }
    if (badCheck(r.check)) { flag = true; notes.push(q.id + ': check ' + r.check); }
    const rt = ratioOf(r, o);
    ratios.push(rt);
    notes.push(`${q.id}: ${ratioText(rt)} (${isFuel(r) ? 'out of fuel' : fmtInt(r.steps) + ' steps'}${o ? ' vs ' + (isFuel(o) ? 'fuel' : fmtInt(o.steps)) : ''})`);
  }
  const cls = flag ? ' flag' : '';
  if (!ratios.length) return { text: '—', cls: 'none' + cls, title: notes.join('\n') };
  if (ratios.some(r => r.kind === 'inf')) return { text: '∞', cls: 'inf' + cls, title: notes.join('\n'), v: Infinity };
  const fin = ratios.filter(r => r.kind === 'exact' || r.kind === 'upper');
  if (!fin.length) {
    const t = ratios.some(r => r.kind === 'bothfuel') ? 'fuel' : '—';
    return { text: t, cls: cls, title: notes.join('\n') };
  }
  const gm = Math.exp(fin.reduce((a, r) => a + Math.log(Math.max(r.v, 1e-12)), 0) / fin.length);
  const upper = fin.some(r => r.kind === 'upper');
  return {
    text: (upper ? '≤' : '') + '×' + fmtRatioNum(gm), v: gm, bg: ratioBg(gm), ink: ratioInk(gm), cls: cls.trim(),
    title: (mode === 'geo' && qs.length > 1 ? `geomean over ${fin.length} quer${fin.length > 1 ? 'ies' : 'y'}\n` : '') + notes.join('\n'),
  };
}

function renderOverview(root) {
  startLoadAll();
  const P = state.prefs;
  const benches = state.index.benches;
  const loadedN = benches.filter(b => state.loaded.has(b.name)).length;

  let cols = [];
  for (const b of benches) cols.push(...(b.specializers || []));
  for (const v of state.loaded.values()) if (v._specOrder) cols.push(...v._specOrder);
  cols = orderSpecs(cols).filter(c => c !== 'original');
  if (!P.ovVariants) cols = cols.filter(c => !c.includes('+'));

  const modeSel = h('select', { onchange: e => { P.ovMode = e.target.value; renderMain(); } },
    h('option', { value: 'first', selected: P.ovMode === 'first' }, 'first query'),
    h('option', { value: 'geo', selected: P.ovMode === 'geo' }, 'geometric mean over queries'));
  const varChk = h('label', null, h('input', { type: 'checkbox', checked: P.ovVariants,
    onchange: e => { P.ovVariants = e.target.checked; renderMain(); } }), ' show +opt/+sched variants');

  const gradient = 'linear-gradient(to right, rgba(var(--div-good),.8), rgba(var(--div-good),.12), var(--surface-2), rgba(var(--div-bad),.12), rgba(var(--div-bad),.8))';
  const head = h('div', { class: 'panel' },
    h('div', { class: 'panel-head' },
      h('h3', null, 'Steps relative to the unspecialized program'),
      h('div', { class: 'toolbar' }, 'Ratio for ', modeSel, h('span', { class: 'sep' }), varChk)),
    h('div', { class: 'toolbar', style: { marginBottom: '8px' } },
      h('span', { class: 'scale' }, 'fewer steps ×0.003', h('span', { class: 'bar', style: { background: gradient } }), 'more ×300'),
      h('span', { class: 'hint' }, '∞ = ran out of fuel where the original did not · ≤ = original ran out of fuel (upper bound) · red ring = MISMATCH / unverified check · err = specialization failed'),
      h('span', { class: 'hint' }, `${loadedN}/${benches.length} benchmark files loaded`)));

  const table = h('table', { class: 'matrix' });
  const thead = h('thead', null, h('tr', null,
    h('th', { style: { textAlign: 'left', verticalAlign: 'bottom' } }, 'benchmark'),
    h('th', { class: 'col', title: 'steps of the unspecialized program' }, 'original steps'),
    cols.map(c => h('th', { class: 'col' }, c))));
  const tbody = h('tbody');
  const colVals = cols.map(() => ({ logs: [], inf: 0 }));
  let lastGroup = null;
  for (const meta of benches) {
    if (meta.group !== lastGroup) {
      lastGroup = meta.group;
      tbody.appendChild(h('tr', { class: 'grouprow' }, h('td', { colspan: cols.length + 2 }, meta.group || 'other')));
    }
    const b = state.loaded.get(meta.name);
    const tr = h('tr', null, h('td', { class: 'rowh' }, h('a', { href: hrefFor(meta.name, 'runs') }, meta.name)));
    if (!b) {
      tr.appendChild(h('td', { class: 'cell none', colspan: cols.length + 1 }, 'loading…'));
    } else if (b.error) {
      tr.appendChild(h('td', { class: 'cell err', colspan: cols.length + 1, style: { textAlign: 'left' } }, b.error));
    } else {
      const q0 = b.queries[0];
      const o = q0 ? getRun(b, 'original', q0.id) : null;
      tr.appendChild(h('td', { class: 'cell none', title: q0 ? q0.id + ': ' + q0.goal : '' },
        o ? (isFuel(o) ? '∞ (fuel)' : isErr(o) ? 'error' : fmtCompact(o.steps)) : '—'));
      cols.forEach((c, i) => {
        const cell = overviewCell(b, c, P.ovMode);
        if (cell.v === Infinity) colVals[i].inf++;
        else if (cell.v > 0) colVals[i].logs.push(Math.log(cell.v));
        const td = h('td', { class: 'cell ' + (cell.cls || ''), style: { background: cell.bg || null, color: cell.ink || null } }, cell.text);
        if (cell.cls !== 'none') {
          td.addEventListener('click', () => { location.hash = hrefFor(meta.name, 'runs'); });
          tip(td, () => ({ title: `${meta.name} · ${c}`, rows: str(cell.title).split('\n').map(t => ({ text: t, mono: true })) }));
        }
        tr.appendChild(td);
      });
    }
    tbody.appendChild(tr);
  }
  const foot = h('tr', { class: 'foot' }, h('td', { class: 'rowh' }, 'geomean (finite)'), h('td', null, ''),
    colVals.map(cv => {
      if (!cv.logs.length) return h('td', { class: 'cell none' }, cv.inf ? cv.inf + '×∞' : '—');
      const gm = Math.exp(cv.logs.reduce((a, x) => a + x, 0) / cv.logs.length);
      const td = h('td', { class: 'cell', style: { background: ratioBg(gm), color: ratioInk(gm) || null } }, '×' + fmtRatioNum(gm));
      tip(td, () => ({ title: 'Column summary', rows: [`geomean of ${cv.logs.length} finite ratios: ×${fmtRatioNum(gm)}`, `${cv.inf} benchmark(s) ran out of fuel (∞)`] }));
      return td;
    }));
  tbody.appendChild(foot);
  append(table, [thead, tbody]);
  head.appendChild(h('div', { class: 'matrix-wrap' }, table));
  root.appendChild(head);
  const gen = state.index.generated ? ` · generated ${state.index.generated}` : '';
  root.appendChild(h('p', { class: 'hint' }, `Fuel ${fmtInt(state.index.fuel)} steps, up to ${state.index.answers ?? '?'} answers per query${gen}. Click a cell to open the runs of that benchmark.`));
}

/* ======================================================================
 * Program tab
 * ==================================================================== */

function renderProgram(root, b) {
  const left = h('div', { class: 'panel' }, h('h3', null, 'Source (.mk)'), b.source ? codeBlock(b.source, { maxHeight: '75vh' }) : h('div', { class: 'msg' }, 'No source.'));
  const qt = h('table', { class: 'data' }, h('thead', null, h('tr', null, h('th', null, 'id'), h('th', null, 'run-time goal'))),
    h('tbody', null, b.queries.map(q => h('tr', null, h('td', { class: 'mono' }, q.id), h('td', null, goalEl(q.goal, false))))));
  const specRows = b._specOrder.map(n => {
    const sp = b._spec.get(n) || {};
    return h('tr', null,
      h('td', { class: 'mono' }, n),
      h('td', { class: 'num' }, sp.specMs !== undefined && sp.specMs !== null ? Number(sp.specMs).toFixed(2) : '—'),
      h('td', { class: 'num' }, fmtInt(sp.defs)), h('td', { class: 'num' }, fmtInt(sp.size)),
      h('td', null, sp.globalTree ? h('a', { href: hrefFor(b.name, 'spec'), onclick: () => { state.ui.tree = n; } }, 'tree') : ''),
      h('td', null, sp.error ? h('span', { class: 'flag-bad wrap' }, sp.error) : ''));
  });
  const right = h('div', { class: 'stack' },
    h('div', { class: 'panel' }, h('h3', null, 'Specialization query'), goalEl(b.specQuery || '—', false),
      h('p', { class: 'hint' }, `Every run-time query is an instance of this goal; it is evaluated against the residual program. Fuel ${fmtInt(b.fuel)}, up to ${b.answers ?? '?'} answers.`)),
    h('div', { class: 'panel' }, h('h3', null, `Run-time queries (${b.queries.length})`), qt,
      b.runsSource ? h('details', { style: { marginTop: '8px' } }, h('summary', { class: 'hint' }, '.runs file'), codeBlock(b.runsSource)) : null),
    h('div', { class: 'panel' }, h('h3', null, 'Specializers'),
      h('table', { class: 'data' }, h('thead', null, h('tr', null, h('th', null, 'name'), h('th', { class: 'num' }, 'spec ms'),
        h('th', { class: 'num' }, 'defs'), h('th', { class: 'num' }, 'size'), h('th', null, ''), h('th', null, 'error'))), h('tbody', null, specRows))));
  root.appendChild(h('div', { class: 'grid2' }, left, right));
}

/* ======================================================================
 * Tree view (global and local trees): SVG with pan/zoom + collapsible, or outline
 * ==================================================================== */

const GKINDS = {
  node: 'processed conjunction (own definition)', leaf: 'instance of an earlier conjunction (folded)',
  prune: 'generalization point', split: 'split into independent parts', transient: 'deterministic pass-through',
  success: 'success', fail: 'failure', truncated: 'cut (tree > 1500 nodes)',
};
const SKINDS = {
  or: 'unfolded call (disjunction)', conj: 'conjunction (current goals)', leaf: 'leaf: goals passed to global control',
  success: 'success', fail: 'failure', truncated: 'cut (tree > 1500 nodes)',
};
const KNOWN_KINDS = new Set(['node', 'leaf', 'prune', 'split', 'transient', 'success', 'fail', 'truncated', 'or', 'conj']);
const kindColor = k => KNOWN_KINDS.has(k) ? `var(--k-${k})` : 'var(--k-unknown)';

function kindLegend(counts, desc) {
  return h('div', { class: 'legend' }, [...counts.entries()].map(([k, n]) =>
    h('span', { class: 'item', title: desc[k] || 'unknown kind' }, h('span', { class: 'sw', style: { background: kindColor(k) } }),
      `${k || '?'} (${n})`, h('span', { class: 'hint' }, desc[k] ? ' – ' + desc[k] : ' – unknown kind'))));
}

function countKinds(root) {
  const m = new Map(), st = root ? [root] : [];
  while (st.length) {
    const n = st.pop();
    m.set(n.kind, (m.get(n.kind) || 0) + 1);
    if (Array.isArray(n.children)) st.push(...n.children);
  }
  return m;
}

/**
 * Collapsible tree with an SVG (left→right, pan/zoom) and an outline mode.
 * opt: { label(d), tipRows(d), onSelect(d), height, nodeW, colW, rowH, autoCollapseDepth, autoCollapseOver }
 */
class TreeView {
  constructor(host, root, opt) {
    this.opt = Object.assign({ height: 520, nodeW: 250, colW: 290, rowH: 26, autoCollapseDepth: 4, autoCollapseOver: 80 }, opt);
    this.host = host;
    this.mode = 'svg';
    this.tf = { k: 1, x: 20, y: 20 };
    this.selected = null;
    this.build(root);
    this.el = h('div', { class: 'treeview' });
    this.tools = h('div', { class: 'tv-tools' });
    this.body = h('div', { style: { height: this.opt.height + 'px' } });
    append(this.el, [this.tools, this.body]);
    host.appendChild(this.el);
    this.renderTools();
    this.render(true);
  }

  build(root) {
    this.nodes = [];
    const st = [[root, 0, null]];
    // iterative DFS (pre-order) so that very deep chains do not blow the stack
    const order = [];
    while (st.length) {
      const [d, depth, parent] = st.pop();
      const n = { d, depth, parent, kids: [], collapsed: false, idx: this.nodes.length, size: 1 };
      this.nodes.push(n);
      order.push(n);
      if (parent) parent.kids.push(n);
      const ch = Array.isArray(d.children) ? d.children.filter(Boolean) : [];
      for (let i = ch.length - 1; i >= 0; i--) st.push([ch[i], depth + 1, n]);
    }
    for (let i = order.length - 1; i >= 0; i--) if (order[i].parent) order[i].parent.size += order[i].size;
    this.root = this.nodes[0];
    this.collapseTo(this.nodes.length > this.opt.autoCollapseOver ? this.opt.autoCollapseDepth : Infinity);
  }

  collapseTo(depth) {
    for (const n of this.nodes) n.collapsed = n.kids.length > 0 && n.depth >= depth - 1;
  }

  renderTools() {
    const t = this.tools;
    t.replaceChildren();
    const btn = (label, title, fn) => h('button', { title, onclick: fn }, label);
    append(t, [
      btn('Fit', 'fit to view', () => { this.fit(); this.applyTf(); }),
      btn('Expand all', 'expand every node (may be large)', () => { this.collapseTo(Infinity); this.render(); }),
      btn('Depth 4', 'collapse below depth 4', () => { this.collapseTo(4); this.render(true); }),
      btn('Depth 2', 'collapse below depth 2', () => { this.collapseTo(2); this.render(true); }),
      h('span', { class: 'sep' }),
      h('button', { class: this.mode === 'svg' ? 'on' : '', onclick: () => { this.mode = 'svg'; this.renderTools(); this.render(true); } }, 'Tree'),
      h('button', { class: this.mode === 'outline' ? 'on' : '', onclick: () => { this.mode = 'outline'; this.renderTools(); this.render(); } }, 'Outline'),
      h('span', { class: 'hint' }, `${this.nodes.length} nodes · drag to pan, wheel to zoom, ⊕ toggles a subtree`),
    ]);
  }

  visible() {
    const out = [], st = [this.root];
    while (st.length) {
      const n = st.pop();
      out.push(n);
      if (!n.collapsed) for (let i = n.kids.length - 1; i >= 0; i--) st.push(n.kids[i]);
    }
    return out;
  }

  layout(vis) {
    const { colW, rowH } = this.opt;
    let slot = 0;
    // post-order: leaves get consecutive rows; parents centered over children
    const post = [];
    const st = [[this.root, false]];
    while (st.length) {
      const [n, done] = st.pop();
      if (done || n.collapsed || !n.kids.length) { post.push(n); continue; }
      st.push([n, true]);
      for (let i = n.kids.length - 1; i >= 0; i--) st.push([n.kids[i], false]);
    }
    for (const n of post) {
      n.x = n.depth * colW;
      if (!n.collapsed && n.kids.length) n.y = (n.kids[0].y + n.kids[n.kids.length - 1].y) / 2;
      else n.y = slot++ * rowH;
    }
    this.extent = { w: Math.max(...vis.map(n => n.x)) + this.opt.nodeW + 60, h: slot * rowH };
  }

  toggle(n) { if (n.kids.length) { n.collapsed = !n.collapsed; this.render(); } }

  select(n) {
    this.selected = n;
    if (this.opt.onSelect) this.opt.onSelect(n.d, n);
    this.render();
  }

  render(refit) {
    hideTip();
    const vis = this.visible();
    if (this.mode === 'outline') { this.renderOutline(vis); return; }
    this.layout(vis);
    const { nodeW, rowH } = this.opt;
    const bh = rowH - 6;
    const maxChars = Math.floor((nodeW - 16) / 6.7);
    if (!this.svg) this.initSvg();
    if (!this.body.contains(this.svg)) this.body.replaceChildren(this.svg);
    const g = this.g;
    g.replaceChildren();
    const edges = s('g'), nodesG = s('g');
    for (const n of vis) {
      if (n.parent) {
        const p = n.parent;
        const x1 = p.x + nodeW + 16, y1 = p.y + bh / 2, x2 = n.x, y2 = n.y + bh / 2, mx = (x1 + x2) / 2;
        edges.appendChild(s('path', { class: 'tv-edge', d: `M${x1},${y1} C${mx},${y1} ${mx},${y2} ${x2},${y2}` }));
      }
      const color = kindColor(n.d.kind);
      const label = this.opt.label(n.d);
      const ng = s('g', { class: 'tn' + (n === this.selected ? ' sel' : ''), transform: `translate(${n.x},${n.y})`, 'data-idx': n.idx });
      ng.appendChild(s('rect', { class: 'box', width: nodeW, height: bh, rx: 4, style: { fill: color, fillOpacity: 0.16, stroke: color } }));
      ng.appendChild(s('rect', { width: 4, height: bh, rx: 2, style: { fill: color } }));
      ng.appendChild(s('text', { x: 9, y: bh / 2 + 4 }, label.length > maxChars ? label.slice(0, maxChars - 1) + '…' : label));
      if (n.kids.length) {
        const tg = s('g', { class: 'toggle', transform: `translate(${nodeW + 8},${bh / 2})`, 'data-toggle': n.idx });
        tg.appendChild(s('circle', { r: 6.5 }));
        tg.appendChild(s('text', { x: 0, y: 4, 'text-anchor': 'middle' }, n.collapsed ? '+' : '−'));
        if (n.collapsed) tg.appendChild(s('text', { x: 11, y: 4, class: 'tg' }, String(n.size - 1)));
        ng.appendChild(tg);
      }
      tip(ng, () => ({ title: `${n.d.kind || '?'}${n.d.id !== undefined ? ' #' + n.d.id : ''}`, rows: this.opt.tipRows(n.d) }));
      nodesG.appendChild(ng);
    }
    append(g, [edges, nodesG]);
    if (refit) this.fit(true);
    this.applyTf();
  }

  renderOutline(vis) {
    const box = h('div', { class: 'outline', style: { height: '100%' } });
    for (const n of vis) {
      const row = h('div', { class: 'ol-row' + (n === this.selected ? ' sel' : ''), style: { paddingLeft: (4 + n.depth * 14) + 'px' } },
        h('span', { class: 'tog', onclick: e => { e.stopPropagation(); this.toggle(n); } }, n.kids.length ? (n.collapsed ? '▸' : '▾') : ''),
        h('span', { class: 'sw', style: { background: kindColor(n.d.kind) } }),
        h('span', { class: 'lbl' }, this.opt.label(n.d), n.collapsed ? h('span', { class: 'hint' }, `  (+${n.size - 1})`) : null));
      row.addEventListener('click', () => this.select(n));
      box.appendChild(row);
    }
    const prevScroll = this.body.firstChild && this.body.firstChild.scrollTop;
    this.body.replaceChildren(box);
    if (prevScroll) box.scrollTop = prevScroll;
  }

  initSvg() {
    this.svg = s('svg', { height: this.opt.height });
    this.g = s('g');
    this.svg.appendChild(this.g);
    let drag = null;
    this.svg.addEventListener('pointerdown', e => {
      if (e.button !== 0) return;
      drag = { x: e.clientX, y: e.clientY, tx: this.tf.x, ty: this.tf.y, moved: false, target: e.target };
    });
    window.addEventListener('pointermove', this._mv = e => {
      if (!drag) return;
      const dx = e.clientX - drag.x, dy = e.clientY - drag.y;
      if (!drag.moved && Math.hypot(dx, dy) > 3) { drag.moved = true; this.svg.classList.add('dragging'); hideTip(); }
      if (drag.moved) { this.tf.x = drag.tx + dx; this.tf.y = drag.ty + dy; this.applyTf(); }
    });
    window.addEventListener('pointerup', this._up = () => {
      if (!drag) return;
      const d = drag; drag = null;
      this.svg.classList.remove('dragging');
      if (d.moved || !d.target || !d.target.closest) return;
      const tgl = d.target.closest('[data-toggle]');
      if (tgl) { this.toggle(this.nodes[+tgl.getAttribute('data-toggle')]); return; }
      const nd = d.target.closest('[data-idx]');
      if (nd) this.select(this.nodes[+nd.getAttribute('data-idx')]);
    });
    state.cleanup.push(() => { window.removeEventListener('pointermove', this._mv); window.removeEventListener('pointerup', this._up); });
    this.svg.addEventListener('wheel', e => {
      e.preventDefault();
      const r = this.svg.getBoundingClientRect();
      const px = e.clientX - r.left, py = e.clientY - r.top;
      const k2 = clamp(this.tf.k * Math.exp(-e.deltaY * 0.0015), 0.05, 4);
      this.tf.x = px - (px - this.tf.x) * (k2 / this.tf.k);
      this.tf.y = py - (py - this.tf.y) * (k2 / this.tf.k);
      this.tf.k = k2;
      this.applyTf();
    }, { passive: false });
  }

  /** Fit the content; with `initial`, never zoom below 0.55 (then anchor the root on the left). */
  fit(initial) {
    const W = this.body.clientWidth || 600, H = this.opt.height;
    const { w, h: hh } = this.extent || { w: 100, h: 100 };
    let k = Math.min(1.2, (W - 30) / w, (H - 30) / Math.max(hh, 1));
    if (initial && k < 0.55) {
      k = 0.55;
      this.tf = { k, x: 15, y: H / 2 - (this.root.y + this.opt.rowH / 2) * k };
      return;
    }
    k = clamp(k, 0.05, 1.2);
    this.tf = { k, x: (W - w * k) / 2 + 10, y: (H - hh * k) / 2 };
  }

  applyTf() { if (this.g) this.g.setAttribute('transform', `translate(${this.tf.x},${this.tf.y}) scale(${this.tf.k})`); }
}

/* ======================================================================
 * Specialization tab
 * ==================================================================== */

const gLabel = d => {
  if (d.kind === 'truncated') return '… truncated';
  const id = d.id !== undefined && d.id !== null ? '#' + d.id + ' ' : '';
  return id + (str(d.goal) || d.kind || '?');
};
const sLabel = d => {
  switch (d.kind) {
    case 'success': return '✓ ' + (str(d.subst) || 'success');
    case 'fail': return '✗ ' + (str(d.subst) || 'fail');
    case 'truncated': return '… truncated';
    default: return str(d.goal) || d.kind || '?';
  }
};
const gTipRows = d => [
  d.goal ? { text: d.goal, mono: true } : null,
  d.gen ? { text: 'gen: ' + d.gen, mono: true } : null,
  d.subst ? { text: 'edge: ' + d.subst, mono: true } : null,
].filter(Boolean);
const sTipRows = d => [
  d.goal ? { text: d.goal, mono: true } : null,
  d.subst ? { text: 'subst: ' + d.subst, mono: true } : null,
].filter(Boolean);

function renderSpec(root, b) {
  const ui = state.ui;
  const treeSpecs = b._specOrder.filter(n => n !== 'original').map(n => b._spec.get(n)).filter(sp => sp && (sp.globalTree || sp.error));
  const withTree = treeSpecs.filter(sp => sp.globalTree);
  if (!ui.tree || !treeSpecs.some(sp => sp.name === ui.tree)) ui.tree = (withTree[0] || treeSpecs[0] || {}).name;
  const sp = b._spec.get(ui.tree);

  const sel = h('select', { onchange: e => { ui.tree = e.target.value; ui.residual = e.target.value; ui.gSel = null; renderMain(); } },
    treeSpecs.map(x => h('option', { value: x.name, selected: x.name === ui.tree }, x.name + (x.error ? ' (error)' : ''))));
  const top = h('div', { class: 'panel' },
    h('div', { class: 'panel-head' },
      h('div', { class: 'toolbar' }, h('h3', null, 'Global tree'), treeSpecs.length ? sel : null,
        sp ? h('span', { class: 'hint' }, `spec ${sp.specMs ?? '?'} ms · ${sp.defs ?? '?'} defs · size ${sp.size ?? '?'}`) : null)));
  root.appendChild(top);

  if (!treeSpecs.length) {
    top.appendChild(h('div', { class: 'msg' }, 'No specializer of this benchmark exported a global tree.'));
  } else {
    if (sp.error) top.appendChild(h('div', { class: 'msg error wrap' }, 'Specialization failed: ' + sp.error));
    if (sp.treeTruncated) top.appendChild(h('div', { class: 'msg warn', style: { marginBottom: '8px' } }, 'The tree was truncated at export (subtrees marked “truncated”).'));
    if (sp.globalTree) {
      top.appendChild(kindLegend(countKinds(sp.globalTree), GKINDS));
      const grid = h('div', { class: 'spec-grid', style: { marginTop: '10px' } });
      const treePanel = h('div');
      const details = h('div', { class: 'panel' });
      append(grid, [treePanel, details]);
      top.appendChild(grid);
      const showDetails = d => renderNodeDetails(details, d);
      const tv = new TreeView(treePanel, sp.globalTree, { label: gLabel, tipRows: gTipRows, onSelect: d => { ui.gSel = d; showDetails(d); }, height: 560 });
      // restore / default the selection to the root
      const want = ui.gSel && tv.nodes.find(n => n.d === ui.gSel || n.d.id === ui.gSel.id) || tv.root;
      tv.selected = want; tv.render(); showDetails(want.d);
    }
  }
  root.appendChild(renderResidualCompare(b));
}

function renderNodeDetails(panel, d) {
  panel.replaceChildren();
  const kids = Array.isArray(d.children) ? d.children.length : 0;
  append(panel, [
    h('div', { class: 'panel-head' }, h('h3', null, 'Global node ' + (d.id !== undefined && d.id !== null ? '#' + d.id : '')),
      h('span', { class: 'legend' }, h('span', { class: 'item' }, h('span', { class: 'sw', style: { background: kindColor(d.kind) } }), d.kind || '?',
        h('span', { class: 'hint' }, ' – ' + (GKINDS[d.kind] || 'unknown kind'))))),
    h('dl', { class: 'kv' },
      h('dt', null, 'goal'), h('dd', null, d.goal ? goalEl(d.goal) : h('span', { class: 'hint' }, '—')),
      h('dt', null, 'generalizer'), h('dd', null, d.gen ? goalEl(d.gen, false) : h('span', { class: 'hint' }, '—')),
      h('dt', null, 'edge subst'), h('dd', null, d.subst ? goalEl(d.subst, false) : h('span', { class: 'hint' }, '—')),
      h('dt', null, 'children'), h('dd', null, String(kids))),
    h('h4', null, 'Local unfolding tree'),
  ]);
  if (!d.local) { panel.appendChild(h('div', { class: 'msg' }, d.kind === 'truncated' ? 'Subtree cut at export.' : 'No local tree for this node.')); return; }
  panel.appendChild(kindLegend(countKinds(d.local), SKINDS));
  const host = h('div', { style: { marginTop: '6px' } });
  const info = h('div', { class: 'kv', style: { marginTop: '8px' } });
  append(panel, [host, info]);
  const showLocal = sd => {
    info.replaceChildren(
      h('dt', null, 'local node'), h('dd', null, h('span', { class: 'sw', style: { background: kindColor(sd.kind), marginRight: '5px' } }), sd.kind || '?'),
      h('dt', null, 'goal'), h('dd', null, sd.goal ? goalEl(sd.goal) : h('span', { class: 'hint' }, '—')),
      h('dt', null, 'subst'), h('dd', null, sd.subst ? goalEl(sd.subst, false) : h('span', { class: 'hint' }, '—')));
  };
  const tv = new TreeView(host, d.local, { label: sLabel, tipRows: sTipRows, onSelect: showLocal, height: 330, nodeW: 220, colW: 252, rowH: 24 });
  tv.selected = tv.root; tv.render(); showLocal(d.local);
}

function renderResidualCompare(b) {
  const ui = state.ui;
  const cands = b._specOrder.filter(n => n !== 'original').map(n => b._spec.get(n)).filter(Boolean);
  if (!ui.residual || !cands.some(c => c.name === ui.residual)) ui.residual = ui.tree && b._spec.has(ui.tree) ? ui.tree : (cands[0] || {}).name;
  const sp = b._spec.get(ui.residual);
  const sel = h('select', { onchange: e => { ui.residual = e.target.value; renderMain(); } },
    cands.map(c => h('option', { value: c.name, selected: c.name === ui.residual }, c.name)));
  const orig = b._spec.get('original');
  const left = h('div', { class: 'panel' },
    h('div', { class: 'panel-head' }, h('div', { class: 'toolbar' }, h('h3', null, 'Residual program'), cands.length ? sel : null)),
    !sp ? h('div', { class: 'msg' }, 'No specialized programs.') :
      [sp.error ? h('div', { class: 'msg error wrap' }, sp.error) : null,
        h('div', { class: 'hint', style: { marginBottom: '6px' } }, `${sp.defs ?? '?'} defs · size ${sp.size ?? '?'} · ${sp.specMs ?? '?'} ms`),
        sp.residualGoal ? h('div', { style: { marginBottom: '6px' } }, h('span', { class: 'hint' }, 'goal: '), h('code', { class: 'wrap' }, sp.residualGoal)) : null,
        sp.residual ? codeBlock(sp.residual) : h('div', { class: 'msg' }, 'No residual program.')]);
  const right = h('div', { class: 'panel' }, h('div', { class: 'panel-head' }, h('h3', null, 'Original program')),
    h('div', { class: 'hint', style: { marginBottom: '6px' } }, orig ? `${orig.defs ?? '?'} defs · size ${orig.size ?? '?'}` : 'source file'),
    b.specQuery ? h('div', { style: { marginBottom: '6px' } }, h('span', { class: 'hint' }, 'spec query: '), h('code', { class: 'wrap' }, b.specQuery)) : null,
    codeBlock(b.source));
  return h('div', { class: 'grid2' }, left, right);
}

/* ======================================================================
 * Runs & comparison tab
 * ==================================================================== */

function renderRuns(root, b) {
  const ui = state.ui;
  if (!b.queries.length) { root.appendChild(h('div', { class: 'msg' }, 'No runs.')); return; }
  if (!ui.query || !b.queries.some(q => q.id === ui.query)) ui.query = b.queries[0].id;
  const q = b.queries.find(x => x.id === ui.query);
  const runs = b._specOrder.map(n => getRun(b, n, q.id)).filter(Boolean);
  const orig = getRun(b, 'original', q.id);
  const best = bestRun(b, q.id);

  // query selector
  root.appendChild(h('div', { class: 'panel' },
    h('div', { class: 'toolbar' }, h('h3', { style: { margin: 0 } }, 'Query'),
      b.queries.map(x => h('button', { class: x.id === q.id ? 'on' : '', title: x.goal,
        onclick: () => { ui.query = x.id; renderMain(); } }, x.id))),
    h('div', { style: { marginTop: '6px' } }, goalEl(q.goal || '—', false))));

  if (!runs.length) { root.appendChild(h('div', { class: 'msg' }, 'No runs for this query.')); return; }

  // focus spec (clicked row) drives the answers panel and lane/profile B defaults
  if (!ui.focus || !runs.some(r => r.spec === ui.focus)) ui.focus = (best || runs.find(r => r.spec !== 'original') || runs[0]).spec;

  root.appendChild(renderRunTable(b, q, runs, orig, best));
  const charts = h('div', { class: 'grid2', style: { marginBottom: '16px' } });
  root.appendChild(charts);
  const barPanel = h('div', { class: 'panel' }), tlPanel = h('div', { class: 'panel' });
  append(charts, [barPanel, tlPanel]);
  renderStepBars(barPanel, b, runs, orig, best);
  renderTimeline(tlPanel, b, q, runs, best);
  const profPanel = h('div', { class: 'panel' });
  root.appendChild(profPanel);
  renderProfiles(profPanel, b, q, runs, best);
  const tracePanel = h('div', { class: 'panel' });
  root.appendChild(tracePanel);
  renderTrace(tracePanel, b, q, runs, best);

  const onResize = debounce(() => { if (state.route.tab === 'runs') { barPanel.replaceChildren(); renderStepBars(barPanel, b, runs, orig, best); tlPanel.replaceChildren(); renderTimeline(tlPanel, b, q, runs, best); } }, 200);
  window.addEventListener('resize', onResize);
  state.cleanup.push(() => window.removeEventListener('resize', onResize));
}

function outcomeBadge(r) {
  if (isSolved(r)) return h('span', { class: 'badge' }, r.outcome);
  if (isFuel(r)) return h('span', { class: 'badge warn', title: 'fuel budget exhausted' }, 'OutOfFuel');
  return h('span', { class: 'badge bad wrap', title: r.outcome }, '⚠ ' + (r.outcome || 'error'));
}
function checkBadge(c) {
  if (c === 'ok' || c === 'ok~') return h('span', { class: 'badge good', title: c === 'ok~' ? 'answers verified by replay (search not exhausted)' : 'answer sets equal' }, c);
  if (badCheck(c)) return h('span', { class: 'badge bad' }, '⚠ ' + c);
  return h('span', { class: 'badge' }, c || '-');
}

function renderRunTable(b, q, runs, orig, best) {
  const ui = state.ui;
  const sortKey = ui.sort || 'order';
  const solvedSteps = runs.filter(isSolved).map(r => r.steps);
  const minSteps = solvedSteps.length ? Math.min(...solvedSteps) : null;
  const anyFuel = runs.some(isFuel);
  const maxSteps = runs.length ? Math.max(...runs.filter(r => !isErr(r)).map(r => r.steps)) : null;
  const sortVal = r => isErr(r) ? 2e18 : isFuel(r) ? 1e18 + r.steps : r.steps;
  const rows = sortKey === 'steps' ? [...runs].sort((a, b2) => sortVal(a) - sortVal(b2))
    : sortKey === 'answers' ? [...runs].sort((a, b2) => b2.answers.length - a.answers.length || sortVal(a) - sortVal(b2)) : runs;
  const th = (key, label, cls) => h('th', { class: (cls || '') + ' sortable', title: 'sort',
    onclick: () => { ui.sort = ui.sort === key ? 'order' : key; renderMain(); } }, label + (sortKey === key ? ' ▾' : ''));
  const tbody = h('tbody');
  for (const r of rows) {
    const rt = ratioOf(r, orig);
    const isBest = minSteps !== null && isSolved(r) && r.steps === minSteps;
    const isWorst = !isErr(r) && (anyFuel ? isFuel(r) : (r.steps === maxSteps && runs.length > 1 && maxSteps !== minSteps));
    const stepsCell = h('td', { class: 'num' + (isBest ? ' best' : isWorst ? ' worst' : ''), title: isFuel(r) ? `≥ ${fmtInt(r.steps)} steps (fuel exhausted)` : '' },
      isFuel(r) ? '∞ (fuel)' : isErr(r) ? '—' : fmtInt(r.steps), isBest ? ' ★' : '');
    const v = rt.kind === 'exact' || rt.kind === 'upper' ? rt.v : null;
    const tr = h('tr', { class: 'clickable' + (r.spec === ui.focus ? ' focus' : ''), onclick: () => { ui.focus = r.spec; ui.profB = r.spec; ui.traceB = r.spec; renderMain(); } },
      h('td', { class: 'mono' }, r.spec),
      stepsCell,
      h('td', { class: 'num' }, h('span', { class: 'badge', style: { background: r.spec === 'original' ? null : ratioBg(rt.kind === 'inf' ? 1e6 : v), color: ratioInk(rt.kind === 'inf' ? 1e6 : v) || null } }, r.spec === 'original' ? '×1' : ratioText(rt))),
      h('td', { class: 'num' }, fmtInt(r.unifs)), h('td', { class: 'num' }, fmtInt(r.calls)),
      h('td', { class: 'num' }, String(r.answers.length)),
      h('td', null, outcomeBadge(r)), h('td', null, checkBadge(r.check)));
    tbody.appendChild(tr);
  }
  const fr = runs.find(r => r.spec === ui.focus);
  const ans = fr ? h('div', { style: { marginTop: '10px' } },
    h('h4', null, `Answers of ${fr.spec} (${fr.answers.length})`),
    fr.answers.length ? h('ol', { class: 'mono', style: { margin: 0, paddingLeft: '2.4em', maxHeight: '180px', overflow: 'auto' } },
      fr.answers.map((a, i) => h('li', { class: 'wrap' }, a, fr.answerSteps[i] !== undefined ? h('span', { class: 'hint' }, `   @ step ${fmtInt(fr.answerSteps[i])}`) : null)))
      : h('div', { class: 'hint' }, 'no answers')) : null;
  return h('div', { class: 'panel' },
    h('div', { class: 'panel-head' }, h('h3', null, 'Summary'),
      h('span', { class: 'hint' }, 'steps = unifications + calls · ★ fewest steps (solved) · red edge = worst · click a row to focus it')),
    h('div', { style: { overflowX: 'auto' } }, h('table', { class: 'data' },
      h('thead', null, h('tr', null, th('order', 'specializer'), th('steps', 'steps', 'num'), h('th', { class: 'num', title: 'steps relative to original' }, 'vs original'),
        h('th', { class: 'num' }, 'unifs'), h('th', { class: 'num' }, 'calls'), th('answers', '#answers', 'num'), h('th', null, 'outcome'), h('th', null, 'check'))),
      tbody)),
    ans);
}

/* ---- (b) steps bar chart ---- */
function renderStepBars(panel, b, runs, orig, best) {
  const P = state.prefs;
  const finiteRuns = runs.filter(r => !isErr(r));
  const maxS = Math.max(1, ...finiteRuns.map(r => r.steps));
  const minS = Math.max(1, Math.min(...finiteRuns.map(r => Math.max(1, r.steps))));
  const log = P.barLog === null ? maxS / minS > 50 : P.barLog;
  append(panel, [h('div', { class: 'panel-head' }, h('h3', null, 'Steps per specializer'),
    h('label', { class: 'hint' }, h('input', { type: 'checkbox', checked: log, onchange: e => { P.barLog = e.target.checked; panel.replaceChildren(); renderStepBars(panel, b, runs, orig, best); } }), ' log scale'))]);
  const W = Math.max(320, panel.clientWidth - 30);
  const labelW = Math.min(170, Math.max(90, ...runs.map(r => r.spec.length * 7 + 10)));
  const valW = 110, rowH = 20, top = 4, plotW = Math.max(60, W - labelW - valW);
  const Hh = top + runs.length * rowH + 24;
  const x = v => {
    if (log) { const lo = 0, hi = Math.log10(maxS * 1.05); return clamp((Math.log10(Math.max(1, v)) - lo) / (hi - lo || 1), 0, 1) * plotW; }
    return clamp(v / maxS, 0, 1) * plotW;
  };
  const svg = s('svg', { width: W, height: Hh });
  const defs = s('defs', null, s('pattern', { id: 'hatch', width: 6, height: 6, patternUnits: 'userSpaceOnUse', patternTransform: 'rotate(45)' },
    s('rect', { width: 6, height: 6, style: { fill: 'var(--critical)', fillOpacity: 0.18 } }),
    s('line', { x1: 0, y1: 0, x2: 0, y2: 6, style: { stroke: 'var(--critical)', strokeWidth: 2 } })));
  svg.appendChild(defs);
  // gridlines / ticks
  const ticks = log ? [] : [0, 0.25, 0.5, 0.75, 1].map(f => f * maxS);
  if (log) for (let p = 0; Math.pow(10, p) <= maxS * 1.05; p++) ticks.push(Math.pow(10, p));
  const axis = s('g', { class: 'axis' });
  for (const t of ticks) {
    const tx = labelW + x(t);
    axis.appendChild(s('line', { class: 'gridline', x1: tx, x2: tx, y1: top, y2: top + runs.length * rowH }));
    axis.appendChild(s('text', { x: tx, y: top + runs.length * rowH + 14, 'text-anchor': 'middle' }, fmtCompact(t)));
  }
  svg.appendChild(axis);
  runs.forEach((r, i) => {
    const y = top + i * rowH;
    const g = s('g', { class: 'row' });
    g.appendChild(s('text', { class: 'lbl mono', x: labelW - 8, y: y + rowH / 2 + 4, 'text-anchor': 'end' }, r.spec));
    let valText;
    if (isErr(r)) valText = 'error';
    else {
      const wBar = Math.max(2, x(r.steps));
      const fill = isFuel(r) ? 'url(#hatch)' : r.spec === 'original' ? 'var(--muted)' : 'var(--s1)';
      g.appendChild(s('rect', { class: 'bar', x: labelW, y: y + 3, width: wBar, height: rowH - 6, rx: 3, style: { fill } }));
      const rt = ratioOf(r, orig);
      valText = isFuel(r) ? '∞ (fuel)' : fmtInt(r.steps) + (r.spec !== 'original' && orig ? '  ' + ratioText(rt) : '');
      if (best && r === best) valText += '  ★';
    }
    g.appendChild(s('text', { x: labelW + (isErr(r) ? 0 : Math.max(2, x(r.steps))) + 6, y: y + rowH / 2 + 4, class: badCheck(r.check) ? 'lbl' : '' }, valText + (badCheck(r.check) ? '  ⚠ ' + r.check : '')));
    const hit = s('rect', { class: 'hit', x: 0, y, width: W, height: rowH });
    g.insertBefore(hit, g.firstChild);
    tip(g, () => ({ title: r.spec, rows: [`steps ${isFuel(r) ? '≥ ' : ''}${fmtInt(r.steps)} (unifs ${fmtInt(r.unifs)}, calls ${fmtInt(r.calls)})`,
      `outcome ${r.outcome} · check ${r.check}`, `${r.answers.length} answer(s)`, orig && r.spec !== 'original' ? 'vs original ' + ratioText(ratioOf(r, orig)) : ''].filter(Boolean) }));
    svg.appendChild(g);
  });
  panel.appendChild(h('div', { class: 'chart', style: { overflowX: 'auto' } }, svg));
  panel.appendChild(h('div', { class: 'legend', style: { marginTop: '4px' } },
    h('span', { class: 'item' }, h('span', { class: 'sw', style: { background: 'var(--muted)' } }), 'original'),
    h('span', { class: 'item' }, h('span', { class: 'sw', style: { background: 'var(--s1)' } }), 'specialized'),
    h('span', { class: 'item' }, h('span', { class: 'sw', style: { background: 'var(--critical)', opacity: 0.5 } }), 'out of fuel (bar = fuel spent)')));
}

/* ---- (c) answer timeline ---- */
const MAX_SERIES = 8;
function renderTimeline(panel, b, q, runs, best) {
  const ui = state.ui, P = state.prefs;
  const key = q.id;
  ui.tl = ui.tl || {};
  // color slots follow the entity: a spec keeps its slot while selected
  if (!ui.tl[key]) {
    const init = runs.filter(r => r.spec !== 'original');
    const pick = init.length <= 4 ? init : [best, ...init.filter(isSolved).sort((a, c) => a.steps - c.steps)].filter(Boolean);
    const slots = new Map();
    for (const r of pick) { if (slots.size >= 3) break; if (!slots.has(r.spec)) slots.set(r.spec, slots.size + 1); }
    ui.tl[key] = slots;
  }
  const slots = ui.tl[key];
  for (const sname of [...slots.keys()]) if (!runs.some(r => r.spec === sname)) slots.delete(sname);
  const colorOf = sp => sp === 'original' ? 'var(--ink-2)' : `var(--s${slots.get(sp)})`;
  const shown = runs.filter(r => r.spec === 'original' || slots.has(r.spec));

  const maxS = Math.max(1, ...shown.map(r => r.steps));
  const minS = Math.max(1, Math.min(...shown.map(r => Math.max(1, r.answerSteps[0] || r.steps))));
  const log = P.tlLog === null ? maxS / minS > 50 : P.tlLog;
  const rerender = () => { panel.replaceChildren(); renderTimeline(panel, b, q, runs, best); };
  append(panel, [h('div', { class: 'panel-head' }, h('h3', null, 'Answer timeline'),
    h('label', { class: 'hint' }, h('input', { type: 'checkbox', checked: log, onchange: e => { P.tlLog = e.target.checked; rerender(); } }), ' log steps'))]);
  const pickBox = h('div', { class: 'series-pick' });
  for (const r of runs) {
    const on = r.spec === 'original' || slots.has(r.spec);
    const full = !on && slots.size >= MAX_SERIES;
    const cb = h('input', { type: 'checkbox', checked: on, disabled: r.spec === 'original' || full,
      onchange: e => {
        if (e.target.checked) { for (let k = 1; k <= MAX_SERIES; k++) if (![...slots.values()].includes(k)) { slots.set(r.spec, k); break; } }
        else slots.delete(r.spec);
        rerender();
      } });
    pickBox.appendChild(h('label', { title: full ? `at most ${MAX_SERIES} series` : '' }, cb,
      h('span', { class: 'sw', style: { background: on ? colorOf(r.spec) : 'transparent', outline: on ? '' : '1px solid var(--border)' } }), h('span', { class: 'mono' }, r.spec)));
  }
  panel.appendChild(pickBox);

  const W = Math.max(320, panel.clientWidth - 30), Hh = 260;
  const m = { l: 40, r: shown.length <= 4 ? 110 : 16, t: 10, b: 30 };
  const pw = W - m.l - m.r, ph = Hh - m.t - m.b;
  const maxA = Math.max(1, ...shown.map(r => r.answers.length || r.answerSteps.length));
  const lo = log ? 1 : 0, hiV = maxS * 1.02;
  const xs = v => m.l + (log ? (Math.log10(Math.max(1, v)) / Math.log10(Math.max(hiV, 10))) : (v - lo) / (hiV - lo)) * pw;
  const ys = a => m.t + ph - (a / maxA) * ph;
  const svg = s('svg', { width: W, height: Hh });
  // axes & grid
  const g = s('g', { class: 'axis' });
  const yt = maxA <= 10 ? [...Array(maxA + 1).keys()] : [0, 0.25, 0.5, 0.75, 1].map(f => Math.round(f * maxA));
  for (const a of yt) { g.appendChild(s('line', { class: 'gridline', x1: m.l, x2: m.l + pw, y1: ys(a), y2: ys(a) })); g.appendChild(s('text', { x: m.l - 6, y: ys(a) + 4, 'text-anchor': 'end' }, String(a))); }
  const xt = [];
  if (log) for (let p = 0; Math.pow(10, p) <= hiV; p++) xt.push(Math.pow(10, p));
  else for (let i = 0; i <= 4; i++) xt.push(lo + (hiV - lo) * i / 4);
  for (const t of xt) g.appendChild(s('text', { x: xs(t), y: m.t + ph + 16, 'text-anchor': 'middle' }, fmtCompact(t)));
  g.appendChild(s('line', { x1: m.l, x2: m.l + pw, y1: m.t + ph, y2: m.t + ph, style: { stroke: 'var(--muted)' } }));
  g.appendChild(s('text', { x: m.l + pw, y: Hh - 2, 'text-anchor': 'end' }, 'steps'));
  g.appendChild(s('text', { x: 4, y: m.t + 4 }, 'answers'));
  svg.appendChild(g);
  // series: step lines; original drawn first (underneath)
  const ends = [];
  for (const r of shown) {
    const pts = [[log ? 1 : 0, 0]];
    r.answerSteps.forEach((st, i) => { pts.push([st, i]); pts.push([st, i + 1]); });
    const end = Math.max(r.steps, r.answerSteps.length ? r.answerSteps[r.answerSteps.length - 1] : 0);
    pts.push([end, r.answerSteps.length]);
    const d = pts.map((p, i) => (i ? 'L' : 'M') + xs(p[0]).toFixed(1) + ',' + ys(p[1]).toFixed(1)).join('');
    svg.appendChild(s('path', { class: 'series', d, style: { stroke: colorOf(r.spec), strokeDasharray: r.spec === 'original' ? '5 3' : null } }));
    const ex = xs(end), ey = ys(r.answerSteps.length);
    svg.appendChild(isFuel(r)
      ? s('text', { x: ex, y: ey - 4, 'text-anchor': 'middle', style: { fill: colorOf(r.spec), fontWeight: 700 } }, '∞')
      : s('circle', { cx: ex, cy: ey, r: 4, style: { fill: colorOf(r.spec), stroke: 'var(--surface)', strokeWidth: 2 } }));
    ends.push({ r, ex, ey });
  }
  // direct labels when few series (nudged apart vertically)
  if (shown.length <= 4) {
    ends.sort((a, c) => a.ey - c.ey);
    let lastY = -Infinity;
    for (const e of ends) { const y = Math.max(e.ey + 4, lastY + 13); lastY = y; svg.appendChild(s('text', { class: 'mono', x: m.l + pw + 8, y }, e.r.spec)); }
  }
  // crosshair + tooltip
  const cross = s('line', { class: 'crosshair', y1: m.t, y2: m.t + ph, visibility: 'hidden' });
  svg.appendChild(cross);
  const hit = s('rect', { class: 'hit', x: m.l, y: m.t, width: pw, height: ph });
  hit.dataset.tipLive = '1';
  const inv = px => log ? Math.pow(10, (px - m.l) / pw * Math.log10(Math.max(hiV, 10))) : lo + (px - m.l) / pw * (hiV - lo);
  tip(hit, e => {
    const rect = svg.getBoundingClientRect();
    const px = clamp(e.clientX - rect.left, m.l, m.l + pw);
    cross.setAttribute('x1', px); cross.setAttribute('x2', px); cross.setAttribute('visibility', 'visible');
    const at = inv(px);
    return { title: `at step ${fmtInt(at)}`, rows: shown.map(r => {
      const n = r.answerSteps.filter(st => st <= at).length;
      const done = at >= r.steps ? (isFuel(r) ? ' (out of fuel)' : ' (done)') : '';
      return { text: `${r.spec}: ${n} answer${n === 1 ? '' : 's'}${done}`, mono: true };
    }) };
  });
  hit.addEventListener('mouseleave', () => cross.setAttribute('visibility', 'hidden'));
  svg.appendChild(hit);
  panel.appendChild(h('div', { class: 'chart' }, svg));
  panel.appendChild(h('div', { class: 'hint' }, 'Cumulative answers vs. steps. Dashed = original. ● search finished, ∞ fuel exhausted. Checkboxes add/remove specializers (max 8).'));
}

/* ---- (d) per-relation profile ---- */
function renderProfiles(panel, b, q, runs, best) {
  const ui = state.ui;
  const names = runs.map(r => r.spec);
  if (!ui.profA || !names.includes(ui.profA)) ui.profA = names.includes('original') ? 'original' : names[0];
  if (!ui.profB || !names.includes(ui.profB)) ui.profB = (best && best.spec) || names.find(n => n !== ui.profA) || names[0];
  const mkSel = key => h('select', { onchange: e => { ui[key] = e.target.value; panel.replaceChildren(); renderProfiles(panel, b, q, runs, best); } },
    names.map(n => h('option', { value: n, selected: n === ui[key] }, n)));
  const showAll = !!ui.profAll;
  append(panel, [h('div', { class: 'panel-head' }, h('h3', null, 'Where the effort goes (per relation)'),
    h('div', { class: 'toolbar' }, mkSel('profA'), 'vs', mkSel('profB'),
      h('label', { class: 'hint' }, h('input', { type: 'checkbox', checked: showAll, onchange: e => { ui.profAll = e.target.checked; panel.replaceChildren(); renderProfiles(panel, b, q, runs, best); } }), ' all relations'))),
    h('div', { class: 'legend', style: { marginBottom: '8px' } },
      h('span', { class: 'item' }, h('span', { class: 'sw', style: { background: 'var(--s1)' } }), 'calls'),
      h('span', { class: 'item' }, h('span', { class: 'sw', style: { background: 'var(--s3)' } }), 'unifications ok'),
      h('span', { class: 'item' }, h('span', { class: 'sw', style: { background: 'var(--s2)' } }), 'unifications failed'),
      h('span', { class: 'hint' }, ' · shared scale; unifications are attributed to the relation whose body performed them; <query> = top-level goal'))]);
  const rA = runs.find(r => r.spec === ui.profA), rB = runs.find(r => r.spec === ui.profB);
  const cost = p => (p.calls || 0) + (p.unifOk || 0) + (p.unifFail || 0);
  const profs = [rA, rB].map(r => (r ? r.profile : []).slice().sort((a, c) => cost(c) - cost(a)));
  const max = Math.max(1, ...profs.flat().map(cost));
  const cols = h('div', { class: 'prof-cols' });
  [rA, rB].forEach((r, i) => {
    const col = h('div');
    if (!r) { col.appendChild(h('div', { class: 'msg' }, 'no run')); cols.appendChild(col); return; }
    const total = profs[i].reduce((a, p) => a + cost(p), 0);
    col.appendChild(h('div', { class: 'prof-total' }, h('b', { class: 'mono' }, r.spec), ` — ${fmtInt(r.steps)} steps${isFuel(r) ? ' (out of fuel)' : ''}, ${profs[i].length} relations`,
      total && Math.abs(total - r.steps) > 0 ? h('span', { class: 'hint' }, ` (profile sums to ${fmtInt(total)})`) : null));
    if (!profs[i].length) col.appendChild(h('div', { class: 'hint' }, 'no profile data'));
    const list = showAll ? profs[i] : profs[i].slice(0, 20);
    for (const p of list) {
      const seg = (v, c) => v > 0 ? h('div', { class: 'seg', style: { width: Math.max(1, v / max * 100) + '%', background: c } }) : null;
      const bars = h('div', { class: 'bars' }, seg(p.calls || 0, 'var(--s1)'), seg(p.unifOk || 0, 'var(--s3)'), seg(p.unifFail || 0, 'var(--s2)'),
        h('span', { class: 'val' }, fmtCompact(cost(p))));
      const row = h('div', { class: 'prof-row' }, h('span', { class: 'rel', title: p.rel }, p.rel), bars);
      tip(row, () => ({ title: str(p.rel), rows: [`calls ${fmtInt(p.calls)}`, `unifications ok ${fmtInt(p.unifOk)}`, `unifications failed ${fmtInt(p.unifFail)}`,
        `total ${fmtInt(cost(p))} (${r.steps ? (cost(p) / r.steps * 100).toFixed(1) : '?'}% of steps)`] }));
      col.appendChild(row);
    }
    if (!showAll && profs[i].length > 20) col.appendChild(h('div', { class: 'hint' }, `… ${profs[i].length - 20} more`));
    cols.appendChild(col);
  });
  panel.appendChild(cols);
}

/* ---- (e) trace viewer / player ---- */
const EV_CLASS = { call: 'call', 'unif+': 'unif-ok', 'unif-': 'unif-fail', answer: 'answer' };
const EV_NAME = { call: 'call', 'unif+': 'unify ✓', 'unif-': 'unify ✗', answer: 'answer' };

function renderTrace(panel, b, q, runs, best) {
  const ui = state.ui;
  const names = runs.map(r => r.spec);
  if (!ui.traceA || !names.includes(ui.traceA)) ui.traceA = names.includes('original') ? 'original' : names[0];
  if (!ui.traceB || !names.includes(ui.traceB)) ui.traceB = (best && best.spec) || names.find(n => n !== ui.traceA) || names[0];
  const player = { cursor: 0, timer: null, speed: ui.traceSpeed || 12 };
  const lanes = [];
  const maxLen = () => Math.max(0, ...lanes.map(l => l.run ? l.run.trace.length : 0));

  const playBtn = h('button', { onclick: () => (player.timer ? stop() : play()) }, '▶ Play');
  const scrub = h('input', { type: 'range', min: 0, max: 0, value: 0, style: { width: '220px' }, oninput: e => { stop(); setCursor(+e.target.value); } });
  const speed = h('input', { type: 'range', min: 1, max: 80, value: player.speed, style: { width: '110px' },
    oninput: e => { player.speed = ui.traceSpeed = +e.target.value; speedLbl.textContent = player.speed + ' ev/s'; if (player.timer) { stop(); play(); } } });
  const speedLbl = h('span', { class: 'hint' }, player.speed + ' ev/s');
  const posLbl = h('span', { class: 'hint mono' });

  function stop() { if (player.timer) clearInterval(player.timer); player.timer = null; playBtn.textContent = '▶ Play'; }
  function play() {
    if (player.cursor >= maxLen()) setCursor(0);
    playBtn.textContent = '❚❚ Pause';
    player.timer = setInterval(() => {
      if (player.cursor >= maxLen()) { stop(); return; }
      setCursor(player.cursor + 1);
    }, 1000 / player.speed);
  }
  state.cleanup.push(stop);

  function setCursor(c) {
    player.cursor = clamp(c, 0, maxLen());
    scrub.value = player.cursor;
    posLbl.textContent = `event ${player.cursor} / ${maxLen()}`;
    for (const l of lanes) l.update(player.cursor);
  }

  append(panel, [
    h('div', { class: 'panel-head' }, h('h3', null, 'Trace: watch the search run'),
      h('div', { class: 'toolbar' }, playBtn,
        h('button', { title: 'step back', onclick: () => { stop(); setCursor(player.cursor - 1); } }, '◀'),
        h('button', { title: 'step forward', onclick: () => { stop(); setCursor(player.cursor + 1); } }, '▶'),
        h('button', { title: 'reset', onclick: () => { stop(); setCursor(0); } }, '⟲'),
        h('button', { title: 'show everything', onclick: () => { stop(); setCursor(maxLen()); } }, 'End'),
        scrub, posLbl, h('span', { class: 'sep' }), 'speed', speed, speedLbl)),
    h('div', { class: 'legend', style: { marginBottom: '8px' } },
      h('span', { class: 'item' }, h('span', { class: 'sw', style: { background: 'var(--ev-call)' } }), 'call'),
      h('span', { class: 'item' }, h('span', { class: 'sw', style: { background: 'var(--ev-ok)' } }), 'unification succeeded'),
      h('span', { class: 'item' }, h('span', { class: 'sw', style: { background: 'var(--ev-fail)' } }), 'unification failed'),
      h('span', { class: 'item' }, h('span', { class: 'sw', style: { background: 'var(--ev-answer)', boxShadow: 'inset 0 0 0 1.5px var(--ink)' } }), 'answer'),
      h('span', { class: 'hint' }, ' · both lanes advance one event per tick; click a cell to jump there')),
  ]);

  for (const key of ['traceA', 'traceB']) {
    const lane = makeLane(key);
    lanes.push(lane);
    panel.appendChild(lane.el);
  }
  scrub.max = maxLen();
  // open at the end of the recorded trace so the counters show the trace totals;
  // Play restarts from event 0 (see play())
  setCursor(maxLen());

  function makeLane(key) {
    const run = runs.find(r => r.spec === ui[key]);
    const sel = h('select', { onchange: e => { stop(); ui[key] = e.target.value; panel.replaceChildren(); renderTrace(panel, b, q, runs, best); } },
      names.map(n => h('option', { value: n, selected: n === ui[key] }, n)));
    const counters = h('div', { class: 'counters' });
    const strip = h('div', { class: 'strip' });
    const evlog = h('div', { class: 'evlog' });
    const tail = h('div', { class: 'tail' });
    const el = h('div', { class: 'lane' }, h('div', { class: 'lane-head' }, sel, counters), strip, evlog, tail);
    const lane = { el, run, update: () => {} };
    if (!run) { strip.appendChild(h('span', { class: 'hint' }, 'no run')); return lane; }
    const tr = run.trace;
    // prefix sums for the counters
    const pre = [{ calls: 0, ok: 0, fail: 0, ans: 0 }];
    tr.forEach(ev => {
      const p = Object.assign({}, pre[pre.length - 1]);
      const t = Array.isArray(ev) ? ev[0] : '';
      if (t === 'call') p.calls++; else if (t === 'unif+') p.ok++; else if (t === 'unif-') p.fail++; else if (t === 'answer') p.ans++;
      pre.push(p);
    });
    const cells = tr.map((ev, i) => {
      const t = Array.isArray(ev) ? str(ev[0]) : '?', a = Array.isArray(ev) ? str(ev[1]) : '';
      const c = h('span', { class: 'ev ' + (EV_CLASS[t] || 'other') });
      tip(c, () => ({ title: `#${i + 1} ${EV_NAME[t] || t}`, rows: [{ text: t === 'answer' ? 'answer ' + a + (run.answers[+a - 1] ? ': ' + run.answers[+a - 1] : '') : a, mono: true }] }));
      c.addEventListener('click', () => { stop(); setCursor(i + 1); });
      strip.appendChild(c);
      return c;
    });
    if (!tr.length) strip.appendChild(h('span', { class: 'hint' }, 'no trace events exported'));
    let prev = -1;
    lane.update = cur => {
      const n = Math.min(cur, tr.length);
      // incremental class update between previous and current cursor
      if (prev < 0) cells.forEach((c, i) => c.classList.toggle('future', i >= n));
      else for (let i = Math.min(prev, n); i < Math.max(prev, n); i++) cells[i].classList.toggle('future', i >= n);
      if (prev > 0 && cells[Math.min(prev, tr.length) - 1]) cells[Math.min(prev, tr.length) - 1].classList.remove('cur');
      if (n > 0) cells[n - 1].classList.add('cur');
      prev = n;
      const p = pre[n];
      counters.replaceChildren(
        h('span', { class: 'hint' }, `after event ${n} of ${tr.length}${run.traceTruncated ? ' recorded' : ''}:`),
        h('span', null, 'steps ', h('b', null, fmtInt(p.calls + p.ok + p.fail))),
        h('span', null, 'calls ', h('b', null, fmtInt(p.calls))),
        h('span', null, 'unif ✓ ', h('b', null, fmtInt(p.ok))),
        h('span', null, 'unif ✗ ', h('b', null, fmtInt(p.fail))),
        h('span', null, 'answers ', h('b', null, String(p.ans))),
        h('span', { class: 'hint' }, `run total: ${isFuel(run) ? '≥' : ''}${fmtInt(run.steps)} steps, ${run.answers.length} answers, ${run.outcome}`));
      evlog.replaceChildren(...tr.slice(Math.max(0, n - 7), n).map((ev, j, arr) => {
        const t = str(ev && ev[0]), a = str(ev && ev[1]);
        return h('div', { class: j === arr.length - 1 ? 'last' : '' }, h('span', { class: 'k' }, EV_NAME[t] || t), t === 'answer' ? `#${a} ${run.answers[+a - 1] || ''}` : a);
      }));
      if (n >= tr.length && tr.length) {
        tail.replaceChildren(run.traceTruncated
          ? h('span', { class: 'flag-bad' }, `… trace truncated after ${tr.length} events (whole run: ${isFuel(run) ? '≥' : ''}${fmtInt(run.steps)} steps)`)
          : h('span', { class: 'hint' }, `end of search (${run.outcome})`));
      } else tail.replaceChildren(run.traceTruncated ? h('span', { class: 'hint' }, `trace shows the first ${tr.length} events`) : '');
    };
    return lane;
  }
}

/* ======================================================================
 * Boot
 * ==================================================================== */

async function boot() {
  const info = document.getElementById('data-info');
  try {
    const idx = await fetchJSON(DATA_DIR + 'index.json');
    idx.benches = Array.isArray(idx.benches) ? idx.benches.filter(b => b && b.name) : [];
    // keep groups contiguous (stable within a group)
    const gOrder = [];
    for (const b of idx.benches) if (!gOrder.includes(b.group || 'other')) gOrder.push(b.group || 'other');
    idx.benches = idx.benches.map((b, i) => ({ b, i })).sort((x, y) => gOrder.indexOf(x.b.group || 'other') - gOrder.indexOf(y.b.group || 'other') || x.i - y.i).map(x => x.b);
    state.index = idx;
    for (const b of idx.benches) state.benchMeta.set(b.name, b);
    info.textContent = `${idx.benches.length} benchmarks · ${DATA_DIR.replace(/\/$/, '')}/`;
  } catch (e) {
    state.indexError = String(e.message || e);
    info.textContent = 'no data';
  }
  restoreViewState();
  window.addEventListener('hashchange', onRoute);
  onRoute();
}
boot();
