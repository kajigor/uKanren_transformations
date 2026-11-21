# Data format of the specialization visualizer

`viz-export` (`app/viz/Main.hs`) writes `viz/data/index.json` plus one `viz/data/<bench>.json`
per benchmark. The front end (`viz/index.html`) only reads these files.

## `index.json`

```json
{ "generated": "2026-11-20T21:00:00Z",
  "fuel": 300000, "answers": 10,
  "benches": [ { "name": "doubleAppend", "group": "dppd", "file": "doubleAppend.json",
                 "queries": 3, "specializers": ["cpd-det", "..."] } ] }
```

## `<bench>.json`

```json
{
  "name": "doubleAppend",
  "group": "dppd",                       // directory the benchmark came from
  "source": "appendo x y xy = ...",      // the .mk file verbatim
  "runsSource": "? double_appendo ...",  // the .runs file verbatim ("" if none)
  "specQuery": "double_appendo x y z res",
  "fuel": 300000, "answers": 10,
  "queries": [ { "id": "q1", "goal": "double_appendo [A, B] ..." } ],

  "specializers": [
    { "name": "cpd-det",                 // "original" is included, with no tree
      "base": "cpd-det",                 // base specializer (variants: cpd-det+opt, ...)
      "specMs": 0.41,
      "error": null,                     // string when specialization failed / timed out
      "residual": "double_appendo y0 y1 y2 y3 = ...\n...",   // pretty-printed program
      "residualGoal": "double_appendo x0 x1 x2 x3",
      "defs": 3, "size": 17,             // #definitions, #goal nodes
      "globalTree": GNode | null,        // only for base specializers that have one
      "treeTruncated": false
    }
  ],

  "runs": [
    { "spec": "original", "query": "q1",
      "steps": 90, "unifs": 67, "calls": 23,          // steps = unifs + calls
      "outcome": "Finished",                          // Finished | GotEnough | OutOfFuel | error text
      "check": "ok",                                  // ok | ok~ | MISMATCH | unverified-* | -
      "answers": ["[A, B, C]"],                       // reified answers (strings)
      "answerSteps": [88],                            // step count at which each answer appeared
      "profile": [ { "rel": "appendo", "calls": 12, "unifOk": 30, "unifFail": 6 } ],
                                                      // per relation; unifications are attributed
                                                      // to the relation whose body performed them;
                                                      // "<query>" = the top-level goal
      "trace": [ ["call", "double_appendo"], ["unif+", "appendo"], ["unif-", "appendo"],
                 ["answer", "1"] ],                   // first ≤ 400 events of the search
      "traceTruncated": true
    }
  ]
}
```

### `GNode` — node of the global (CPD/ConsPD) tree

```json
{ "id": 0,
  "kind": "node" | "leaf" | "prune" | "truncated",
  "goal": "jo_solve [Jo_double_appendo v.0 v.1 v.2 v.3]",   // conjunction, " & "-separated
  "gen": "v.4 ↦ ...",            // generalizer (may be "")
  "subst": "v.0 ↦ ...",          // substitution on the edge from the parent (may be "")
  "local": SNode | null,         // local unfolding (SLD) tree of this node
  "children": [ GNode ] }
```

`leaf`: an instance/variant of an already processed conjunction (folded, no own residual
definition). `prune`: generalization point.

ConsPD trees (`conspd-*`) additionally use `"split"` (the conjunction was split into
independent parts, children are the parts; `goal` lists all of them), `"transient"`
(a deterministic pass-through node with one child), `"success"` and `"fail"`.

### `SNode` — node of a local unfolding tree

```json
{ "kind": "or" | "conj" | "leaf" | "success" | "fail" | "truncated",
  "goal": "appendo v.1 v.2 v.4",  // or: the unfolded call; conj/leaf: current goals; success/fail: ""
  "subst": "v.1 ↦ []",            // substitution at this node (may be "")
  "children": [ SNode ] }
```

Trees are cut at 1500 nodes per tree; a cut subtree is `{ "kind": "truncated", "children": [] }`.
