# References and existing work

The papers and ideas diffract draws on, and where each one is used in the
implementation. Entries are grouped by subsystem; for the change-summary
entries, the design doc's related-work section
([change-summary-design.md §10](change-summary-design.md)) carries the
deeper technical comparisons — this file is the repo-wide index.

## Parsing and pattern language

- **tree-sitter** — M. Brunsfeld et al.,
  <https://tree-sitter.github.io/tree-sitter/>.
  The parsing foundation. Grammars are statically linked
  (`grammars/build-grammars.sh`, `lib/languages.ml`); a thin C layer
  (`lib/tree_sitter_helper.c`) wraps `TSNode`/`TSTree` in OCaml custom
  blocks, and parses are converted once into a pure OCaml tree
  (`lib/tree.ml`) so traversal pays no FFI cost. The matcher also uses
  tree-sitter in an unusual role: as a *lexer* for pattern bodies
  (`lib/tokenize.ml`, see
  [universal-tokenizer.md](universal-tokenizer.md)).

- **Coccinelle / SmPL** — Y. Padioleau, J. Lawall, R. R. Hansen,
  G. Muller, *Documenting and automating collateral evolutions in Linux
  device drivers* (EuroSys 2008).
  The model for diffract's pattern/transform language: `@@`-delimited
  preambles, `-`/`+` edit lines with context, metavariables, `...`
  ellipses ([patterns.md](patterns.md), [transforms.md](transforms.md)).
  Diffract's dialect is "not-really semantic patches": matching is
  syntactic over tree-sitter parses (no control-flow reasoning), in
  exchange for working across many languages without per-language
  frontends. Coccinelle's SmPL is also the *output* language of the
  change-summary feature — inferred rules are diffract patterns.

## Tree diffing

- **GumTree** — J.-R. Falleri, F. Morandat, X. Blanc, M. Martinez,
  M. Monperrus, *Fine-grained and accurate source code differencing*
  (ASE 2014).
  The node-mapping algorithm behind `lib/tree_diff.ml`
  (`Tree_diff.compute_mapping`): match identical subtrees top-down by
  hash, then extend the mapping bottom-up. `summarize` derives all of
  its change pairs from this diff. Known sensitivity inherited with it:
  reordered siblings are matched by content and reported *unchanged*,
  so pure reorders (argument swaps) are invisible to clustering and
  fall to residuals.

- **Merkle / structural hashing** — used throughout rather than cited
  from one source: `Tree.hash` is a position-independent structural
  digest per subtree, serving as GumTree's subtree-equality oracle, as
  hdiff's `wcs` ("which common subtree") oracle in cross-side
  extraction, and as the containment test in proposal-side coarsening.
  truediff (below) independently validates deciding tree equivalence by
  hash.

- **Ordered tree inclusion** — P. Kilpeläinen, H. Mannila, *Ordered and
  unordered tree inclusion* (SIAM J. Comput. 24(2), 1995).
  `lib/tree_inclusion.ml`: can one tree be obtained from another by
  deleting nodes, where deleting an internal node promotes its children
  (a genuine embedding, not a contiguous-subtree match)? Implemented as
  their memoised forest recursion, keyed on head-node pairs. Used by
  the change-summary safety gate to classify a rule's leftover gap:
  inclusion-comparable (pure insertion or pure deletion — an honest
  residual) versus a relabel (a detour the rule must not claim).
  Ordered inclusion is polynomial; unordered is NP-complete and
  deliberately out of scope.

- **Linear-space tree inclusion** — P. Bille, I. L. Gørtz, *The Tree
  Inclusion Problem: In Linear Space and Faster* (arXiv cs/0608124).
  Not implemented; cited in `lib/tree_inclusion.ml` as the upgrade path
  (O(n_T) space via pre-order arrays) should inclusion ever run over
  whole files on a hot path. The current quadratic scheme is adequate
  for the gate's inputs.

- **Tree edit distance family** — P. Bille, *A survey on tree edit
  distance and related problems* (Theor. Comput. Sci. 337, 2005);
  K. Zhang, D. Shasha, *Simple fast algorithms for the editing distance
  between trees and related problems* (SIAM J. Comput. 18(6), 1989).
  The map this project's "geodesic" tests live in: inclusion is the
  delete-only specialization of tree edit distance. The recorded
  upgrade path for the safety gate — when inclusion's conservatism
  starts costing coverage (relabel-bearing or mixed insert+delete
  residuals) — is exact Zhang–Shasha TED on changed-region subtrees,
  where the geodesic equality `d(b,a) = d(b,t'') + d(t'',a)` can be
  computed exactly. Approximate distances cannot back that equality
  test (an early node-count proxy failed exactly this way); see the
  design doc's M1.9b milestone notes.

## Change-summary inference (`summarize`)

- **Generic Patch Inference / spdiff** — J. Andersen, J. L. Lawall,
  *Generic Patch Inference* (ASE 2008; Autom. Softw. Eng. 17(2), 2010).
  The closest ancestor of the whole feature. Three ideas transfer
  directly: the **safety property** (their one-step-reachability
  Defs. 5–6 is the design's §2.3 geodesic condition — a patch may only
  make changes that are part of the change), the **subpatch partial
  order** (their Def. 8; realised in selection's subsumption behaviour),
  and **compactness** (their "largest common" criterion; echoed in the
  net-progress guard and the set-cover ranking). The decisive
  difference: spdiff requires one patch safe for *every* input pair, so
  one odd pair empties the result; diffract instead *discovers*
  per-rule file sets — clustering proposes, the per-site gate sheds
  unsafe sites, `min_support` realises their proposed frequency
  threshold, and residuals account for what their setting silently
  drops. Their sequential composition `gp1; gp2` is computed
  constructively here as tiered rules over residuals (M2).

- **Getafix** — J. Bader, A. Scott, M. Pradel, S. Chandra, *Getafix:
  learning to fix bugs automatically* (OOPSLA 2019).
  The clustering machinery: anti-unification with memoised hole
  assignment (`mk_anti_unify` in `lib/change_summary.ml`) and
  agglomerative (dendrogram) clustering of change pairs with a
  coherence cut. Diffract uses the hierarchy as a candidate *proposer*
  only — rules' sites and support are re-derived by evaluation
  (design §3.3), not taken from cluster membership.

- **Anti-unification** — G. Plotkin, *A note on inductive
  generalization*; J. Reynolds, *Transformational systems and the
  algebraic structure of atomic formulas* (both Machine Intelligence 5,
  1970). The least-general-generalization underlying the clustering:
  two concrete subtrees generalize to a pattern with a metavariable at
  every position they disagree.

- **hdiff** — V. C. Miraldo, W. Swierstra, *An efficient algorithm for
  type-safe structural diffing* (ICFP 2019).
  Its change representation — a `(deletion-context, insertion-context)`
  pair sharing metavariables — *is* a diffract `-`/`+` rule, which is
  why its mechanisms transfer: metavariables assigned by *content* via
  the `wcs` oracle (`Tree.hash`) drive the cross-side extraction in
  §4.3 (`extraction_pairs`: a removed and an added sibling where one
  embeds in the other become a rewrite, e.g. `box($X).get() ⤳ $X`).
  Scoped deliberately: content-keying is used where positional
  anti-unification mis-aligns, not as a replacement for it.

- **truediff** — S. Erdweg, T. Szabó, A. Pacak, *Concise, type-safe,
  and efficient structural diffing* (PLDI 2021).
  Not implemented; the reference design for the deferred work of making
  residuals *applicable structured edits* (linearly-typed edit scripts
  with guaranteed well-formed intermediates) rather than textual
  unified diffs. Also independently confirms the hash-equivalence
  oracle.

## Where ideas were adapted rather than adopted

A few load-bearing pieces are home-grown, shaped by the papers above but
not found in them — recorded here so the provenance is honest:

- **Per-site safety shedding and discovered partitions** (design §3.1,
  §3.3): evaluating every candidate against every changed file and
  letting the safety gate determine each rule's file set — the answer
  to spdiff's all-pairs brittleness.
- **The two-leg geodesic gate** (design §2.3, M1.9b): tree inclusion
  for the residual leg plus a net-progress (compactness) guard for the
  rule leg, with a well-formedness precondition (a transform may not
  introduce parse errors absent from both endpoints).
- **Proposal-side orphan coarsening** (M1.9c): when anti-unification
  orphans an after-side hole (content with no before counterpart, e.g.
  a fresh dependency array), coarsen it to the instances' common
  embedded skeleton (`[]`) and let the gate claim sites decomposably —
  propose liberally, verify strictly.
- **Tiered factorization** (M2): re-running the whole
  propose/evaluate/select pipeline on the residuals of all rules
  globally, with per-site `after=` attribution — spdiff's `gp1; gp2`
  composition made constructive.
