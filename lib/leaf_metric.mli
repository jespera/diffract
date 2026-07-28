(** Token-level edit metric over tree-sitter leaf streams.

    The change-summary safety gate states its property as a metric equation
    (design §2.3): with [t'' = apply(rule, t)], [d(t,t'') + d(t'',t') = d(t,t')]
    — the rule's step plus the residual's step compose to the site's change with
    no detour. This module supplies the [d]: a file state is its stream of
    tree-sitter leaf texts (the matcher's own alphabet), and [d] is LCS edit
    distance — the minimum number of token insertions plus deletions
    transforming one stream into the other. That is a genuine metric (symmetric,
    triangle inequality, zero iff equal streams), unlike [Tree_diff]'s heuristic
    script sizes, and it is cheap enough for the gate's eval loop via Myers'
    O(ND) greedy algorithm — cost scales with the distance, and gate
    intermediates are close to both endpoints by construction.

    Whitespace between tokens is not a leaf, so the metric is formatting-blind
    by construction — two layouts of the same code are at distance 0. Comments
    {e are} leaves and do count: they are file content, and a rule that rewrites
    one must answer for it.

    Geodesic betweenness is deliberately weaker than tree inclusion in one
    direction and stronger in another: it admits an intra-node partial step (a
    single leaf flip inside a bigger change — a relabel, which inclusion calls a
    detour), while rejecting same-position delete-then-readd (each re-added
    token pays twice), which inclusion blessed as "pure insertion" and only
    [net_progress] caught. A {e moved} token's delete/readd is on the geodesic,
    though — policing wasted-but-metric-neutral work remains [net_progress]'s
    job. *)

(** A file state for the metric: its leaf texts in document order. *)
type stream = string array

(** [leaves ~source root] — the leaf (childless-node) texts of the tree in
    document order. Zero-width "missing" leaves (tree-sitter error-recovery
    phantoms) are skipped; extras (comments) are kept. *)
val leaves : source:string -> 'a Tree.t -> stream

(** LCS edit distance between two streams. Myers O(ND): linear in the stream
    lengths when the streams are close, O((n+m)·D) in general. *)
val distance : stream -> stream -> int

(** [distance_upto ~bound a b] is [Some (distance a b)] when that distance is at
    most [bound], else [None] — the search is cut off at [bound], so a far-apart
    pair costs O((n+m)·bound) instead of the full distance. *)
val distance_upto : bound:int -> stream -> stream -> int option

(** Is [mid] on a geodesic between [before] and [after] — does
    [distance before mid + distance mid after = distance before after] hold?
    [d_endpoints], when given, must equal [distance before after] (the gate
    caches it per site across the many candidates it evaluates there). Both
    legs' searches are cut off by the triangle inequality —
    [d(before,mid) ≤ d(before,after)] and
    [d(mid,after) = d(before,after) − d(before,mid)] on a geodesic — so an
    off-geodesic [mid] is rejected without ever computing a full far-pair
    distance. *)
val geodesic :
  ?d_endpoints:int ->
  before:stream ->
  mid:stream ->
  after:stream ->
  unit ->
  bool
