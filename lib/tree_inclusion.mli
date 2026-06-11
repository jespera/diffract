(** Ordered tree inclusion (embedding) for AST nodes.

    [sub] is {e included} in [sup] when [sub] can be obtained from [sup] by
    deleting nodes — and deleting an {e internal} node promotes its children
    into its place, preserving left-to-right order (Kilpeläinen & Mannila,
    "Ordered and unordered tree inclusion", SIAM J. Comput. 1995). So inclusion
    is a genuine embedding, not a contiguous-subtree match:

    - [x] is included in [x + 1] (delete the [+ 1], promoting [x])
    - [[]] is included in [[b, c]] (insert elements)
    - [[b, d]] is included in [[a(b, d)]] (delete a wrapper, promote)
    - [g] is {e not} included in [h] (a relabel is not a deletion)

    Two nodes are deemed equal by the caller-supplied [eq] (matched on label).
    The change-summary geodesic gate uses this to decide whether a rule's output
    and a site's after differ only by pure insertion or pure deletion
    (decomposable) versus a relabel (a detour). *)

val included :
  eq:('a Tree.t -> 'a Tree.t -> bool) -> sub:'a Tree.t -> sup:'a Tree.t -> bool
(** [included ~eq ~sub ~sup] — is [sub] an ordered tree inclusion of [sup]? [eq]
    compares node labels (e.g. node type, plus source text for leaves).
    Memoized; polynomial in the two subtree sizes. *)

val node_label_eq :
  sub_source:string -> sup_source:string -> 'a Tree.t -> 'a Tree.t -> bool
(** The standard AST label equality: node types must match, and a pair of
    {e leaf} (childless) nodes must additionally have equal source text (so [x]
    and [y] differ, but [[]] still matches [[…]] on type).
    [sub_source]/[sup_source] are the source buffers of the two trees. *)

val included_src : sub:string * 'a Tree.t -> sup:string * 'a Tree.t -> bool
(** [included] with [node_label_eq]; each tree paired with its source. *)
