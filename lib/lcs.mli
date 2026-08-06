(** Longest-common-subsequence edit script over two arrays. See {!Lcs} for why
    this is separate from {!Leaf_metric}'s distance-only LCS. *)

type 'a op = Keep of 'a | Remove of 'a | Add of 'a

(** [ops a b] is the edit script turning [a] into [b]: the elements of [a] and
    [b] in order, each tagged {!Keep} (in both), {!Remove} (in [a] only) or
    {!Add} (in [b] only). Keeps form a longest common subsequence.

    Ties are broken toward emitting {!Remove} before {!Add}, so a replaced run
    renders as its deletions followed by its insertions — the order a unified
    diff expects. O(n·m) time and space. *)
val ops : 'a array -> 'a array -> 'a op list
