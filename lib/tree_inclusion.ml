(* Ordered tree inclusion (embedding). See tree_inclusion.mli.

   The recurrence is over ordered forests. [forest ps ts] asks whether the
   ordered forest [ps] embeds in the ordered forest [ts]:

     forest []        _          = true
     forest (_ :: _)  []         = false
     forest (p :: ps) (t :: ts)  =
          (eq p t && forest (kids p) (kids t) && forest ps ts)   (* match p ↔ t *)
       || forest (p :: ps) (kids t @ ts)                          (* delete t's root,
                                                                      promoting its
                                                                      children *)

   The first disjunct maps [p] onto [t] (and [p]'s children onto [t]'s).
   The second deletes [t]'s root, splicing its children in front of [t]'s
   siblings — this is what lets a [p] (or several) map into a *descendant*
   forest, i.e. the embedding (vs. contiguous-subtree) semantics. Without
   it, [\[b, d\]] would not embed in [\[a(b, d)\]].

   Memoization. Every reachable forest is determined by its *head* node:
   a sub-forest is always a within-parent sibling suffix (head, up to its
   parent's last child), and a sup-forest is always a pre-order tail (head
   plus the head's fixed right-context up the ancestors — the
   [kids t @ ts'] splice is exactly the pre-order tail from [t]'s first
   child). So the state [(ps, ts)] is keyed by the pair of head nodes,
   identified by [(node_type, start, end)] — unique within a tree (the key
   [Tree_diff] relies on). That gives O(n_P · n_T) memoized states; sub
   heads only ever sit in [ps] and sup heads only in [ts], so a key shared
   across the two trees cannot collide.

   This is Kilpeläinen–Mannila's quadratic scheme — adequate for the small
   changed-region subtrees the geodesic gate compares. It is *not* space
   optimal: Bille & Gørtz, "The Tree Inclusion Problem: In Linear Space
   and Faster" (arXiv cs/0608124), achieve O(n_T) space (and faster time)
   via a pre-order array representation rather than materialised forests —
   the path to take should inclusion ever run over whole files. *)

let included ~eq ~sub ~sup =
  let memo : ((string * int * int) * (string * int * int), bool) Hashtbl.t =
    Hashtbl.create 256
  in
  let key (n : 'a Tree.t) =
    (n.Tree.node_type, n.Tree.start_byte, n.Tree.end_byte)
  in
  let kids (n : 'a Tree.t) =
    List.map (fun (c : 'a Tree.child) -> c.Tree.node) n.Tree.children
  in
  let rec forest (ps : 'a Tree.t list) (ts : 'a Tree.t list) : bool =
    match (ps, ts) with
    | [], _ -> true
    | _ :: _, [] -> false
    | p :: ps', t :: ts' ->
        let k = (key p, key t) in
        (match Hashtbl.find_opt memo k with
        | Some b -> b
        | None ->
            let b =
              (eq p t && forest (kids p) (kids t) && forest ps' ts')
              || forest ps (kids t @ ts')
            in
            Hashtbl.replace memo k b;
            b)
  in
  forest [ sub ] [ sup ]

let node_label_eq ~sub_source ~sup_source (a : 'a Tree.t) (b : 'a Tree.t) =
  a.Tree.node_type = b.Tree.node_type
  &&
  if a.Tree.children = [] && b.Tree.children = [] then
    Tree.text sub_source a = Tree.text sup_source b
  else true

let included_src ~sub:(sub_source, sub) ~sup:(sup_source, sup) =
  included ~eq:(node_label_eq ~sub_source ~sup_source) ~sub ~sup
