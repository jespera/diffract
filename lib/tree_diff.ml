(** Tree-level diffing with GumTree-inspired node matching. *)

(** {1 Types} *)

type node_key = string * int * int

type node_mapping = {
  forward : (node_key, node_key) Hashtbl.t;
  backward : (node_key, node_key) Hashtbl.t;
}

type node_change =
  | Unchanged
  | Modified of { child_changes : child_change list }
  | Replaced

and child_change =
  | Same of { node : Tree.src Tree.t }
  | Changed of {
      before : Tree.src Tree.t;
      after : Tree.src Tree.t;
      change : node_change;
    }
  | Removed of { node : Tree.src Tree.t }
  | Added of { node : Tree.src Tree.t }

type diff = {
  before_source : string;
  after_source : string;
  before_root : Tree.src Tree.t;
  after_root : Tree.src Tree.t;
  root_change : node_change;
  mapping : node_mapping;
}

type change_pair = {
  before_node : Tree.src Tree.t;
  after_node : Tree.src Tree.t;
  before_source : string;
  after_source : string;
}

(** {1 Node key helpers} *)

let key_of_node (n : Tree.src Tree.t) : node_key =
  (n.node_type, n.start_byte, n.end_byte)

let is_matched_before mapping (n : Tree.src Tree.t) =
  Hashtbl.mem mapping.forward (key_of_node n)

let is_matched_after mapping (n : Tree.src Tree.t) =
  Hashtbl.mem mapping.backward (key_of_node n)

let get_match_forward mapping (n : Tree.src Tree.t) =
  Hashtbl.find_opt mapping.forward (key_of_node n)

let add_match mapping (before_n : Tree.src Tree.t) (after_n : Tree.src Tree.t) =
  let bk = key_of_node before_n in
  let ak = key_of_node after_n in
  if not (Hashtbl.mem mapping.forward bk || Hashtbl.mem mapping.backward ak)
  then begin
    Hashtbl.replace mapping.forward bk ak;
    Hashtbl.replace mapping.backward ak bk
  end

(** {1 Step 1: Build hash indices} *)

(** Build a map from structural hash to list of nodes with that hash. *)
let build_hash_index (root : Tree.src Tree.t) =
  let index = Hashtbl.create 256 in
  Tree.traverse
    (fun (n : Tree.src Tree.t) ->
      let h = n.hash in
      let existing = try Hashtbl.find index h with Not_found -> [] in
      Hashtbl.replace index h (n :: existing))
    root;
  index

(** {1 Step 2: Match unique-hash subtrees} *)

(** Recursively match all descendants of two structurally identical subtrees by
    walking them in parallel. *)
let rec match_descendants mapping (before_n : Tree.src Tree.t)
    (after_n : Tree.src Tree.t) =
  add_match mapping before_n after_n;
  let before_children = before_n.named_children in
  let after_children = after_n.named_children in
  List.iter2
    (fun bn an -> match_descendants mapping bn an)
    before_children after_children

(** Match subtrees whose hash appears exactly once in each tree. *)
let match_unique_subtrees mapping ~before_source ~after_source before_index
    after_index =
  Hashtbl.iter
    (fun h before_nodes ->
      match (before_nodes, Hashtbl.find_opt after_index h) with
      | [ before_n ], Some [ after_n ] ->
          (* Unique in both trees — verify with full equality to handle
             hash collisions, then match the subtrees *)
          if
            (not (is_matched_before mapping before_n))
            && (not (is_matched_after mapping after_n))
            && Tree.equal before_source before_n after_source after_n
          then match_descendants mapping before_n after_n
      | _ -> ())
    before_index

(** {1 Step 3: Top-down alignment of children} *)

(** Count the number of descendants of a node (including itself). *)
let descendant_count (n : Tree.src Tree.t) =
  let count = ref 0 in
  Tree.traverse (fun _ -> incr count) n;
  !count

(** Dice similarity: fraction of node's descendants that are matched to
    descendants of the other node. Returns 0.0 to 1.0. *)
let dice_similarity mapping (before_n : Tree.src Tree.t)
    (after_n : Tree.src Tree.t) =
  let before_desc = descendant_count before_n in
  let after_desc = descendant_count after_n in
  if before_desc = 0 || after_desc = 0 then 0.0
  else
    let shared = ref 0 in
    Tree.traverse
      (fun (bn : Tree.src Tree.t) ->
        match get_match_forward mapping bn with
        | Some ak ->
            (* Check if the matched after-node is a descendant of after_n *)
            let _, as_byte, ae_byte = ak in
            if as_byte >= after_n.start_byte && ae_byte <= after_n.end_byte then
              incr shared
        | None -> ())
      before_n;
    let s = float_of_int !shared in
    2.0 *. s /. float_of_int (before_desc + after_desc)

(** Align children of a matched pair. Uses already-matched children as anchors,
    then matches remaining children by type + similarity. *)
let align_children mapping ~before_source ~after_source
    (before_children : Tree.src Tree.t list)
    (after_children : Tree.src Tree.t list) =
  let before_arr = Array.of_list before_children in
  let after_arr = Array.of_list after_children in
  let blen = Array.length before_arr in
  let alen = Array.length after_arr in
  let after_used = Array.make alen false in
  (* Result accumulator: (before_index option * after_index option) pairs *)
  let result = ref [] in
  (* Phase A: Find anchors — before-children that are matched to after-children *)
  let before_anchor = Array.make blen (-1) in
  for bi = 0 to blen - 1 do
    match get_match_forward mapping before_arr.(bi) with
    | Some ak ->
        (* Find which after-child this maps to *)
        let found = ref false in
        for ai = 0 to alen - 1 do
          if (not !found) && not after_used.(ai) then
            if key_of_node after_arr.(ai) = ak then begin
              before_anchor.(bi) <- ai;
              after_used.(ai) <- true;
              found := true
            end
        done
    | None -> ()
  done;
  (* Phase B: Match remaining by type + similarity *)
  let similarity_threshold = 0.4 in
  for bi = 0 to blen - 1 do
    if
      before_anchor.(bi) = -1 && not (is_matched_before mapping before_arr.(bi))
    then begin
      let best_ai = ref (-1) in
      let best_sim = ref neg_infinity in
      for ai = 0 to alen - 1 do
        if
          (not after_used.(ai))
          && (not (is_matched_after mapping after_arr.(ai)))
          && before_arr.(bi).node_type = after_arr.(ai).node_type
        then begin
          let sim = dice_similarity mapping before_arr.(bi) after_arr.(ai) in
          if sim > !best_sim then begin
            best_sim := sim;
            best_ai := ai
          end
        end
      done;
      if !best_ai >= 0 && !best_sim >= similarity_threshold then begin
        before_anchor.(bi) <- !best_ai;
        after_used.(!best_ai) <- true;
        add_match mapping before_arr.(bi) after_arr.(!best_ai)
      end
      else if
        !best_ai >= 0
        && before_arr.(bi).node_type = after_arr.(!best_ai).node_type
      then begin
        (* Same type but low similarity — still pair them if there's some
           shared structure or they're the only candidate of that type *)
        let same_type_count = ref 0 in
        for ai = 0 to alen - 1 do
          if
            (not after_used.(ai))
            && (not (is_matched_after mapping after_arr.(ai)))
            && before_arr.(bi).node_type = after_arr.(ai).node_type
          then incr same_type_count
        done;
        if !same_type_count = 1 || !best_sim > 0.0 then begin
          before_anchor.(bi) <- !best_ai;
          after_used.(!best_ai) <- true;
          add_match mapping before_arr.(bi) after_arr.(!best_ai)
        end
      end
    end
  done;
  (* Phase C: Build result in order *)
  (* Walk through before-children, emitting anchored pairs, removals *)
  let next_after = ref 0 in
  for bi = 0 to blen - 1 do
    let ai = before_anchor.(bi) in
    if ai >= 0 then begin
      (* Emit any unmatched after-children before this anchor *)
      while !next_after < ai do
        if not after_used.(!next_after) then begin
          (* This after-child was claimed by similarity match, skip *)
          if not (Array.exists (fun a -> a = !next_after) before_anchor) then
            result := Added { node = after_arr.(!next_after) } :: !result
        end;
        incr next_after
      done;
      (* Emit the matched pair *)
      if Tree.equal before_source before_arr.(bi) after_source after_arr.(ai)
      then result := Same { node = before_arr.(bi) } :: !result
      else
        result :=
          Changed
            {
              before = before_arr.(bi);
              after = after_arr.(ai);
              change = Replaced;
            }
          :: !result;
      next_after := ai + 1
    end
    else
      (* No match — removed *)
      result := Removed { node = before_arr.(bi) } :: !result
  done;
  (* Emit remaining unmatched after-children *)
  for ai = !next_after to alen - 1 do
    if not (Array.exists (fun a -> a = ai) before_anchor) then
      result := Added { node = after_arr.(ai) } :: !result
  done;
  List.rev !result

(** {1 Step 3 orchestration: top-down pass over matched pairs} *)

(** Process all matched pairs top-down, aligning their children. *)
let top_down_alignment mapping ~before_source ~after_source before_root
    after_root =
  (* Collect matched pairs that need child alignment by traversing the
     before tree and finding pairs where children aren't fully matched *)
  let worklist = Queue.create () in
  Queue.push (before_root, after_root) worklist;
  while not (Queue.is_empty worklist) do
    let before_n, after_n = Queue.pop worklist in
    let before_children = before_n.Tree.named_children in
    let after_children = after_n.Tree.named_children in
    if before_children <> [] || after_children <> [] then begin
      let child_changes =
        align_children mapping ~before_source ~after_source before_children
          after_children
      in
      (* For Changed pairs, recurse into their children *)
      List.iter
        (function
          | Changed { before; after; _ } ->
              if
                is_matched_before mapping before
                && is_matched_after mapping after
              then Queue.push (before, after) worklist
          | _ -> ())
        child_changes
    end
  done

(** {1 Compute mapping — main entry point} *)

let compute_mapping ~before_source ~after_source before_root after_root =
  let mapping =
    { forward = Hashtbl.create 256; backward = Hashtbl.create 256 }
  in
  (* Always match the roots *)
  add_match mapping before_root after_root;
  (* Step 1: Build hash indices *)
  let before_index = build_hash_index before_root in
  let after_index = build_hash_index after_root in
  (* Step 2: Match unique-hash subtrees *)
  match_unique_subtrees mapping ~before_source ~after_source before_index
    after_index;
  (* Step 3: Top-down alignment *)
  top_down_alignment mapping ~before_source ~after_source before_root after_root;
  mapping

(** {1 Diff derivation} *)

(** Derive the structured diff for a matched pair of nodes. *)
let rec derive_change mapping ~before_source ~after_source
    (before_n : Tree.src Tree.t) (after_n : Tree.src Tree.t) =
  if Tree.equal before_source before_n after_source after_n then Unchanged
  else if before_n.node_type <> after_n.node_type then Replaced
  else
    let before_children = before_n.named_children in
    let after_children = after_n.named_children in
    if before_children = [] && after_children = [] then
      (* Both leaves, different content *)
      Replaced
    else
      let child_changes =
        derive_child_changes mapping ~before_source ~after_source
          before_children after_children
      in
      if List.for_all (function Same _ -> true | _ -> false) child_changes
      then Unchanged
      else Modified { child_changes }

(** Derive child_change list from the mapping. *)
and derive_child_changes mapping ~before_source ~after_source before_children
    after_children =
  let before_arr = Array.of_list before_children in
  let after_arr = Array.of_list after_children in
  let blen = Array.length before_arr in
  let alen = Array.length after_arr in
  let after_used = Array.make alen false in
  let result = ref [] in
  (* Match before-children to their mapped after-children *)
  let before_partner = Array.make blen (-1) in
  for bi = 0 to blen - 1 do
    match get_match_forward mapping before_arr.(bi) with
    | Some ak ->
        for ai = 0 to alen - 1 do
          if (not after_used.(ai)) && key_of_node after_arr.(ai) = ak then begin
            before_partner.(bi) <- ai;
            after_used.(ai) <- true
          end
        done
    | None -> ()
  done;
  (* Build result in order *)
  let next_after = ref 0 in
  for bi = 0 to blen - 1 do
    let ai = before_partner.(bi) in
    if ai >= 0 then begin
      (* Emit unmatched after-children before this partner *)
      while !next_after < ai do
        if not after_used.(!next_after) then
          result := Added { node = after_arr.(!next_after) } :: !result;
        incr next_after
      done;
      (* Emit the pair *)
      let change =
        derive_change mapping ~before_source ~after_source before_arr.(bi)
          after_arr.(ai)
      in
      (match change with
      | Unchanged -> result := Same { node = before_arr.(bi) } :: !result
      | _ ->
          result :=
            Changed { before = before_arr.(bi); after = after_arr.(ai); change }
            :: !result);
      next_after := ai + 1
    end
    else result := Removed { node = before_arr.(bi) } :: !result
  done;
  (* Remaining unmatched after-children *)
  for ai = !next_after to alen - 1 do
    if not after_used.(ai) then
      result := Added { node = after_arr.(ai) } :: !result
  done;
  List.rev !result

let diff ~(before : Tree.src Tree.tree) ~(after : Tree.src Tree.tree) =
  let mapping =
    compute_mapping ~before_source:before.source ~after_source:after.source
      before.root after.root
  in
  let root_change =
    derive_change mapping ~before_source:before.source
      ~after_source:after.source before.root after.root
  in
  {
    before_source = before.source;
    after_source = after.source;
    before_root = before.root;
    after_root = after.root;
    root_change;
    mapping;
  }

(** {1 Change pair extraction} *)

let change_pairs (d : diff) =
  let pairs = ref [] in
  let rec collect_from_change ~before_node ~after_node = function
    | Unchanged -> ()
    | Replaced ->
        pairs :=
          {
            before_node;
            after_node;
            before_source = d.before_source;
            after_source = d.after_source;
          }
          :: !pairs
    | Modified { child_changes } ->
        List.iter
          (function
            | Same _ -> ()
            | Removed _ -> ()
            | Added _ -> ()
            | Changed { before; after; change } ->
                collect_from_change ~before_node:before ~after_node:after change)
          child_changes
  in
  collect_from_change ~before_node:d.before_root ~after_node:d.after_root
    d.root_change;
  List.rev !pairs
