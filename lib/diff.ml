(** Tree diff computation *)

(** Context describing where a node appears in the tree *)
type context = {
  parent_type: string option;
  field_name: string option;
  index: int;  (** Index among siblings *)
}

(** Summary of a node's structure (for comparison) *)
type node_shape = {
  node_type: string;
  children: node_shape list;
}

(** A single change between two trees *)
type change =
  | Added of {
      context: context;
      node: Tree.t;
      shape: node_shape;
    }
  | Removed of {
      context: context;
      node: Tree.t;
      shape: node_shape;
    }
  | Modified of {
      context: context;
      node_type: string;
      before: Tree.t;
      after: Tree.t;
      children_changed: change list;
    }
  | Replaced of {
      context: context;
      before: Tree.t;
      after: Tree.t;
      before_shape: node_shape;
      after_shape: node_shape;
    }

(** Result of diffing two trees *)
type diff_result = {
  changes: change list;
  before_source: string;
  after_source: string;
}

(** {1 Node shape and hashing} *)

(** Compute the shape of a node (structure without text content) *)
let rec node_shape node =
  {
    node_type = Tree.node_type node;
    children = List.map node_shape (Tree.named_children node);
  }

(** Hash a node shape for quick comparison *)
let rec hash_shape shape =
  let h = Hashtbl.hash shape.node_type in
  List.fold_left (fun acc child ->
    acc * 31 + hash_shape child
  ) h shape.children

(** Hash a node including its text content *)
let hash_node source node =
  let shape_hash = hash_shape (node_shape node) in
  let text_hash = Hashtbl.hash (Tree.text source node) in
  shape_hash * 31 + text_hash

(** Check if two nodes are structurally identical (same shape and text) *)
let nodes_equal source1 node1 source2 node2 =
  Tree.text source1 node1 = Tree.text source2 node2

(** Check if two nodes have the same shape (ignoring text) *)
let same_shape node1 node2 =
  node_shape node1 = node_shape node2

(** {1 Matching} *)

(** Compute similarity between two nodes (0.0 to 1.0) *)
let node_similarity source1 node1 source2 node2 =
  if Tree.node_type node1 <> Tree.node_type node2 then 0.0
  else if nodes_equal source1 node1 source2 node2 then 1.0
  else if same_shape node1 node2 then 0.8
  else
    (* Partial similarity based on type match *)
    0.3

(** Greedy matching of children lists *)
let match_children source1 children1 source2 children2 =
  let n1 = Array.length children1 in
  let n2 = Array.length children2 in
  let used1 = Array.make n1 false in
  let used2 = Array.make n2 false in
  let matches = ref [] in

  (* First pass: exact matches *)
  for i = 0 to n1 - 1 do
    if not used1.(i) then
      for j = 0 to n2 - 1 do
        if not used2.(j) && not used1.(i) then
          if nodes_equal source1 children1.(i) source2 children2.(j) then begin
            matches := (i, j, 1.0) :: !matches;
            used1.(i) <- true;
            used2.(j) <- true
          end
      done
  done;

  (* Second pass: same type matches *)
  for i = 0 to n1 - 1 do
    if not used1.(i) then
      for j = 0 to n2 - 1 do
        if not used2.(j) && not used1.(i) then
          let sim = node_similarity source1 children1.(i) source2 children2.(j) in
          if sim >= 0.3 then begin
            matches := (i, j, sim) :: !matches;
            used1.(i) <- true;
            used2.(j) <- true
          end
      done
  done;

  let matched = List.rev !matches in
  let unmatched1 = List.filter_map (fun i ->
    if used1.(i) then None else Some i
  ) (List.init n1 Fun.id) in
  let unmatched2 = List.filter_map (fun j ->
    if used2.(j) then None else Some j
  ) (List.init n2 Fun.id) in

  (matched, unmatched1, unmatched2)

(** {1 Diff computation} *)

let make_context ?parent_type ?field_name index =
  { parent_type; field_name; index }

(** Compute diff between two nodes *)
let rec diff_nodes source1 node1 source2 node2 ctx =
  if nodes_equal source1 node1 source2 node2 then
    []  (* Identical *)
  else if Tree.node_type node1 <> Tree.node_type node2 then
    [Replaced {
      context = ctx;
      before = node1;
      after = node2;
      before_shape = node_shape node1;
      after_shape = node_shape node2;
    }]
  else
    (* Same type, diff children *)
    let children1 = Array.of_list (Tree.named_children node1) in
    let children2 = Array.of_list (Tree.named_children node2) in
    let (matched, removed_indices, added_indices) =
      match_children source1 children1 source2 children2
    in

    let parent_type = Some (Tree.node_type node1) in

    (* Recursively diff matched children *)
    let child_changes = List.concat_map (fun (i, j, sim) ->
      let child1 = children1.(i) in
      let child2 = children2.(j) in
      let field_name = Tree.field_name_for_child node1 i in
      let child_ctx = make_context ?parent_type ?field_name i in
      if sim >= 1.0 then []
      else diff_nodes source1 child1 source2 child2 child_ctx
    ) matched in

    (* Removed children *)
    let removed = List.map (fun i ->
      let child = children1.(i) in
      let field_name = Tree.field_name_for_child node1 i in
      Removed {
        context = make_context ?parent_type ?field_name i;
        node = child;
        shape = node_shape child;
      }
    ) removed_indices in

    (* Added children *)
    let added = List.map (fun j ->
      let child = children2.(j) in
      let field_name = Tree.field_name_for_child node2 j in
      Added {
        context = make_context ?parent_type ?field_name j;
        node = child;
        shape = node_shape child;
      }
    ) added_indices in

    let all_changes = removed @ added @ child_changes in
    if all_changes = [] then
      (* Same type, no child changes, but we know texts differ (nodes_equal was false).
         This happens for leaf nodes with different text content. *)
      [Replaced {
        context = ctx;
        before = node1;
        after = node2;
        before_shape = node_shape node1;
        after_shape = node_shape node2;
      }]
    else
      [Modified {
        context = ctx;
        node_type = Tree.node_type node1;
        before = node1;
        after = node2;
        children_changed = all_changes;
      }]

(** Compute diff between two trees *)
let diff ~before_source ~before_root ~after_source ~after_root =
  let ctx = make_context 0 in
  let changes = diff_nodes before_source before_root after_source after_root ctx in
  { changes; before_source; after_source }

(** {1 Change inspection} *)

(** Get text of a node from the appropriate source *)
let change_text_before result change =
  match change with
  | Added _ -> None
  | Removed { node; _ } -> Some (Tree.text result.before_source node)
  | Modified { before; _ } -> Some (Tree.text result.before_source before)
  | Replaced { before; _ } -> Some (Tree.text result.before_source before)

let change_text_after result change =
  match change with
  | Added { node; _ } -> Some (Tree.text result.after_source node)
  | Removed _ -> None
  | Modified { after; _ } -> Some (Tree.text result.after_source after)
  | Replaced { after; _ } -> Some (Tree.text result.after_source after)

(** Get the context of a change *)
let change_context = function
  | Added { context; _ } -> context
  | Removed { context; _ } -> context
  | Modified { context; _ } -> context
  | Replaced { context; _ } -> context

(** Get the node type involved in a change *)
let change_node_type = function
  | Added { shape; _ } -> shape.node_type
  | Removed { shape; _ } -> shape.node_type
  | Modified { node_type; _ } -> node_type
  | Replaced { before_shape; _ } -> before_shape.node_type

(** Flatten nested changes into a list *)
let rec flatten_changes changes =
  List.concat_map (fun change ->
    match change with
    | Modified { children_changed; _ } ->
      change :: flatten_changes children_changed
    | _ -> [change]
  ) changes

(** {1 Pretty printing} *)

let pp_context ppf ctx =
  match ctx.parent_type, ctx.field_name with
  | Some parent, Some field ->
    Format.fprintf ppf "%s.%s[%d]" parent field ctx.index
  | Some parent, None ->
    Format.fprintf ppf "%s[%d]" parent ctx.index
  | None, _ ->
    Format.fprintf ppf "root"

let rec pp_change ppf result change =
  match change with
  | Added { context; shape; node } ->
    Format.fprintf ppf "@[<v2>+ %s at %a@,  %s@]"
      shape.node_type pp_context context
      (String.escaped (Tree.text result.after_source node))
  | Removed { context; shape; node } ->
    Format.fprintf ppf "@[<v2>- %s at %a@,  %s@]"
      shape.node_type pp_context context
      (String.escaped (Tree.text result.before_source node))
  | Modified { context; node_type; children_changed; _ } ->
    Format.fprintf ppf "@[<v2>~ %s at %a@,%a@]"
      node_type pp_context context
      (fun ppf cs -> pp_changes result ppf cs) children_changed
  | Replaced { context; before_shape; after_shape; before; after } ->
    Format.fprintf ppf "@[<v2>! %s -> %s at %a@,  before: %s@,  after: %s@]"
      before_shape.node_type after_shape.node_type pp_context context
      (String.escaped (Tree.text result.before_source before))
      (String.escaped (Tree.text result.after_source after))

and pp_changes result ppf changes =
  Format.pp_print_list ~pp_sep:Format.pp_print_cut
    (fun ppf c -> pp_change ppf result c) ppf changes

let pp ppf result =
  if result.changes = [] then
    Format.fprintf ppf "No changes"
  else
    Format.fprintf ppf "@[<v>%a@]"
      (fun ppf changes ->
        List.iter (fun c ->
          pp_change ppf result c;
          Format.pp_print_cut ppf ()
        ) changes
      ) result.changes

let to_string result =
  Format.asprintf "%a" pp result
