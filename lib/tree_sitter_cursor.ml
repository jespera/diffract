(** See [tree_sitter_cursor.mli]. *)

type leaf = { leaf_text : string; leaf_node_type : string }

(* A frame represents a node we've descended into, plus the right
   siblings that follow it within the parent's children list. The
   stack's top is the current cursor position. *)
type frame = {
  mutable curr : Tree.src Tree.t;
  mutable right : Tree.src Tree.t list;
}

type t = {
  source : string;  (** shared with all clones; immutable *)
  mutable stack : frame list;
}

(* Filter [Tree.child list] down to the nodes the matcher should see.
   Drops tree-sitter "extras" (typically comments and whitespace
   tokens declared via the grammar's [extras] field). ERROR nodes
   are kept so they remain matchable / visible to the matcher. *)
let filter_children (children : Tree.src Tree.child list) : Tree.src Tree.t list
    =
  List.filter_map
    (fun (c : Tree.src Tree.child) ->
      if c.node.is_extra then None else Some c.node)
    children

let of_node ~source node = { source; stack = [ { curr = node; right = [] } ] }
let of_tree (tree : Tree.src Tree.tree) = of_node ~source:tree.source tree.root

let clone c =
  {
    source = c.source;
    stack = List.map (fun f -> { curr = f.curr; right = f.right }) c.stack;
  }

(* Reset the cursor's stack so the current node is the root of navigation
   — no upward escape via [move_next_subtree]. Used by multi-section
   search to scope a section's walk to the subtree bound by [on $VAR]. *)
let narrow c =
  match c.stack with
  | [] -> failwith "Tree_sitter_cursor: empty stack (logic error)"
  | f :: _ -> { source = c.source; stack = [ { curr = f.curr; right = [] } ] }

let current_node c =
  match c.stack with
  | [] -> failwith "Tree_sitter_cursor: empty stack (logic error)"
  | f :: _ -> f.curr

let rec move_first_leaf c =
  let n = current_node c in
  match filter_children n.children with
  | [] -> { leaf_text = Tree.text c.source n; leaf_node_type = n.node_type }
  | first :: rest ->
      c.stack <- { curr = first; right = rest } :: c.stack;
      move_first_leaf c

let move_first_child c =
  let n = current_node c in
  match filter_children n.children with
  | [] -> false
  | first :: rest ->
      c.stack <- { curr = first; right = rest } :: c.stack;
      true

let move_next_sibling c =
  match c.stack with
  | [] -> false
  | f :: _ -> (
      match f.right with
      | [] -> false
      | next :: rest ->
          f.curr <- next;
          f.right <- rest;
          true)

let rec move_next_subtree c =
  if move_next_sibling c then true
  else
    match c.stack with
    | [] | [ _ ] -> false (* at root or empty: no more subtrees *)
    | _ :: parent_rest ->
        c.stack <- parent_rest;
        move_next_subtree c

let leaf_text l = l.leaf_text
let leaf_node_type l = l.leaf_node_type

(* Structural equality with hash-based fast rejection. The cursors are
   assumed to share a source (true for all matcher use cases — both
   cursors come from the same parse during a single match attempt). *)
let subtree_equal c1 c2 =
  let n1 = current_node c1 in
  let n2 = current_node c2 in
  Tree.hash n1 = Tree.hash n2 && Tree.equal c1.source n1 c2.source n2

let is_named c = (current_node c).is_named
let node_type c = (current_node c).node_type
let source c = c.source

let byte_range c =
  let n = current_node c in
  (n.start_byte, n.end_byte)

let named_children c =
  let n = current_node c in
  List.filter_map
    (fun (child : Tree.src Tree.child) ->
      if child.node.is_named && not child.node.is_extra then
        Some (of_node ~source:c.source child.node)
      else None)
    n.children

let all_children c =
  let n = current_node c in
  List.filter_map
    (fun (child : Tree.src Tree.child) ->
      if child.node.is_extra then None
      else Some (of_node ~source:c.source child.node))
    n.children

(* Collect every non-extra leaf reachable under [node] in pre-order. For the
   bracket-like anonymous children we care about this is normally a single
   leaf, but recursion keeps us safe against a non-named child that has
   substructure. *)
let rec collect_non_extra_leaves source (node : Tree.src Tree.t) acc =
  if node.is_extra then acc
  else
    match node.children with
    | [] ->
        { leaf_text = Tree.text source node; leaf_node_type = node.node_type }
        :: acc
    | _ ->
        List.fold_left
          (fun acc (child : Tree.src Tree.child) ->
            collect_non_extra_leaves source child.node acc)
          acc node.children

let leading_anonymous_leaves c =
  let n = current_node c in
  let rec scan acc = function
    | [] -> List.rev acc
    | (child : Tree.src Tree.child) :: rest ->
        if child.node.is_extra then scan acc rest
        else if child.node.is_named then List.rev acc
        else
          let acc = collect_non_extra_leaves c.source child.node acc in
          scan acc rest
  in
  scan [] n.children

let trailing_anonymous_leaves c =
  let n = current_node c in
  (* Walk children right-to-left, collecting trailing anon leaves until we
     hit the last named child. [acc] is already in correct (left-to-right)
     order because [collect_non_extra_leaves] prepends its leaves; here we
     also prepend each anon child's run. *)
  let rec scan acc = function
    | [] -> acc
    | (child : Tree.src Tree.child) :: rest ->
        if child.node.is_extra then scan acc rest
        else if child.node.is_named then acc
        else
          let child_leaves =
            List.rev (collect_non_extra_leaves c.source child.node [])
          in
          scan (child_leaves @ acc) rest
  in
  scan [] (List.rev n.children)

let source_substring c start_byte end_byte =
  String.sub c.source start_byte (end_byte - start_byte)
