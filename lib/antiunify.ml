(** Anti-unification for finding minimal abstractions of changes *)

(** {1 Single-change representation} *)

(** Annotated node showing what's same vs different within a single change.
    All content remains CONCRETE - no placeholders. *)
type annotated =
  | Same of {
      node_type: string;
      text: string;  (** The actual text (same in before and after) *)
      children: annotated list;
    }
  | Diff of {
      node_type: string;  (** The node type (same in both, or we'd have Replaced) *)
      before: string;  (** Concrete text from before *)
      after: string;   (** Concrete text from after *)
    }
  | Added of {
      node_type: string;
      text: string;  (** Concrete text that was added *)
    }
  | Removed of {
      node_type: string;
      text: string;  (** Concrete text that was removed *)
    }
  | Replaced of {
      before_type: string;
      before_text: string;
      after_type: string;
      after_text: string;
    }
  | Modified of {
      node_type: string;
      children: annotated list;  (** Same structure, children have differences *)
    }

(** {1 Anti-unification within a single change} *)

(** Anti-unify two nodes to find what's same vs different *)
let rec antiunify_nodes source1 node1 source2 node2 =
  let text1 = Tree.text source1 node1 in
    let text2 = Tree.text source2 node2 in
    let type1 = Tree.node_type node1 in
    let type2 = Tree.node_type node2 in

    if text1 = text2 then
      (* Completely identical *)
      None  (* Will be represented as Same at parent level *)
    else if type1 <> type2 then
      (* Different types - replaced *)
      Some (Replaced {
        before_type = type1;
        before_text = text1;
        after_type = type2;
        after_text = text2;
      })
    else
      (* Same type, but content differs - recurse into children *)
      let children1 = Array.of_list (Tree.named_children node1) in
      let children2 = Array.of_list (Tree.named_children node2) in

      if Array.length children1 = 0 && Array.length children2 = 0 then
        (* Leaf nodes with different text *)
        Some (Diff {
          node_type = type1;
          before = text1;
          after = text2;
        })
      else
        (* Has children - match them up *)
        let child_results = antiunify_children source1 children1 source2 children2 in
        if List.for_all Option.is_none child_results then
          None  (* All children are same *)
        else
          let annotated_children = List.mapi (fun i result ->
            match result with
            | None ->
              (* Child is the same - but we need to get it from one of the arrays *)
              if i < Array.length children1 then
                Same {
                  node_type = Tree.node_type children1.(i);
                  text = Tree.text source1 children1.(i);
                  children = [];
                }
              else
                Same {
                  node_type = Tree.node_type children2.(i);
                  text = Tree.text source2 children2.(i);
                  children = [];
                }
            | Some ann -> ann
          ) child_results in
          Some (Modified {
            node_type = type1;
            children = annotated_children;
          })

(** Match up children and anti-unify each pair *)
and antiunify_children source1 children1 source2 children2 =
  let n1 = Array.length children1 in
  let n2 = Array.length children2 in

  (* Simple case: same number of children, match by position *)
  if n1 = n2 then
    List.init n1 (fun i ->
      antiunify_nodes source1 children1.(i) source2 children2.(i)
    )
  else
    (* Different number - need to find best matching *)
    (* For now, use a simple LCS-like approach *)
    let results = ref [] in
    let used2 = Array.make n2 false in

    (* First pass: find exact matches *)
    for i = 0 to n1 - 1 do
      let found = ref false in
      for j = 0 to n2 - 1 do
        if not !found && not used2.(j) then
          let text1 = Tree.text source1 children1.(i) in
          let text2 = Tree.text source2 children2.(j) in
          if text1 = text2 then begin
            results := (i, j, None) :: !results;
            used2.(j) <- true;
            found := true
          end
      done;
      if not !found then
        (* Try to find same-type match *)
        for j = 0 to n2 - 1 do
          if not !found && not used2.(j) then
            let type1 = Tree.node_type children1.(i) in
            let type2 = Tree.node_type children2.(j) in
            if type1 = type2 then begin
              let ann = antiunify_nodes source1 children1.(i) source2 children2.(j) in
              results := (i, j, ann) :: !results;
              used2.(j) <- true;
              found := true
            end
        done;
      if not !found then
        (* No match found - this child was removed *)
        results := (i, -1, Some (Removed {
          node_type = Tree.node_type children1.(i);
          text = Tree.text source1 children1.(i);
        })) :: !results
    done;

    (* Add unmatched children from array2 as Added *)
    for j = 0 to n2 - 1 do
      if not used2.(j) then
        results := (-1, j, Some (Added {
          node_type = Tree.node_type children2.(j);
          text = Tree.text source2 children2.(j);
        })) :: !results
    done;

    (* Sort by position and extract annotations *)
    let sorted = List.sort (fun (i1, j1, _) (i2, j2, _) ->
      let pos1 = if i1 >= 0 then i1 else j1 + 1000 in
      let pos2 = if i2 >= 0 then i2 else j2 + 1000 in
      compare pos1 pos2
    ) !results in
    List.map (fun (_, _, ann) -> ann) sorted

(** Anti-unify a diff change *)
let antiunify_change diff_result change =
  match change with
  | Diff.Added { node; _ } ->
    Added {
      node_type = Tree.node_type node;
      text = Tree.text diff_result.Diff.after_source node;
    }
  | Diff.Removed { node; _ } ->
    Removed {
      node_type = Tree.node_type node;
      text = Tree.text diff_result.Diff.before_source node;
    }
  | Diff.Modified { before; after; _ } | Diff.Replaced { before; after; _ } ->
    let result = antiunify_nodes
      diff_result.Diff.before_source before
      diff_result.Diff.after_source after
    in
    match result with
    | Some ann -> ann
    | None -> Same {
        node_type = Tree.node_type before;
        text = Tree.text diff_result.Diff.before_source before;
        children = [];
      }

(** {1 Pretty printing} *)

let rec pp_annotated ppf = function
  | Same { node_type; text; children } ->
    if children = [] then
      Format.fprintf ppf "%s" text
    else
      Format.fprintf ppf "(%s %a)" node_type
        (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_annotated) children
  | Diff { node_type = _; before; after } ->
    Format.fprintf ppf "[%s → %s]" before after
  | Added { node_type = _; text } ->
    Format.fprintf ppf "{+%s}" text
  | Removed { node_type = _; text } ->
    Format.fprintf ppf "{-%s}" text
  | Replaced { before_type = _; before_text; after_type = _; after_text } ->
    Format.fprintf ppf "[%s → %s]" before_text after_text
  | Modified { node_type; children } ->
    Format.fprintf ppf "(%s %a)" node_type
      (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_annotated) children

let to_string ann =
  Format.asprintf "%a" pp_annotated ann

(** {1 Cross-change pattern finding} *)

(** A pattern with potential variables for cross-change comparison *)
type pattern =
  | PConcrete of string  (** Concrete text that's the same across all instances *)
  | PVar of int * string list  (** Variable with index and list of concrete values it represents *)
  | PTransform of { before: pattern; after: pattern }  (** A transformation pattern *)
  | PNode of string * pattern list  (** Node with pattern children *)

(** Extract a pattern from an annotated change *)
let rec pattern_of_annotated = function
  | Same { text; children; node_type } ->
    if children = [] then
      PConcrete text
    else
      PNode (node_type, List.map pattern_of_annotated children)
  | Diff { before; after; _ } ->
    PTransform { before = PConcrete before; after = PConcrete after }
  | Added { text; _ } ->
    PConcrete ("{+" ^ text ^ "}")
  | Removed { text; _ } ->
    PConcrete ("{-" ^ text ^ "}")
  | Replaced { before_text; after_text; _ } ->
    PTransform { before = PConcrete before_text; after = PConcrete after_text }
  | Modified { node_type; children } ->
    PNode (node_type, List.map pattern_of_annotated children)

(** Anti-unify two patterns to find common structure *)
let rec antiunify_patterns var_counter p1 p2 =
  match p1, p2 with
  | PConcrete s1, PConcrete s2 when s1 = s2 ->
    (PConcrete s1, var_counter)
  | PConcrete s1, PConcrete s2 ->
    (* Different concrete values - create a variable *)
    (PVar (var_counter, [s1; s2]), var_counter + 1)
  | PVar (i, vals), PConcrete s ->
    (PVar (i, s :: vals), var_counter)
  | PConcrete s, PVar (i, vals) ->
    (PVar (i, s :: vals), var_counter)
  | PVar (i, vals1), PVar (_, vals2) ->
    (PVar (i, vals1 @ vals2), var_counter)
  | PTransform { before = b1; after = a1 }, PTransform { before = b2; after = a2 } ->
    let (before, vc1) = antiunify_patterns var_counter b1 b2 in
    let (after, vc2) = antiunify_patterns vc1 a1 a2 in
    (PTransform { before; after }, vc2)
  | PNode (t1, c1), PNode (t2, c2) when t1 = t2 && List.length c1 = List.length c2 ->
    let (children, vc) = List.fold_left2 (fun (acc, vc) p1 p2 ->
      let (p, vc') = antiunify_patterns vc p1 p2 in
      (p :: acc, vc')
    ) ([], var_counter) c1 c2 in
    (PNode (t1, List.rev children), vc)
  | _ ->
    (* Incompatible structures - create a variable *)
    (PVar (var_counter, []), var_counter + 1)

(** Anti-unify a list of patterns *)
let antiunify_pattern_list patterns =
  match patterns with
  | [] -> None
  | [p] -> Some p
  | p :: rest ->
    let (result, _) = List.fold_left (fun (acc, vc) p ->
      antiunify_patterns vc acc p
    ) (p, 0) rest in
    Some result

(** Group changes by their transformation pattern and find common patterns *)
let find_common_patterns annotated_changes =
  (* Convert to patterns *)
  let patterns = List.map pattern_of_annotated annotated_changes in

  (* Group by structure similarity *)
  (* For now, just anti-unify all and report *)
  match antiunify_pattern_list patterns with
  | None -> None
  | Some pattern -> Some pattern

(** Pretty print a pattern *)
let rec pp_pattern ppf = function
  | PConcrete s -> Format.fprintf ppf "%s" s
  | PVar (i, _) -> Format.fprintf ppf "$%d" i
  | PTransform { before; after } ->
    Format.fprintf ppf "[%a → %a]" pp_pattern before pp_pattern after
  | PNode (t, children) ->
    if children = [] then
      Format.fprintf ppf "%s" t
    else
      Format.fprintf ppf "(%s %a)" t
        (Format.pp_print_list ~pp_sep:Format.pp_print_space pp_pattern) children

let pattern_to_string p =
  Format.asprintf "%a" pp_pattern p

(** Get the concrete values for a variable in a pattern *)
let rec get_variable_values var_id = function
  | PConcrete _ -> []
  | PVar (i, vals) when i = var_id -> vals
  | PVar _ -> []
  | PTransform { before; after } ->
    get_variable_values var_id before @ get_variable_values var_id after
  | PNode (_, children) ->
    List.concat_map (get_variable_values var_id) children
