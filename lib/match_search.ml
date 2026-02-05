open Match_types

(** Find all matches of a pattern within a specific subtree.
    Checks inherited bindings for conflicts and merges them with new bindings. *)
let find_matches_in_subtree ~pattern ~inherited_bindings ~source
    ~(root_node : Tree.src Tree.t) =
  let pattern_node = Match_engine.get_pattern_content pattern in
  let results = ref [] in
  root_node |> Tree.traverse (fun source_node ->
    match Match_engine.match_node
            ~pattern
            ~pattern_source:pattern.source
            ~source
            ~substitutions:pattern.substitutions
            pattern_node source_node with
    | Some mb ->
      (* Check if new bindings are consistent with inherited ones *)
      (match Match_engine.check_and_merge_bindings inherited_bindings mb with
       | Some merged ->
         results := {
           node = source_node;
           bindings = merged.text_bindings;
           node_bindings = merged.node_bindings;
           sequence_node_bindings = merged.sequence_node_bindings;
           start_point = Tree.start_point source_node;
           end_point = Tree.end_point source_node;
         } :: !results
       | None -> ())  (* Binding conflict - skip this match *)
    | None -> ()
  );
  List.rev !results

(** Find matches for a pattern, respecting the 'on $VAR' directive.
    If pattern has on_var, searches within the bound node(s).
    For single metavars, treats as a sequence of one node.
    Otherwise, traverses the subtree looking for matches. *)
let find_pattern_matches ~pattern ~inherited_bindings ~source
    ~(root_node : Tree.src Tree.t) =
  match pattern.on_var with
  | Some var_name ->
    (* 'on $VAR' mode: get the target node(s) to search within *)
    let target_nodes =
      match List.assoc_opt var_name inherited_bindings.node_bindings with
      | Some node -> [node]  (* Single node - treat as sequence of one *)
      | None ->
        match List.assoc_opt var_name inherited_bindings.sequence_node_bindings with
        | Some nodes -> nodes
        | None -> failwith (Printf.sprintf "on %s: variable not bound in previous pattern" var_name)
    in
    (* Search within each target node *)
    List.concat_map (fun target_node ->
      find_matches_in_subtree ~pattern ~inherited_bindings ~source ~root_node:target_node
    ) target_nodes
  | None ->
    (* Normal mode: traverse subtree looking for matches *)
    find_matches_in_subtree ~pattern ~inherited_bindings ~source ~root_node

(** Find all matches of a pattern in source code *)
let find_matches ~language ~pattern_text ~source_text =
  let pattern = Match_parse.parse_pattern ~language pattern_text in
  let source_tree = Tree.parse ~language source_text in
  find_matches_in_subtree ~pattern
    ~inherited_bindings:Match_engine.empty_bindings
    ~source:source_tree.source
    ~root_node:source_tree.root

(** Find matches in a file *)
let find_matches_in_file ~language ~pattern_text ~source_path =
  let source_text = In_channel.with_open_text source_path In_channel.input_all in
  find_matches ~language ~pattern_text ~source_text

(** Recursively find nested matches.
    Process patterns left-to-right. For each context pattern, find matches,
    then recurse into each match's subtree with remaining patterns.
    Respects 'on $VAR' directive for direct matching against bound nodes. *)
let rec find_nested_matches_impl ~source ~inherited_bindings
    ~accumulated_contexts ~(root_node : Tree.src Tree.t) patterns =
  match patterns with
  | [] -> []  (* No patterns *)
  | [target] ->
    (* Base case: match final/target pattern *)
    let matches = find_pattern_matches
        ~pattern:target
        ~inherited_bindings
        ~source
        ~root_node in
    List.map (fun m -> {
      inner_node = m.node;
      inner_bindings = m.bindings;
      inner_node_bindings = m.node_bindings;
      inner_sequence_node_bindings = m.sequence_node_bindings;
      all_bindings = m.bindings;
      all_node_bindings = m.node_bindings;
      all_sequence_node_bindings = m.sequence_node_bindings;
      contexts = List.rev accumulated_contexts;
      start_point = m.start_point;
      end_point = m.end_point;
    }) matches
  | ctx :: rest ->
    (* Find context matches, then recurse into each *)
    let ctx_matches = find_pattern_matches
        ~pattern:ctx
        ~inherited_bindings
        ~source
        ~root_node in
    List.concat_map (fun ctx_match ->
      let ctx_record = {
        context_node = ctx_match.node;
        context_bindings = ctx_match.bindings;
        context_node_bindings = ctx_match.node_bindings;
        context_sequence_node_bindings = ctx_match.sequence_node_bindings;
        context_start_point = ctx_match.start_point;
        context_end_point = ctx_match.end_point;
      } in
      let new_inherited = {
        text_bindings = ctx_match.bindings;
        node_bindings = ctx_match.node_bindings;
        sequence_node_bindings = ctx_match.sequence_node_bindings;
      } in
      find_nested_matches_impl
        ~source
        ~inherited_bindings:new_inherited
        ~accumulated_contexts:(ctx_record :: accumulated_contexts)
        ~root_node:ctx_match.node
        rest
    ) ctx_matches

(** Internal: find nested matches and return with source tree for error checking *)
let find_nested_matches_internal ~language ~pattern_text ~source_text =
  let nested = Match_parse.parse_nested_pattern ~language pattern_text in
  let source_tree = Tree.parse ~language source_text in
  let source = source_tree.source in
  let source_root = source_tree.root in
  let matches =
    if List.length nested.patterns = 1 then
      (* Single pattern: use existing logic, convert to nested_match_result *)
      let pattern = List.hd nested.patterns in
      let matches = find_matches_in_subtree
          ~pattern
          ~inherited_bindings:Match_engine.empty_bindings
          ~source
          ~root_node:source_root in
      List.map (fun m -> {
        inner_node = m.node;
        inner_bindings = m.bindings;
        inner_node_bindings = m.node_bindings;
        inner_sequence_node_bindings = m.sequence_node_bindings;
        all_bindings = m.bindings;
        all_node_bindings = m.node_bindings;
        all_sequence_node_bindings = m.sequence_node_bindings;
        contexts = [];
        start_point = m.start_point;
        end_point = m.end_point;
      }) matches
    else
      find_nested_matches_impl
        ~source
        ~inherited_bindings:Match_engine.empty_bindings
        ~accumulated_contexts:[]
        ~root_node:source_root
        nested.patterns
  in
  (matches, source_tree)

(** Find nested matches using a pattern with multiple sections.
    Auto-detects: single section behaves like find_matches, multiple sections
    enable nested/scoped matching. Supports 'on $VAR' for direct matching. *)
let find_nested_matches ~language ~pattern_text ~source_text =
  let (matches, _) = find_nested_matches_internal ~language ~pattern_text ~source_text in
  matches

(** Find nested matches and return parse error information.
    Returns both matches and the count of parse errors (ERROR nodes) in the source. *)
let search ~language ~pattern_text ~source_text =
  let (matches, source_tree) = find_nested_matches_internal ~language ~pattern_text ~source_text in
  { matches; parse_error_count = Tree.error_count source_tree }

(** Build an index from a source tree root. O(n) where n is node count. *)
let build_index (root : Tree.src Tree.t) =
  let by_type = Hashtbl.create 64 in
  Tree.traverse (fun node ->
    let typ = Tree.node_type node in
    let existing = Hashtbl.find_opt by_type typ |> Option.value ~default:[] in
    Hashtbl.replace by_type typ (node :: existing)
  ) root;
  { by_type }

(** Find matches using a pre-built index.
    Falls back to full traversal if pattern root is a metavar. *)
let find_matches_with_index ~index ~pattern ~source ~(source_root : Tree.src Tree.t) =
  let pattern_node = Match_engine.get_pattern_content pattern in
  (* Check if pattern root is a metavar placeholder *)
  match Match_engine.is_placeholder pattern.substitutions pattern.source pattern_node with
  | Some _ ->
    (* Pattern root is a metavar - can't filter by type, fall back to traversal *)
    find_matches_in_subtree ~pattern ~inherited_bindings:Match_engine.empty_bindings ~source ~root_node:source_root
  | None ->
    (* Query index for candidate nodes *)
    let pattern_type = Tree.node_type pattern_node in
    let candidates = Hashtbl.find_opt index.by_type pattern_type
                     |> Option.value ~default:[] in
    List.filter_map (fun source_node ->
      match Match_engine.match_node
              ~pattern
              ~pattern_source:pattern.source
              ~source
              ~substitutions:pattern.substitutions
              pattern_node source_node with
      | Some mb ->
        Some {
          node = source_node;
          bindings = mb.text_bindings;
          node_bindings = mb.node_bindings;
          sequence_node_bindings = mb.sequence_node_bindings;
          start_point = Tree.start_point source_node;
          end_point = Tree.end_point source_node;
        }
      | None -> None
    ) candidates

(** Match multiple patterns against source, building index once.
    Returns list of (pattern_index, match_result) pairs. *)
let find_matches_multi ~language ~patterns ~source_text =
  let source_tree = Tree.parse ~language source_text in
  let source = source_tree.source in
  let index = build_index source_tree.root in
  List.concat (List.mapi (fun i pattern_text ->
    let pattern = Match_parse.parse_pattern ~language pattern_text in
    let matches = find_matches_with_index ~index ~pattern ~source ~source_root:source_tree.root in
    List.map (fun m -> (i, m)) matches
  ) patterns)

(** Format a match result for display *)
let format_match source_text (result : match_result) =
  let line = result.start_point.row + 1 in
  let matched_text = Tree.text source_text result.node in
  (* Deduplicate bindings - same var always has same value due to consistency check *)
  let unique_bindings =
    result.bindings
    |> List.sort_uniq (fun (v1, _) (v2, _) -> String.compare v1 v2)
  in
  let bindings_str =
    unique_bindings
    |> List.map (fun (var, value) -> Printf.sprintf "  %s = %s" var value)
    |> String.concat "\n"
  in
  Printf.sprintf "line %d: %s\n%s" line matched_text bindings_str

(** Format a nested match result for display.
    Shows each context level with indentation and all bindings. *)
let format_nested_match source_text result =
  let buf = Buffer.create 256 in
  (* Helper to truncate long text for display *)
  let truncate_text text max_len =
    let text = String.map (fun c -> if c = '\n' then ' ' else c) text in
    if String.length text <= max_len then text
    else String.sub text 0 (max_len - 3) ^ "..."
  in
  (* Format bindings with given indentation *)
  let format_bindings indent bindings =
    let unique = List.sort_uniq (fun (v1, _) (v2, _) -> String.compare v1 v2) bindings in
    List.iter (fun (var, value) ->
      Buffer.add_string buf (String.make indent ' ');
      Buffer.add_string buf (Printf.sprintf "%s = %s\n" var value)
    ) unique
  in
  (* Format each context level *)
  List.iteri (fun i ctx ->
    let line = ctx.context_start_point.row + 1 in
    let matched_text = Tree.text source_text ctx.context_node in
    let preview = truncate_text matched_text 50 in
    Buffer.add_string buf (String.make (i * 2) ' ');
    Buffer.add_string buf (Printf.sprintf "context[%d] line %d: %s\n" i line preview);
    format_bindings ((i * 2) + 2) ctx.context_bindings
  ) result.contexts;
  (* Format the inner match (target) *)
  let inner_indent = List.length result.contexts * 2 in
  let line = result.start_point.row + 1 in
  let matched_text = Tree.text source_text result.inner_node in
  let preview = truncate_text matched_text 50 in
  Buffer.add_string buf (String.make inner_indent ' ');
  Buffer.add_string buf (Printf.sprintf "=> line %d: %s\n" line preview);
  format_bindings (inner_indent + 3) result.inner_bindings;
  Buffer.contents buf
