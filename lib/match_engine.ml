open Match_types

let empty_bindings = { text_bindings = []; node_bindings = []; sequence_node_bindings = []; correspondences = [] }

(** Merge text bindings, checking that same var doesn't bind to different values *)
let check_and_merge_text_bindings existing new_bindings =
  let rec merge acc = function
    | [] -> Some acc
    | (var, value) :: rest ->
      (match List.assoc_opt var acc with
       | Some existing_value when existing_value <> value ->
         None  (* Conflict *)
       | _ ->
         merge ((var, value) :: acc) rest)
  in
  merge existing new_bindings

(** Merge match_bindings, checking text binding consistency *)
let check_and_merge_bindings existing new_bindings =
  match check_and_merge_text_bindings existing.text_bindings new_bindings.text_bindings with
  | None -> None
  | Some merged_text ->
    (* Node bindings don't need consistency check - just accumulate *)
    Some { text_bindings = merged_text;
           node_bindings = existing.node_bindings @ new_bindings.node_bindings;
           sequence_node_bindings = existing.sequence_node_bindings @ new_bindings.sequence_node_bindings;
           correspondences = existing.correspondences @ new_bindings.correspondences }

(** Check if a pattern node's text is a placeholder, return the original metavar name if so.
    Also handles wrapper nodes (like expression_statement) that contain just a placeholder
    plus a trailing semicolon. This allows patterns like `$X;` in PHP to work. *)
let is_placeholder substitutions source (node : Tree.pat Tree.t) =
  let text = Tree.text source node in
  (* First check if node's text directly matches *)
  match List.find_map (fun (var, placeholder) ->
    if text = placeholder then Some var else None
  ) substitutions with
  | Some _ as result -> result
  | None ->
    (* Check if node has a single named child whose text matches, and the parent
       text is exactly the child's text plus a semicolon.
       This handles expression_statement wrapping an identifier placeholder. *)
    match Tree.named_children node with
    | [single_child] ->
      let child_text = Tree.text source single_child in
      (* Only unwrap if parent is exactly child + ";" *)
      if text = child_text ^ ";" then
        List.find_map (fun (var, placeholder) ->
          if child_text = placeholder then Some var else None
        ) substitutions
      else
        None
    | _ -> None

(** Check if a pattern node's text is a placeholder for a sequence metavar *)
let is_sequence_placeholder ~pattern substitutions source (node : Tree.pat Tree.t) =
  match is_placeholder substitutions source node with
  | Some var_name -> List.mem var_name pattern.sequence_metavars
  | None -> false

(** Build index mapping node_type -> list of positions in array.
    Used to skip impossible split points when matching sequences. *)
let build_type_index (arr : Tree.src Tree.t array) start len =
  let index = Hashtbl.create 16 in
  for i = 0 to len - 1 do
    let node = arr.(start + i) in
    let typ = node.Tree.node_type in
    let positions = try Hashtbl.find index typ with Not_found -> [] in
    Hashtbl.replace index typ (i :: positions)
  done;
  (* Reverse lists so positions are in ascending order *)
  Hashtbl.iter (fun k v -> Hashtbl.replace index k (List.rev v)) index;
  index

(** Get the node type of the next non-sequence pattern element after index pi.
    Returns None if no more pattern elements or next is also a sequence. *)
let get_next_anchor_type ~pattern ~pattern_source ~substitutions
    (pattern_arr : Tree.pat Tree.t array) pi =
  let len = Array.length pattern_arr in
  if pi >= len then None
  else
    let p = pattern_arr.(pi) in
    let is_seq = is_sequence_placeholder ~pattern substitutions pattern_source p in
    if is_seq then None  (* Next element is also a sequence *)
    else Some p.Tree.node_type

(** Precompute cumulative texts for all possible sequence lengths.
    cumulative_texts.(i) = text of children 0..i-1 (empty string for i=0) *)
let precompute_cumulative_texts source (arr : Tree.src Tree.t array) start len =
  if len = 0 then [| "" |]
  else
    let result = Array.make (len + 1) "" in
    let buf = Buffer.create 256 in
    for i = 0 to len - 1 do
      if i > 0 then Buffer.add_char buf ' ';
      Buffer.add_string buf (Tree.text source arr.(start + i));
      result.(i + 1) <- Buffer.contents buf
    done;
    result

(** Try to match a pattern node against a source node.
    Returns Some bindings if match succeeds, None otherwise.
    Bindings include both text (for display/consistency) and nodes (for 'on $VAR'). *)
let rec match_node ~pattern ~pattern_source ~source ~substitutions
    (pattern_node : Tree.pat Tree.t) (source_node : Tree.src Tree.t) =
  (* Check if pattern node is a metavar placeholder *)
  match is_placeholder substitutions pattern_source pattern_node with
  | Some var_name ->
    (* This is a metavar - bind it to the source node's text and node *)
    let source_text = Tree.text source source_node in
    Some { text_bindings = [(var_name, source_text)];
           node_bindings = [(var_name, source_node)];
           sequence_node_bindings = [];
           correspondences = [] }
  | None ->
    (* Not a metavar - must match structurally *)
    let pattern_type = Tree.node_type pattern_node in
    let source_type = Tree.node_type source_node in
    if pattern_type <> source_type then
      None
    else
      (* Check if this is a leaf node *)
      let pattern_children = Tree.named_children pattern_node in
      let source_children = Tree.named_children source_node in
      if pattern_children = [] && source_children = [] then
        (* Both are leaves - text must match exactly *)
        let pattern_text = Tree.text pattern_source pattern_node in
        let source_text = Tree.text source source_node in
        if pattern_text = source_text then Some empty_bindings
        else None
      else if pattern_children = [] then
        (* Pattern is leaf but source has children - no match *)
        None
      else
        (* Both have children - match them *)
        match_children ~pattern ~pattern_source ~source ~substitutions
          pattern_node source_node pattern_children source_children

(** Match lists of children using arrays for O(n) sequence matching.
    Sequence metavars (marked with : sequence in preamble) can match 0 or more source children.
    Match mode determines the matching strategy:
    - Strict: exact positional matching, no extra children allowed
    - Field: match by field name, extras in other fields ignored, ordered within fields
    - Partial: unordered subset matching, extras ignored *)
and match_children ~pattern ~pattern_source ~source ~substitutions
    (pattern_node : Tree.pat Tree.t) (source_node : Tree.src Tree.t)
    (pattern_children : Tree.pat Tree.t list) (source_children : Tree.src Tree.t list) =
  match pattern.match_mode with
  | Field ->
    match_children_field ~pattern ~pattern_source ~source ~substitutions
      pattern_node source_node
  | Partial ->
    match_children_partial ~pattern ~pattern_source ~source ~substitutions pattern_children source_children
  | Strict ->
    match_children_exact ~pattern ~pattern_source ~source ~substitutions pattern_children source_children

(** Exact (ordered) child matching with index-based optimization.
    When a sequence is followed by a concrete pattern element, we only
    try split points where that element's node type appears in source. *)
and match_children_exact ~pattern ~pattern_source ~source ~substitutions
    (pattern_children : Tree.pat Tree.t list) (source_children : Tree.src Tree.t list) =
  (* Convert to arrays for efficient indexing *)
  let pattern_arr = Array.of_list pattern_children in
  let source_arr = Array.of_list source_children in
  let pattern_len = Array.length pattern_arr in
  let source_len = Array.length source_arr in

  (* Build type index once: node_type -> positions where that type appears *)
  let type_index = build_type_index source_arr 0 source_len in

  (* Match starting from pattern index pi and source index si *)
  let rec match_from bindings pi si =
    if pi >= pattern_len then
      (* No more pattern - succeed only if no source left *)
      if si >= source_len then Some bindings else None
    else
      let p = pattern_arr.(pi) in
      let is_seq = is_sequence_placeholder ~pattern substitutions pattern_source p in
      if is_seq then
        (* Sequence metavar: try matching source children *)
        match is_placeholder substitutions pattern_source p with
        | None -> None
        | Some var_name ->
          let remaining = source_len - si in
          let cumulative = precompute_cumulative_texts source source_arr si remaining in

          (* Determine which positions to try based on what follows *)
          let next_anchor_type = get_next_anchor_type ~pattern ~pattern_source
              ~substitutions pattern_arr (pi + 1) in

          let positions_to_try = match next_anchor_type with
            | None ->
              (* No anchor or next is also a sequence - must try all positions *)
              List.init (remaining + 1) Fun.id
            | Some anchor_type ->
              (* Only try positions where anchor type appears, plus end position *)
              let anchor_positions =
                try Hashtbl.find type_index anchor_type with Not_found -> []
              in
              (* Filter to positions >= si and convert to offsets from si *)
              let valid_offsets = List.filter_map (fun pos ->
                if pos >= si then Some (pos - si) else None
              ) anchor_positions in
              (* Also try taking all remaining (sequence consumes everything) *)
              let offsets = valid_offsets @ [remaining] in
              (* Remove duplicates and sort *)
              List.sort_uniq compare offsets
          in

          (* Try each candidate split point *)
          let rec try_positions = function
            | [] -> None
            | n :: rest ->
              let seq_text = cumulative.(n) in
              let seq_nodes = Array.to_list (Array.sub source_arr si n) in
              let seq_corrs = List.init n (fun k ->
                { pattern_index = pi; source_index = si + k }
              ) in
              let seq_binding = {
                text_bindings = [(var_name, seq_text)];
                node_bindings = [];
                sequence_node_bindings = [(var_name, seq_nodes)];
                correspondences = seq_corrs;
              } in
              match check_and_merge_bindings bindings seq_binding with
              | None -> try_positions rest  (* Binding conflict *)
              | Some merged ->
                match match_from merged (pi + 1) (si + n) with
                | Some result -> Some result
                | None -> try_positions rest
          in
          try_positions positions_to_try
      else
        (* Regular metavar or structural match *)
        if si >= source_len then None
        else
          match match_node ~pattern ~pattern_source ~source ~substitutions
                  p source_arr.(si) with
          | None -> None
          | Some new_bindings ->
            let corr = { pattern_index = pi; source_index = si } in
            let new_bindings = { new_bindings with
              correspondences = corr :: new_bindings.correspondences } in
            match check_and_merge_bindings bindings new_bindings with
            | None -> None
            | Some merged -> match_from merged (pi + 1) (si + 1)
  in
  match_from empty_bindings 0 0

(** Partial (unordered subset) child matching.
    Each pattern child must find a matching source child (any position, consumed once matched).
    Extra source children are ignored. Order doesn't matter. *)
and match_children_partial ~pattern ~pattern_source ~source ~substitutions
    (pattern_children : Tree.pat Tree.t list) (source_children : Tree.src Tree.t list) =
  let source_arr = Array.of_list source_children in
  let source_len = Array.length source_arr in
  (* Track which source children have been consumed *)
  let used = Array.make source_len false in

  (* Match each pattern child with backtracking over source choices *)
  let rec match_all bindings pi = function
    | [] -> Some bindings
    | p :: rest ->
      let is_seq = is_sequence_placeholder ~pattern substitutions pattern_source p in
      if is_seq then
        (* Sequence metavars don't make sense in partial mode - reject *)
        None
      else
        let rec try_source si =
          if si >= source_len then None
          else if used.(si) then try_source (si + 1)
          else
            match match_node ~pattern ~pattern_source ~source ~substitutions
                    p source_arr.(si) with
            | None -> try_source (si + 1)
            | Some new_bindings ->
              let corr = { pattern_index = pi; source_index = si } in
              (* Only keep our top-level correspondence; discard inner ones from
                 recursive matching — they use a different index space and would
                 corrupt the pattern_index → source_index mapping used by
                 apply_alignment_with_correspondences in transforms. *)
              let new_bindings = { new_bindings with
                correspondences = [corr] } in
              match check_and_merge_bindings bindings new_bindings with
              | None -> try_source (si + 1)  (* Binding conflict, try next *)
              | Some merged ->
                used.(si) <- true;
                match match_all merged (pi + 1) rest with
                | Some result -> Some result
                | None ->
                  used.(si) <- false;
                  try_source (si + 1)
        in
        try_source 0
  in
  match_all empty_bindings 0 pattern_children

(** Field-based child matching.
    Matches children by tree-sitter field name instead of position.
    Children with the same field name are matched in order within that field group.
    Extra source fields (not in pattern) are ignored.
    Children without field names are matched positionally within their group. *)
and match_children_field ~pattern ~pattern_source ~source ~substitutions
    (pattern_node : Tree.pat Tree.t) (source_node : Tree.src Tree.t) =
  let pattern_children = Tree.named_children_with_fields pattern_node in
  let source_children = Tree.named_children_with_fields source_node in

  (* Group children by field name *)
  let group_by_field children =
    (* let tbl : (string option, Tree.pat Tree.t list) Hashtbl.t = Hashtbl.create 8 in *)
    let tbl = Hashtbl.create 8 in
    let order = ref [] in
    List.iter (fun (field, node) ->
      let existing = Hashtbl.find_opt tbl field |> Option.value ~default:[] in
      if existing = [] then order := field :: !order;
      Hashtbl.replace tbl field (existing @ [node])
    ) children;
    List.rev_map (fun field -> (field, Hashtbl.find tbl field)) !order
  in

  let pattern_grouped = group_by_field pattern_children in
  let source_grouped = group_by_field source_children in

  (* Match each field group from pattern against corresponding source field group *)
  let rec match_groups bindings = function
    | [] -> Some bindings
    | (field_name, pat_nodes) :: rest ->
      let src_nodes =
        List.assoc_opt field_name source_grouped
        |> Option.value ~default:[]
      in
      (* Use exact matching within field group - order matters within a field *)
      match match_children_exact ~pattern ~pattern_source ~source ~substitutions
              pat_nodes src_nodes with
      | None -> None
      | Some new_bindings ->
        match check_and_merge_bindings bindings new_bindings with
        | None -> None
        | Some merged -> match_groups merged rest
  in
  match_groups empty_bindings pattern_grouped

(** Get the innermost meaningful node from a pattern.
    This unwraps program/module wrappers and expression_statement wrappers
    to get to the actual pattern content. *)
let get_pattern_content pattern : Tree.pat Tree.t =
  let rec unwrap (node : Tree.pat Tree.t) =
    let node_type = Tree.node_type node in
    let children = Tree.named_children node in
    match node_type, children with
    (* Unwrap program/module/source_file/compilation_unit with single child *)
    | ("program" | "module" | "source_file" | "compilation_unit"), [child] -> unwrap child
    (* Unwrap expression_statement with single child *)
    | "expression_statement", [child] -> unwrap child
    (* PHP: skip php_tag prefix in program node *)
    | "program", first :: rest when Tree.node_type first = "php_tag" ->
      (match rest with
       | [child] -> unwrap child
       | _ -> node)
    | _ -> node
  in
  unwrap pattern.tree.root
