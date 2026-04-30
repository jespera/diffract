open Match_types

let empty_bindings =
  {
    text_bindings = [];
    node_bindings = [];
    sequence_node_bindings = [];
    correspondences = [];
  }

(** Filter source children to drop tree-sitter "extras" (typically
    comments) when the pattern has none. The default semantics is
    "comments are transparent": a pattern that doesn't mention a
    comment matches source regardless of whether comments are present.
    When the pattern itself contains a comment, all source extras are
    kept and the existing positional/field/partial logic compares
    them as ordinary children — so the pattern's comment must align
    with a matching source comment. *)
let filter_irrelevant_extras (type k) (pattern_children : Tree.pat Tree.t list)
    (source_children : k Tree.t list) : k Tree.t list =
  let pattern_has_extras =
    List.exists (fun (n : Tree.pat Tree.t) -> n.is_extra) pattern_children
  in
  if pattern_has_extras then source_children
  else
    List.filter (fun (n : k Tree.t) -> not n.is_extra) source_children

(** Check if two lists of source nodes are structurally identical *)
let nodes_equal source (l1 : Tree.src Tree.t list) (l2 : Tree.src Tree.t list) =
  let rec check = function
    | [], [] -> true
    | n1 :: r1, n2 :: r2 ->
        if Tree.equal source n1 source n2 then check (r1, r2) else false
    | _ -> false
  in
  check (l1, l2)

(** Merge text bindings, checking that same var doesn't bind to different values
*)
let check_and_merge_text_bindings existing new_bindings =
  let rec merge acc = function
    | [] -> Some acc
    | (var, value) :: rest -> (
        match List.assoc_opt var acc with
        | Some existing_value when existing_value <> value ->
            None (* Conflict *)
        | Some _ -> merge acc rest (* Already present and same *)
        | None -> merge ((var, value) :: acc) rest)
  in
  merge existing new_bindings

(** Merge match_bindings, checking text and sequence binding consistency *)
let check_and_merge_bindings ~source existing new_bindings =
  match
    check_and_merge_text_bindings existing.text_bindings
      new_bindings.text_bindings
  with
  | None -> None
  | Some merged_text -> (
      (* Merge node bindings (single) *)
      let merged_nodes =
        List.fold_left
          (fun acc (var, node) ->
            if List.mem_assoc var acc then acc else (var, node) :: acc)
          existing.node_bindings new_bindings.node_bindings
      in
      (* Check and merge sequence node bindings *)
      let rec merge_sequences acc = function
        | [] -> Some acc
        | (var, nodes) :: rest -> (
            match List.assoc_opt var acc with
            | Some existing_nodes
              when not (nodes_equal source existing_nodes nodes) ->
                None (* Conflict in sequence binding *)
            | Some _ -> merge_sequences acc rest (* Identical, skip *)
            | None -> merge_sequences ((var, nodes) :: acc) rest)
      in
      match
        merge_sequences existing.sequence_node_bindings
          new_bindings.sequence_node_bindings
      with
      | None -> None
      | Some merged_sequences ->
          Some
            {
              text_bindings = merged_text;
              node_bindings = merged_nodes;
              sequence_node_bindings = merged_sequences;
              correspondences =
                existing.correspondences @ new_bindings.correspondences;
            })

(** Check if a pattern node's text is a placeholder, return the original metavar
    name if so. Also handles wrapper nodes (like expression_statement) that
    contain just a placeholder plus a trailing semicolon. This allows patterns
    like `$X;` to work in languages where semicolons are parsed as part of the
    statement wrapper. *)
let is_placeholder substitutions source (node : Tree.pat Tree.t) =
  let text = Tree.text source node in
  (* First check if node's text directly matches *)
  match
    List.find_map
      (fun (var, placeholder) -> if text = placeholder then Some var else None)
      substitutions
  with
  | Some _ as result -> result
  | None -> (
      (* Check if node has a single named child whose text matches, and the parent
       text is exactly the child's text plus a semicolon.
       This handles expression_statement wrapping an identifier placeholder. *)
      match Tree.named_children node with
      | [ single_child ] ->
          let child_text = Tree.text source single_child in
          (* Only unwrap if parent is exactly child + ";" *)
          if text = child_text ^ ";" then
            List.find_map
              (fun (var, placeholder) ->
                if child_text = placeholder then Some var else None)
              substitutions
          else None
      | _ -> None)

(** Check if a pattern node's text is a placeholder for a sequence metavar *)
let is_sequence_placeholder ~pattern substitutions source
    (node : Tree.pat Tree.t) =
  match is_placeholder substitutions source node with
  | Some var_name -> List.mem var_name pattern.sequence_metavars
  | None -> false

(** Build index mapping node_type -> list of positions in array. Used to skip
    impossible split points when matching sequences. *)
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
    let is_seq =
      is_sequence_placeholder ~pattern substitutions pattern_source p
    in
    if is_seq then None (* Next element is also a sequence *)
    else Some p.Tree.node_type

(** Precompute cumulative texts for all possible sequence lengths.
    cumulative_texts.(i) = text of children 0..i-1 (empty string for i=0) *)
let precompute_cumulative_texts source (arr : Tree.src Tree.t array) start len =
  if len = 0 then [| "" |]
  else
    let result = Array.make (len + 1) "" in
    for i = 1 to len do
      let first = arr.(start) in
      let last = arr.(start + i - 1) in
      result.(i) <-
        String.sub source first.start_byte (last.end_byte - first.start_byte)
    done;
    result

(** Try to match a pattern node against a source node. Returns Some bindings if
    match succeeds, None otherwise. Bindings include both text (for
    display/consistency) and nodes (for 'on $VAR'). *)
let rec match_node ~pattern ~pattern_source ~source ~substitutions
    ?(bindings = empty_bindings) (pattern_node : Tree.pat Tree.t)
    (source_node : Tree.src Tree.t) =
  (* Check if pattern node is a metavar placeholder *)
  match is_placeholder substitutions pattern_source pattern_node with
  | Some var_name ->
      (* This is a metavar - bind it to the source node's text and node *)
      let source_text = Tree.text source source_node in
      let new_bindings =
        {
          text_bindings = [ (var_name, source_text) ];
          node_bindings = [ (var_name, source_node) ];
          sequence_node_bindings = [];
          correspondences = [];
        }
      in
      check_and_merge_bindings ~source bindings new_bindings
  | None ->
      (* Not a metavar - must match structurally *)
      let pattern_type = Tree.node_type pattern_node in
      let source_type = Tree.node_type source_node in
      if pattern_type <> source_type then
        (* Bare-sequence fallback: if the pattern root is a program/module/etc.
           wrapper node with 2+ named children, skip the type check and match
           the children sequence against the source node's children. This lets
           bare multi-statement patterns (e.g. `bar(); foo();`) match inside
           function bodies (statement_block) or other containers without
           requiring the statements to be wrapped in a function in the pattern.

           In strict mode without sequence metavars, the matched span need
           not be the parent's full children list — the pattern matches any
           consecutive run of N source children whose positional shape lines
           up. This mirrors the natural reading of a multi-statement patch
           ("find these statements as immediate successors anywhere") and
           matches Coccinelle's semantics for sequenced statements. The
           correspondences are adjusted by the chosen window offset so they
           describe positions in the parent's full children list, which
           [compute_edits_strict] uses to compute the matched byte range
           for the diff. *)
        let is_root_wrapper =
          match pattern_type with
          | "program" | "module" | "source_file" | "compilation_unit" -> true
          | _ -> false
        in
        if is_root_wrapper then
          let pattern_children = Tree.named_children pattern_node in
          match pattern_children with
          | [] | [ _ ] -> None
          | _ ->
              let source_children = Tree.named_children source_node in
              let pat_len = List.length pattern_children in
              let has_seq =
                List.exists
                  (fun p ->
                    is_sequence_placeholder ~pattern substitutions
                      pattern_source p)
                  pattern_children
              in
              if (not has_seq) && pattern.match_mode = Strict then
                (* Build windows over non-extra source children so that a
                   pattern like `bar(); foo();` still matches when an
                   inline comment separates the two statements in source.
                   The comment is a tree-sitter "extra" and is filtered
                   out for the windowing/matching, but the
                   correspondences are mapped back to the original
                   children-list indices so [compute_edits_strict]'s
                   bare-seq byte-range computation (which indexes the
                   unfiltered children of the parent) lines up. *)
                let indexed_non_extras =
                  List.mapi (fun idx c -> (idx, c)) source_children
                  |> List.filter
                       (fun (_, (c : Tree.src Tree.t)) -> not c.is_extra)
                in
                let non_extras_arr = Array.of_list indexed_non_extras in
                let filt_len = Array.length non_extras_arr in
                if filt_len < pat_len then None
                else begin
                  let result = ref None in
                  let i = ref 0 in
                  while !result = None && !i + pat_len <= filt_len do
                    let window =
                      Array.sub non_extras_arr !i pat_len
                      |> Array.to_list
                      |> List.map snd
                    in
                    (match
                       match_children_exact ~pattern ~pattern_source ~source
                         ~substitutions pattern_children window bindings
                     with
                    | Some bs ->
                        let start_i = !i in
                        let adjusted =
                          List.map
                            (fun (c : child_correspondence) ->
                              let filtered_idx = start_i + c.source_index in
                              let original_idx =
                                fst non_extras_arr.(filtered_idx)
                              in
                              { c with source_index = original_idx })
                            bs.correspondences
                        in
                        result :=
                          Some { bs with correspondences = adjusted }
                    | None -> ());
                    incr i
                  done;
                  !result
                end
              else
                match_children ~pattern ~pattern_source ~source
                  ~substitutions pattern_node source_node pattern_children
                  source_children bindings
        else None
      else
        (* Check if this is a leaf node *)
        let pattern_children = Tree.named_children pattern_node in
        let source_children = Tree.named_children source_node in
        if pattern_children = [] && source_children = [] then
          (* Both are leaves - text must match exactly *)
          if pattern_node.hash <> source_node.hash then None
          else
            let pattern_text = Tree.text pattern_source pattern_node in
            let source_text = Tree.text source source_node in
            if pattern_text = source_text then Some bindings else None
        else if pattern_children = [] then
          (* Pattern is leaf but source has children - no match *)
          None
        else
          (* Both have children - match them *)
          match_children ~pattern ~pattern_source ~source ~substitutions
            pattern_node source_node pattern_children source_children bindings

(** Match lists of children using arrays for O(n) sequence matching. Sequence
    metavars (marked with : sequence in preamble) can match 0 or more sibling
    nodes:
    - Strict: exact positional matching, no extra children allowed
    - Field: match by field name, extras in other fields ignored, ordered within
      fields
    - Partial: unordered subset matching, extras ignored *)
and match_children ~pattern ~pattern_source ~source ~substitutions
    (pattern_node : Tree.pat Tree.t) (source_node : Tree.src Tree.t)
    (pattern_children : Tree.pat Tree.t list)
    (source_children : Tree.src Tree.t list) bindings =
  match pattern.match_mode with
  | Field ->
      match_children_field ~pattern ~pattern_source ~source ~substitutions
        pattern_node source_node bindings
  | Partial ->
      match_children_partial ~pattern ~pattern_source ~source ~substitutions
        pattern_children source_children bindings
  | Strict ->
      match_children_exact ~pattern ~pattern_source ~source ~substitutions
        pattern_children source_children bindings

(** Exact (ordered) child matching with index-based optimization. When a
    sequence is followed by a concrete pattern element, we only try split points
    where that element's node type appears in source. *)
and match_children_exact ~pattern ~pattern_source ~source ~substitutions
    (pattern_children : Tree.pat Tree.t list)
    (source_children : Tree.src Tree.t list) bindings =
  let source_children = filter_irrelevant_extras pattern_children source_children in
  (* Convert to arrays for efficient indexing *)
  let pattern_arr = Array.of_list pattern_children in
  let source_arr = Array.of_list source_children in
  let pattern_len = Array.length pattern_arr in
  let source_len = Array.length source_arr in

  (* Build type index once: node_type -> positions where that type appears *)
  let type_index = build_type_index source_arr 0 source_len in

  let is_seq p =
    is_sequence_placeholder ~pattern substitutions pattern_source p
  in

  (* Whether the pattern contains any sequence metavar *)
  let has_seq_metavar = Array.exists is_seq pattern_arr in
  (* Whether the first / last element is itself a sequence *)
  let first_is_seq =
    pattern_len > 0 && is_seq pattern_arr.(0)
  in
  let last_is_seq =
    pattern_len > 0 && is_seq pattern_arr.(pattern_len - 1)
  in

  (* Match starting from pattern index pi and source index si *)
  let rec match_from bindings pi si =
    if pi >= pattern_len then
      (* Succeed if source is exhausted, or if the pattern has sequences and
         does not end with one: items after the last concrete element are
         implicitly accepted (Coccinelle-style subsequence semantics). When the
         pattern ends with an explicit sequence metavar, that sequence must
         consume the remaining children so their text is captured correctly. *)
      if si >= source_len || (has_seq_metavar && not last_is_seq) then
        Some bindings
      else None
    else
      let p = pattern_arr.(pi) in
      let is_seq =
        is_sequence_placeholder ~pattern substitutions pattern_source p
      in
      if is_seq then
        (* Sequence metavar: try matching source children *)
        match is_placeholder substitutions pattern_source p with
        | None -> None
        | Some var_name ->
            let remaining = source_len - si in
            let cumulative =
              precompute_cumulative_texts source source_arr si remaining
            in

            (* Determine which positions to try based on what follows *)
            let next_anchor_type =
              get_next_anchor_type ~pattern ~pattern_source ~substitutions
                pattern_arr (pi + 1)
            in

            let positions_to_try =
              match next_anchor_type with
              | None ->
                  (* No anchor or next is also a sequence - must try all positions *)
                  List.init (remaining + 1) Fun.id
              | Some anchor_type ->
                  (* Only try positions where anchor type appears, plus end position *)
                  let anchor_positions =
                    try Hashtbl.find type_index anchor_type
                    with Not_found -> []
                  in
                  (* Filter to positions >= si and convert to offsets from si *)
                  let valid_offsets =
                    List.filter_map
                      (fun pos -> if pos >= si then Some (pos - si) else None)
                      anchor_positions
                  in
                  (* Also try taking all remaining (sequence consumes everything) *)
                  let offsets = valid_offsets @ [ remaining ] in
                  (* Remove duplicates and sort *)
                  List.sort_uniq compare offsets
            in

            (* Try each candidate split point *)
            let rec try_positions = function
              | [] -> None
              | n :: rest -> (
                  let seq_text = cumulative.(n) in
                  let seq_nodes = Array.to_list (Array.sub source_arr si n) in
                  let seq_corrs =
                    List.init n (fun k ->
                        { pattern_index = pi; source_index = si + k })
                  in
                  let seq_binding =
                    {
                      text_bindings = [ (var_name, seq_text) ];
                      node_bindings = [];
                      sequence_node_bindings = [ (var_name, seq_nodes) ];
                      correspondences = seq_corrs;
                    }
                  in
                  match
                    check_and_merge_bindings ~source bindings seq_binding
                  with
                  | None -> try_positions rest (* Binding conflict *)
                  | Some merged -> (
                      match match_from merged (pi + 1) (si + n) with
                      | Some result -> Some result
                      | None -> try_positions rest))
            in
            try_positions positions_to_try
      else if
        (* Regular metavar or structural match *)
        si >= source_len
      then None
      else
        match
          match_node ~pattern ~pattern_source ~source ~substitutions ~bindings p
            source_arr.(si)
        with
        | None -> None
        | Some merged ->
            let corr = { pattern_index = pi; source_index = si } in
            (* Reset correspondences to outer-level only: inner recursive matching
               (via match_node) propagates and accumulates correspondences from
               sub-tree matches (e.g., the children of a lexical_declaration).
               Those inner indices belong to a different index space and would
               corrupt the span calculation in compute_edits_strict if kept.
               We only need top-level correspondences at this container level. *)
            let merged =
              { merged with correspondences = corr :: bindings.correspondences }
            in
            match_from merged (pi + 1) (si + 1)
  in
  (* If the pattern has sequences but doesn't start with one, try starting at
     each position where the first element's node type appears. This gives
     implicit-leading semantics: items before the first concrete element are
     silently accepted without requiring a leading `...` in the pattern.
     When the pattern starts with an explicit sequence it already handles the
     prefix by consuming 0 or more items before the first concrete element. *)
  if not has_seq_metavar || first_is_seq then match_from bindings 0 0
  else
    let anchor_type = pattern_arr.(0).Tree.node_type in
    let start_positions =
      try Hashtbl.find type_index anchor_type with Not_found -> []
    in
    let rec try_starts = function
      | [] -> None
      | si :: rest -> (
          match match_from bindings 0 si with
          | Some _ as r -> r
          | None -> try_starts rest)
    in
    try_starts start_positions

(** Partial (unordered subset) child matching. Each pattern child must find a
    matching source child (any position, consumed once matched). Extra source
    children are ignored. Order doesn't matter. *)
and match_children_partial ~pattern ~pattern_source ~source ~substitutions
    (pattern_children : Tree.pat Tree.t list)
    (source_children : Tree.src Tree.t list) bindings =
  let source_children = filter_irrelevant_extras pattern_children source_children in
  let source_arr = Array.of_list source_children in
  let source_len = Array.length source_arr in
  (* Track which source children have been consumed *)
  let used = Array.make source_len false in

  (* Match each pattern child with backtracking over source choices *)
  let rec match_all bindings pi = function
    | [] -> Some bindings
    | p :: rest ->
        let is_seq =
          is_sequence_placeholder ~pattern substitutions pattern_source p
        in
        if is_seq then
          (* Sequence metavars don't make sense in partial mode - reject *)
          None
        else
          let rec try_source si =
            if si >= source_len then None
            else if used.(si) then try_source (si + 1)
            else
              match
                match_node ~pattern ~pattern_source ~source ~substitutions
                  ~bindings p source_arr.(si)
              with
              | None -> try_source (si + 1)
              | Some merged -> (
                  let corr = { pattern_index = pi; source_index = si } in
                  (* Keep accumulated outer correspondences plus our new top-level
                 one; discard inner ones from recursive matching — they use a
                 different index space and would corrupt the
                 pattern_index → source_index mapping used by
                 apply_alignment_with_correspondences in transforms. *)
                  let merged =
                    {
                      merged with
                      correspondences = bindings.correspondences @ [ corr ];
                    }
                  in
                  used.(si) <- true;
                  match match_all merged (pi + 1) rest with
                  | Some result -> Some result
                  | None ->
                      used.(si) <- false;
                      try_source (si + 1))
          in
          try_source 0
  in
  match_all bindings 0 pattern_children

(** Field-based child matching. Matches children by tree-sitter field name
    instead of position. Children with the same field name are matched in order
    within that field group. Extra source fields (not in pattern) are ignored.
    Children without field names are matched positionally within their group. *)
and match_children_field ~pattern ~pattern_source ~source ~substitutions
    (pattern_node : Tree.pat Tree.t) (source_node : Tree.src Tree.t) bindings =
  let pattern_children = Tree.named_children_with_fields pattern_node in
  let source_children = Tree.named_children_with_fields source_node in

  (* Group children by field name *)
  let group_by_field children =
    (* let tbl : (string option, Tree.pat Tree.t list) Hashtbl.t = Hashtbl.create 8 in *)
    let tbl = Hashtbl.create 8 in
    let order = ref [] in
    List.iter
      (fun (field, node) ->
        let existing = Hashtbl.find_opt tbl field |> Option.value ~default:[] in
        if existing = [] then order := field :: !order;
        Hashtbl.replace tbl field (existing @ [ node ]))
      children;
    List.rev_map (fun field -> (field, Hashtbl.find tbl field)) !order
  in

  let pattern_grouped = group_by_field pattern_children in
  let source_grouped = group_by_field source_children in

  (* Match each field group from pattern against corresponding source field group *)
  let rec match_groups bindings = function
    | [] -> Some bindings
    | (field_name, pat_nodes) :: rest -> (
        let src_nodes =
          List.assoc_opt field_name source_grouped |> Option.value ~default:[]
        in
        (* Use exact matching within field group - order matters within a field *)
        match
          match_children_exact ~pattern ~pattern_source ~source ~substitutions
            pat_nodes src_nodes bindings
        with
        | None -> None
        | Some merged -> match_groups merged rest)
  in
  match_groups bindings pattern_grouped

(** Re-run partial matching on [pattern_children] against [source_children] and
    return the resulting correspondences. Used by the transform step to recover
    the pattern→source child mapping inside container nodes that were initially
    aligned as same-type EReplace pairs. *)
let rematch_partial_correspondences ~pattern ~pattern_source ~source
    ~substitutions pattern_children source_children =
  match
    match_children_partial ~pattern ~pattern_source ~source ~substitutions
      pattern_children source_children empty_bindings
  with
  | None -> None
  | Some bindings -> Some bindings.correspondences

(** Get the innermost meaningful node from a pattern. Unwraps
    program/module/expression_statement wrappers to get to the actual pattern
    content. *)
let get_pattern_content pattern : Tree.pat Tree.t =
  Tree.unwrap_root pattern.tree.root

(** Try to match inner pattern children directly against source element children
    in strict (ordered) mode, bypassing the top-level node type check. This is
    used as a fallback when the pattern parses to a different AST node type than
    the source element (e.g., 'key: value' parses as labeled_statement but
    source nodes are pair). Returns None if the pattern has no children or if
    children don't match. *)
let try_match_children_directly ~pattern ~source (source_node : Tree.src Tree.t)
    =
  let pattern_node = get_pattern_content pattern in
  let pattern_children = Tree.named_children pattern_node in
  if pattern_children = [] then None
  else
    let source_children = Tree.named_children source_node in
    match_children_exact ~pattern ~pattern_source:pattern.source ~source
      ~substitutions:pattern.substitutions pattern_children source_children
      empty_bindings
