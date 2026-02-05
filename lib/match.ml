(** Pattern matching using concrete syntax with metavariables *)

(** Index for fast node lookup by type *)
type ast_index = {
  by_type: (string, Tree.src Tree.t list) Hashtbl.t;
}

(** Match mode for child matching *)
type match_mode =
  | Strict   (** Exact positional matching - no extra children allowed *)
  | Field    (** Match by field name - extras in other fields ignored, ordered within fields *)
  | Partial  (** Unordered subset matching - extras ignored, order doesn't matter *)

(** A parsed pattern with metavariable information *)
type pattern = {
  metavars: string list;  (** Original metavar names (e.g., ["$msg"; "$fn"]) *)
  sequence_metavars: string list;  (** Metavars that match sequences (declared with * suffix) *)
  substitutions: (string * string) list;  (** Maps original name -> placeholder *)
  tree: Tree.pat Tree.tree;  (** Parsed pattern tree (phantom-typed as pattern) *)
  source: string;  (** Transformed source (with placeholders) *)
  original_source: string;  (** Original pattern source (after preamble) *)
  match_mode: match_mode;  (** How to match children *)
  on_var: string option;  (** If set, match against the node bound to this var instead of traversing *)
}

(** A single match result *)
type match_result = {
  node: Tree.src Tree.t;  (** The matched source node *)
  bindings: (string * string) list;  (** Metavar name -> matched text *)
  node_bindings: (string * Tree.src Tree.t) list;  (** Metavar name -> matched node (for 'on $VAR') *)
  sequence_node_bindings: (string * Tree.src Tree.t list) list;  (** Metavar name -> matched nodes for sequences *)
  start_point: Tree.point;
  end_point: Tree.point;
}

(** A context match for nested patterns *)
type context_match = {
  context_node: Tree.src Tree.t;
  context_bindings: (string * string) list;
  context_node_bindings: (string * Tree.src Tree.t) list;
  context_sequence_node_bindings: (string * Tree.src Tree.t list) list;
  context_start_point: Tree.point;
  context_end_point: Tree.point;
}

(** Result of nested pattern matching *)
type nested_match_result = {
  inner_node: Tree.src Tree.t;
  inner_bindings: (string * string) list;
  inner_node_bindings: (string * Tree.src Tree.t) list;
  inner_sequence_node_bindings: (string * Tree.src Tree.t list) list;
  all_bindings: (string * string) list;
  all_node_bindings: (string * Tree.src Tree.t) list;
  all_sequence_node_bindings: (string * Tree.src Tree.t list) list;
  contexts: context_match list;  (** outermost first *)
  start_point: Tree.point;
  end_point: Tree.point;
}

(** Result of matching with parse information *)
type match_search_result = {
  matches: nested_match_result list;
  parse_error_count: int;  (** Number of ERROR nodes in source *)
}

(** A nested pattern with multiple sections *)
type nested_pattern = {
  patterns: pattern list;  (** All sections; last is target, rest are contexts *)
}

(** Result of parsing a single preamble line *)
type preamble_line =
  | Metavar of string * bool  (** name, is_sequence *)
  | MatchMode of match_mode
  | OnVar of string

(** Parse a single preamble line.
    Formats:
    - "metavar $name: single" or "metavar $name: sequence"
    - "match: partial"
    - "on $var"
    Returns None for empty lines, Some directive otherwise. *)
let parse_preamble_line line =
  let line = String.trim line in
  if line = "" then None
  else if String.starts_with ~prefix:"metavar " line then
    (* Extract everything after "metavar " *)
    let rest = String.sub line 8 (String.length line - 8) |> String.trim in
    (* Split on ":" to get name and type *)
    match String.split_on_char ':' rest with
    | [name_part; type_part] ->
      let name = String.trim name_part in
      let typ = String.trim type_part in
      if not (String.starts_with ~prefix:"$" name) then
        failwith (Printf.sprintf "Metavar name must start with '$': %s" name);
      (match typ with
       | "single" -> Some (Metavar (name, false))
       | "sequence" -> Some (Metavar (name, true))
       | _ -> failwith (Printf.sprintf "Invalid metavar type '%s' (expected 'single' or 'sequence')" typ))
    | _ ->
      failwith (Printf.sprintf "Invalid metavar declaration (expected 'metavar $name: type'): %s" line)
  else if line = "match: strict" then
    Some (MatchMode Strict)
  else if line = "match: field" then
    Some (MatchMode Field)
  else if line = "match: partial" then
    Some (MatchMode Partial)
  else if String.starts_with ~prefix:"on " line then
    let var = String.sub line 3 (String.length line - 3) |> String.trim in
    if not (String.starts_with ~prefix:"$" var) then
      failwith (Printf.sprintf "on directive requires a metavar (starting with '$'): %s" var);
    Some (OnVar var)
  else
    failwith (Printf.sprintf "Invalid preamble line: %s" line)

(** Parsed preamble result *)
type preamble_result = {
  p_metavars: string list;
  p_sequence_metavars: string list;
  p_match_mode: match_mode option;
  p_on_var: string option;
  p_pattern_body: string;
}

(** Parse the @@ preamble from pattern text.
    Format:
      @@
      metavar $name: single
      metavar $other: sequence
      match: partial
      on $var
      @@
      pattern body
    Returns parsed preamble or raises Failure if malformed. *)
let parse_preamble text =
  let text = String.trim text in
  if not (String.starts_with ~prefix:"@@" text) then
    failwith "Pattern must start with @@ preamble"
  else
    (* Skip first @@ *)
    let after_first = String.sub text 2 (String.length text - 2) in
    let after_first = String.trim after_first in
    (* Find closing @@ *)
    let rec find_closing pos =
      if pos >= String.length after_first - 1 then
        failwith "Pattern must have closing @@"
      else if after_first.[pos] = '@' && after_first.[pos + 1] = '@' then
        pos
      else
        find_closing (pos + 1)
    in
    let closing_pos = find_closing 0 in
    let vars_section = String.sub after_first 0 closing_pos in
    let pattern_body = String.sub after_first (closing_pos + 2)
                         (String.length after_first - closing_pos - 2) in
    let pattern_body = String.trim pattern_body in
    (* Parse preamble lines *)
    let lines = String.split_on_char '\n' vars_section in
    let parsed = List.filter_map parse_preamble_line lines in
    (* Extract components *)
    let metavars = List.filter_map (function Metavar (n, _) -> Some n | _ -> None) parsed in
    let sequence_metavars = List.filter_map (function Metavar (n, true) -> Some n | _ -> None) parsed in
    let match_mode = List.find_map (function MatchMode m -> Some m | _ -> None) parsed in
    let on_var = List.find_map (function OnVar v -> Some v | _ -> None) parsed in
    { p_metavars = metavars;
      p_sequence_metavars = sequence_metavars;
      p_match_mode = match_mode;
      p_on_var = on_var;
      p_pattern_body = pattern_body }

(** Generate a valid placeholder identifier for a metavar *)
let placeholder_for_index i = Printf.sprintf "__meta_%d__" i

(** Substitute metavars with valid placeholders in pattern text *)
let substitute_metavars metavars pattern_text =
  let substitutions = List.mapi (fun i var ->
    (var, placeholder_for_index i)
  ) metavars in
  let result = List.fold_left (fun text (var, placeholder) ->
    (* Simple string replacement - replace all occurrences *)
    let replace s =
      match String.split_on_char var.[0] s with
      | [_] -> s  (* var not found *)
      | _ ->
        (* More careful replacement to match whole var *)
        let buf = Buffer.create (String.length s) in
        let i = ref 0 in
        while !i < String.length s do
          if !i + String.length var <= String.length s &&
             String.sub s !i (String.length var) = var then begin
            Buffer.add_string buf placeholder;
            i := !i + String.length var
          end else begin
            Buffer.add_char buf s.[!i];
            incr i
          end
        done;
        Buffer.contents buf
    in
    replace text
  ) pattern_text substitutions in
  (result, substitutions)

(** Find all metavars used in pattern text (tokens starting with $ followed by uppercase).
    This distinguishes metavars ($MSG, $PROP) from language variables ($this, $name).
    Metavars must start with an uppercase letter after the $. *)
let find_metavars_in_text text =
  let results = ref [] in
  let i = ref 0 in
  while !i < String.length text do
    if text.[!i] = '$' && !i + 1 < String.length text then begin
      let first_char = text.[!i + 1] in
      (* Only consider it a metavar if first char after $ is uppercase *)
      if first_char >= 'A' && first_char <= 'Z' then begin
        let start = !i in
        incr i;
        (* Collect identifier characters *)
        while !i < String.length text &&
              (let c = text.[!i] in
               (c >= 'a' && c <= 'z') ||
               (c >= 'A' && c <= 'Z') ||
               (c >= '0' && c <= '9') ||
               c = '_') do
          incr i
        done;
        if !i > start + 1 then
          results := String.sub text start (!i - start) :: !results
      end else
        incr i
    end else
      incr i
  done;
  List.sort_uniq String.compare !results

(** Find undeclared metavars in pattern body *)
let find_undeclared_metavars declared_metavars pattern_body =
  let used = find_metavars_in_text pattern_body in
  List.filter (fun var -> not (List.mem var declared_metavars)) used

(** Validate that all metavars in pattern body are declared *)
let validate_metavars metavars pattern_body =
  let undeclared = find_undeclared_metavars metavars pattern_body in
  if undeclared <> [] then
    failwith (Printf.sprintf "Undeclared metavars: %s" (String.concat ", " undeclared))

(** Preprocess ellipsis (...) in pattern text.
    Replaces ... with __ellipsis_N__ placeholders.
    Adds trailing ; if ... is followed by only whitespace then newline (statement context).
    Does NOT replace ... if immediately followed by $ or alphanumeric (PHP spread operator).
    Returns (transformed_text, ellipsis_substitutions) where ellipsis_substitutions
    maps unique var names to placeholder strings (e.g., [("..._0", "__ellipsis_0__")]). *)
let preprocess_ellipsis text =
  let len = String.length text in
  let result = Buffer.create len in
  let substitutions = ref [] in
  let counter = ref 0 in
  let i = ref 0 in
  while !i < len do
    (* Check for ... *)
    if !i + 2 < len && text.[!i] = '.' && text.[!i+1] = '.' && text.[!i+2] = '.' then begin
      (* Check what follows - don't replace if followed by $ or alphanumeric (spread operator) *)
      let next_char = if !i + 3 < len then Some text.[!i+3] else None in
      let is_spread = match next_char with
        | Some '$' -> true
        | Some c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') -> true
        | _ -> false
      in
      if is_spread then begin
        (* Keep as literal ... *)
        Buffer.add_string result "...";
        i := !i + 3
      end else begin
        (* Replace with placeholder - use unique var name for each ellipsis *)
        let n = !counter in
        let var_name = Printf.sprintf "..._%d" n in
        let placeholder = Printf.sprintf "__ellipsis_%d__" n in
        substitutions := (var_name, placeholder) :: !substitutions;
        incr counter;
        Buffer.add_string result placeholder;
        i := !i + 3;
        (* Check if followed by only whitespace then newline (or end) - add ; for statement context *)
        let j = ref !i in
        while !j < len && (text.[!j] = ' ' || text.[!j] = '\t') do
          incr j
        done;
        if !j >= len || text.[!j] = '\n' then
          Buffer.add_char result ';'
      end
    end else begin
      Buffer.add_char result text.[!i];
      incr i
    end
  done;
  (Buffer.contents result, List.rev !substitutions)

(** Parse a pattern file/string into a pattern structure *)
let parse_pattern ~language pattern_text =
  let preamble = parse_preamble pattern_text in
  (* Require explicit match mode *)
  let match_mode = match preamble.p_match_mode with
    | Some m -> m
    | None -> failwith "Pattern must specify match mode (match: strict, match: field, or match: partial)"
  in
  validate_metavars preamble.p_metavars preamble.p_pattern_body;
  (* First preprocess ellipsis (...) *)
  let (after_ellipsis, ellipsis_subs) = preprocess_ellipsis preamble.p_pattern_body in
  (* Then substitute declared metavars *)
  let (transformed_source, metavar_subs) = substitute_metavars preamble.p_metavars after_ellipsis in
  (* Combine substitutions *)
  let all_substitutions = metavar_subs @ ellipsis_subs in
  (* Ellipsis var names (e.g., "..._0", "..._1") are sequence metavars *)
  let ellipsis_var_names = List.map fst ellipsis_subs in
  (* Parse the transformed pattern with tree-sitter (typed as pattern tree) *)
  let tree = Tree.parse_as_pattern ~language transformed_source in
  (* Add ellipsis var names to sequence_metavars *)
  let sequence_metavars = preamble.p_sequence_metavars @ ellipsis_var_names in
  {
    metavars = preamble.p_metavars;
    sequence_metavars;
    substitutions = all_substitutions;
    tree;
    source = transformed_source;
    original_source = preamble.p_pattern_body;
    match_mode;
    on_var = preamble.p_on_var;
  }

(** Split pattern text into sections.
    Each section has format: @@ metavars @@ pattern_code
    A new section starts when we see @@ after having seen two @@ in the current section.
    Returns list of section strings (each starting with @@). *)
let split_pattern_sections text =
  let text = String.trim text in
  if not (String.starts_with ~prefix:"@@" text) then
    failwith "Pattern must start with @@ preamble"
  else
    let lines = String.split_on_char '\n' text in
    (* State: at_count tracks how many @@ we've seen in the current section *)
    let rec find_sections acc current_section at_count lines =
      match lines with
      | [] ->
        (* End of input - add current section if non-empty *)
        let section = String.trim (String.concat "\n" (List.rev current_section)) in
        if section = "" then List.rev acc
        else List.rev (section :: acc)
      | line :: rest ->
        let trimmed = String.trim line in
        let line_has_at = String.starts_with ~prefix:"@@" trimmed in
        if line_has_at && at_count >= 2 then
          (* We've already seen two @@, so this @@ starts a new section *)
          let section = String.trim (String.concat "\n" (List.rev current_section)) in
          find_sections (section :: acc) [line] 1 rest
        else if line_has_at then
          (* This is part of the current section's preamble markers *)
          find_sections acc (line :: current_section) (at_count + 1) rest
        else
          find_sections acc (line :: current_section) at_count rest
    in
    let sections = find_sections [] [] 0 lines in
    if sections = [] then [text] else sections

(** Parse a nested pattern from text with multiple @@ sections *)
let parse_nested_pattern ~language pattern_text =
  let sections = split_pattern_sections pattern_text in
  let patterns = List.map (parse_pattern ~language) sections in
  { patterns }

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

(** Match bindings: text bindings and node bindings (from source) *)
type match_bindings = {
  text_bindings: (string * string) list;
  node_bindings: (string * Tree.src Tree.t) list;
  sequence_node_bindings: (string * Tree.src Tree.t list) list;  (** For sequence metavars *)
}

let empty_bindings = { text_bindings = []; node_bindings = []; sequence_node_bindings = [] }

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
           sequence_node_bindings = [] }
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

(** Build index mapping node_type -> list of positions in array.
    Used to skip impossible split points when matching sequences. *)
and build_type_index (arr : Tree.src Tree.t array) start len =
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
and get_next_anchor_type ~pattern ~pattern_source ~substitutions
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
and precompute_cumulative_texts source (arr : Tree.src Tree.t array) start len =
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

(** Merge text bindings, checking that same var doesn't bind to different values *)
and check_and_merge_text_bindings existing new_bindings =
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
and check_and_merge_bindings existing new_bindings =
  match check_and_merge_text_bindings existing.text_bindings new_bindings.text_bindings with
  | None -> None
  | Some merged_text ->
    (* Node bindings don't need consistency check - just accumulate *)
    Some { text_bindings = merged_text;
           node_bindings = existing.node_bindings @ new_bindings.node_bindings;
           sequence_node_bindings = existing.sequence_node_bindings @ new_bindings.sequence_node_bindings }

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
              let seq_binding = {
                text_bindings = [(var_name, seq_text)];
                node_bindings = [];
                sequence_node_bindings = [(var_name, seq_nodes)]
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

  (* Try to find a matching source child for pattern child p *)
  let find_match_for bindings (p : Tree.pat Tree.t) =
    let is_seq = is_sequence_placeholder ~pattern substitutions pattern_source p in
    if is_seq then
      (* Sequence metavars don't make sense in partial mode - treat as single *)
      None
    else
      (* Try each unconsumed source child *)
      let rec try_source si =
        if si >= source_len then None
        else if used.(si) then try_source (si + 1)
        else
          match match_node ~pattern ~pattern_source ~source ~substitutions
                  p source_arr.(si) with
          | None -> try_source (si + 1)
          | Some new_bindings ->
            match check_and_merge_bindings bindings new_bindings with
            | None -> try_source (si + 1)  (* Binding conflict, try next *)
            | Some merged ->
              used.(si) <- true;
              Some merged
      in
      try_source 0
  in

  (* Match each pattern child *)
  let rec match_all bindings = function
    | [] -> Some bindings
    | p :: rest ->
      match find_match_for bindings p with
      | None -> None
      | Some merged -> match_all merged rest
  in
  match_all empty_bindings pattern_children

(** Field-based child matching.
    Matches children by tree-sitter field name instead of position.
    Children with the same field name are matched in order within that field group.
    Extra source fields (not in pattern) are ignored.
    Children without field names are matched positionally within their group. *)
and match_children_field ~pattern ~pattern_source ~source ~substitutions
    (pattern_node : Tree.pat Tree.t) (source_node : Tree.src Tree.t) =
  let pattern_children = Tree.named_children_with_fields pattern_node in
  let source_children = Tree.named_children_with_fields source_node in

  (* Group pattern children by field name *)
  let group_by_field_pat children =
    let tbl : (string option, Tree.pat Tree.t list) Hashtbl.t = Hashtbl.create 8 in
    let order = ref [] in
    List.iter (fun (field, node) ->
      let existing = Hashtbl.find_opt tbl field |> Option.value ~default:[] in
      if existing = [] then order := field :: !order;
      Hashtbl.replace tbl field (existing @ [node])
    ) children;
    List.rev_map (fun field -> (field, Hashtbl.find tbl field)) !order
  in

  (* Group source children by field name *)
  let group_by_field_src children =
    let tbl : (string option, Tree.src Tree.t list) Hashtbl.t = Hashtbl.create 8 in
    let order = ref [] in
    List.iter (fun (field, node) ->
      let existing = Hashtbl.find_opt tbl field |> Option.value ~default:[] in
      if existing = [] then order := field :: !order;
      Hashtbl.replace tbl field (existing @ [node])
    ) children;
    List.rev_map (fun field -> (field, Hashtbl.find tbl field)) !order
  in

  let pattern_grouped = group_by_field_pat pattern_children in
  let source_grouped = group_by_field_src source_children in

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

(** Find all matches of a pattern in source code *)
let find_matches ~language ~pattern_text ~source_text =
  let pattern = parse_pattern ~language pattern_text in
  let source_tree = Tree.parse ~language source_text in
  let source = source_tree.source in
  let pattern_node = get_pattern_content pattern in
  let source_root = source_tree.root in
  let results = ref [] in
  (* Traverse source tree, trying to match at each node *)
  Tree.traverse (fun source_node ->
    match match_node
            ~pattern
            ~pattern_source:pattern.source
            ~source
            ~substitutions:pattern.substitutions
            pattern_node source_node with
    | Some mb ->
      results := {
        node = source_node;
        bindings = mb.text_bindings;
        node_bindings = mb.node_bindings;
        sequence_node_bindings = mb.sequence_node_bindings;
        start_point = Tree.start_point source_node;
        end_point = Tree.end_point source_node;
      } :: !results
    | None -> ()
  ) source_root;
  List.rev !results

(** Find matches in a file *)
let find_matches_in_file ~language ~pattern_text ~source_path =
  let source_text = In_channel.with_open_text source_path In_channel.input_all in
  find_matches ~language ~pattern_text ~source_text

(** Find all matches of a pattern within a specific subtree.
    Checks inherited bindings for conflicts and merges them with new bindings. *)
let find_matches_in_subtree ~pattern ~inherited_bindings ~source
    ~(root_node : Tree.src Tree.t) =
  let pattern_node = get_pattern_content pattern in
  let results = ref [] in
  root_node |> Tree.traverse (fun source_node ->
    match match_node
            ~pattern
            ~pattern_source:pattern.source
            ~source
            ~substitutions:pattern.substitutions
            pattern_node source_node with
    | Some mb ->
      (* Check if new bindings are consistent with inherited ones *)
      (match check_and_merge_bindings inherited_bindings mb with
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
  let nested = parse_nested_pattern ~language pattern_text in
  let source_tree = Tree.parse ~language source_text in
  let source = source_tree.source in
  let source_root = source_tree.root in
  let matches =
    if List.length nested.patterns = 1 then
      (* Single pattern: use existing logic, convert to nested_match_result *)
      let pattern = List.hd nested.patterns in
      let matches = find_matches_in_subtree
          ~pattern
          ~inherited_bindings:empty_bindings
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
        ~inherited_bindings:empty_bindings
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
  let pattern_node = get_pattern_content pattern in
  (* Check if pattern root is a metavar placeholder *)
  match is_placeholder pattern.substitutions pattern.source pattern_node with
  | Some _ ->
    (* Pattern root is a metavar - can't filter by type, fall back to traversal *)
    find_matches_in_subtree ~pattern ~inherited_bindings:empty_bindings ~source ~root_node:source_root
  | None ->
    (* Query index for candidate nodes *)
    let pattern_type = Tree.node_type pattern_node in
    let candidates = Hashtbl.find_opt index.by_type pattern_type
                     |> Option.value ~default:[] in
    List.filter_map (fun source_node ->
      match match_node
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
    let pattern = parse_pattern ~language pattern_text in
    let matches = find_matches_with_index ~index ~pattern ~source ~source_root:source_tree.root in
    List.map (fun m -> (i, m)) matches
  ) patterns)
