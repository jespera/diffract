(** Pattern matching using concrete syntax with metavariables *)

(** Index for fast node lookup by type *)
type ast_index = {
  by_type: (string, Tree.t list) Hashtbl.t;
}

(** A parsed pattern with metavariable information *)
type pattern = {
  metavars: string list;  (** Original metavar names (e.g., ["$msg"; "$fn"]) *)
  sequence_metavars: string list;  (** Metavars that match sequences (declared with * suffix) *)
  substitutions: (string * string) list;  (** Maps original name -> placeholder *)
  tree: Tree.tree;  (** Parsed pattern tree *)
  source: string;  (** Transformed source (with placeholders) *)
  original_source: string;  (** Original pattern source (after preamble) *)
}

(** A single match result *)
type match_result = {
  node: Tree.t;  (** The matched node *)
  bindings: (string * string) list;  (** Metavar name -> matched text *)
  start_point: Tree.point;
  end_point: Tree.point;
}

(** A context match for nested patterns *)
type context_match = {
  context_node: Tree.t;
  context_bindings: (string * string) list;
  context_start_point: Tree.point;
  context_end_point: Tree.point;
}

(** Result of nested pattern matching *)
type nested_match_result = {
  inner_node: Tree.t;
  inner_bindings: (string * string) list;
  all_bindings: (string * string) list;
  contexts: context_match list;  (** outermost first *)
  start_point: Tree.point;
  end_point: Tree.point;
}

(** A nested pattern with multiple sections *)
type nested_pattern = {
  patterns: pattern list;  (** All sections; last is target, rest are contexts *)
}

(** Parse a metavar declaration line.
    Format: "metavar $name: single" or "metavar $name: sequence"
    Returns (name, is_sequence) or raises Failure if malformed. *)
let parse_metavar_decl line =
  let line = String.trim line in
  if line = "" then None
  else if not (String.starts_with ~prefix:"metavar " line) then
    failwith (Printf.sprintf "Invalid declaration (expected 'metavar'): %s" line)
  else
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
       | "single" -> Some (name, false)
       | "sequence" -> Some (name, true)
       | _ -> failwith (Printf.sprintf "Invalid metavar type '%s' (expected 'single' or 'sequence')" typ))
    | _ ->
      failwith (Printf.sprintf "Invalid metavar declaration (expected 'metavar $name: type'): %s" line)

(** Parse the @@ preamble from pattern text.
    Format:
      @@
      metavar $name: single
      metavar $other: sequence
      @@
      pattern body
    Returns (metavars, sequence_metavars, pattern_body) or raises Failure if malformed. *)
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
    (* Parse metavar declarations - one per line *)
    let lines = String.split_on_char '\n' vars_section in
    let parsed = List.filter_map parse_metavar_decl lines in
    let metavars = List.map fst parsed in
    let sequence_metavars = parsed
                            |> List.filter snd
                            |> List.map fst in
    (metavars, sequence_metavars, pattern_body)

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

(** Find all metavars used in pattern text (tokens starting with $) *)
let find_metavars_in_text text =
  let results = ref [] in
  let i = ref 0 in
  while !i < String.length text do
    if text.[!i] = '$' then begin
      (* Found a potential metavar *)
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

(** Parse a pattern file/string into a pattern structure *)
let parse_pattern ~language pattern_text =
  let (metavars, sequence_metavars, pattern_body) = parse_preamble pattern_text in
  validate_metavars metavars pattern_body;
  let (transformed_source, substitutions) = substitute_metavars metavars pattern_body in
  (* Parse the transformed pattern with tree-sitter *)
  let tree = Tree.parse ~language transformed_source in
  {
    metavars;
    sequence_metavars;
    substitutions;
    tree;
    source = transformed_source;
    original_source = pattern_body;
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

(** Check if a node's text is a placeholder, return the original metavar name if so *)
let is_placeholder substitutions source node =
  let text = Tree.text source node in
  List.find_map (fun (var, placeholder) ->
    if text = placeholder then Some var else None
  ) substitutions

(** Check if a node's text is a placeholder for a sequence metavar *)
let is_sequence_placeholder ~pattern substitutions source node =
  match is_placeholder substitutions source node with
  | Some var_name -> List.mem var_name pattern.sequence_metavars
  | None -> false

(** Try to match a pattern node against a source node.
    Returns Some bindings if match succeeds, None otherwise. *)
let rec match_node ~pattern ~pattern_source ~source ~substitutions pattern_node source_node =
  (* Check if pattern node is a metavar placeholder *)
  match is_placeholder substitutions pattern_source pattern_node with
  | Some var_name ->
    (* This is a metavar - bind it to the source node's text *)
    let source_text = Tree.text source source_node in
    Some [(var_name, source_text)]
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
        if pattern_text = source_text then Some []
        else None
      else if pattern_children = [] then
        (* Pattern is leaf but source has children - no match *)
        None
      else
        (* Both have children - match them *)
        match_children ~pattern ~pattern_source ~source ~substitutions
          pattern_children source_children

(** Precompute cumulative texts for all possible sequence lengths.
    cumulative_texts.(i) = text of children 0..i-1 (empty string for i=0) *)
and precompute_cumulative_texts source arr start len =
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

(** Match lists of children using arrays for O(n) sequence matching.
    Sequence metavars (marked with * in preamble) can match 0 or more source children. *)
and match_children ~pattern ~pattern_source ~source ~substitutions pattern_children source_children =
  (* Convert to arrays for efficient indexing *)
  let pattern_arr = Array.of_list pattern_children in
  let source_arr = Array.of_list source_children in
  let pattern_len = Array.length pattern_arr in
  let source_len = Array.length source_arr in

  (* Match starting from pattern index pi and source index si *)
  let rec match_from bindings pi si =
    if pi >= pattern_len then
      (* No more pattern - succeed only if no source left *)
      if si >= source_len then Some bindings else None
    else
      let p = pattern_arr.(pi) in
      let is_seq = is_sequence_placeholder ~pattern substitutions pattern_source p in
      if is_seq then
        (* Sequence metavar: try matching 0, 1, 2, ... source children *)
        match is_placeholder substitutions pattern_source p with
        | None -> None
        | Some var_name ->
          (* Precompute all possible sequence texts once - O(n) *)
          let remaining = source_len - si in
          let cumulative = precompute_cumulative_texts source source_arr si remaining in
          (* Try each split point with O(1) text lookup *)
          let rec try_take n =
            if n > remaining then None
            else
              let seq_text = cumulative.(n) in
              let seq_binding = [(var_name, seq_text)] in
              match check_and_merge_bindings bindings seq_binding with
              | None -> try_take (n + 1)  (* Binding conflict *)
              | Some merged ->
                match match_from merged (pi + 1) (si + n) with
                | Some result -> Some result
                | None -> try_take (n + 1)
          in
          try_take 0
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
  match_from [] 0 0

(** Merge bindings, checking that same var doesn't bind to different values *)
and check_and_merge_bindings existing new_bindings =
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

(** Get the innermost meaningful node from a pattern.
    This unwraps program/module wrappers and expression_statement wrappers
    to get to the actual pattern content. *)
let get_pattern_content pattern =
  let rec unwrap node =
    let node_type = Tree.node_type node in
    let children = Tree.named_children node in
    match node_type, children with
    (* Unwrap program/module with single child *)
    | ("program" | "module"), [child] -> unwrap child
    (* Unwrap expression_statement with single child *)
    | "expression_statement", [child] -> unwrap child
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
    | Some bindings ->
      results := {
        node = source_node;
        bindings;
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
let find_matches_in_subtree ~pattern ~inherited_bindings ~source ~root_node =
  let pattern_node = get_pattern_content pattern in
  let results = ref [] in
  Tree.traverse (fun source_node ->
    match match_node
            ~pattern
            ~pattern_source:pattern.source
            ~source
            ~substitutions:pattern.substitutions
            pattern_node source_node with
    | Some bindings ->
      (* Check if new bindings are consistent with inherited ones *)
      (match check_and_merge_bindings inherited_bindings bindings with
       | Some merged_bindings ->
         results := {
           node = source_node;
           bindings = merged_bindings;
           start_point = Tree.start_point source_node;
           end_point = Tree.end_point source_node;
         } :: !results
       | None -> ())  (* Binding conflict - skip this match *)
    | None -> ()
  ) root_node;
  List.rev !results

(** Recursively find nested matches.
    Process patterns left-to-right. For each context pattern, find matches,
    then recurse into each match's subtree with remaining patterns. *)
let rec find_nested_matches_impl ~source ~inherited_bindings
    ~accumulated_contexts ~root_node patterns =
  match patterns with
  | [] -> []  (* No patterns *)
  | [target] ->
    (* Base case: match final/target pattern in current subtree *)
    let matches = find_matches_in_subtree
        ~pattern:target
        ~inherited_bindings
        ~source
        ~root_node in
    List.map (fun m -> {
      inner_node = m.node;
      inner_bindings = m.bindings;
      all_bindings = m.bindings;
      contexts = List.rev accumulated_contexts;
      start_point = m.start_point;
      end_point = m.end_point;
    }) matches
  | ctx :: rest ->
    (* Find context matches, then recurse into each *)
    let ctx_matches = find_matches_in_subtree
        ~pattern:ctx
        ~inherited_bindings
        ~source
        ~root_node in
    List.concat_map (fun ctx_match ->
      let ctx_record = {
        context_node = ctx_match.node;
        context_bindings = ctx_match.bindings;
        context_start_point = ctx_match.start_point;
        context_end_point = ctx_match.end_point;
      } in
      find_nested_matches_impl
        ~source
        ~inherited_bindings:ctx_match.bindings
        ~accumulated_contexts:(ctx_record :: accumulated_contexts)
        ~root_node:ctx_match.node
        rest
    ) ctx_matches

(** Find nested matches using a pattern with multiple sections.
    Auto-detects: single section behaves like find_matches, multiple sections
    enable nested/scoped matching. *)
let find_nested_matches ~language ~pattern_text ~source_text =
  let nested = parse_nested_pattern ~language pattern_text in
  let source_tree = Tree.parse ~language source_text in
  let source = source_tree.source in
  let source_root = source_tree.root in
  if List.length nested.patterns = 1 then
    (* Single pattern: use existing logic, convert to nested_match_result *)
    let pattern = List.hd nested.patterns in
    let matches = find_matches_in_subtree
        ~pattern
        ~inherited_bindings:[]
        ~source
        ~root_node:source_root in
    List.map (fun m -> {
      inner_node = m.node;
      inner_bindings = m.bindings;
      all_bindings = m.bindings;
      contexts = [];
      start_point = m.start_point;
      end_point = m.end_point;
    }) matches
  else
    find_nested_matches_impl
      ~source
      ~inherited_bindings:[]
      ~accumulated_contexts:[]
      ~root_node:source_root
      nested.patterns

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

(** Build an index from a tree root. O(n) where n is node count. *)
let build_index root =
  let by_type = Hashtbl.create 64 in
  Tree.traverse (fun node ->
    let typ = Tree.node_type node in
    let existing = Hashtbl.find_opt by_type typ |> Option.value ~default:[] in
    Hashtbl.replace by_type typ (node :: existing)
  ) root;
  { by_type }

(** Find matches using a pre-built index.
    Falls back to full traversal if pattern root is a metavar. *)
let find_matches_with_index ~index ~pattern ~source ~source_root =
  let pattern_node = get_pattern_content pattern in
  (* Check if pattern root is a metavar placeholder *)
  match is_placeholder pattern.substitutions pattern.source pattern_node with
  | Some _ ->
    (* Pattern root is a metavar - can't filter by type, fall back to traversal *)
    find_matches_in_subtree ~pattern ~inherited_bindings:[] ~source ~root_node:source_root
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
      | Some bindings ->
        Some {
          node = source_node;
          bindings;
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
