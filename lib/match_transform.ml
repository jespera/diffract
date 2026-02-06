open Match_types

(** Replace all occurrences of needle in haystack with replacement *)
let string_replace_all ~needle ~replacement haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen = 0 then haystack
  else
    let buf = Buffer.create hlen in
    let i = ref 0 in
    while !i <= hlen - nlen do
      if String.sub haystack !i nlen = needle then begin
        Buffer.add_string buf replacement;
        i := !i + nlen
      end else begin
        Buffer.add_char buf haystack.[!i];
        incr i
      end
    done;
    while !i < hlen do
      Buffer.add_char buf haystack.[!i];
      incr i
    done;
    Buffer.contents buf

(** Instantiate a template by replacing metavar placeholders with their bound values.
    For each (var_name, placeholder) in substitutions, replaces all occurrences of
    placeholder in the template with the corresponding text from text_bindings. *)
let instantiate_template ~substitutions ~text_bindings template =
  List.fold_left (fun tmpl (var_name, placeholder) ->
    match List.assoc_opt var_name text_bindings with
    | Some value ->
      string_replace_all ~needle:placeholder ~replacement:value tmpl
    | None -> tmpl
  ) template substitutions

(** Adjust indentation of replacement text to match the source location.
    The first line keeps its indentation. Subsequent lines are adjusted
    by the difference between source column and template base column. *)
let adjust_indentation ~source_column ~template_base_column text =
  let delta = source_column - template_base_column in
  if delta = 0 then text
  else
    let lines = String.split_on_char '\n' text in
    match lines with
    | [] -> text
    | [single] -> single
    | first :: rest ->
      let adjust_line line =
        if delta > 0 then
          (* Add spaces *)
          String.make delta ' ' ^ line
        else
          (* Remove spaces (don't go below 0) *)
          let remove = min (- delta) (String.length line - String.length (String.trim line)) in
          if remove > 0 then String.sub line remove (String.length line - remove)
          else line
      in
      let adjusted = first :: List.map adjust_line rest in
      String.concat "\n" adjusted

(** Compute the base column (minimum leading whitespace) of a multi-line template *)
let template_base_column text =
  let lines = String.split_on_char '\n' text in
  let non_empty_lines = List.filter (fun l -> String.trim l <> "") lines in
  match non_empty_lines with
  | [] -> 0
  | _ ->
    List.fold_left (fun min_col line ->
      let len = String.length line in
      let spaces = ref 0 in
      while !spaces < len && line.[!spaces] = ' ' do incr spaces done;
      min min_col !spaces
    ) max_int non_empty_lines

(** Unwrap program/module/expression_statement wrappers from a tree root.
    Same logic as Match_engine.get_pattern_content but works on a raw root node. *)
let unwrap_content_node (root : Tree.pat Tree.t) : Tree.pat Tree.t =
  let rec unwrap (node : Tree.pat Tree.t) =
    let node_type = Tree.node_type node in
    let children = Tree.named_children node in
    match node_type, children with
    | ("program" | "module" | "source_file" | "compilation_unit"), [child] -> unwrap child
    | "expression_statement", [child] -> unwrap child
    | "program", first :: rest when Tree.node_type first = "php_tag" ->
      (match rest with
       | [child] -> unwrap child
       | _ -> node)
    | _ -> node
  in
  unwrap root

(** Extract the replacement template text from replace_source, trimming wrapper
    nodes (e.g., <?php prefix) that surround the content node.
    Uses the match content node's position within pattern.source to compute
    how much wrapper text to strip from both ends of replace_source. *)
let get_replace_template pattern =
  let match_content = unwrap_content_node pattern.tree.root in
  let front_trim = match_content.Tree.start_byte in
  let back_trim = String.length pattern.source - match_content.Tree.end_byte in
  let rlen = String.length pattern.replace_source in
  let content_len = rlen - front_trim - back_trim in
  if content_len > 0 && (front_trim > 0 || back_trim > 0) then
    String.sub pattern.replace_source front_trim content_len
  else
    pattern.replace_source

(** Compute edits for strict-mode transform.
    The entire matched node's byte range gets replaced with the instantiated template. *)
let compute_edits_strict ~pattern ~match_result ~source =
  let template = get_replace_template pattern in
  (* Instantiate metavars in the template (replace_source has placeholders) *)
  let instantiated = instantiate_template
    ~substitutions:pattern.substitutions
    ~text_bindings:match_result.bindings
    template in
  (* Adjust indentation to match source location *)
  let source_col = match_result.start_point.column in
  let tmpl_col = template_base_column template in
  let replacement = adjust_indentation ~source_column:source_col
    ~template_base_column:tmpl_col instantiated in
  ignore source;
  [{ start_byte = match_result.node.start_byte;
     end_byte = match_result.node.end_byte;
     replacement }]

(** {1 Partial/field-mode transform support} *)

module Alignment = struct
  (** Edit action for a pattern child during partial/field transforms *)
  type edit_action =
    | EKeep     (** Context child - no change *)
    | ERemove   (** Minus-only child - delete from source *)
    | EReplace of int  (** Minus child paired with plus child at this replace index *)

  (** Alignment between match tree children and replace tree children *)
  type alignment = {
    entries: (int * edit_action) list;    (** (match_child_index, action) *)
    insertions: (int * int) list;         (** (after_match_child_index, replace_child_index); -1 = before all *)
  }

  (** Align children of the match content node and replace content node.
      Context children (identical type + text) serve as anchors.
      Between anchors, minus-only children pair with plus-only children in order. *)
  let align_children ~match_source ~replace_source
      (match_list : Tree.pat Tree.t list) (replace_list : Tree.pat Tree.t list) =
    let match_arr = Array.of_list match_list in
    let replace_arr = Array.of_list replace_list in
    let mlen = Array.length match_arr in
    let rlen = Array.length replace_arr in
    let children_equal (mc : Tree.pat Tree.t) (rc : Tree.pat Tree.t) =
      Tree.node_type mc = Tree.node_type rc &&
      Tree.text match_source mc = Tree.text replace_source rc
    in
    let entries = ref [] in
    let insertions = ref [] in
    let mi = ref 0 in
    let ri = ref 0 in
    while !mi < mlen || !ri < rlen do
      if !mi < mlen && !ri < rlen && children_equal match_arr.(!mi) replace_arr.(!ri) then begin
        (* Context anchor - keep *)
        entries := (!mi, EKeep) :: !entries;
        incr mi; incr ri
      end else begin
        (* Find next anchor by scanning both lists *)
        let anchor = ref None in
        begin try
          for try_mi = !mi to mlen - 1 do
            for try_ri = !ri to rlen - 1 do
              if children_equal match_arr.(try_mi) replace_arr.(try_ri) then begin
                anchor := Some (try_mi, try_ri);
                raise Exit
              end
            done
          done
        with Exit -> () end;
        let (minus_end, plus_end) = match !anchor with
          | Some (ami, ari) -> (ami, ari)
          | None -> (mlen, rlen)
        in
        let minus_count = minus_end - !mi in
        let plus_count = plus_end - !ri in
        let paired = min minus_count plus_count in
        (* Pair minus with plus children *)
        for k = 0 to paired - 1 do
          entries := (!mi + k, EReplace (!ri + k)) :: !entries
        done;
        (* Extra minus children → removals *)
        for k = paired to minus_count - 1 do
          entries := (!mi + k, ERemove) :: !entries
        done;
        (* Extra plus children → insertions *)
        for k = paired to plus_count - 1 do
          (* Insert after the last match child before this gap *)
          let after = if !mi + minus_count > 0 then !mi + minus_count - 1 else -1 in
          insertions := (after, !ri + k) :: !insertions
        done;
        mi := minus_end;
        ri := plus_end
      end
    done;
    { entries = List.rev !entries; insertions = List.rev !insertions }

  (** Compute the byte range to remove for a child, including surrounding separator.
      Extends to consume the separator between this child and a sibling. *)
  let removal_range source (source_arr : Tree.src Tree.t array) child_idx =
    let child = source_arr.(child_idx) in
    let num = Array.length source_arr in
    ignore source;
    if child_idx > 0 then
      (* Remove separator before (from previous child's end to this child's end) *)
      let prev = source_arr.(child_idx - 1) in
      (prev.Tree.end_byte, child.Tree.end_byte)
    else if child_idx < num - 1 then
      (* Remove child and separator after (from this child's start to next child's start) *)
      let next = source_arr.(child_idx + 1) in
      (child.Tree.start_byte, next.Tree.start_byte)
    else
      (* Only child *)
      (child.Tree.start_byte, child.Tree.end_byte)

  (** Detect the separator text between two adjacent source children.
      Used to replicate the separator when inserting new children. *)
  let detect_separator source (source_arr : Tree.src Tree.t array) after_idx =
    let num = Array.length source_arr in
    if after_idx + 1 < num then
      (* Separator between after_idx and next child *)
      let prev_end = source_arr.(after_idx).Tree.end_byte in
      let next_start = source_arr.(after_idx + 1).Tree.start_byte in
      String.sub source prev_end (next_start - prev_end)
    else if after_idx > 0 then
      (* Use separator before after_idx as template *)
      let prev_end = source_arr.(after_idx - 1).Tree.end_byte in
      let cur_start = source_arr.(after_idx).Tree.start_byte in
      String.sub source prev_end (cur_start - prev_end)
    else
      ", "

  (** Insert position classification *)
  type insertion_kind =
    | Before_first
    | After_child of int
    | At_end
    | Empty

  (** Instantiate and adjust indentation for a single child replacement/insertion *)
  let instantiate_with_indent ~pattern ~match_result ~source_column text =
    let instantiated = instantiate_template
      ~substitutions:pattern.substitutions
      ~text_bindings:match_result.bindings text in
    let tmpl_col = template_base_column text in
    adjust_indentation ~source_column ~template_base_column:tmpl_col instantiated

  (** Build insertion replacement with separator placement policy. *)
  let insertion_replacement ~kind ~separator ~instantiated =
    match kind with
    | Before_first -> instantiated ^ separator
    | Empty -> instantiated
    | After_child _ | At_end -> separator ^ instantiated

  (** Compute insertion spec for partial mode (with correspondences). *)
  let insertion_spec_with_correspondences ~source ~match_result
      (source_arr : Tree.src Tree.t array) ~after_mi ~best_source_after =
    let source_len = Array.length source_arr in
    if source_len = 0 then
      (Empty, match_result.node.Tree.start_byte, "", match_result.start_point.column)
    else if after_mi < 0 then
      (Before_first,
       source_arr.(0).Tree.start_byte,
       detect_separator source source_arr 0,
       source_arr.(0).Tree.start_point.column)
    else
      match best_source_after after_mi with
      | Some si when si < source_len ->
        (After_child si,
         source_arr.(si).Tree.end_byte,
         detect_separator source source_arr si,
         source_arr.(si).Tree.start_point.column)
      | _ ->
        let last = source_arr.(source_len - 1) in
        (At_end, match_result.node.Tree.end_byte, ", ", last.Tree.start_point.column)

  (** Compute insertion spec for field mode (direct indices). *)
  let insertion_spec_direct ~source ~match_result
      (source_arr : Tree.src Tree.t array) ~after_mi =
    let source_len = Array.length source_arr in
    if source_len = 0 then
      (Empty, match_result.node.Tree.start_byte, "", match_result.start_point.column)
    else if after_mi < 0 then
      (Before_first,
       source_arr.(0).Tree.start_byte,
       detect_separator source source_arr 0,
       source_arr.(0).Tree.start_point.column)
    else if after_mi < source_len then
      (After_child after_mi,
       source_arr.(after_mi).Tree.end_byte,
       detect_separator source source_arr after_mi,
       source_arr.(after_mi).Tree.start_point.column)
    else
      let last = source_arr.(source_len - 1) in
      (At_end, match_result.node.Tree.end_byte, ", ", last.Tree.start_point.column)

  let apply_alignment ~pattern ~(match_result : match_result) ~source alignment
      ~(source_index_of_pattern : int -> int option)
      ~(insertion_spec : after_mi:int -> insertion_kind * int * string * int)
      (source_arr : Tree.src Tree.t array) (replace_arr : Tree.pat Tree.t array) =
    let edits = ref [] in
    List.iter (fun (mi, action) ->
      match action with
      | EKeep -> ()
      | ERemove ->
        (match source_index_of_pattern mi with
         | Some si when si < Array.length source_arr ->
           let (start_b, end_b) = removal_range source source_arr si in
           edits := { start_byte = start_b; end_byte = end_b; replacement = "" } :: !edits
         | _ -> ())
      | EReplace ri ->
        (match source_index_of_pattern mi with
         | Some si when si < Array.length source_arr && ri < Array.length replace_arr ->
           let src_child = source_arr.(si) in
           let repl_text = Tree.text pattern.replace_source replace_arr.(ri) in
           let instantiated = instantiate_with_indent ~pattern ~match_result
             ~source_column:src_child.Tree.start_point.column repl_text in
           edits := { start_byte = src_child.Tree.start_byte;
                      end_byte = src_child.Tree.end_byte;
                      replacement = instantiated } :: !edits
         | _ -> ())
    ) alignment.entries;
    List.iter (fun (after_mi, ri) ->
      if ri < Array.length replace_arr then begin
        let repl_text = Tree.text pattern.replace_source replace_arr.(ri) in
        let (kind, insert_byte, separator, insert_col) = insertion_spec ~after_mi in
        let instantiated = instantiate_with_indent ~pattern ~match_result
          ~source_column:insert_col repl_text in
        let replacement = insertion_replacement ~kind ~separator ~instantiated in
        edits := { start_byte = insert_byte; end_byte = insert_byte;
                   replacement } :: !edits
      end
    ) alignment.insertions;
    List.rev !edits

  (** Apply alignment edits using a correspondence map (pattern child → source child).
      Used for partial mode where matching is unordered. *)
  let apply_with_correspondences ~pattern ~(match_result : match_result) ~source alignment
      (source_arr : Tree.src Tree.t array) (replace_arr : Tree.pat Tree.t array) =
    let corr_map = Hashtbl.create 16 in
    List.iter (fun (c : child_correspondence) ->
      Hashtbl.replace corr_map c.pattern_index c.source_index
    ) match_result.correspondences;
    let best_source_after after_mi =
      let best = ref None in
      Hashtbl.iter (fun pi si ->
        if pi <= after_mi then
          match !best with
          | None -> best := Some si
          | Some prev when si > prev -> best := Some si
          | Some _ -> ()
      ) corr_map;
      !best
    in
    let source_index_of_pattern mi = Hashtbl.find_opt corr_map mi in
    let insertion_spec ~after_mi =
      insertion_spec_with_correspondences ~source ~match_result
        source_arr ~after_mi ~best_source_after
    in
    apply_alignment ~pattern ~match_result ~source alignment
      ~source_index_of_pattern ~insertion_spec source_arr replace_arr

  (** Apply alignment edits using direct index mapping (pattern child i → source child i).
      Used for field mode where matching is exact/ordered within each field group. *)
  let apply_direct ~pattern ~(match_result : match_result) ~source alignment
      (source_arr : Tree.src Tree.t array) (replace_arr : Tree.pat Tree.t array) =
    let source_index_of_pattern mi = Some mi in
    let insertion_spec ~after_mi =
      insertion_spec_direct ~source ~match_result source_arr ~after_mi
    in
    apply_alignment ~pattern ~match_result ~source alignment
      ~source_index_of_pattern ~insertion_spec source_arr replace_arr
end

(** Compute edits for partial-mode transform.
    Uses correspondences to map pattern children to source children,
    then applies alignment-based edits to individual children. *)
let compute_edits_partial ~pattern ~match_result ~source =
  match pattern.replace_tree with
  | None -> compute_edits_strict ~pattern ~match_result ~source
  | Some replace_tree ->
    let match_content = unwrap_content_node pattern.tree.root in
    let replace_content = unwrap_content_node replace_tree.root in
    let match_children = Tree.named_children match_content in
    let replace_children = Tree.named_children replace_content in
    let alignment = Alignment.align_children
      ~match_source:pattern.source ~replace_source:pattern.replace_source
      match_children replace_children in
    let source_children = Tree.named_children match_result.node in
    let source_arr = Array.of_list source_children in
    let replace_arr = Array.of_list replace_children in
    Alignment.apply_with_correspondences ~pattern ~match_result ~source
      alignment source_arr replace_arr

(** Find the end byte of the last child for a given field name in a field list.
    Returns None if the field is not present. *)
let field_end_byte fields field_name =
  let result = ref None in
  List.iter (fun (f, node) ->
    if f = Some field_name then result := Some node.Tree.end_byte
  ) fields;
  !result

(** Find the start byte of the first child for a given field name in a field list.
    Returns None if the field is not present. *)
let field_start_byte fields field_name =
  List.find_map (fun (f, node) ->
    if f = Some field_name then Some node.Tree.start_byte else None
  ) fields

(** Handle field additions: for new fields in the replace pattern (not in match),
    compute edits by replacing the source span between adjacent anchor fields
    with the corresponding span from the replace source (which includes the new field).
    Anchor fields are fields common to both match and replace. *)
let compute_field_addition_edits ~pattern ~(match_result : match_result) ~source:_
    replace_fields source_fields match_field_set =
  (* Build contiguous segments between anchor fields that contain new fields. *)
  let segments = ref [] in
  let prev_anchor = ref None in
  let has_added = ref false in
  List.iter (fun (f, _) ->
    match f with
    | Some fname when Hashtbl.mem match_field_set fname ->
      if !has_added then
        segments := (!prev_anchor, Some fname) :: !segments;
      prev_anchor := Some fname;
      has_added := false
    | Some fname ->
      if not (Hashtbl.mem match_field_set fname) then has_added := true
    | None -> ()
  ) replace_fields;
  if !has_added then
    segments := (!prev_anchor, None) :: !segments;
  let segments = List.rev !segments in
  if segments = [] then []
  else
    let edits = ref [] in
    List.iter (fun (prev_anchor, next_anchor) ->
      let r_start = match prev_anchor with
        | Some p -> (match field_end_byte replace_fields p with
          | Some b -> b | None -> 0)
        | None -> 0
      in
      let r_end = match next_anchor with
        | Some n -> (match field_start_byte replace_fields n with
          | Some b -> b | None -> String.length pattern.replace_source)
        | None -> String.length pattern.replace_source
      in
      let s_start = match prev_anchor with
        | Some p -> (match field_end_byte source_fields p with
          | Some b -> b | None -> match_result.node.Tree.start_byte)
        | None -> match_result.node.Tree.start_byte
      in
      let s_end = match next_anchor with
        | Some n -> (match field_start_byte source_fields n with
          | Some b -> b | None -> match_result.node.Tree.end_byte)
        | None -> match_result.node.Tree.end_byte
      in
      if r_start < r_end && r_end <= String.length pattern.replace_source then begin
        let replace_text = String.sub pattern.replace_source r_start (r_end - r_start) in
        let instantiated = instantiate_template
          ~substitutions:pattern.substitutions
          ~text_bindings:match_result.bindings replace_text in
        edits := { start_byte = s_start; end_byte = s_end;
                   replacement = instantiated } :: !edits
      end
    ) segments;
    List.rev !edits

(** Handle field removals: for fields in the match pattern not in replace,
    delete the source span that contains those fields. *)
let compute_field_removal_edits ~(match_result : match_result)
    match_fields source_fields replace_field_set =
  let match_field_list = List.filter_map (fun (f, _) -> f) match_fields in
  let match_field_uniq = List.sort_uniq String.compare match_field_list in
  let removed_fields = List.filter (fun f ->
    not (Hashtbl.mem replace_field_set f)
  ) match_field_uniq in
  if removed_fields = [] then []
  else
    let edits = ref [] in
    List.iter (fun removed_field ->
      (* Find the span of this field in the source *)
      let s_start = field_start_byte source_fields removed_field in
      let s_end = field_end_byte source_fields removed_field in
      (* Find adjacent fields in source to determine how much whitespace/syntax to remove *)
      let prev_end = ref None in
      let next_start = ref None in
      let found = ref false in
      List.iter (fun (f, node) ->
        match f with
        | Some fname when fname = removed_field -> found := true
        | Some _ ->
          if not !found then prev_end := Some node.Tree.end_byte
          else if !next_start = None then next_start := Some node.Tree.start_byte
        | None -> ()
      ) source_fields;
      match s_start, s_end with
      | Some ss, Some se ->
        (* Extend removal to include separator between fields *)
        let remove_start = match !prev_end with
          | Some pe -> pe  (* Remove from previous field's end *)
          | None -> ss     (* No previous field, just remove the field itself *)
        in
        let remove_end = match !next_start with
          | Some _ -> se   (* Next field exists, just remove up to field end *)
          | None ->
            (* No next field - remove from prev_end to field end *)
            match !prev_end with
            | Some _ -> se
            | None -> match_result.node.Tree.end_byte
        in
        edits := { start_byte = remove_start; end_byte = remove_end;
                   replacement = "" } :: !edits
      | _ -> ()
    ) removed_fields;
    List.rev !edits

(** Compute edits for field-mode transform.
    Groups children by field name and applies alignment within each field group.
    Pattern child i within a group maps directly to source child i (exact matching).
    Handles field additions (fields only in replace) and removals (fields only in match)
    by computing span-based edits between anchor fields. *)
let compute_edits_field ~pattern ~match_result ~source =
  match pattern.replace_tree with
  | None -> compute_edits_strict ~pattern ~match_result ~source
  | Some replace_tree ->
    let match_content = unwrap_content_node pattern.tree.root in
    let replace_content = unwrap_content_node replace_tree.root in
    let match_fields = Tree.named_children_with_fields match_content in
    let replace_fields = Tree.named_children_with_fields replace_content in
    let source_fields = Tree.named_children_with_fields match_result.node in
    (* Build field name sets *)
    let make_field_set fields =
      let s = Hashtbl.create 8 in
      List.iter (fun (f, _) -> match f with Some f -> Hashtbl.replace s f () | None -> ()) fields;
      s
    in
    let match_field_set = make_field_set match_fields in
    let replace_field_set = make_field_set replace_fields in
    (* Group by field name *)
    let group_by_field children =
      let tbl = Hashtbl.create 8 in
      List.iter (fun (field, node) ->
        let existing = Hashtbl.find_opt tbl field |> Option.value ~default:[] in
        Hashtbl.replace tbl field (existing @ [node])
      ) children;
      tbl
    in
    let match_tbl = group_by_field match_fields in
    let replace_tbl = group_by_field replace_fields in
    let source_tbl = group_by_field source_fields in
    let edits = ref [] in
    (* Process each field group present in both match and replace *)
    Hashtbl.iter (fun field match_nodes ->
      if Hashtbl.mem replace_tbl field then begin
        let replace_nodes = Hashtbl.find replace_tbl field in
        let source_nodes = Hashtbl.find_opt source_tbl field |> Option.value ~default:[] in
        let alignment = Alignment.align_children
          ~match_source:pattern.source ~replace_source:pattern.replace_source
          match_nodes replace_nodes in
        let source_arr = Array.of_list source_nodes in
        let replace_arr = Array.of_list replace_nodes in
        let field_edits = Alignment.apply_direct ~pattern ~match_result ~source
          alignment source_arr replace_arr in
        edits := field_edits @ !edits
      end
    ) match_tbl;
    (* Handle field additions *)
    let addition_edits = compute_field_addition_edits ~pattern ~match_result ~source
      replace_fields source_fields match_field_set in
    edits := addition_edits @ !edits;
    (* Handle field removals *)
    let removal_edits = compute_field_removal_edits ~match_result
      match_fields source_fields replace_field_set in
    edits := removal_edits @ !edits;
    List.rev !edits

(** Compute edits for a single match, dispatching by match mode *)
let compute_edits ~pattern ~match_result ~source =
  match pattern.match_mode with
  | Strict ->
    compute_edits_strict ~pattern ~match_result ~source
  | Partial ->
    compute_edits_partial ~pattern ~match_result ~source
  | Field ->
    compute_edits_field ~pattern ~match_result ~source

(** Filter overlapping edits: keep first in document order (by start_byte).
    Assumes edits are sorted by start_byte ascending. *)
let filter_overlapping edits =
  let rec go acc last_end = function
    | [] -> List.rev acc
    | edit :: rest ->
      if edit.start_byte < last_end then
        (* Overlaps with previous accepted edit - skip *)
        go acc last_end rest
      else
        go (edit :: acc) edit.end_byte rest
  in
  match edits with
  | [] -> []
  | first :: rest -> go [first] first.end_byte rest

(** Apply edits to source text.
    Sorts edits by start_byte descending and applies bottom-to-top
    to avoid offset invalidation. Filters overlapping edits first. *)
let apply_edits source edits =
  (* Sort by start_byte ascending for overlap filtering *)
  let sorted_asc = List.sort (fun a b ->
    let c = compare a.start_byte b.start_byte in
    if c <> 0 then c else compare b.end_byte a.end_byte
  ) edits in
  let non_overlapping = filter_overlapping sorted_asc in
  (* Apply in reverse order (bottom-to-top) *)
  let sorted_desc = List.rev non_overlapping in
  List.fold_left (fun s edit ->
    let before = String.sub s 0 edit.start_byte in
    let after = String.sub s edit.end_byte (String.length s - edit.end_byte) in
    before ^ edit.replacement ^ after
  ) source sorted_desc

(** Transform source text using a semantic patch pattern.
    Returns a transform_result with all edits and the transformed source. *)
let transform ~ctx ~language ~pattern_text ~source_text =
  let pattern = Match_parse.parse_pattern ~ctx ~language pattern_text in
  if not pattern.spatch.is_transform then
    { edits = []; original_source = source_text; transformed_source = source_text }
  else
    let source_tree = Tree.parse ~ctx ~language source_text in
    let source = source_tree.source in
    let matches = Match_search.find_matches_in_subtree
      ~pattern
      ~inherited_bindings:Match_engine.empty_bindings
      ~source
      ~root_node:source_tree.root in
    let edits = List.concat_map (fun m ->
      compute_edits ~pattern ~match_result:m ~source
    ) matches in
    let transformed = apply_edits source_text edits in
    { edits; original_source = source_text; transformed_source = transformed }

(** Transform a file using a semantic patch pattern.
    Returns a transform_result. *)
let transform_file ~ctx ~language ~pattern_text ~source_path =
  let source_text = In_channel.with_open_text source_path In_channel.input_all in
  transform ~ctx ~language ~pattern_text ~source_text

(** Diff operation: keep, remove, or add a line *)
type diff_op = DKeep of string | DRemove of string | DAdd of string

(** Generate a unified diff between original and transformed text. *)
let generate_diff ~file_path ~original ~transformed =
  if original = transformed then ""
  else
    let orig_lines = String.split_on_char '\n' original in
    let trans_lines = String.split_on_char '\n' transformed in
    let buf = Buffer.create 1024 in
    Buffer.add_string buf (Printf.sprintf "--- a/%s\n" file_path);
    Buffer.add_string buf (Printf.sprintf "+++ b/%s\n" file_path);
    (* Simple line-by-line diff using longest common subsequence *)
    let orig_arr = Array.of_list orig_lines in
    let trans_arr = Array.of_list trans_lines in
    let n = Array.length orig_arr in
    let m = Array.length trans_arr in
    (* LCS table *)
    let dp = Array.make_matrix (n + 1) (m + 1) 0 in
    for i = n - 1 downto 0 do
      for j = m - 1 downto 0 do
        if orig_arr.(i) = trans_arr.(j) then
          dp.(i).(j) <- dp.(i+1).(j+1) + 1
        else
          dp.(i).(j) <- max dp.(i+1).(j) dp.(i).(j+1)
      done
    done;
    (* Produce diff operations *)
    let ops = ref [] in
    let i = ref 0 in
    let j = ref 0 in
    while !i < n || !j < m do
      if !i < n && !j < m && orig_arr.(!i) = trans_arr.(!j) then begin
        ops := DKeep orig_arr.(!i) :: !ops;
        incr i; incr j
      end else if !i < n && (!j >= m || dp.(!i+1).(!j) >= dp.(!i).(!j+1)) then begin
        ops := DRemove orig_arr.(!i) :: !ops;
        incr i
      end else begin
        ops := DAdd trans_arr.(!j) :: !ops;
        incr j
      end
    done;
    let ops = List.rev !ops in
    (* Group into hunks *)
    let context_lines = 3 in
    let ops_arr = Array.of_list ops in
    let total = Array.length ops_arr in
    (* Find ranges of changed lines *)
    let in_change = Array.make total false in
    Array.iteri (fun idx op ->
      match op with DKeep _ -> () | _ -> in_change.(idx) <- true
    ) ops_arr;
    (* Expand context around changes *)
    let in_hunk = Array.make total false in
    for idx = 0 to total - 1 do
      if in_change.(idx) then
        for k = max 0 (idx - context_lines) to min (total - 1) (idx + context_lines) do
          in_hunk.(k) <- true
        done
    done;
    (* Output hunks *)
    let idx = ref 0 in
    while !idx < total do
      if in_hunk.(!idx) then begin
        (* Find end of this hunk *)
        let hunk_start = !idx in
        while !idx < total && in_hunk.(!idx) do incr idx done;
        let hunk_end = !idx in
        (* Count original and transformed lines in this hunk *)
        let orig_start = ref 0 in
        let trans_start = ref 0 in
        for k = 0 to hunk_start - 1 do
          match ops_arr.(k) with
          | DKeep _ -> incr orig_start; incr trans_start
          | DRemove _ -> incr orig_start
          | DAdd _ -> incr trans_start
        done;
        let orig_count = ref 0 in
        let trans_count = ref 0 in
        for k = hunk_start to hunk_end - 1 do
          match ops_arr.(k) with
          | DKeep _ -> incr orig_count; incr trans_count
          | DRemove _ -> incr orig_count
          | DAdd _ -> incr trans_count
        done;
        Buffer.add_string buf (Printf.sprintf "@@ -%d,%d +%d,%d @@\n"
          (!orig_start + 1) !orig_count (!trans_start + 1) !trans_count);
        for k = hunk_start to hunk_end - 1 do
          match ops_arr.(k) with
          | DKeep line -> Buffer.add_string buf (Printf.sprintf " %s\n" line)
          | DRemove line -> Buffer.add_string buf (Printf.sprintf "-%s\n" line)
          | DAdd line -> Buffer.add_string buf (Printf.sprintf "+%s\n" line)
        done
      end else
        incr idx
    done;
    Buffer.contents buf
