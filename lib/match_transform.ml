open Match_types

(** Match-and-replace transformation using semantic patches. *)

type alignment_action = EKeep | ERemove | EReplace of int

type alignment_result = {
  entries : (int * alignment_action) list;
  insertions : (int * int) list;
}

(** Utility: replace all occurrences of a substring *)
let string_replace_all ~needle ~replacement s =
  if needle = "" then s
  else
    let result = Buffer.create (String.length s) in
    let needle_len = String.length needle in
    let rec loop start =
      if start >= String.length s then ()
      else
        try
          let i = String.index_from s start needle.[0] in
          if
            i + needle_len <= String.length s
            && String.sub s i needle_len = needle
          then begin
            Buffer.add_substring result s start (i - start);
            Buffer.add_string result replacement;
            loop (i + needle_len)
          end
          else begin
            Buffer.add_char result s.[start];
            loop (start + 1)
          end
        with Not_found ->
          Buffer.add_substring result s start (String.length s - start)
    in
    loop 0;
    Buffer.contents result

(** Compute the base column (minimum leading whitespace) of a multi-line
    template *)
let template_base_column text =
  let lines = String.split_on_char '\n' text in
  let non_empty_lines = List.filter (fun l -> String.trim l <> "") lines in
  match non_empty_lines with
  | [] -> 0
  | _ ->
      List.fold_left
        (fun min_col line ->
          let len = String.length line in
          let spaces = ref 0 in
          while !spaces < len && line.[!spaces] = ' ' do
            incr spaces
          done;
          min min_col !spaces)
        max_int non_empty_lines

(** Adjust indentation of replacement text to match the source location. The
    first line keeps its indentation. Subsequent lines are adjusted by the
    difference between source column and template base column. *)
let adjust_indentation ~source_column ~template_base_column text =
  let delta = source_column - template_base_column in
  if delta = 0 then text
  else
    let lines = String.split_on_char '\n' text in
    match lines with
    | [] -> text
    | [ single ] -> single
    | first :: rest ->
        let adjust_line line =
          if delta > 0 then String.make delta ' ' ^ line
          else
            let remove =
              min (-delta)
                (String.length line - String.length (String.trim line))
            in
            if remove > 0 then
              String.sub line remove (String.length line - remove)
            else line
        in
        let adjusted = first :: List.map adjust_line rest in
        String.concat "\n" adjusted

(** Replace placeholders *)
let instantiate_template ~substitutions ~text_bindings template =
  List.fold_left
    (fun instantiated (var_name, placeholder) ->
      match List.assoc_opt var_name text_bindings with
      | Some value ->
          (* Avoid double semicolons: the ellipsis preprocessing appends ';' to
             each placeholder so it parses as a valid statement, but the bound
             sequence text already ends with ';' for statement-type nodes.
             Replace 'placeholder;' → value first (consuming the template ';')
             when the value ends with ';', then do a plain pass for any remaining
             occurrences without a trailing ';'. *)
          let instantiated =
            if
              String.length value > 0
              && value.[String.length value - 1] = ';'
            then
              string_replace_all ~needle:(placeholder ^ ";")
                ~replacement:value instantiated
            else instantiated
          in
          string_replace_all ~needle:placeholder ~replacement:value instantiated
      | None -> instantiated)
    template substitutions

(** Apply edits with overlap protection (keep first) *)
let apply_edits_flagged source edits =
  let sorted = List.sort (fun a b -> compare a.start_byte b.start_byte) edits in
  let filtered = ref [] in
  let last_end = ref (-1) in
  let had_overlaps = ref false in
  List.iter
    (fun e ->
      if e.start_byte >= !last_end then begin
        filtered := e :: !filtered;
        last_end := e.end_byte
      end
      else had_overlaps := true)
    sorted;
  let to_apply =
    List.sort (fun a b -> compare b.start_byte a.start_byte) !filtered
  in
  let rec apply current_s = function
    | [] -> current_s
    | e :: rest ->
        let prefix = String.sub current_s 0 e.start_byte in
        let suffix =
          String.sub current_s e.end_byte (String.length current_s - e.end_byte)
        in
        apply (prefix ^ e.replacement ^ suffix) rest
  in
  (apply source to_apply, !had_overlaps)

let apply_edits source edits = fst (apply_edits_flagged source edits)

let get_replace_template pattern =
  let match_content = Tree.unwrap_root pattern.tree.root in
  let front_trim = match_content.Tree.start_byte in
  let back_trim = String.length pattern.source - match_content.Tree.end_byte in
  let rlen = String.length pattern.replace_source in
  let content_len = rlen - front_trim - back_trim in
  if content_len > 0 && (front_trim > 0 || back_trim > 0) then
    String.sub pattern.replace_source front_trim content_len
  else pattern.replace_source

module Alignment = struct
  let align_children ~match_source ~replace_source match_nodes replace_nodes =
    let nodes_equal mc rc =
      Tree.node_type mc = Tree.node_type rc
      && Tree.text match_source mc = Tree.text replace_source rc
    in
    let mlen = List.length match_nodes in
    let rlen = List.length replace_nodes in
    let match_arr = Array.of_list match_nodes in
    let replace_arr = Array.of_list replace_nodes in
    let entries = ref [] in
    let insertions = ref [] in
    let mi = ref 0 in
    let ri = ref 0 in
    while !mi < mlen || !ri < rlen do
      if !mi < mlen && !ri < rlen then begin
        let m_node = match_arr.(!mi) in
        let r_node = replace_arr.(!ri) in
        if nodes_equal m_node r_node then begin
          entries := (!mi, EKeep) :: !entries;
          incr mi;
          incr ri
        end
        else begin
          let find_next_match () =
            let found = ref None in
            for k = !mi to mlen - 1 do
              for l = !ri to rlen - 1 do
                if nodes_equal match_arr.(k) replace_arr.(l) && !found = None
                then found := Some (k, l)
              done
            done;
            !found
          in
          match find_next_match () with
          | Some (next_mi, next_ri) ->
              let minus_gap = next_mi - !mi in
              let plus_gap = next_ri - !ri in
              let paired = min minus_gap plus_gap in
              for k = 0 to paired - 1 do
                entries := (!mi + k, EReplace (!ri + k)) :: !entries
              done;
              for k = paired to minus_gap - 1 do
                entries := (!mi + k, ERemove) :: !entries
              done;
              for k = paired to plus_gap - 1 do
                insertions := (!mi + paired - 1, !ri + k) :: !insertions
              done;
              mi := next_mi;
              ri := next_ri
          | None ->
              entries := (!mi, EReplace !ri) :: !entries;
              incr mi;
              incr ri
        end
      end
      else begin
        let next_anchor =
          let found = ref None in
          for k = !mi to mlen - 1 do
            for l = !ri to rlen - 1 do
              if nodes_equal match_arr.(k) replace_arr.(l) && !found = None then
                found := Some (k, l)
            done
          done;
          !found
        in
        let minus_end, plus_end =
          match next_anchor with
          | Some (ami, ari) -> (ami, ari)
          | None -> (mlen, rlen)
        in
        let minus_count = minus_end - !mi in
        let plus_count = plus_end - !ri in
        let paired = min minus_count plus_count in
        for k = 0 to paired - 1 do
          entries := (!mi + k, EReplace (!ri + k)) :: !entries
        done;
        for k = paired to minus_count - 1 do
          entries := (!mi + k, ERemove) :: !entries
        done;
        for k = paired to plus_count - 1 do
          let after =
            if !mi + minus_count > 0 then !mi + minus_count - 1 else -1
          in
          insertions := (after, !ri + k) :: !insertions
        done;
        mi := minus_end;
        ri := plus_end
      end
    done;
    { entries = List.rev !entries; insertions = List.rev !insertions }

  let removal_range (source_arr : Tree.src Tree.t array) child_idx =
    let child = source_arr.(child_idx) in
    let num = Array.length source_arr in
    if child_idx > 0 then
      let prev = source_arr.(child_idx - 1) in
      (prev.Tree.end_byte, child.Tree.end_byte)
    else if child_idx < num - 1 then
      let next = source_arr.(child_idx + 1) in
      (child.Tree.start_byte, next.Tree.start_byte)
    else (child.Tree.start_byte, child.Tree.end_byte)

  let detect_separator source (source_arr : Tree.src Tree.t array) after_idx =
    let num = Array.length source_arr in
    if after_idx + 1 < num then
      let prev_end = source_arr.(after_idx).Tree.end_byte in
      let next_start = source_arr.(after_idx + 1).Tree.start_byte in
      String.sub source prev_end (next_start - prev_end)
    else if after_idx > 0 then
      let prev_end = source_arr.(after_idx - 1).Tree.end_byte in
      let cur_start = source_arr.(after_idx).Tree.start_byte in
      String.sub source prev_end (cur_start - prev_end)
    else ", "

  type insertion_kind = Before_first | After_child of int | At_end | Empty

  let insertion_replacement ~kind ~separator ~instantiated =
    match kind with
    | Before_first -> instantiated ^ separator
    | Empty -> instantiated
    | After_child _ | At_end -> separator ^ instantiated

  let insertion_spec_with_correspondences ~source ~(match_result : match_result)
      (source_arr : Tree.src Tree.t array) ~after_mi ~best_source_after =
    let source_len = Array.length source_arr in
    if source_len = 0 then
      ( Empty,
        match_result.node.Tree.start_byte,
        "",
        match_result.start_point.column )
    else if after_mi < 0 then
      ( Before_first,
        source_arr.(0).Tree.start_byte,
        detect_separator source source_arr 0,
        source_arr.(0).Tree.start_point.column )
    else
      match best_source_after after_mi with
      | Some si when si < source_len ->
          ( After_child si,
            source_arr.(si).Tree.end_byte,
            detect_separator source source_arr si,
            source_arr.(si).Tree.start_point.column )
      | _ ->
          let last = source_arr.(source_len - 1) in
          ( At_end,
            match_result.node.Tree.end_byte,
            ", ",
            last.Tree.start_point.column )

  let insertion_spec_direct ~source ~(match_result : match_result)
      (source_arr : Tree.src Tree.t array) ~after_mi =
    let source_len = Array.length source_arr in
    if source_len = 0 then
      ( Empty,
        match_result.node.Tree.start_byte,
        "",
        match_result.start_point.column )
    else if after_mi < 0 then
      ( Before_first,
        source_arr.(0).Tree.start_byte,
        detect_separator source source_arr 0,
        source_arr.(0).Tree.start_point.column )
    else if after_mi < source_len then
      ( After_child after_mi,
        source_arr.(after_mi).Tree.end_byte,
        detect_separator source source_arr after_mi,
        source_arr.(after_mi).Tree.start_point.column )
    else
      let last = source_arr.(source_len - 1) in
      ( At_end,
        match_result.node.Tree.end_byte,
        ", ",
        last.Tree.start_point.column )

  let field_end_byte fields field_name =
    let result = ref None in
    List.iter
      (fun (f, node) ->
        if f = Some field_name then result := Some node.Tree.end_byte)
      fields;
    !result

  let field_start_byte fields field_name =
    List.find_map
      (fun (f, node) ->
        if f = Some field_name then Some node.Tree.start_byte else None)
      fields
end

let rec compute_edits ~pattern ~(match_result : match_result) ~source
    ~inner_patterns =
  match pattern.match_mode with
  | Strict ->
      compute_edits_strict ~pattern ~match_result ~source ~inner_patterns
  | Partial ->
      compute_edits_partial ~pattern ~match_result ~source ~inner_patterns
  | Field -> compute_edits_field ~pattern ~match_result ~source ~inner_patterns

and compute_edits_strict ~pattern ~(match_result : match_result) ~source
    ~inner_patterns =
  let template = get_replace_template pattern in
  let inherited_bindings =
    {
      text_bindings = match_result.bindings;
      node_bindings = match_result.node_bindings;
      sequence_node_bindings = match_result.sequence_node_bindings;
      correspondences = match_result.correspondences;
    }
  in
  let template =
    if pattern.expansion_slots = [] then template
    else
      apply_expansion_slots ~expansion_slots:pattern.expansion_slots ~source
        ~sequence_node_bindings:match_result.sequence_node_bindings
        ~inherited_bindings ~inner_patterns template
  in
  let instantiated =
    instantiate_template ~substitutions:pattern.substitutions
      ~text_bindings:match_result.bindings template
  in
  (* For bare-sequence matches the matched node is the container (e.g.
     statement_block), but the edit must only cover the explicitly-matched
     statements — items before the first anchor and after the last anchor were
     matched implicitly and must be left unchanged.
     Detect by checking whether the pattern root (after unwrapping) is a
     program/module wrapper while the matched source node is something else. *)
  let pattern_root = Match_engine.get_pattern_content pattern in
  let is_bare_seq =
    (match pattern_root.Tree.node_type with
    | "program" | "module" | "source_file" | "compilation_unit" -> true
    | _ -> false)
    && pattern_root.Tree.node_type <> match_result.node.Tree.node_type
  in
  let start_byte, end_byte, source_col =
    if is_bare_seq && match_result.correspondences <> [] then
      let source_children =
        Array.of_list (Tree.named_children match_result.node)
      in
      let corrs = match_result.correspondences in
      let min_si =
        List.fold_left (fun a c -> min a c.source_index) max_int corrs
      in
      let max_si =
        List.fold_left (fun a c -> max a c.source_index) min_int corrs
      in
      ( source_children.(min_si).Tree.start_byte,
        source_children.(max_si).Tree.end_byte,
        (Tree.start_point source_children.(min_si)).column )
    else
      ( match_result.node.Tree.start_byte,
        match_result.node.Tree.end_byte,
        match_result.start_point.column )
  in
  let tmpl_col = template_base_column template in
  let replacement =
    adjust_indentation ~source_column:source_col ~template_base_column:tmpl_col
      instantiated
  in
  [
    {
      start_byte;
      end_byte;
      replacement;
    };
  ]

and compute_edits_partial ~pattern ~(match_result : match_result) ~source
    ~inner_patterns =
  match pattern.replace_tree with
  | None -> compute_edits_strict ~pattern ~match_result ~source ~inner_patterns
  | Some replace_tree ->
      let match_content = Tree.unwrap_root pattern.tree.root in
      let replace_content = Tree.unwrap_root replace_tree.root in
      let match_children = Tree.named_children match_content in
      let replace_children = Tree.named_children replace_content in
      let alignment =
        Alignment.align_children ~match_source:pattern.source
          ~replace_source:pattern.replace_source match_children replace_children
      in
      let source_children = Tree.named_children match_result.node in
      let source_arr = Array.of_list source_children in
      let replace_arr = Array.of_list replace_children in
      apply_with_correspondences ~pattern ~match_result ~source
        ~match_children_list:match_children ~inner_patterns alignment source_arr
        replace_arr

and compute_edits_field ~pattern ~(match_result : match_result) ~source
    ~inner_patterns =
  if pattern.expansion_slots <> [] then
    compute_edits_strict ~pattern ~match_result ~source ~inner_patterns
  else
    match pattern.replace_tree with
    | None ->
        compute_edits_strict ~pattern ~match_result ~source ~inner_patterns
    | Some replace_tree ->
        let match_content = Tree.unwrap_root pattern.tree.root in
        let replace_content = Tree.unwrap_root replace_tree.root in
        let match_fields = Tree.named_children_with_fields match_content in
        let replace_fields = Tree.named_children_with_fields replace_content in
        let source_fields = Tree.named_children_with_fields match_result.node in
        let make_field_set fields =
          let s = Hashtbl.create 8 in
          List.iter
            (fun (f, _) ->
              match f with Some f -> Hashtbl.replace s f () | None -> ())
            fields;
          s
        in
        let match_field_set = make_field_set match_fields in
        let replace_field_set = make_field_set replace_fields in
        let group_by_field children =
          let tbl = Hashtbl.create 8 in
          List.iter
            (fun (field, node) ->
              let existing =
                Hashtbl.find_opt tbl field |> Option.value ~default:[]
              in
              Hashtbl.replace tbl field (existing @ [ node ]))
            children;
          tbl
        in
        let match_tbl = group_by_field match_fields in
        let replace_tbl = group_by_field replace_fields in
        let source_tbl = group_by_field source_fields in
        let edits = ref [] in
        Hashtbl.iter
          (fun field match_nodes ->
            if Hashtbl.mem replace_tbl field then begin
              let replace_nodes = Hashtbl.find replace_tbl field in
              let source_nodes =
                Hashtbl.find_opt source_tbl field |> Option.value ~default:[]
              in
              let alignment =
                Alignment.align_children ~match_source:pattern.source
                  ~replace_source:pattern.replace_source match_nodes
                  replace_nodes
              in
              let source_arr = Array.of_list source_nodes in
              let replace_arr = Array.of_list replace_nodes in
              let field_edits =
                apply_direct ~pattern ~match_result ~source ~inner_patterns
                  alignment source_arr replace_arr
              in
              edits := field_edits @ !edits
            end)
          match_tbl;
        let addition_edits =
          compute_field_addition_edits ~pattern ~match_result ~source
            replace_fields source_fields match_field_set
        in
        edits := addition_edits @ !edits;
        let removal_edits =
          compute_field_removal_edits ~match_result match_fields source_fields
            replace_field_set
        in
        edits := removal_edits @ !edits;
        List.rev !edits

and try_match_and_transform ~inner_pattern ~source ~inherited_bindings node =
  if not inner_pattern.is_transform then None
  else
    let node_start = node.Tree.start_byte in
    let node_text = Tree.text source node in
    let subtree_matches =
      Match_search.find_matches_in_subtree ~pattern:inner_pattern
        ~inherited_bindings ~source ~root_node:node
    in
    if subtree_matches <> [] then
      let edits =
        List.concat_map
          (fun m ->
            compute_edits ~pattern:inner_pattern ~match_result:m ~source
              ~inner_patterns:[])
          subtree_matches
      in
      let adjusted =
        List.map
          (fun e ->
            {
              e with
              start_byte = e.start_byte - node_start;
              end_byte = e.end_byte - node_start;
            })
          edits
      in
      Some (apply_edits node_text adjusted)
    else
      match
        Match_engine.try_match_children_directly ~pattern:inner_pattern ~source
          node
      with
      | None -> None
      | Some mb -> (
          match
            Match_engine.check_and_merge_bindings ~source inherited_bindings mb
          with
          | None -> None
          | Some merged ->
              let mr =
                {
                  node;
                  bindings = merged.text_bindings;
                  node_bindings = merged.node_bindings;
                  sequence_node_bindings = merged.sequence_node_bindings;
                  correspondences = merged.correspondences;
                  start_point = Tree.start_point node;
                  end_point = Tree.end_point node;
                }
              in
              let edits =
                compute_edits_strict ~pattern:inner_pattern ~match_result:mr
                  ~source ~inner_patterns:[]
              in
              let adjusted =
                List.map
                  (fun e ->
                    {
                      e with
                      start_byte = e.start_byte - node_start;
                      end_byte = e.end_byte - node_start;
                    })
                  edits
              in
              Some (apply_edits node_text adjusted))

and expand_slot ~slot ~source ~sequence_node_bindings ~inherited_bindings
    ~inner_pattern_opt =
  let node_to_text node =
    match inner_pattern_opt with
    | Some ip -> (
        match
          try_match_and_transform ~inner_pattern:ip ~source ~inherited_bindings
            node
        with
        | Some text -> text
        | None -> Tree.text source node)
    | None -> Tree.text source node
  in
  let texts =
    List.concat_map
      (fun var ->
        match List.assoc_opt var sequence_node_bindings with
        | Some nodes -> List.map node_to_text nodes
        | None -> [])
      slot.exp_vars
  in
  String.concat slot.exp_separator texts

and apply_expansion_slots ~expansion_slots ~source ~sequence_node_bindings
    ~inherited_bindings ~inner_patterns template =
  List.fold_left
    (fun tmpl slot ->
      let inner_pattern_opt =
        List.find_opt
          (fun p ->
            match p.on_var with
            | Some v -> List.mem v slot.exp_vars
            | None -> false)
          inner_patterns
      in
      let joined =
        expand_slot ~slot ~source ~sequence_node_bindings ~inherited_bindings
          ~inner_pattern_opt
      in
      string_replace_all ~needle:slot.exp_placeholder ~replacement:joined tmpl)
    template expansion_slots

and instantiate_with_indent ~pattern ~(match_result : match_result)
    ~inner_patterns ~inherited_bindings ~source_column text =
  let instantiated =
    instantiate_template ~substitutions:pattern.substitutions
      ~text_bindings:match_result.bindings text
  in
  let instantiated =
    if pattern.expansion_slots = [] then instantiated
    else
      apply_expansion_slots ~expansion_slots:pattern.expansion_slots
        ~source:pattern.source
        ~sequence_node_bindings:match_result.sequence_node_bindings
        ~inherited_bindings ~inner_patterns instantiated
  in
  let tmpl_col = template_base_column text in
  adjust_indentation ~source_column ~template_base_column:tmpl_col instantiated

and apply_partial_recursive ~pattern ~(match_result : match_result) ~source
    ~inner_patterns ~inherited_bindings (match_node : Tree.pat Tree.t)
    (replace_node : Tree.pat Tree.t) (src_node : Tree.src Tree.t) =
  let inner_match_ch = Tree.named_children match_node in
  let inner_replace_ch = Tree.named_children replace_node in
  let inner_source_ch = Tree.named_children src_node in
  if inner_match_ch = [] && inner_replace_ch = [] then []
  else
    match
      Match_engine.rematch_partial_correspondences ~pattern
        ~pattern_source:pattern.source ~source
        ~substitutions:pattern.substitutions inner_match_ch inner_source_ch
    with
    | None -> []
    | Some inner_corrs ->
        let inner_corr_map = Hashtbl.create 8 in
        List.iter
          (fun (c : child_correspondence) ->
            Hashtbl.replace inner_corr_map c.pattern_index c.source_index)
          inner_corrs;
        let inner_source_arr = Array.of_list inner_source_ch in
        let inner_replace_arr = Array.of_list inner_replace_ch in
        let inner_alignment =
          Alignment.align_children ~match_source:pattern.source
            ~replace_source:pattern.replace_source inner_match_ch
            inner_replace_ch
        in
        let source_index_of_pattern mi = Hashtbl.find_opt inner_corr_map mi in
        let best_source_after after_mi =
          let best = ref None in
          Hashtbl.iter
            (fun pi si ->
              if pi <= after_mi then
                match !best with
                | None -> best := Some si
                | Some prev when si > prev -> best := Some si
                | Some _ -> ())
            inner_corr_map;
          !best
        in
        let insertion_spec ~after_mi =
          Alignment.insertion_spec_with_correspondences ~source ~match_result
            inner_source_arr ~after_mi ~best_source_after
        in
        let edits = ref [] in
        List.iter
          (fun (mi, action) ->
            match action with
            | EKeep -> ()
            | ERemove -> (
                match source_index_of_pattern mi with
                | Some si when si < Array.length inner_source_arr ->
                    let sb, eb = Alignment.removal_range inner_source_arr si in
                    edits :=
                      { start_byte = sb; end_byte = eb; replacement = "" }
                      :: !edits
                | _ -> ())
            | EReplace ri -> (
                match source_index_of_pattern mi with
                | Some si
                  when si < Array.length inner_source_arr
                       && ri < Array.length inner_replace_arr ->
                    let m_ch = List.nth inner_match_ch mi in
                    let r_ch = inner_replace_arr.(ri) in
                    let s_ch = inner_source_arr.(si) in
                    if
                      Tree.node_type m_ch = Tree.node_type r_ch
                      && (Tree.named_children m_ch <> []
                         || Tree.named_children r_ch <> [])
                    then
                      let sub =
                        apply_partial_recursive ~pattern ~match_result ~source
                          ~inner_patterns ~inherited_bindings m_ch r_ch s_ch
                      in
                      if sub <> [] then edits := sub @ !edits
                      else
                        let repl_text = Tree.text pattern.replace_source r_ch in
                        let inst =
                          instantiate_with_indent ~pattern ~match_result
                            ~inner_patterns ~inherited_bindings
                            ~source_column:s_ch.Tree.start_point.column
                            repl_text
                        in
                        edits :=
                          {
                            start_byte = s_ch.Tree.start_byte;
                            end_byte = s_ch.Tree.end_byte;
                            replacement = inst;
                          }
                          :: !edits
                    else
                      let repl_text = Tree.text pattern.replace_source r_ch in
                      let inst =
                        instantiate_with_indent ~pattern ~match_result
                          ~inner_patterns ~inherited_bindings
                          ~source_column:s_ch.Tree.start_point.column repl_text
                      in
                      edits :=
                        {
                          start_byte = s_ch.Tree.start_byte;
                          end_byte = s_ch.Tree.end_byte;
                          replacement = inst;
                        }
                        :: !edits
                | _ -> ()))
          inner_alignment.entries;
        List.iter
          (fun (after_mi, ri) ->
            if ri < Array.length inner_replace_arr then
              let repl_text =
                Tree.text pattern.replace_source inner_replace_arr.(ri)
              in
              let kind, insert_byte, separator, insert_col =
                insertion_spec ~after_mi
              in
              let inst =
                instantiate_with_indent ~pattern ~match_result ~inner_patterns
                  ~inherited_bindings ~source_column:insert_col repl_text
              in
              let replacement =
                Alignment.insertion_replacement ~kind ~separator
                  ~instantiated:inst
              in
              edits :=
                {
                  start_byte = insert_byte;
                  end_byte = insert_byte;
                  replacement;
                }
                :: !edits)
          inner_alignment.insertions;
        List.rev !edits

and apply_alignment ~pattern ~(match_result : match_result) ~inner_patterns
    alignment ~(source_index_of_pattern : int -> int option)
    ~(insertion_spec :
       after_mi:int -> Alignment.insertion_kind * int * string * int)
    (source_arr : Tree.src Tree.t array) (replace_arr : Tree.pat Tree.t array) =
  let inherited_bindings =
    {
      text_bindings = match_result.bindings;
      node_bindings = match_result.node_bindings;
      sequence_node_bindings = match_result.sequence_node_bindings;
      correspondences = match_result.correspondences;
    }
  in
  let edits = ref [] in
  List.iter
    (fun (mi, action) ->
      match action with
      | EKeep -> ()
      | ERemove -> (
          match source_index_of_pattern mi with
          | Some si when si < Array.length source_arr ->
              let start_b, end_b = Alignment.removal_range source_arr si in
              edits :=
                { start_byte = start_b; end_byte = end_b; replacement = "" }
                :: !edits
          | _ -> ())
      | EReplace ri -> (
          match source_index_of_pattern mi with
          | Some si
            when si < Array.length source_arr && ri < Array.length replace_arr
            ->
              let src_child = source_arr.(si) in
              let repl_text =
                Tree.text pattern.replace_source replace_arr.(ri)
              in
              let instantiated =
                instantiate_with_indent ~pattern ~match_result ~inner_patterns
                  ~inherited_bindings
                  ~source_column:src_child.Tree.start_point.column repl_text
              in
              edits :=
                {
                  start_byte = src_child.Tree.start_byte;
                  end_byte = src_child.Tree.end_byte;
                  replacement = instantiated;
                }
                :: !edits
          | _ -> ()))
    alignment.entries;
  List.iter
    (fun (after_mi, ri) ->
      if ri < Array.length replace_arr then
        let repl_text = Tree.text pattern.replace_source replace_arr.(ri) in
        let kind, insert_byte, separator, insert_col =
          insertion_spec ~after_mi
        in
        let instantiated =
          instantiate_with_indent ~pattern ~match_result ~inner_patterns
            ~inherited_bindings ~source_column:insert_col repl_text
        in
        let replacement =
          Alignment.insertion_replacement ~kind ~separator ~instantiated
        in
        edits :=
          { start_byte = insert_byte; end_byte = insert_byte; replacement }
          :: !edits)
    alignment.insertions;
  List.rev !edits

and apply_with_correspondences ~pattern ~(match_result : match_result) ~source
    ~match_children_list ~inner_patterns alignment
    (source_arr : Tree.src Tree.t array) (replace_arr : Tree.pat Tree.t array) =
  let corr_map = Hashtbl.create 16 in
  List.iter
    (fun (c : child_correspondence) ->
      Hashtbl.replace corr_map c.pattern_index c.source_index)
    match_result.correspondences;
  let best_source_after after_mi =
    let best = ref None in
    Hashtbl.iter
      (fun pi si ->
        if pi <= after_mi then
          match !best with
          | None -> best := Some si
          | Some prev when si > prev -> best := Some si
          | Some _ -> ())
      corr_map;
    !best
  in
  let source_index_of_pattern mi = Hashtbl.find_opt corr_map mi in
  let insertion_spec ~after_mi =
    Alignment.insertion_spec_with_correspondences ~source ~match_result
      source_arr ~after_mi ~best_source_after
  in
  let inherited_bindings =
    {
      text_bindings = match_result.bindings;
      node_bindings = match_result.node_bindings;
      sequence_node_bindings = match_result.sequence_node_bindings;
      correspondences = match_result.correspondences;
    }
  in
  let edits = ref [] in
  List.iter
    (fun (mi, action) ->
      match action with
      | EKeep -> ()
      | ERemove -> (
          match source_index_of_pattern mi with
          | Some si when si < Array.length source_arr ->
              let start_b, end_b = Alignment.removal_range source_arr si in
              edits :=
                { start_byte = start_b; end_byte = end_b; replacement = "" }
                :: !edits
          | _ -> ())
      | EReplace ri -> (
          match source_index_of_pattern mi with
          | Some si
            when si < Array.length source_arr && ri < Array.length replace_arr
            ->
              let src_child = source_arr.(si) in
              let m_ch = List.nth match_children_list mi in
              let r_ch = replace_arr.(ri) in
              if
                Tree.node_type m_ch = Tree.node_type r_ch
                && (Tree.named_children m_ch <> []
                   || Tree.named_children r_ch <> [])
              then
                let sub =
                  apply_partial_recursive ~pattern ~match_result ~source
                    ~inner_patterns ~inherited_bindings m_ch r_ch src_child
                in
                if sub <> [] then edits := sub @ !edits
                else
                  let repl_text = Tree.text pattern.replace_source r_ch in
                  let instantiated =
                    instantiate_with_indent ~pattern ~match_result
                      ~inner_patterns ~inherited_bindings
                      ~source_column:src_child.Tree.start_point.column repl_text
                  in
                  edits :=
                    {
                      start_byte = src_child.Tree.start_byte;
                      end_byte = src_child.Tree.end_byte;
                      replacement = instantiated;
                    }
                    :: !edits
              else
                let repl_text = Tree.text pattern.replace_source r_ch in
                let instantiated =
                  instantiate_with_indent ~pattern ~match_result ~inner_patterns
                    ~inherited_bindings
                    ~source_column:src_child.Tree.start_point.column repl_text
                in
                edits :=
                  {
                    start_byte = src_child.Tree.start_byte;
                    end_byte = src_child.Tree.end_byte;
                    replacement = instantiated;
                  }
                  :: !edits
          | _ -> ()))
    alignment.entries;
  List.iter
    (fun (after_mi, ri) ->
      if ri < Array.length replace_arr then
        let repl_text = Tree.text pattern.replace_source replace_arr.(ri) in
        let kind, insert_byte, separator, insert_col =
          insertion_spec ~after_mi
        in
        let instantiated =
          instantiate_with_indent ~pattern ~match_result ~inner_patterns
            ~inherited_bindings ~source_column:insert_col repl_text
        in
        let replacement =
          Alignment.insertion_replacement ~kind ~separator ~instantiated
        in
        edits :=
          { start_byte = insert_byte; end_byte = insert_byte; replacement }
          :: !edits)
    alignment.insertions;
  List.rev !edits

and apply_direct ~pattern ~(match_result : match_result) ~source ~inner_patterns
    alignment (source_arr : Tree.src Tree.t array)
    (replace_arr : Tree.pat Tree.t array) =
  let source_index_of_pattern mi = Some mi in
  let insertion_spec ~after_mi =
    Alignment.insertion_spec_direct ~source ~match_result source_arr ~after_mi
  in
  apply_alignment ~pattern ~match_result ~inner_patterns alignment
    ~source_index_of_pattern ~insertion_spec source_arr replace_arr

and compute_field_addition_edits ~pattern ~(match_result : match_result)
    ~source:_ replace_fields source_fields match_field_set =
  let segments = ref [] in
  let prev_anchor = ref None in
  let has_added = ref false in
  List.iter
    (fun (f, _) ->
      match f with
      | Some fname when Hashtbl.mem match_field_set fname ->
          if !has_added then segments := (!prev_anchor, Some fname) :: !segments;
          prev_anchor := Some fname;
          has_added := false
      | Some fname ->
          if not (Hashtbl.mem match_field_set fname) then has_added := true
      | None -> ())
    replace_fields;
  if !has_added then segments := (!prev_anchor, None) :: !segments;
  let segments = List.rev !segments in
  if segments = [] then []
  else
    let edits = ref [] in
    List.iter
      (fun (prev_anchor, next_anchor) ->
        let r_start =
          match prev_anchor with
          | Some p -> (
              match Alignment.field_end_byte replace_fields p with
              | Some b -> b
              | None -> 0)
          | None -> 0
        in
        let r_end =
          match next_anchor with
          | Some n -> (
              match Alignment.field_start_byte replace_fields n with
              | Some b -> b
              | None -> String.length pattern.replace_source)
          | None -> String.length pattern.replace_source
        in
        let s_start =
          match prev_anchor with
          | Some p -> (
              match Alignment.field_end_byte source_fields p with
              | Some b -> b
              | None -> match_result.node.Tree.start_byte)
          | None -> match_result.node.Tree.start_byte
        in
        let s_end =
          match next_anchor with
          | Some n -> (
              match Alignment.field_start_byte source_fields n with
              | Some b -> b
              | None -> match_result.node.Tree.end_byte)
          | None -> match_result.node.Tree.end_byte
        in
        if r_start < r_end && r_end <= String.length pattern.replace_source then begin
          let snippet =
            String.sub pattern.replace_source r_start (r_end - r_start)
          in
          let instantiated =
            instantiate_template ~substitutions:pattern.substitutions
              ~text_bindings:match_result.bindings snippet
          in
          let tmpl_col = template_base_column snippet in
          let replacement =
            adjust_indentation ~source_column:match_result.start_point.column
              ~template_base_column:tmpl_col instantiated
          in
          edits :=
            { start_byte = s_start; end_byte = s_end; replacement } :: !edits
        end)
      segments;
    List.rev !edits

and compute_field_removal_edits ~(match_result : match_result) match_fields
    source_fields replace_field_set =
  let match_field_list = List.filter_map (fun (f, _) -> f) match_fields in
  let match_field_uniq = List.sort_uniq String.compare match_field_list in
  let removed_fields =
    List.filter
      (fun f -> not (Hashtbl.mem replace_field_set f))
      match_field_uniq
  in
  if removed_fields = [] then []
  else
    let edits = ref [] in
    List.iter
      (fun removed_field ->
        let s_start = Alignment.field_start_byte source_fields removed_field in
        let s_end = Alignment.field_end_byte source_fields removed_field in
        let prev_end = ref None in
        let next_start = ref None in
        let found = ref false in
        List.iter
          (fun (f, node) ->
            match f with
            | Some fname when fname = removed_field -> found := true
            | Some _ ->
                if not !found then prev_end := Some node.Tree.end_byte
                else if !next_start = None then
                  next_start := Some node.Tree.start_byte
            | None -> ())
          source_fields;
        match (s_start, s_end) with
        | Some ss, Some se ->
            let remove_start =
              match !prev_end with Some pe -> pe | None -> ss
            in
            let remove_end =
              match !next_start with
              | Some _ -> se
              | None -> (
                  match !prev_end with
                  | Some _ -> se
                  | None -> match_result.node.Tree.end_byte)
            in
            edits :=
              {
                start_byte = remove_start;
                end_byte = remove_end;
                replacement = "";
              }
              :: !edits
        | _ -> ())
      removed_fields;
    List.rev !edits

let transform_with_overlap_flag ~ctx ~language ~pattern_text ~source_text =
  let pattern = Match_parse.parse_pattern ~ctx ~language pattern_text in
  if not pattern.is_transform then
    ( {
        edits = [];
        original_source = source_text;
        transformed_source = source_text;
      },
      false )
  else
    let source_tree = Tree.parse ~ctx ~language source_text in
    let source = source_tree.source in
    let matches =
      Match_search.find_matches_in_subtree ~pattern
        ~inherited_bindings:Match_engine.empty_bindings ~source
        ~root_node:source_tree.root
    in
    let edits =
      List.concat_map
        (fun m ->
          compute_edits ~pattern ~match_result:m ~source ~inner_patterns:[])
        matches
    in
    let transformed, had_overlaps = apply_edits_flagged source_text edits in
    ( { edits; original_source = source_text; transformed_source = transformed },
      had_overlaps )

let transform ~ctx ~language ~pattern_text ~source_text =
  let rec loop source =
    let result, had_overlaps =
      transform_with_overlap_flag ~ctx ~language ~pattern_text
        ~source_text:source
    in
    if had_overlaps then loop result.transformed_source else result
  in
  let result = loop source_text in
  { result with original_source = source_text }

let transform_nested ~ctx ~language ~pattern_text ~source_text =
  let nested = Match_parse.parse_nested_pattern ~ctx ~language pattern_text in
  match nested.patterns with
  | [] ->
      {
        edits = [];
        original_source = source_text;
        transformed_source = source_text;
      }
  | outer_pattern :: inner_patterns ->
      if not outer_pattern.is_transform then
        {
          edits = [];
          original_source = source_text;
          transformed_source = source_text;
        }
      else
        let source_tree = Tree.parse ~ctx ~language source_text in
        let source = source_tree.source in
        let matches =
          Match_search.find_matches_in_subtree ~pattern:outer_pattern
            ~inherited_bindings:Match_engine.empty_bindings ~source
            ~root_node:source_tree.root
        in
        let edits =
          List.concat_map
            (fun m ->
              compute_edits ~pattern:outer_pattern ~match_result:m ~source
                ~inner_patterns)
            matches
        in
        let transformed = apply_edits source_text edits in
        {
          edits;
          original_source = source_text;
          transformed_source = transformed;
        }

let transform_file ~ctx ~language ~pattern_text ~source_path =
  let source_text =
    In_channel.with_open_text source_path In_channel.input_all
  in
  transform ~ctx ~language ~pattern_text ~source_text

type diff_op = DKeep of string | DRemove of string | DAdd of string

let generate_diff ~file_path ~original ~transformed =
  if original = transformed then ""
  else
    let orig_lines = String.split_on_char '\n' original in
    let trans_lines = String.split_on_char '\n' transformed in
    let buf = Buffer.create 1024 in
    Buffer.add_string buf (Printf.sprintf "--- a/%s\n" file_path);
    Buffer.add_string buf (Printf.sprintf "+++ b/%s\n" file_path);
    let orig_arr = Array.of_list orig_lines in
    let trans_arr = Array.of_list trans_lines in
    let n = Array.length orig_arr in
    let m = Array.length trans_arr in
    let dp = Array.make_matrix (n + 1) (m + 1) 0 in
    for i = n - 1 downto 0 do
      for j = m - 1 downto 0 do
        if orig_arr.(i) = trans_arr.(j) then
          dp.(i).(j) <- dp.(i + 1).(j + 1) + 1
        else dp.(i).(j) <- max dp.(i + 1).(j) dp.(i).(j + 1)
      done
    done;
    let ops = ref [] in
    let i = ref 0 in
    let j = ref 0 in
    while !i < n || !j < m do
      if !i < n && !j < m && orig_arr.(!i) = trans_arr.(!j) then (
        ops := DKeep orig_arr.(!i) :: !ops;
        incr i;
        incr j)
      else if !i < n && (!j >= m || dp.(!i + 1).(!j) >= dp.(!i).(!j + 1)) then (
        ops := DRemove orig_arr.(!i) :: !ops;
        incr i)
      else (
        ops := DAdd trans_arr.(!j) :: !ops;
        incr j)
    done;
    let ops = List.rev !ops in
    let context_lines = 3 in
    let ops_arr = Array.of_list ops in
    let n_ops = Array.length ops_arr in
    let rec find_hunks start_op =
      if start_op >= n_ops then []
      else
        let rec find_change k =
          if k >= n_ops then None
          else
            match ops_arr.(k) with
            | DKeep _ -> find_change (k + 1)
            | _ -> Some k
        in
        match find_change start_op with
        | None -> []
        | Some change_idx ->
            let hunk_start = max start_op (change_idx - context_lines) in
            let rec find_hunk_end k last_change =
              if k >= n_ops then n_ops
              else
                match ops_arr.(k) with
                | DKeep _ ->
                    if k - last_change > 2 * context_lines then k
                    else find_hunk_end (k + 1) last_change
                | _ -> find_hunk_end (k + 1) k
            in
            let hunk_end = find_hunk_end change_idx change_idx in
            let hunk = Array.sub ops_arr hunk_start (hunk_end - hunk_start) in
            hunk :: find_hunks hunk_end
    in
    let hunks = find_hunks 0 in
    List.iter
      (fun hunk ->
        Buffer.add_string buf "@@ ... @@\n";
        Array.iter
          (fun op ->
            match op with
            | DKeep s ->
                Buffer.add_char buf ' ';
                Buffer.add_string buf s;
                Buffer.add_char buf '\n'
            | DRemove s ->
                Buffer.add_char buf '-';
                Buffer.add_string buf s;
                Buffer.add_char buf '\n'
            | DAdd s ->
                Buffer.add_char buf '+';
                Buffer.add_string buf s;
                Buffer.add_char buf '\n')
          hunk)
      hunks;
    Buffer.contents buf
