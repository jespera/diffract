open Match_types

(** Parse a single preamble line. Formats:
    - "metavar $name: single" or "metavar $name: sequence"
    - "match: partial"
    - "on $var" Returns None for empty lines, Some directive otherwise. *)
let parse_preamble_line line =
  let line = String.trim line in
  if line = "" then None
  else if String.starts_with ~prefix:"metavar " line then
    (* Extract everything after "metavar " *)
    let rest = String.sub line 8 (String.length line - 8) |> String.trim in
    (* Split on ":" to get name and type *)
    match String.split_on_char ':' rest with
    | [ name_part; type_part ] -> (
        let name = String.trim name_part in
        let typ = String.trim type_part in
        if not (String.starts_with ~prefix:"$" name) then
          failwith (Printf.sprintf "Metavar name must start with '$': %s" name);
        match typ with
        | "single" -> Some (Metavar (name, false))
        | "sequence" -> Some (Metavar (name, true))
        | _ ->
            failwith
              (Printf.sprintf
                 "Invalid metavar type '%s' (expected 'single' or 'sequence')"
                 typ))
    | _ ->
        failwith
          (Printf.sprintf
             "Invalid metavar declaration (expected 'metavar $name: type'): %s"
             line)
  else if line = "match: strict" then Some (MatchMode Strict)
  else if line = "match: field" then Some (MatchMode Field)
  else if line = "match: partial" then Some (MatchMode Partial)
  else if String.starts_with ~prefix:"on " line then (
    let var = String.sub line 3 (String.length line - 3) |> String.trim in
    if not (String.starts_with ~prefix:"$" var) then
      failwith
        (Printf.sprintf
           "on directive requires a metavar (starting with '$'): %s" var);
    Some (OnVar var))
  else failwith (Printf.sprintf "Invalid preamble line: %s" line)

(** Parse the \@@ preamble from pattern text. Format:
    {v
      @@
      metavar $name: single
      metavar $other: sequence
      match: partial
      on $var
      @@
      pattern body
    v}
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
      else if after_first.[pos] = '@' && after_first.[pos + 1] = '@' then pos
      else find_closing (pos + 1)
    in
    let closing_pos = find_closing 0 in
    let vars_section = String.sub after_first 0 closing_pos in
    let pattern_body =
      String.sub after_first (closing_pos + 2)
        (String.length after_first - closing_pos - 2)
    in
    (* Strip only surrounding newlines (not spaces) so that a leading space on
       the first body line is preserved as the role-indicator prefix. *)
    let pattern_body =
      let n = String.length pattern_body in
      let is_nl c = c = '\n' || c = '\r' in
      let i = ref 0 and j = ref (n - 1) in
      while !i < n && is_nl pattern_body.[!i] do
        incr i
      done;
      while !j >= !i && is_nl pattern_body.[!j] do
        decr j
      done;
      if !j < !i then "" else String.sub pattern_body !i (!j - !i + 1)
    in
    (* Parse preamble lines *)
    let lines = String.split_on_char '\n' vars_section in
    let parsed = List.filter_map parse_preamble_line lines in
    (* Extract components *)
    let metavars =
      List.filter_map (function Metavar (n, _) -> Some n | _ -> None) parsed
    in
    let sequence_metavars =
      List.filter_map
        (function Metavar (n, true) -> Some n | _ -> None)
        parsed
    in
    let match_mode =
      List.find_map (function MatchMode m -> Some m | _ -> None) parsed
    in
    let on_var =
      List.find_map (function OnVar v -> Some v | _ -> None) parsed
    in
    {
      p_metavars = metavars;
      p_sequence_metavars = sequence_metavars;
      p_match_mode = match_mode;
      p_on_var = on_var;
      p_pattern_body = pattern_body;
    }

(** Generate a valid placeholder identifier for a metavar *)
let placeholder_for_index i = Printf.sprintf "__meta_%d__" i

(** Substitute metavars with valid placeholders in pattern text *)
let substitute_metavars metavars pattern_text =
  let substitutions =
    List.mapi (fun i var -> (var, placeholder_for_index i)) metavars
  in
  let subst_tbl = Hashtbl.create (List.length substitutions) in
  List.iter
    (fun (var, placeholder) -> Hashtbl.add subst_tbl var placeholder)
    substitutions;
  let is_ident_char = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  let len = String.length pattern_text in
  let buf = Buffer.create len in
  let i = ref 0 in
  while !i < len do
    if
      pattern_text.[!i] = '$'
      && !i + 1 < len
      && is_ident_char pattern_text.[!i + 1]
    then begin
      let start = !i in
      incr i;
      while !i < len && is_ident_char pattern_text.[!i] do
        incr i
      done;
      let var = String.sub pattern_text start (!i - start) in
      match Hashtbl.find_opt subst_tbl var with
      | Some placeholder -> Buffer.add_string buf placeholder
      | None -> Buffer.add_string buf var
    end
    else begin
      Buffer.add_char buf pattern_text.[!i];
      incr i
    end
  done;
  (Buffer.contents buf, substitutions)

(** Find all metavars used in pattern text (tokens starting with $ followed by
    uppercase). This distinguishes metavars ($MSG, $PROP) from language
    variables ($this, $name). Metavars must start with an uppercase letter after
    the $. *)
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
        while
          !i < String.length text
          &&
          let c = text.[!i] in
          (c >= 'a' && c <= 'z')
          || (c >= 'A' && c <= 'Z')
          || (c >= '0' && c <= '9')
          || c = '_'
        do
          incr i
        done;
        if !i > start + 1 then
          results := String.sub text start (!i - start) :: !results
      end
      else incr i
    end
    else incr i
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
    failwith
      (Printf.sprintf "Undeclared metavars: %s" (String.concat ", " undeclared))

(** Check if a '...' at position [i] in [text] is a spread/rest operator rather
    than an ellipsis pattern (i.e., immediately followed by '$' or an
    alphanumeric character, as in `...$args` or `...rest`). *)
let is_spread_at text i =
  let len = String.length text in
  if i + 3 < len then
    match text.[i + 3] with
    | '$' -> true
    | c
      when (c >= 'a' && c <= 'z')
           || (c >= 'A' && c <= 'Z')
           || (c >= '0' && c <= '9') ->
        true
    | _ -> false
  else false

(** Preprocess ellipsis (...) in pattern text. Replaces ... with __ellipsis_N__
    placeholders. Adds trailing ; if ... is followed by only whitespace then
    newline (statement context). Does NOT replace ... if immediately followed by
    $ or alphanumeric (spread/rest operator). Returns (transformed_text,
    ellipsis_substitutions) where ellipsis_substitutions maps unique var names
    to placeholder strings (e.g., [("..._0", "__ellipsis_0__")]). *)
let preprocess_ellipsis text =
  let len = String.length text in
  let result = Buffer.create len in
  let substitutions = ref [] in
  let counter = ref 0 in
  let i = ref 0 in
  while !i < len do
    (* Check for ... *)
    if
      !i + 2 < len
      && text.[!i] = '.'
      && text.[!i + 1] = '.'
      && text.[!i + 2] = '.'
    then begin
      if is_spread_at text !i then begin
        (* Keep as literal ... *)
        Buffer.add_string result "...";
        i := !i + 3
      end
      else begin
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
        if !j >= len || text.[!j] = '\n' then Buffer.add_char result ';'
      end
    end
    else begin
      Buffer.add_char result text.[!i];
      incr i
    end
  done;
  (Buffer.contents result, List.rev !substitutions)

(** True if [c] can serve as an expansion-line prefix. Column 0 of a pattern
    line is always a role indicator (like unified diff), so any character that
    is not already a reserved role marker is a valid expansion separator.
    Reserved: '-', '+', ' ', '\t' (spatch roles), and characters that start
    identifiers/metavars ('$', letters, digits, '_'). *)
let is_expansion_prefix c =
  c <> '-' && c <> '+' && c <> ' ' && c <> '\t' && c <> '$'
  && (not (c >= 'a' && c <= 'z'))
  && (not (c >= 'A' && c <= 'Z'))
  && (not (c >= '0' && c <= '9'))
  && c <> '_'

(** Classify and preprocess spatch lines in a single pass. Lines prefixed with
    "- " are match-only (minus lines). Lines prefixed with "+ " are replace-only
    (plus lines). Lines with " " prefix or no prefix are context (appear in
    both). A line whose first character satisfies [is_expansion_prefix] and
    whose content (rest of the line, trimmed) contains at least one declared
    sequence metavar is an expansion line: the prefix character is the join
    separator ('~' stands for newline), and the line becomes a placeholder in
    replace_lines only. Lines with expansion-prefix characters but NO declared
    sequence metavar are treated as ordinary context lines so that code lines
    starting with characters like ['\{'] or ['('] are never misidentified.
    Ellipsis in context/minus lines becomes placeholders and is bound. Ellipsis
    in plus-only lines is rejected (unbound). Returns (match_text,
    replace_template, ellipsis_subs, expansion_slots, is_transform). *)
let classify_and_preprocess_spatch ~sequence_metavars body =
  let lines = String.split_on_char '\n' body in
  let match_lines = ref [] in
  let replace_lines = ref [] in
  let substitutions = ref [] in
  let counter = ref 0 in
  let expand_counter = ref 0 in
  let expansion_slots = ref [] in
  let replace_ellipsis_in_line line =
    let len = String.length line in
    let buf = Buffer.create len in
    let i = ref 0 in
    while !i < len do
      if
        !i + 2 < len
        && line.[!i] = '.'
        && line.[!i + 1] = '.'
        && line.[!i + 2] = '.'
      then begin
        if is_spread_at line !i then begin
          Buffer.add_string buf "...";
          i := !i + 3
        end
        else begin
          let n = !counter in
          let var_name = Printf.sprintf "..._%d" n in
          let placeholder = Printf.sprintf "__ellipsis_%d__" n in
          substitutions := (var_name, placeholder) :: !substitutions;
          incr counter;
          Buffer.add_string buf placeholder;
          i := !i + 3;
          let j = ref !i in
          while !j < len && (line.[!j] = ' ' || line.[!j] = '\t') do
            incr j
          done;
          if !j >= len then Buffer.add_char buf ';'
        end
      end
      else begin
        Buffer.add_char buf line.[!i];
        incr i
      end
    done;
    Buffer.contents buf
  in
  List.iter
    (fun line ->
      if String.length line >= 2 && line.[0] = '-' && line.[1] = ' ' then begin
        let content = String.sub line 2 (String.length line - 2) in
        let transformed = replace_ellipsis_in_line content in
        match_lines := transformed :: !match_lines
      end
      else if String.length line >= 2 && line.[0] = '+' && line.[1] = ' ' then begin
        let content = String.sub line 2 (String.length line - 2) in
        (* Disallow ellipsis in plus-only lines (unbound) *)
        let rec scan i =
          if i + 2 >= String.length content then false
          else if
            content.[i] = '.' && content.[i + 1] = '.' && content.[i + 2] = '.'
          then if is_spread_at content i then scan (i + 3) else true
          else scan (i + 1)
        in
        if scan 0 then
          failwith
            "Ellipsis (...) in replacement-only line is not bound by match";
        replace_lines := content :: !replace_lines
      end
      else if
        String.length line >= 2
        && is_expansion_prefix line.[0]
        && line.[1] = ' '
      then begin
        (* Potential expansion line: prefix char is separator, rest is content.
           The second character must be a space so that code lines starting with
           punctuation (e.g. PHP '($ARGS)') are not misidentified. *)
        let sep_char = line.[0] in
        let content =
          String.trim (String.sub line 1 (String.length line - 1))
        in
        let vars = find_metavars_in_text content in
        (* Only treat as expansion when content contains a declared sequence
           metavar; otherwise the line is an ordinary context line (no vars)
           or an error (vars present but none are sequence metavars). *)
        let has_seq_var =
          List.exists (fun v -> List.mem v sequence_metavars) vars
        in
        if vars = [] then begin
          (* No metavars at all — treat as a normal context line *)
          let transformed = replace_ellipsis_in_line line in
          match_lines := transformed :: !match_lines;
          replace_lines := transformed :: !replace_lines
        end
        else if not has_seq_var then begin
          (* Has metavars but none are sequence — user likely intended an
             expansion line but used a single metavar; raise a clear error. *)
          let bad =
            List.find (fun v -> not (List.mem v sequence_metavars)) vars
          in
          failwith
            (Printf.sprintf
               "Variable '%s' in expansion line must be declared as sequence \
                metavar"
               bad)
        end
        else begin
          (* Expansion line: replace with a unique placeholder in replace side *)
          let separator =
            if sep_char = '~' then "\n" else String.make 1 sep_char
          in
          let n = !expand_counter in
          let placeholder = Printf.sprintf "__expand_%d__" n in
          incr expand_counter;
          expansion_slots :=
            {
              exp_placeholder = placeholder;
              exp_separator = separator;
              exp_vars = vars;
            }
            :: !expansion_slots;
          replace_lines := placeholder :: !replace_lines
          (* Not added to match_lines — replace-only, like a '+' line *)
        end
      end
      else begin
        let content =
          if String.length line >= 1 && line.[0] = ' ' then
            String.sub line 1 (String.length line - 1)
          else line
        in
        let transformed = replace_ellipsis_in_line content in
        match_lines := transformed :: !match_lines;
        replace_lines := transformed :: !replace_lines
      end)
    lines;
  let match_text = String.concat "\n" (List.rev !match_lines) in
  let replace_template = String.concat "\n" (List.rev !replace_lines) in
  let is_transform =
    !expansion_slots <> []
    || List.exists
         (fun line ->
           String.length line >= 2
           && (line.[0] = '-' || line.[0] = '+')
           && line.[1] = ' ')
         lines
  in
  ( match_text,
    replace_template,
    List.rev !substitutions,
    List.rev !expansion_slots,
    is_transform )

(** Parse a pattern file/string into a pattern structure. [inherited_metavars]
    and [inherited_sequences] are from previous sections. *)
let parse_pattern ~ctx ~language ?(inherited_metavars = [])
    ?(inherited_sequences = []) pattern_text =
  let preamble = parse_preamble pattern_text in
  (* Check for re-declarations (No Shadowing Rule) *)
  List.iter
    (fun var ->
      if List.mem var inherited_metavars then
        failwith
          (Printf.sprintf
             "Metavariable '%s' is already declared in a previous section. \
              Shadowing is not permitted."
             var))
    preamble.p_metavars;

  let all_metavars = inherited_metavars @ preamble.p_metavars in
  let all_sequences = inherited_sequences @ preamble.p_sequence_metavars in

  (* Require explicit match mode *)
  let match_mode =
    match preamble.p_match_mode with
    | Some m -> m
    | None ->
        failwith
          "Pattern must specify match mode (match: strict, match: field, or \
           match: partial)"
  in
  let ( pattern_body,
        replace_template,
        ellipsis_subs,
        expansion_slots,
        is_transform ) =
    classify_and_preprocess_spatch ~sequence_metavars:all_sequences
      preamble.p_pattern_body
  in
  (* Validate all used metavars are declared in current or parent scope *)
  validate_metavars all_metavars pattern_body;
  (* Then substitute declared metavars *)
  let transformed_source, metavar_subs =
    substitute_metavars all_metavars pattern_body
  in
  (* Combine substitutions *)
  let all_substitutions = metavar_subs @ ellipsis_subs in
  (* Ellipsis var names (e.g., "..._0", "..._1") are sequence metavars *)
  let ellipsis_var_names = List.map fst ellipsis_subs in
  (* Parse the transformed pattern with tree-sitter (typed as pattern tree) *)
  let tree = Tree.parse_as_pattern ~ctx ~language transformed_source in
  (* Add ellipsis var names to sequence_metavars *)
  let all_sequences = all_sequences @ ellipsis_var_names in
  (* Disallow locally-declared sequence metavars in partial mode.
     Inherited sequences (from outer sections) are fine — they are used only
     in expansion lines, not as match patterns. *)
  if match_mode = Partial then begin
    let local_sequences = preamble.p_sequence_metavars @ ellipsis_var_names in
    if local_sequences <> [] then
      failwith "Sequence metavars are not supported in match: partial"
  end;
  (* Handle replacement template if this is a transform *)
  let replace_tree, replace_source =
    if is_transform then begin
      (* Validate: metavars in + lines must appear in - or context lines *)
      let replace_metavars = find_metavars_in_text replace_template in
      let match_metavars = find_metavars_in_text pattern_body in
      (* For transform, replace_metavars must be in match_metavars OR inherited *)
      let undefined =
        List.filter
          (fun v ->
            not (List.mem v match_metavars || List.mem v inherited_metavars))
          replace_metavars
      in
      if undefined <> [] then
        failwith
          (Printf.sprintf "Metavars in replacement not bound in match: %s"
             (String.concat ", " undefined));
      (* Validate expansion slots: vars must be sequence metavars and bound in match *)
      List.iter
        (fun slot ->
          List.iter
            (fun var ->
              if not (List.mem var all_sequences) then
                failwith
                  (Printf.sprintf
                     "Variable '%s' in expansion line must be declared as \
                      sequence metavar"
                     var);
              if
                not
                  (List.mem var match_metavars
                  || List.mem var inherited_metavars)
              then
                failwith
                  (Printf.sprintf
                     "Variable '%s' in expansion line not bound in match \
                      pattern"
                     var))
            slot.exp_vars)
        expansion_slots;
      (* Substitute metavars in replacement template *)
      let replace_transformed, _ =
        substitute_metavars all_metavars replace_template
      in
      let replace_tree =
        Tree.parse_as_pattern ~ctx ~language replace_transformed
      in
      (Some replace_tree, replace_transformed)
    end
    else (None, "")
  in
  {
    metavars = all_metavars;
    sequence_metavars = all_sequences;
    substitutions = all_substitutions;
    tree;
    source = transformed_source;
    original_source = preamble.p_pattern_body;
    match_mode;
    on_var = preamble.p_on_var;
    is_transform;
    replace_tree;
    replace_source;
    expansion_slots;
  }

(** Split pattern text into sections. Each section has format: \@@ metavars \@@
    pattern_code A new section starts when we see \@@ after having seen two \@@
    in the current section. Returns list of section strings (each starting with
    \@@). *)
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
          let section =
            String.trim (String.concat "\n" (List.rev current_section))
          in
          if section = "" then List.rev acc else List.rev (section :: acc)
      | line :: rest ->
          let trimmed = String.trim line in
          let line_has_at = String.starts_with ~prefix:"@@" trimmed in
          if line_has_at && at_count >= 2 then
            (* We've already seen two @@, so this @@ starts a new section *)
            let section =
              String.trim (String.concat "\n" (List.rev current_section))
            in
            find_sections (section :: acc) [ line ] 1 rest
          else if line_has_at then
            (* This is part of the current section's preamble markers *)
            find_sections acc (line :: current_section) (at_count + 1) rest
          else find_sections acc (line :: current_section) at_count rest
    in
    let sections = find_sections [] [] 0 lines in
    if sections = [] then [ text ] else sections

(** Parse a nested pattern from text with multiple \@@ sections *)
let parse_nested_pattern ~ctx ~language pattern_text =
  let sections = split_pattern_sections pattern_text in
  let rec parse_all inherited_vars inherited_seqs = function
    | [] -> []
    | s :: rest ->
        let p =
          parse_pattern ~ctx ~language ~inherited_metavars:inherited_vars
            ~inherited_sequences:inherited_seqs s
        in
        (* Pass all cumulative metavars to next sections. Note that p.metavars
           already contains inherited_vars because we passed them in. *)
        p :: parse_all p.metavars p.sequence_metavars rest
  in
  let patterns = parse_all [] [] sections in
  (* Validate: a non-first section with transform lines but no 'on $VAR'
     declaration has transforms that can never be applied. This is almost
     certainly a mistake (forgotten 'on $VAR'). *)
  List.iteri
    (fun i p ->
      if i > 0 && p.is_transform && p.on_var = None then
        failwith
          (Printf.sprintf
             "Section %d has transform lines (- / +) but no 'on $VAR' \
              declaration. Inner section transforms are only applied when \
              targeting an expansion variable via 'on $VAR'."
             (i + 1)))
    patterns;
  { patterns }
