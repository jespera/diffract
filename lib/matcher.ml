(** See [matcher.mli]. *)

module M = Stmatch.Make (Tree_sitter_cursor)

type mode = Strict | Partial | Field

(* A section's scope directive. [Global] runs against the whole source;
   [On name] scopes to the single subtree bound by [name] (run once);
   [Foreach name] runs the section once per element of the sequence bound
   by [name]. [On] requires a single metavar, [Foreach] a sequence — see
   {!validate_sections}. *)
type scope = Global | On of string | Foreach of string

type section = {
  mode : mode;
  scope : scope;
  single_metavars : string list;
  sequence_metavars : string list;
  joins : (string * string) list;
      (** [join $VAR by "<sep>"] directives declared in this section's preamble:
          [(var, separator)]. The separator joins the rendered elements when
          [$VAR] is spliced into a replacement (default empty when there's no
          directive). *)
  body : string;
}

type parsed_pattern = { sections : section list }
type composite_match = { sections : M.match_result list }

let chop prefix s =
  String.sub s (String.length prefix) (String.length s - String.length prefix)

let parse_mode = function
  | "strict" -> Strict
  | "partial" -> Partial
  | "field" -> Field
  | s -> failwith (Printf.sprintf "Unknown match mode: %s" s)

(* Parse one section's preamble lines into a [section] record (with [body]
   left empty — the caller fills it in). *)
(* Find the first occurrence of [sub] in [s], returning its start index. *)
let find_sub sub s =
  let ls = String.length s and lsub = String.length sub in
  let rec go i =
    if i + lsub > ls then None
    else if String.sub s i lsub = sub then Some i
    else go (i + 1)
  in
  go 0

(* Strip a single pair of surrounding double quotes, if present. *)
let unquote s =
  let l = String.length s in
  if l >= 2 && s.[0] = '"' && s.[l - 1] = '"' then String.sub s 1 (l - 2) else s

(* Interpret the common backslash escapes in a separator literal so a [join]
   directive can carry a newline or tab: [\n] -> newline, [\t] -> tab,
   [\\] -> backslash. Unknown escapes are left as-is. *)
let interpret_escapes s =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    if s.[!i] = '\\' && !i + 1 < n then begin
      (match s.[!i + 1] with
      | 'n' -> Buffer.add_char buf '\n'
      | 't' -> Buffer.add_char buf '\t'
      | '\\' -> Buffer.add_char buf '\\'
      | c ->
          Buffer.add_char buf '\\';
          Buffer.add_char buf c);
      i := !i + 2
    end
    else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let parse_section_preamble ~section_index preamble_lines =
  let mode = ref None in
  let scope = ref Global in
  let singles = ref [] in
  let sequences = ref [] in
  let joins = ref [] in
  let set_scope s =
    match !scope with
    | Global -> scope := s
    | _ ->
        failwith
          (Printf.sprintf
             "Section %d: duplicate scope directive (at most one of 'on' / \
              'foreach')"
             (section_index + 1))
  in
  List.iter
    (fun line ->
      let line = String.trim line in
      if line = "" then ()
      else if String.starts_with ~prefix:"match:" line then
        mode := Some (parse_mode (String.trim (chop "match:" line)))
      else if String.starts_with ~prefix:"on " line then
        set_scope (On (String.trim (chop "on " line)))
      else if String.starts_with ~prefix:"foreach " line then
        set_scope (Foreach (String.trim (chop "foreach " line)))
      else if String.starts_with ~prefix:"join " line then
        let rest = String.trim (chop "join " line) in
        match find_sub " by " rest with
        | None ->
            failwith
              (Printf.sprintf
                 "Section %d: malformed join (expected: join <var> by \
                  \"<sep>\"): %s"
                 (section_index + 1) line)
        | Some idx ->
            let var = String.trim (String.sub rest 0 idx) in
            let sep =
              interpret_escapes
                (unquote
                   (String.trim
                      (String.sub rest (idx + 4) (String.length rest - idx - 4))))
            in
            joins := (var, sep) :: !joins
      else if String.starts_with ~prefix:"metavar " line then
        let rest = chop "metavar " line in
        match String.split_on_char ':' rest with
        | [ name; kind ] -> (
            let name = String.trim name in
            match String.trim kind with
            | "single" -> singles := name :: !singles
            | "sequence" -> sequences := name :: !sequences
            | k ->
                failwith
                  (Printf.sprintf
                     "Section %d: invalid metavar kind '%s' (expected single \
                      or sequence)"
                     (section_index + 1) k))
        | _ ->
            failwith
              (Printf.sprintf
                 "Section %d: invalid metavar declaration (expected 'metavar \
                  <name>: <kind>'): %s"
                 (section_index + 1) line)
      else
        failwith
          (Printf.sprintf "Section %d: unknown preamble line: %s"
             (section_index + 1) line))
    preamble_lines;
  let mode =
    match !mode with
    | Some m -> m
    | None ->
        failwith
          (Printf.sprintf
             "Section %d must specify a match mode (match: strict | partial | \
              field)"
             (section_index + 1))
  in
  {
    mode;
    scope = !scope;
    single_metavars = List.rev !singles;
    sequence_metavars = List.rev !sequences;
    joins = List.rev !joins;
    body = "";
  }

(* Cross-section validation: same metavar name in multiple sections must
   carry the same kind, and any [on $VAR] must reference a [single]
   metavar declared in some earlier section. *)
type kind = K_single | K_sequence

let kind_string = function K_single -> "single" | K_sequence -> "sequence"

let validate_sections sections =
  let decls = Hashtbl.create 8 in
  (* name -> (kind, first section index that declared it) *)
  List.iteri
    (fun idx s ->
      let check name kind =
        match Hashtbl.find_opt decls name with
        | Some (prior_kind, prior_idx) when prior_kind <> kind ->
            failwith
              (Printf.sprintf
                 "Metavar '%s' declared as %s in section %d and as %s in \
                  section %d"
                 name (kind_string prior_kind) (prior_idx + 1)
                 (kind_string kind) (idx + 1))
        | Some _ -> ()
        | None -> Hashtbl.add decls name (kind, idx)
      in
      List.iter (fun n -> check n K_single) s.single_metavars;
      List.iter (fun n -> check n K_sequence) s.sequence_metavars;
      (* A scope directive ([on]/[foreach]) must reference a metavar of the
         right kind declared in an earlier section. *)
      let check_scope_ref name expected directive =
        match Hashtbl.find_opt decls name with
        | None ->
            failwith
              (Printf.sprintf
                 "Section %d: '%s %s' references %s, which is not declared in \
                  any prior section"
                 (idx + 1) directive name name)
        | Some (_, prior_idx) when prior_idx >= idx ->
            failwith
              (Printf.sprintf
                 "Section %d: '%s %s' must reference a metavar declared in an \
                  earlier section"
                 (idx + 1) directive name)
        | Some (k, _) when k <> expected ->
            let want, other =
              match expected with
              | K_single -> ("single", "foreach")
              | K_sequence -> ("sequence", "on")
            in
            failwith
              (Printf.sprintf
                 "Section %d: '%s %s' requires a %s metavar, but %s is \
                  declared as %s (use '%s' instead)"
                 (idx + 1) directive name want name (kind_string k) other)
        | Some _ -> ()
      in
      match s.scope with
      | Global -> ()
      | On name -> check_scope_ref name K_single "on"
      | Foreach name -> check_scope_ref name K_sequence "foreach")
    sections

(* Multi-section pattern parser. A pattern file's structure is:

     @@                  <- delim 0, opens section 1 preamble
     <preamble §1>
     @@                  <- delim 1, opens section 1 body
     <body §1>
     @@                  <- delim 2, opens section 2 preamble
     <preamble §2>
     @@                  <- delim 3, opens section 2 body
     <body §2>
     ...                 <- and so on; body of last section runs to EOF

   So a file with N sections has 2N [@@] delimiter lines. We collect all
   [@@] line indices, validate the count is even and >= 2, then carve
   out preamble + body slices for each section.

   Metavar names are arbitrary tokens — no [$] prefix is required or
   assumed. The body of each section is taken verbatim (the spatch-style
   [- ]/[+ ] role indicators are handled by [match_side]). *)
let parse_pattern text =
  let lines = Array.of_list (String.split_on_char '\n' text) in
  let n = Array.length lines in
  let delims = ref [] in
  Array.iteri
    (fun i line -> if String.trim line = "@@" then delims := i :: !delims)
    lines;
  let delims = Array.of_list (List.rev !delims) in
  let nd = Array.length delims in
  if nd = 0 then failwith "Pattern: no @@ delimiter found";
  if nd mod 2 <> 0 then
    failwith
      (Printf.sprintf
         "Pattern: odd number of @@ delimiters (%d) — each section needs an \
          opening and a closing @@"
         nd);
  let num_sections = nd / 2 in
  let extract_section i =
    let preamble_start = delims.(2 * i) + 1 in
    let preamble_end = delims.((2 * i) + 1) in
    let body_start = delims.((2 * i) + 1) + 1 in
    let body_end = if i = num_sections - 1 then n else delims.(2 * (i + 1)) in
    let preamble_lines =
      Array.sub lines preamble_start (preamble_end - preamble_start)
      |> Array.to_list
    in
    let body_lines =
      Array.sub lines body_start (body_end - body_start) |> Array.to_list
    in
    (* Drop empty lines at the body's edges — they are delimiter/file
       artifacts (notably the trailing [\n] every pattern file ends with),
       not content. Left in, a trailing empty line becomes a context line
       that leaks a stray newline into the replacement template. Only
       truly-empty ([""]) lines are dropped, so a deliberately blank context
       line written as a single space is preserved. *)
    let trim_blank_edges ls =
      let drop_leading_blanks = function
        | l when List.for_all (fun s -> s = "") l -> []
        | l ->
            let rec go = function "" :: r -> go r | rest -> rest in
            go l
      in
      ls |> drop_leading_blanks |> List.rev |> drop_leading_blanks |> List.rev
    in
    let section = parse_section_preamble ~section_index:i preamble_lines in
    { section with body = String.concat "\n" (trim_blank_edges body_lines) }
  in
  let sections = List.init num_sections extract_section in
  validate_sections sections;
  ({ sections } : parsed_pattern)

(* Extract the match side of a (possibly transform-style) body. Following
   diffract's spatch conventions, column 0 is a role indicator:

     "- " line  -> match-only; content is the rest of the line
     "+ " line  -> replace-only; excluded from the match side
     other line -> context (matches); one leading space (the role-indicator
                   slot) is stripped if present

   For a pure-match body (no "-"/"+" lines), every line is context, so this
   just strips an optional leading space per line — harmless for matching,
   since tree-sitter ignores leading whitespace anyway. Ellipsis is left for
   the tokenizer to handle. Expansion lines are not handled here; that's
   transform-side work. *)
(* A spatch body, line by line, with its role: context (kept on both sides),
   a `- ` removal, or a `+ ` addition. The role marker is column 0 (like a
   unified diff); a context line has at most one leading role-indicator space
   stripped. This is the structured form from which both the match side and
   the replace side derive — and the basis surgical transforms build on. *)
type spatch_line = Ctx of string | Del of string | Add of string

let classify_spatch body : spatch_line list =
  String.split_on_char '\n' body
  |> List.map (fun line ->
      let len = String.length line in
      if len >= 2 && line.[0] = '-' && line.[1] = ' ' then
        Del (String.sub line 2 (len - 2))
      else if len >= 2 && line.[0] = '+' && line.[1] = ' ' then
        Add (String.sub line 2 (len - 2))
      else if len >= 1 && line.[0] = ' ' then Ctx (String.sub line 1 (len - 1))
      else Ctx line)

(* The match side: context + removed lines (additions dropped). *)
let match_side body =
  classify_spatch body
  |> List.filter_map (function Ctx s | Del s -> Some s | Add _ -> None)
  |> String.concat "\n"

(* The replace side — the mirror with [-]/[+] swapped: context + added lines,
   removals dropped.

   Returns [None] only for a pure-context body (no [- ]/[+ ] lines) — a
   match-only guard, which produces no transform. A body with any [- ] or
   [+ ] line IS a transform; in particular a body with [- ] lines and no [+ ]
   is a *removal* (spatch convention): [- foo] alone replaces the matched span
   with the empty string. *)
let replace_side body : string option =
  let segs = classify_spatch body in
  if not (List.exists (function Del _ | Add _ -> true | Ctx _ -> false) segs)
  then None
  else
    Some
      (segs
      |> List.filter_map (function Ctx s | Add s -> Some s | Del _ -> None)
      |> String.concat "\n")

(* Intermediate representation for a parsed pattern.

   The IR is the matcher's internal vocabulary: parse_pattern + compile
   produce an [ir] tree, and a single recursive [eval_ir] function evaluates
   it. Multi-section, [on $VAR] scoping, and cross-section binding sharing
   all collapse into a handful of generic IR cases — the section-specific
   driver disappears.

   Constructors today:
   - [StrictSeq] / [PartialContainer]: leaf matchers that wrap the existing
     STMatch entry points. Each carries a pre-tokenized pattern body.
   - [Within (name, sub)]: evaluate [sub] in a cursor narrowed to the
     subtree bound by [name] in the accumulated bindings.
   - [All]: conjunction of sibling IRs sharing bindings left-to-right; the
     output is the cross-product of per-child matches that satisfy binding
     consistency.

   Tokenization happens during compile: the [pattern_token list] is
   embedded in each leaf, so eval_ir doesn't need to know about [ctx] or
   [language]. New IR constructors are additive and don't restructure the
   evaluator. *)
(* A replace template carried by a leaf IR node. [text] is the replacement
   body (the [+ ] lines, joined) with [$NAME] placeholders left literal;
   [singles] is the section's single metavar names, used to substitute
   those placeholders with bound source text. Sequence-metavar rendering
   ([foreach]/[join]) is not part of this leaf yet — that's later transform
   work; today only single-binding substitution is performed. *)
type replace_template = {
  text : string;
  singles : string list;
  sequences : string list;
}

(* A surgical edit hunk: a maximal run of [-]/[+] lines in a transform body,
   bounded by context lines (or the body edges). It localizes one edit within
   a match:

   - [del_idxs] are the indices (into a match's [spans] / the pattern token
     list) of the tokens on this hunk's [-] lines — the source bytes to delete
     or replace. Empty for a [+]-only hunk (a pure insertion).
   - [add] is the instantiated text of this hunk's [+] lines (with metavar
     placeholders), or [None] for a [-]-only hunk (a pure deletion).
   - [prev_tok]/[next_tok] are the indices of the context tokens immediately
     before/after the hunk, used to anchor a pure insertion (which has no
     [del_idxs] span of its own).

   A whole-construct rewrite ([- foo($x)] / [+ bar($x)] with no context) is the
   special case of a single hunk whose [del_idxs] cover every token — the edit
   then spans the whole match, i.e. the former whole-span behaviour. *)
type hunk = {
  del_idxs : int list;
  add : replace_template option;
  prev_tok : int option;
  next_tok : int option;
}

type ir =
  | StrictSeq of {
      tokens : Stmatch.pattern_token list;
      hunks : hunk list;
          (** Surgical edit hunks for this section's transform body. *)
      replace : replace_template option;
    }
  | PartialContainer of {
      tokens : Stmatch.pattern_token list;
      hunks : hunk list;
      replace : replace_template option;
    }
  | FieldContainer of {
      tokens : Stmatch.pattern_token list;
          (** Standalone tokenization, the fallback when source-context
              tokenization can't be established (see
              {!field_contextual_tokens}). *)
      hunks : hunk list;
      match_text : string;  (** The match-side body, for re-tokenization. *)
      singles : string list;
      sequences : string list;
      replace : replace_template option;
    }
  | Within of string * ir
  | All of ir list

(* A pattern token is an anonymous delimiter iff it is a [Concrete] leaf
   whose node-type equals its literal text — the tree-sitter convention for
   anonymous nodes (punctuation like [{], [<], [/], [>]). A named leaf
   ([identifier], [string], ...) has a grammar-rule node-type distinct from
   its text, and a wildcard is never a delimiter. *)
let is_anon_delim = function
  | Stmatch.Concrete { text; node_type } -> text = node_type
  | Stmatch.Subtree _ | Stmatch.Siblings _ -> false

(* A [match: partial] pattern must be a single bracketed container: the
   matcher peels the source node's leading/trailing anonymous runs and
   strict-matches the pattern's first/last tokens against them
   ({!Stmatch.match_partial_at}). A pattern whose outer form is a named
   wrapper — [foo({a: $x})], "a call whose object arg contains a" — begins
   with an [identifier] token that can never match a container delimiter, so
   it could only ever yield zero matches. Rather than fail silently, reject
   it at compile time and point at the multi-section idiom that does express
   it (a strict section binds the container with [on $VAR]; a partial section
   matches inside it). *)
let check_partial_is_container tokens =
  let ok =
    match (tokens, List.rev tokens) with
    | first :: _, last :: _ -> is_anon_delim first && is_anon_delim last
    | _ -> false
  in
  if not ok then
    failwith
      "match: partial requires the pattern to be a single bracketed container \
       (e.g. `{a: $x}` or `<Foo a={$x} />`); the given pattern's outer form is \
       not a delimited container. For \"a call/element whose argument contains \
       X\" use a multi-section pattern: a strict section binds the container \
       and scopes a `match: partial` section to it with `on $VAR`."

(* Tokenize [body]'s match side and group its tokens into surgical {!hunk}s.
   Returns the plain match tokens (parallel to the [spans] a match records) and
   the hunks.

   The match tokens are exactly {!match_side}'s — context + removal lines, in
   order — so token index aligns with the recorded [spans]. Each token is
   located back to its source line via {!Tokenize.tokenize_with_lines}; a
   maximal run of [-]/[+] body lines (between context lines) becomes one hunk.
   [single_metavars]/[sequence_metavars] tag the per-hunk [+] templates so
   placeholder substitution works on each hunk's added text. *)
let compile_hunks ~ctx ~language ~single_metavars ~sequence_metavars body =
  let segs = classify_spatch body in
  (* The context+removal lines, in order — line i here is line i of the
     reconstructed match text the tokenizer sees. *)
  let cd_segs =
    List.filter (function Ctx _ | Del _ -> true | Add _ -> false) segs
  in
  let match_text =
    cd_segs
    |> List.map (function Ctx s | Del s -> s | Add _ -> "")
    |> String.concat "\n"
  in
  let toks_lines =
    Tokenize.tokenize_with_lines ~ctx ~language ~single_metavars
      ~sequence_metavars match_text
  in
  let tokens = List.map fst toks_lines in
  (* token indices grouped by their context/removal-line index, in order. *)
  let n_cd = List.length cd_segs in
  let idxs_by_cd = Array.make (max 1 n_cd) [] in
  List.iteri
    (fun idx (_tok, line) ->
      if line >= 0 && line < n_cd then
        idxs_by_cd.(line) <- idx :: idxs_by_cd.(line))
    toks_lines;
  Array.iteri (fun i l -> idxs_by_cd.(i) <- List.rev l) idxs_by_cd;
  let mk_template add_lines =
    {
      text = add_lines |> List.rev |> String.concat "\n";
      singles = single_metavars;
      sequences = sequence_metavars;
    }
  in
  let hunks = ref [] in
  (* current open hunk: (del_idxs rev, add_lines rev, prev_tok). *)
  let cur = ref None in
  let cd = ref 0 in
  let prev_tok = ref None in
  let last_of = function
    | [] -> None
    | l -> Some (List.nth l (List.length l - 1))
  in
  let open_hunk () =
    match !cur with Some _ -> () | None -> cur := Some ([], [], !prev_tok)
  in
  let flush ~next_tok =
    match !cur with
    | None -> ()
    | Some (dels, adds, ptok) ->
        let add = if adds = [] then None else Some (mk_template adds) in
        hunks :=
          { del_idxs = List.rev dels; add; prev_tok = ptok; next_tok } :: !hunks;
        cur := None
  in
  List.iter
    (fun seg ->
      match seg with
      | Ctx _ ->
          let line_idxs = idxs_by_cd.(!cd) in
          (match line_idxs with
          | x :: _ -> flush ~next_tok:(Some x)
          | [] -> flush ~next_tok:None);
          (match last_of line_idxs with
          | Some _ as l -> prev_tok := l
          | None -> ());
          incr cd
      | Del _ ->
          open_hunk ();
          let line_idxs = idxs_by_cd.(!cd) in
          (match !cur with
          | Some (dels, adds, p) ->
              cur := Some (List.rev_append line_idxs dels, adds, p)
          | None -> ());
          incr cd
      | Add s -> (
          open_hunk ();
          match !cur with
          | Some (dels, adds, p) -> cur := Some (dels, s :: adds, p)
          | None -> ()))
    segs;
  flush ~next_tok:None;
  (tokens, List.rev !hunks)

let compile_section ~ctx ~language section =
  let tokens, hunks =
    compile_hunks ~ctx ~language ~single_metavars:section.single_metavars
      ~sequence_metavars:section.sequence_metavars section.body
  in
  if section.mode = Partial then check_partial_is_container tokens;
  let replace =
    match replace_side section.body with
    | None -> None
    | Some text ->
        Some
          {
            text;
            singles = section.single_metavars;
            sequences = section.sequence_metavars;
          }
  in
  let leaf =
    match section.mode with
    | Strict -> StrictSeq { tokens; hunks; replace }
    | Partial -> PartialContainer { tokens; hunks; replace }
    | Field ->
        FieldContainer
          {
            tokens;
            hunks;
            match_text = match_side section.body;
            singles = section.single_metavars;
            sequences = section.sequence_metavars;
            replace;
          }
  in
  match section.scope with
  | Global -> leaf
  | On name -> Within (name, leaf)
  | Foreach _ ->
      (* Foreach sections are transform directives, compiled separately via
         [compile_foreach]; they do not appear in the match IR. *)
      assert false

(* A per-element transform compiled from a [foreach $VAR] section. During a
   transform, for each element of the sequence bound to [seq_var] the
   element is matched against [elem_tokens]; on a match, [elem_replace] is
   instantiated with the per-element bindings to produce an in-place edit at
   that element's span. Elements that don't match are left untouched. *)
type foreach_transform = {
  seq_var : string;
  elem_tokens : Stmatch.pattern_token list;
  elem_replace : replace_template option;
}

(* Tokenize a foreach's element pattern *in the context of its container*,
   so context-sensitive leaves get the right node-types. The container comes
   from the match section that binds [seq_var]: we splice the element pattern
   into that section's body at the [seq_var] reference (e.g. [foo({ $PROPS })]
   becomes [foo({ color: $V })]), tokenize the whole thing, and extract the
   element's own leaves by the byte span where they were spliced. This is
   what makes a concrete object key parse as [property_identifier] rather
   than the standalone [statement_identifier].

   Returns [None] (caller falls back to standalone tokenization) when the
   context can't be established: no binding section found, the [seq_var]
   reference isn't textually located, ellipsis is involved (byte offsets
   would shift under ellipsis preprocessing), or extraction yields nothing.
   The fix is therefore a pure improvement — never worse than standalone. *)
let contextual_elem_tokens ~ctx ~language ~match_sections section seq_var =
  let has_ellipsis t = find_sub "..." t <> None in
  let element = match_side section.body in
  match
    List.find_opt (fun s -> List.mem seq_var s.sequence_metavars) match_sections
  with
  | None -> None
  | Some scaffold_section -> (
      let scaffold = match_side scaffold_section.body in
      if has_ellipsis scaffold || has_ellipsis element then None
      else
        match find_sub seq_var scaffold with
        | None -> None
        | Some pos ->
            let before = String.sub scaffold 0 pos in
            let after_pos = pos + String.length seq_var in
            let after =
              String.sub scaffold after_pos (String.length scaffold - after_pos)
            in
            let wrapped = before ^ element ^ after in
            let lo = String.length before in
            let hi = lo + String.length element in
            let toks =
              Tokenize.tokenize_span ~ctx ~language
                ~single_metavars:section.single_metavars
                ~sequence_metavars:section.sequence_metavars ~lo ~hi wrapped
            in
            if toks = [] then None else Some toks)

let compile_foreach ~ctx ~language ~match_sections section =
  let seq_var = match section.scope with Foreach n -> n | _ -> assert false in
  let elem_tokens =
    match
      contextual_elem_tokens ~ctx ~language ~match_sections section seq_var
    with
    | Some toks -> toks
    | None ->
        Tokenize.tokenize ~ctx ~language
          ~single_metavars:section.single_metavars
          ~sequence_metavars:section.sequence_metavars (match_side section.body)
  in
  let elem_replace =
    match replace_side section.body with
    | None -> None
    | Some text ->
        Some
          {
            text;
            singles = section.single_metavars;
            sequences = section.sequence_metavars;
          }
  in
  { seq_var; elem_tokens; elem_replace }

let is_foreach s = match s.scope with Foreach _ -> true | _ -> false

(* Compile a pattern into the match IR plus the list of per-element foreach
   transforms. Foreach sections are transform directives (they iterate a
   sequence bound by a match section), so they are partitioned out of the
   match IR. *)
(* The wildcard metavar names ([Subtree]/[Siblings]) present in a token list. *)
let wildcard_names tokens =
  List.filter_map
    (function
      | Stmatch.Subtree { name = Some n } | Stmatch.Siblings { name = Some n }
        ->
          Some n
      | _ -> None)
    tokens

(* The match-side tokens carried by every leaf of an [ir]. *)
let rec ir_tokens = function
  | StrictSeq { tokens; _ }
  | PartialContainer { tokens; _ }
  | FieldContainer { tokens; _ } ->
      tokens
  | Within (_, sub) -> ir_tokens sub
  | All children -> List.concat_map ir_tokens children

(* Reject a pattern that declares a metavar which never appears as a wildcard
   token in any section's match body. Such a metavar is almost always a
   mistake — a typo, or (the common case) a metavar swallowed when the
   surrounding syntax failed to parse, e.g. [data class n(p, q)] where [q] is
   not valid class-parameter syntax and is silently dropped from the tokens.
   Without this check the pattern just matches nothing with no explanation.

   The check is across all sections (an "appears in some section" rule), so a
   metavar shared between conjunctive sections — declared in one, used in
   another — is not falsely flagged. Error-tolerant fragments (e.g. [} else {])
   are unaffected: they declare no metavars. *)
let validate_declared_metavars_present (p : parsed_pattern) ~ir ~foreaches =
  let appeared =
    wildcard_names (ir_tokens ir)
    @ List.concat_map
        (fun (f : foreach_transform) -> wildcard_names f.elem_tokens)
        foreaches
  in
  List.iter
    (fun (s : section) ->
      List.iter
        (fun name ->
          if not (List.mem name appeared) then
            failwith
              (Printf.sprintf
                 "Metavar %s is declared but never appears in the pattern. \
                  Check for a typo, or whether the surrounding syntax parsed \
                  (a metavar in an invalid position is silently dropped). Run \
                  with --debug-tokens to see the tokenized pattern."
                 name))
        (s.single_metavars @ s.sequence_metavars))
    p.sections

(* Reject a *standalone* ellipsis on a [-] or [+] line — a line whose whole
   content is [...]. The ellipsis is a match-side-only construct (it matches
   zero or more sibling nodes); marking the ellipsis itself for removal is
   meaningless, and on the replace side it would be emitted as the literal text
   [...]. Surgical transforms keep [...]-captured source by leaving the ellipsis
   on a context line, so a bare ellipsis only ever belongs on context.

   An ellipsis *nested inside* a marked expression (e.g. [- $K: bar(...)],
   where [bar(...)] is replaced wholesale) is fine and not flagged — there the
   [-] applies to the expression, not to the ellipsis. *)
let validate_no_ellipsis_in_edits (p : parsed_pattern) =
  List.iteri
    (fun i (s : section) ->
      List.iter
        (function
          | (Del content | Add content) when String.trim content = "..." ->
              failwith
                (Printf.sprintf
                   "Section %d: a bare '...' (ellipsis) cannot be on a '-' or \
                    '+' line — it is a match-only construct. Leave it on a \
                    context line (the source it captures is preserved) and \
                    mark only the part you are changing."
                   (i + 1))
          | _ -> ())
        (classify_spatch s.body))
    p.sections

let compile_to_ir ~ctx ~language (p : parsed_pattern) :
    ir * foreach_transform list =
  validate_no_ellipsis_in_edits p;
  let foreach_sections, match_sections = List.partition is_foreach p.sections in
  let ir =
    match match_sections with
    | [] ->
        failwith "Pattern has no match section (a foreach needs one to bind)"
    | [ s ] -> compile_section ~ctx ~language s
    | ss -> All (List.map (compile_section ~ctx ~language) ss)
  in
  let foreaches =
    List.map (compile_foreach ~ctx ~language ~match_sections) foreach_sections
  in
  validate_declared_metavars_present p ~ir ~foreaches;
  (ir, foreaches)

(* Resolve a [Single] binding by name. Returns the bound cursor or [None]
   if the name isn't bound — the latter should be unreachable in practice
   because [validate_sections] catches dangling [on $VAR] references at
   parse time, but [eval_ir] still handles it cleanly with an empty
   result. *)
let lookup_single name bindings =
  List.find_map
    (function
      | M.Single { name = n; cursor } when n = name -> Some cursor | _ -> None)
    bindings

(* Tokenize a field pattern in the context of a specific source span: splice
   the pattern's [match_text] into the source where a candidate node sits,
   tokenize the whole, and keep the pattern's own leaves by byte span. The
   leaves then carry the node-types they have in that real grammatical
   position — e.g. a class method name as [property_identifier] rather than
   the standalone [identifier], which is what makes [getUsers() { $b }] match.

   The surrounding text is the actual source, so the splice lands in the true
   ancestral context; a parent subtree in isolation is not reliably parsable
   into the same roles (e.g. a class body's braces reparse as an object
   literal). Returns [None] — caller falls back to standalone tokens — when
   the pattern has an ellipsis ([tokenize_span] needs stable offsets) or
   extraction yields nothing. *)
let field_contextual_tokens ~ctx ~language ~source ~match_text ~singles
    ~sequences ~lo ~hi =
  if find_sub "..." match_text <> None then None
  else
    let before = String.sub source 0 lo in
    let after = String.sub source hi (String.length source - hi) in
    let wrapped = before ^ match_text ^ after in
    let toks =
      Tokenize.tokenize_span ~ctx ~language ~single_metavars:singles
        ~sequence_metavars:sequences ~lo
        ~hi:(lo + String.length match_text)
        wrapped
    in
    if toks = [] then None else Some toks

(* Does the pattern have any concrete token whose node-type could be
   context-sensitive? Only [Concrete] leaves whose node-type differs from
   their text (identifiers, literals — not punctuation, whose node-type
   equals its text) can be re-aliased by context. If there are none, source-
   context tokenization can't change anything, so field search uses the plain
   standalone path with zero reparses. *)
let pattern_needs_context tokens =
  List.exists
    (function
      | Stmatch.Concrete { text; node_type } -> node_type <> text | _ -> false)
    tokens

(* Per-candidate token provider for field-mode source-context tokenization,
   with two cost controls:

   - A relaxed-alignment {b probe}: [match_field_at ~ignore_node_type] with the
     standalone tokens is a cheap superset test ({precise matches} is a subset
     of {relaxed matches}), so if it fails the candidate can't match precisely
     either — skip it (return [[]]), doing no reparse. This limits reparses to
     genuine candidate node-types rather than every node-type in the tree.
   - Memoization by candidate node-type: the contextual node-types depend on
     the grammatical role, which the node-type captures, so we re-tokenize at
     most once per distinct candidate node-type. *)
let field_tokens_for ~ctx ~language ~source ~match_text ~singles ~sequences
    ~standalone =
  let memo : (string, Stmatch.pattern_token list) Hashtbl.t =
    Hashtbl.create 8
  in
  fun candidate ->
    match M.match_field_at ~ignore_node_type:true standalone candidate with
    | None -> [] (* not a plausible candidate; skip, no reparse *)
    | Some _ -> (
        let nt = Tree_sitter_cursor.node_type candidate in
        match Hashtbl.find_opt memo nt with
        | Some toks -> toks
        | None ->
            let lo, hi = Tree_sitter_cursor.byte_range candidate in
            let toks =
              match
                field_contextual_tokens ~ctx ~language ~source ~match_text
                  ~singles ~sequences ~lo ~hi
              with
              | Some t -> t
              | None -> standalone
            in
            Hashtbl.replace memo nt toks;
            toks)

(* Evaluate an IR tree against the source.

   Returns a list of [(matches, bindings)] tuples — one per valid composite.
   [matches] holds the per-leaf [match_result]s encountered during the
   traversal, in IR-order (which is the user's section-declaration order).
   [bindings] holds the accumulated bindings; subsequent sibling IRs see
   them via [initial_bindings], which is how cross-section non-linearity is
   enforced. *)
let rec eval_ir ?(overlapping = true) ~ctx ~language ~source ir cursor
    initial_bindings : (M.match_result list * M.binding list) list =
  match ir with
  | StrictSeq { tokens; _ } ->
      M.find_matches ~overlapping ~initial_bindings tokens cursor
      |> List.map (fun (m : M.match_result) -> ([ m ], m.bindings))
  | PartialContainer { tokens; _ } ->
      M.find_partial_matches ~initial_bindings tokens cursor
      |> List.map (fun (m : M.match_result) -> ([ m ], m.bindings))
  | FieldContainer { tokens; match_text; singles; sequences; _ } ->
      let results =
        if not (pattern_needs_context tokens) then
          (* No context-sensitive concrete token: standalone tokenization is
             already correct, so skip source-context (and its reparses)
             entirely. *)
          M.find_field_matches ~initial_bindings tokens cursor
        else
          (* Source-context tokenization: re-tokenize the field pattern in each
             candidate's surrounding source so context-sensitive node-types
             (e.g. a method name as [property_identifier]) line up. The probe in
             [field_tokens_for] confines reparses to genuine candidate nodes. *)
          let tokens_for =
            field_tokens_for ~ctx ~language ~source ~match_text ~singles
              ~sequences ~standalone:tokens
          in
          M.find_field_matches_with ~initial_bindings ~tokens_for cursor
      in
      results |> List.map (fun (m : M.match_result) -> ([ m ], m.bindings))
  | Within (name, sub) -> (
      match lookup_single name initial_bindings with
      | None -> []
      | Some bound ->
          (* [narrow] re-roots the bound cursor at its current node so the
             scoped walk can't climb out via [move_next_subtree]. *)
          eval_ir ~overlapping ~ctx ~language ~source sub
            (Tree_sitter_cursor.narrow bound)
            initial_bindings)
  | All children ->
      let extend acc ir_child =
        List.concat_map
          (fun (matches, bindings) ->
            eval_ir ~overlapping ~ctx ~language ~source ir_child cursor bindings
            |> List.map (fun (ms, bs) -> (matches @ ms, bs)))
          acc
      in
      List.fold_left extend [ ([], initial_bindings) ] children

let find_in_tree ~ctx ~language ~pattern_text (tree : Tree.src Tree.tree) =
  let p = parse_pattern pattern_text in
  (* Search ignores foreach sections — they are transform directives. *)
  let ir, _foreaches = compile_to_ir ~ctx ~language p in
  let root_cursor = Tree_sitter_cursor.of_tree tree in
  eval_ir ~ctx ~language ~source:tree.source ir root_cursor []
  |> List.map (fun (matches, _) -> ({ sections = matches } : composite_match))

(* Text-only matches of a {b single-section strict} pattern: the same search
   but comparing [Concrete] leaves on text alone, ignoring node-type. Backs the
   [search --explain] hint — when a strict search finds nothing, these are the
   locations whose tokens occur as text but in a different syntactic role (the
   reason strict rejected them, e.g. [React.FC] written as an expression vs.
   used as a type). Returns [[]] for anything but a single global strict
   section (partial/field/multi-section/[on]/[foreach]): the hint is scoped to
   the case where this context-sensitivity actually bites. *)
let text_only_find_in_tree ~ctx ~language ~pattern_text
    (tree : Tree.src Tree.tree) =
  let p = parse_pattern pattern_text in
  let ir, _foreaches = compile_to_ir ~ctx ~language p in
  match ir with
  | StrictSeq { tokens; _ } ->
      let cursor = Tree_sitter_cursor.of_tree tree in
      M.find_matches ~overlapping:true ~ignore_node_type:true tokens cursor
      |> List.map (fun (m : M.match_result) ->
          ({ sections = [ m ] } : composite_match))
  | _ -> []

let find ~ctx ~language ~pattern_text ~source_text =
  let tree = Tree.parse ~ctx ~language source_text in
  find_in_tree ~ctx ~language ~pattern_text tree

let find_file ~ctx ~language ~pattern_file ~source_text =
  let pattern_text =
    In_channel.with_open_text pattern_file In_channel.input_all
  in
  find ~ctx ~language ~pattern_text ~source_text

let mode_name = function
  | Strict -> "strict"
  | Partial -> "partial"
  | Field -> "field"

(* Static, source-independent warnings about a pattern.

   Partial/field transforms are surgical: a [-]/[+] on a *sub-part* edits only
   that part and preserves the tolerated extras / ignored fields. The one case
   that still drops them is marking the {b whole container} — a body with no
   context line, so every matched token is removed and the edit spans the whole
   match. That is the honest reading of "replace the whole thing", but an easy
   mistake in modes whose entire point is to tolerate/ignore content, so it
   warns (not errors). A section with context lines (surgical sub-part marking)
   or a [foreach] scope is exempt. *)
let pattern_warnings pattern_text =
  let p = parse_pattern pattern_text in
  List.filter_map
    (fun (s : section) ->
      let is_transform = replace_side s.body <> None in
      let is_foreach = match s.scope with Foreach _ -> true | _ -> false in
      (* Whole-container marking: no context line carries content, so nothing
         in the match is preserved. *)
      let marks_whole =
        not
          (List.exists
             (function Ctx s -> String.trim s <> "" | _ -> false)
             (classify_spatch s.body))
      in
      if
        is_transform && (not is_foreach) && marks_whole
        && (s.mode = Partial || s.mode = Field)
      then
        let what, dropped =
          match s.mode with
          | Partial -> ("container", "extra elements it doesn't list")
          | _ ->
              ( "declaration",
                "ignored fields (decorators, return types, modifiers)" )
        in
        Some
          (Printf.sprintf
             "warning: this %s-mode section marks the whole matched %s for \
              replacement, so %s will be dropped. To change part of it while \
              keeping the rest, mark only that part (context lines are \
              preserved) or use a `foreach`/`on` section."
             (mode_name s.mode) what dropped)
      else None)
    p.sections

let token_str = function
  | Stmatch.Concrete { text; node_type } ->
      Printf.sprintf "Concrete %S : %s" text node_type
  | Stmatch.Subtree { name } ->
      Printf.sprintf "Subtree  %s" (Option.value name ~default:"<anon>")
  | Stmatch.Siblings { name } ->
      Printf.sprintf "Siblings %s" (Option.value name ~default:"<anon>")

let debug_tokens ~ctx ~language ~pattern_text =
  let p = parse_pattern pattern_text in
  let buf = Buffer.create 256 in
  List.iteri
    (fun i (s : section) ->
      let scope_note =
        match s.scope with
        | Global -> ""
        | On n -> Printf.sprintf ", on %s" n
        | Foreach n -> Printf.sprintf ", foreach %s" n
      in
      Buffer.add_string buf
        (Printf.sprintf "Section %d (match: %s%s):\n" (i + 1) (mode_name s.mode)
           scope_note);
      let toks =
        Tokenize.tokenize ~ctx ~language ~single_metavars:s.single_metavars
          ~sequence_metavars:s.sequence_metavars (match_side s.body)
      in
      if toks = [] then Buffer.add_string buf "  (no tokens)\n"
      else
        List.iter
          (fun t -> Buffer.add_string buf ("  " ^ token_str t ^ "\n"))
          toks;
      (* Flag declared metavars that produced no token — the same condition
         the parse-time validation rejects, shown here for diagnosis. *)
      let present = wildcard_names toks in
      let declared = s.single_metavars @ s.sequence_metavars in
      if declared <> [] then begin
        Buffer.add_string buf "  metavars:";
        List.iter
          (fun n ->
            Buffer.add_string buf
              (Printf.sprintf " %s%s" n
                 (if List.mem n present then "" else " (ABSENT!)")))
          declared;
        Buffer.add_char buf '\n'
      end)
    p.sections;
  Buffer.contents buf

(* ── Transforms (the [+] side) ──────────────────────────────────────── *)

(* A single rewrite: replace [start_byte, end_byte) in the source with
   [replacement]. *)
type edit = { start_byte : int; end_byte : int; replacement : string }

(* Order metavar names longest-first (deduped) so a greedy per-position
   scan matches [$xy] before [$x] and never mistakes one for a prefix of
   the other. *)
let names_longest_first names =
  names |> List.sort_uniq compare
  |> List.sort (fun a b -> compare (String.length b) (String.length a))

(* Substitute metavar placeholders in [rt.text]. Single metavars
   ([rt.singles]) are replaced with the source text of their bindings;
   sequence metavars ([rt.sequences]) are replaced with their pre-rendered
   string from [seq_renderings] (the [foreach]-rendered, joined elements —
   see {!render_foreach}). The scan is greedy longest-match per position so
   [$x] does not corrupt [$xy]. A placeholder with no binding / rendering is
   left literal. *)
(* An identifier character for the purpose of placeholder-boundary detection.
   [$] is deliberately excluded so a sigil-free metavar [x] still substitutes
   inside a string template like ["v=$x"] (the [$] is a boundary), while a
   metavar [a] is NOT substituted inside a literal identifier like [fallback]
   (the surrounding letters are boundaries). *)
let is_ident_char c =
  (c >= 'A' && c <= 'Z')
  || (c >= 'a' && c <= 'z')
  || (c >= '0' && c <= '9')
  || c = '_'

let instantiate_template rt seq_renderings bindings source =
  let names = names_longest_first (rt.singles @ rt.sequences) in
  let buf = Buffer.create (String.length rt.text) in
  let n = String.length rt.text in
  let i = ref 0 in
  while !i < n do
    let matched =
      List.find_opt
        (fun name ->
          let ln = String.length name in
          ln > 0
          && !i + ln <= n
          && String.sub rt.text !i ln = name
          (* Whole-token only: a metavar name must not be a substring of a
             larger identifier in the template (e.g. [a] inside [fallback]).
             Critical for sigil-free metavars; harmless for sigil-bearing
             ones (the [$]/[(] boundaries already satisfy this). *)
          && (!i = 0 || not (is_ident_char rt.text.[!i - 1]))
          && (!i + ln >= n || not (is_ident_char rt.text.[!i + ln])))
        names
    in
    match matched with
    | Some name ->
        (if List.mem name rt.singles then
           match lookup_single name bindings with
           | Some cursor ->
               let s, e = Tree_sitter_cursor.byte_range cursor in
               Buffer.add_string buf (String.sub source s (e - s))
           | None -> Buffer.add_string buf name
         else
           match List.assoc_opt name seq_renderings with
           | Some rendered -> Buffer.add_string buf rendered
           | None -> Buffer.add_string buf name);
        i := !i + String.length name
    | None ->
        Buffer.add_char buf rt.text.[!i];
        incr i
  done;
  Buffer.contents buf

(* The leaf IRs in document order. Their order matches a composite's
   [sections] list, since [eval_ir] appends one match per leaf in IR order
   ([Within] passes through, [All] concatenates). *)
let rec ir_leaves ir =
  match ir with
  | StrictSeq _ | PartialContainer _ | FieldContainer _ -> [ ir ]
  | Within (_, sub) -> ir_leaves sub
  | All children -> List.concat_map ir_leaves children

let leaf_replace = function
  | StrictSeq { replace; _ }
  | PartialContainer { replace; _ }
  | FieldContainer { replace; _ } ->
      replace
  | _ -> None

let leaf_hunks = function
  | StrictSeq { hunks; _ }
  | PartialContainer { hunks; _ }
  | FieldContainer { hunks; _ } ->
      hunks
  | _ -> []

(* The surgical edits one leaf match contributes: one localized edit per hunk,
   leaving context (and partial/field's tolerated extras, and [...]-captured
   source) untouched. Requires the per-token [spans] the match recorded; the
   caller only takes this path when [m.spans] is populated.

   - A hunk with concrete deleted token spans: replace [first.start, last.end)
     with its [+] text (empty [+] = deletion).
   - A hunk whose [-] tokens have no concrete span (e.g. a removed [Siblings]
     whose span lives in the binding): delete from the preceding context
     token's end to the following context token's start.
   - A [+]-only hunk (pure insertion): insert at the preceding context token's
     end, falling back to the following context token's start. *)
(* Whole-line cleanup: when the bytes from the previous newline to [lo] are
   only indentation and [hi] sits at end-of-line, absorb the leading
   indentation and the trailing newline so the line vanishes cleanly instead of
   leaving a blank, mis-indented stub. Otherwise keep the tight span. *)
let line_cleanup source lo hi =
  let len = String.length source in
  let is_hspace c = c = ' ' || c = '\t' in
  let j = ref lo in
  while !j > 0 && is_hspace source.[!j - 1] do
    decr j
  done;
  let leading_blank = !j = 0 || source.[!j - 1] = '\n' in
  let k = ref hi in
  if !k < len && source.[!k] = '\r' then incr k;
  let trailing_nl = !k < len && source.[!k] = '\n' in
  if leading_blank && trailing_nl then (!j, !k + 1) else (lo, hi)

(* Cleanup for a deleted list element (partial/field): an adjacent element
   separator ([,] / [;]) would otherwise dangle ([{ , size }]). Absorb the
   *following* separator and the whitespace after it; failing that the
   *preceding* separator and the whitespace before it — the same rule
   {!removal_span} uses for [foreach] removal, here driven off the raw source.
   Falls back to {!line_cleanup} when there is no separator on either side.

   This is only applied in container (partial/field) contexts. In strict
   contexts a [;] is a statement terminator, not a list separator, so absorbing
   it would eat the previous statement's terminator — strict deletions use
   {!line_cleanup} alone. *)
let element_cleanup source lo hi =
  let len = String.length source in
  let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let is_sep c = c = ',' || c = ';' in
  let f = ref hi in
  while !f < len && is_ws source.[!f] do
    incr f
  done;
  if !f < len && is_sep source.[!f] then begin
    let e = ref (!f + 1) in
    while !e < len && is_ws source.[!e] do
      incr e
    done;
    (lo, !e)
  end
  else begin
    let p = ref lo in
    while !p > 0 && is_ws source.[!p - 1] do
      decr p
    done;
    if !p > 0 && is_sep source.[!p - 1] then begin
      let s = ref (!p - 1) in
      while !s > 0 && is_ws source.[!s - 1] do
        decr s
      done;
      (!s, hi)
    end
    else
      (* No separator (e.g. whitespace-separated JSX attributes). A whole-line
         element is handled by line_cleanup; otherwise absorb the following
         horizontal-whitespace gap so adjacent elements close up to one space
         rather than leaving a double gap. *)
      let lo', hi' = line_cleanup source lo hi in
      if (lo', hi') <> (lo, hi) then (lo', hi')
      else begin
        let e = ref hi in
        while !e < len && (source.[!e] = ' ' || source.[!e] = '\t') do
          incr e
        done;
        (lo, !e)
      end
  end

let surgical_edits ~list_context hunks (m : M.match_result) seq_renderings
    source =
  let delete_cleanup = if list_context then element_cleanup else line_cleanup in
  let spans = m.spans in
  let span_of i =
    if i >= 0 && i < Array.length spans then spans.(i) else (-1, -1)
  in
  let valid (s, _) = s >= 0 in
  List.filter_map
    (fun h ->
      let repl =
        match h.add with
        | Some rt -> instantiate_template rt seq_renderings m.bindings source
        | None -> ""
      in
      let del_spans =
        List.filter_map
          (fun i ->
            let sp = span_of i in
            if valid sp then Some sp else None)
          h.del_idxs
      in
      match del_spans with
      | _ :: _ ->
          let lo = List.fold_left (fun a (s, _) -> min a s) max_int del_spans in
          let hi = List.fold_left (fun a (_, e) -> max a e) min_int del_spans in
          (* A deleted boundary token with no recorded span — a partial-mode
             container delimiter ([{]/[}], [<Foo]/[/>]), which never records a
             span — sits at the match edge. Extend the deletion to the match
             boundary so a whole-container [-]/[+] covers the delimiters too,
             rather than deleting only the inner elements and leaving the
             brackets behind. (A sub-part edit leaves the delimiters as context,
             so token 0 / the last token are not in [del_idxs] and this is a
             no-op.) *)
          let ntok = Array.length spans in
          let lo =
            if List.mem 0 h.del_idxs && not (valid (span_of 0)) then
              min lo m.start_byte
            else lo
          in
          let hi =
            if
              List.mem (ntok - 1) h.del_idxs && not (valid (span_of (ntok - 1)))
            then max hi m.end_byte
            else hi
          in
          let lo, hi =
            if repl = "" then delete_cleanup source lo hi else (lo, hi)
          in
          Some { start_byte = lo; end_byte = hi; replacement = repl }
      | [] when h.del_idxs <> [] ->
          (* removed tokens, but none carried a concrete span — delete the
             gap between the surrounding context tokens. *)
          let lo =
            match h.prev_tok with
            | Some i when valid (span_of i) -> snd (span_of i)
            | _ -> m.start_byte
          in
          let hi =
            match h.next_tok with
            | Some i when valid (span_of i) -> fst (span_of i)
            | _ -> m.end_byte
          in
          let lo, hi =
            if repl = "" then delete_cleanup source lo hi else (lo, hi)
          in
          if hi >= lo then
            Some { start_byte = lo; end_byte = hi; replacement = repl }
          else None
      | [] -> (
          (* pure insertion: anchor at preceding context end, else following
             context start. Nothing to insert -> no edit. *)
          match h.add with
          | None -> None
          | Some _ ->
              let pos =
                match h.prev_tok with
                | Some i when valid (span_of i) -> snd (span_of i)
                | _ -> (
                    match h.next_tok with
                    | Some i when valid (span_of i) -> fst (span_of i)
                    | _ -> m.start_byte)
              in
              Some { start_byte = pos; end_byte = pos; replacement = repl }))
    hunks

(* The edits a composite contributes. For a match that recorded per-token
   [spans] (the strict transform path), edits are {b surgical}: each hunk's
   [-]/[+] region is edited and everything else — context, partial/field's
   tolerated extras, [...]-captured source — is left byte-for-byte. For a match
   without spans (partial/field, which don't yet record them), fall back to the
   former {b whole-span} replacement: the leaf's full replace template
   overwrites the entire matched span. [seq_renderings] supplies rendered text
   for any spliced sequence metavars. *)
let edits_of_composite ir (composite : composite_match) seq_renderings source =
  List.map2
    (fun leaf (m : M.match_result) ->
      if Array.length m.spans > 0 then
        (* Only partial matches a list of elements separated by [,]/[;], where a
           deletion must absorb the dangling separator. Field matches a
           declaration's positional children (not a separated list) and strict a
           positional construct; a [;] there is a statement/declaration
           terminator, not a list separator, so their deletions are
           line-oriented (see {!surgical_edits}). *)
        let list_context =
          match leaf with PartialContainer _ -> true | _ -> false
        in
        surgical_edits ~list_context (leaf_hunks leaf) m seq_renderings source
      else
        match leaf_replace leaf with
        | None -> []
        | Some rt ->
            let replacement =
              instantiate_template rt seq_renderings m.bindings source
            in
            [
              { start_byte = m.start_byte; end_byte = m.end_byte; replacement };
            ])
    (ir_leaves ir) composite.sections
  |> List.concat

(* Apply edits to [source]. Identical edits are de-duplicated first —
   multi-section transforms can produce the same edit from several
   composites (e.g. a section-1 match shared across several section-2
   matches). Remaining edits must be non-overlapping: a nested or
   conflicting pair (one span inside another, or two spans claiming
   overlapping bytes with different replacements) is rejected rather than
   silently corrupting the output. Nested transforms — an outer rewrite
   whose replacement should incorporate an inner one — are a later feature.
   Edits are then spliced in a single forward pass: the source is copied
   into a buffer, alternating unchanged slices with replacements. *)
let apply_edits source edits =
  (* [sort_uniq compare] orders ascending by start_byte (the record's first
     field) and drops exact duplicates. *)
  let edits = List.sort_uniq compare edits in
  let rec check_overlap = function
    | a :: (b :: _ as rest) ->
        if b.start_byte < a.end_byte then
          failwith
            (Printf.sprintf
               "transform: overlapping edits at [%d,%d) and [%d,%d) — nested \
                or conflicting rewrites are not supported"
               a.start_byte a.end_byte b.start_byte b.end_byte)
        else check_overlap rest
    | _ -> ()
  in
  check_overlap edits;
  (* Edits are sorted ascending and non-overlapping, so [pos] (the next
     unconsumed source byte) only moves forward. For each edit, copy the
     untouched slice up to its start, then its replacement; finally the tail. *)
  let buf = Buffer.create (String.length source) in
  let pos =
    List.fold_left
      (fun pos e ->
        Buffer.add_substring buf source pos (e.start_byte - pos);
        Buffer.add_string buf e.replacement;
        e.end_byte)
      0 edits
  in
  Buffer.add_substring buf source pos (String.length source - pos);
  Buffer.contents buf

(* Find the cursors a [Sequence] binding holds, by name. *)
let lookup_sequence name bindings =
  List.find_map
    (function
      | M.Sequence { name = n; cursors } when n = name -> Some cursors
      | _ -> None)
    bindings

(* All bindings carried by a composite (across its section matches). May
   contain duplicates from accumulated bindings; lookups take the first. *)
let composite_bindings (c : composite_match) =
  List.concat_map (fun (m : M.match_result) -> m.M.bindings) c.sections

(* The byte span to delete when removing element [arr.(i)] from a list. The
   element's own span isn't enough — leaving its separator behind yields a
   parse error (`{ , x }`). Rule: also take the separator that *follows* the
   element; if it has none (it's the last element with no trailing comma),
   take the *preceding* one. A trailing comma counts as a following
   separator, so removing the only/last element of a trailing-comma list
   (`f(a,)`) cleans it up. Whitespace on the far side of the consumed
   separator is absorbed so the surviving neighbours close up. [arr] is the
   full sequence binding — named elements interleaved with the separator
   leaves — so an adjacent separator is just a non-named neighbour. *)
let removal_span arr i source =
  let is_ws c = c = ' ' || c = '\t' || c = '\n' || c = '\r' in
  let n = Array.length arr in
  let es, ee = Tree_sitter_cursor.byte_range arr.(i) in
  let is_sep j = j >= 0 && j < n && not (Tree_sitter_cursor.is_named arr.(j)) in
  if is_sep (i + 1) then begin
    let _, sep_end = Tree_sitter_cursor.byte_range arr.(i + 1) in
    let hi = ref sep_end in
    while !hi < String.length source && is_ws source.[!hi] do
      incr hi
    done;
    (es, !hi)
  end
  else if is_sep (i - 1) then begin
    let sep_start, _ = Tree_sitter_cursor.byte_range arr.(i - 1) in
    let lo = ref sep_start in
    while !lo > 0 && is_ws source.[!lo - 1] do
      decr lo
    done;
    (!lo, ee)
  end
  else (es, ee)

(* The edits a foreach transform contributes for one composite: match each
   named element against the per-element pattern, and on a match emit an
   edit. A non-empty replacement rewrites the element in place; an empty
   replacement is a *removal* — the edit span extends to swallow an adjacent
   separator (see {!removal_span}) so the list stays well-formed. Non-named
   cursors (separator leaves that landed in the binding) are skipped as
   match candidates but used to locate separators. *)
let foreach_edits ft (composite : composite_match) source =
  match lookup_sequence ft.seq_var (composite_bindings composite) with
  | None -> []
  | Some cursors ->
      let arr = Array.of_list cursors in
      let edits = ref [] in
      Array.iteri
        (fun i elem ->
          if Tree_sitter_cursor.is_named elem then
            let scoped = Tree_sitter_cursor.narrow elem in
            match M.match_at ft.elem_tokens scoped with
            | None -> ()
            | Some (_, elem_bindings) -> (
                match ft.elem_replace with
                | None -> ()
                | Some rt ->
                    let replacement =
                      instantiate_template rt [] elem_bindings source
                    in
                    let edit =
                      if replacement = "" then
                        let lo, hi = removal_span arr i source in
                        { start_byte = lo; end_byte = hi; replacement = "" }
                      else
                        let s, e = Tree_sitter_cursor.byte_range elem in
                        { start_byte = s; end_byte = e; replacement }
                    in
                    edits := edit :: !edits))
        arr;
      List.rev !edits

(* Render a foreach's sequence for splicing into an outer template: match
   each bound element against the per-element pattern, instantiate the
   per-element template for the matches, and join with [sep]. Elements that
   don't match (including separator leaves in the binding) are dropped, so
   the rendering is exactly the matched elements. *)
let render_foreach ft bindings source sep =
  match lookup_sequence ft.seq_var bindings with
  | None -> ""
  | Some cursors ->
      cursors
      |> List.filter Tree_sitter_cursor.is_named
      |> List.filter_map (fun elem ->
          let scoped = Tree_sitter_cursor.narrow elem in
          match M.match_at ft.elem_tokens scoped with
          | None -> None
          | Some (_, elem_bindings) ->
              Option.map
                (fun rt -> instantiate_template rt [] elem_bindings source)
                ft.elem_replace)
      |> String.concat sep

(* Render a referenced sequence that has no foreach (identity splice): the
   bound elements' source text, joined with [sep]. Separator leaves in the
   binding are dropped via [is_named], so only the named elements render —
   e.g. [a, b, c] from an array binding that also holds the commas. *)
let render_identity seq bindings source sep =
  match lookup_sequence seq bindings with
  | None -> ""
  | Some cursors ->
      cursors
      |> List.filter Tree_sitter_cursor.is_named
      |> List.map (fun c ->
          let s, e = Tree_sitter_cursor.byte_range c in
          String.sub source s (e - s))
      |> String.concat sep

(* The metavar names (from [names]) that occur as placeholders in [text],
   found with the same greedy longest-match scan as instantiation. *)
let referenced_names names text =
  let names = names_longest_first names in
  let found = ref [] in
  let n = String.length text in
  let i = ref 0 in
  while !i < n do
    match
      List.find_opt
        (fun name ->
          let ln = String.length name in
          ln > 0
          && !i + ln <= n
          && String.sub text !i ln = name
          (* Whole-token only — same boundary rule as {!instantiate_template},
             so a sequence metavar isn't spuriously "referenced" because its
             name is a substring of a literal identifier. *)
          && (!i = 0 || not (is_ident_char text.[!i - 1]))
          && (!i + ln >= n || not (is_ident_char text.[!i + ln])))
        names
    with
    | Some name ->
        if not (List.mem name !found) then found := name :: !found;
        i := !i + String.length name
    | None -> incr i
  done;
  !found

(* Everything [transform] does up to (but not including) applying the
   edits: parse, match, and compute the edit list. Shared by {!transform}
   and {!transform_edits}. *)
let compute_edits ~ctx ~language ~pattern_text ~source_text =
  let tree = Tree.parse ~ctx ~language source_text in
  let p = parse_pattern pattern_text in
  let ir, foreaches = compile_to_ir ~ctx ~language p in
  let cursor = Tree_sitter_cursor.of_tree tree in
  (* Transforms use non-overlapping matching: each source span is rewritten
     once, greedily left-to-right. (Search uses overlapping; see [find].) *)
  let composites =
    eval_ir ~overlapping:false ~ctx ~language ~source:source_text ir cursor []
  in
  (* A sequence is *spliced* when its name is referenced in some outer
     template; its rendered elements are substituted there and the whole
     construct is replaced. A foreach whose sequence is NOT referenced does
     in-place per-element edits (the map case). Splice rendering uses the
     [join] separator (default empty), and renders via the sequence's
     foreach if it has one, or as the elements' source text otherwise
     (identity splice). All of this is static across composites. *)
  let joins_map = List.concat_map (fun s -> s.joins) p.sections in
  let sep_of seq = Option.value ~default:"" (List.assoc_opt seq joins_map) in
  let foreach_of seq = List.find_opt (fun ft -> ft.seq_var = seq) foreaches in
  let outer_templates = List.filter_map leaf_replace (ir_leaves ir) in
  let all_metavar_names =
    List.concat_map (fun rt -> rt.singles @ rt.sequences) outer_templates
  in
  let all_seq_names =
    List.concat_map (fun rt -> rt.sequences) outer_templates
    @ List.map (fun ft -> ft.seq_var) foreaches
    |> List.sort_uniq compare
  in
  let referenced_seqs =
    List.concat_map
      (fun rt -> referenced_names all_metavar_names rt.text)
      outer_templates
    |> List.sort_uniq compare
    |> List.filter (fun n -> List.mem n all_seq_names)
  in
  let edits =
    List.concat_map
      (fun (matches, _) ->
        let comp = { sections = matches } in
        let binds = composite_bindings comp in
        let seq_renderings =
          List.map
            (fun seq ->
              let sep = sep_of seq in
              let rendered =
                match foreach_of seq with
                | Some ft -> render_foreach ft binds source_text sep
                | None -> render_identity seq binds source_text sep
              in
              (seq, rendered))
            referenced_seqs
        in
        let outer = edits_of_composite ir comp seq_renderings source_text in
        let inplace =
          List.concat_map
            (fun ft ->
              if List.mem ft.seq_var referenced_seqs then []
              else foreach_edits ft comp source_text)
            foreaches
        in
        outer @ inplace)
      composites
  in
  edits

let transform ~ctx ~language ~pattern_text ~source_text =
  apply_edits source_text
    (compute_edits ~ctx ~language ~pattern_text ~source_text)

let transform_edits ~ctx ~language ~pattern_text ~source_text =
  List.sort_uniq compare
    (compute_edits ~ctx ~language ~pattern_text ~source_text)
