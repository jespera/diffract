(** Per-language grammar metadata derived from each tree-sitter grammar's
    [node-types.json] and [grammar.json]. *)

(* ===================================================================== *)
(* List-shape wrappers — from node-types.json                            *)
(* ===================================================================== *)

(** [parse_wrapper_set json_str] parses a [node-types.json] document and returns
    a hashtable whose keys are the node-type names of list-shape wrappers in
    that grammar.

    Criterion: [named: true] and [fields] is missing or empty. The runtime peel
    logic in [Tree.unwrap_root] adds the necessary structural guards
    (single-named-child, byte-range equality), so a slightly loose superset is
    fine here. *)
let parse_wrapper_set (json_str : string) : (string, unit) Hashtbl.t =
  let open Yojson.Safe.Util in
  let entries =
    try Yojson.Safe.from_string json_str |> to_list with _ -> []
  in
  let set = Hashtbl.create 64 in
  List.iter
    (fun entry ->
      let named = match member "named" entry with `Bool b -> b | _ -> false in
      let has_fields =
        match member "fields" entry with `Assoc fs -> fs <> [] | _ -> false
      in
      let type_name =
        match member "type" entry with `String s -> Some s | _ -> None
      in
      match type_name with
      | Some t when named && not has_fields -> Hashtbl.replace set t ()
      | _ -> ())
    entries;
  set

let wrapper_cache : (string, (string, unit) Hashtbl.t) Hashtbl.t =
  Hashtbl.create 8

let load_wrapper_set (language : string) : (string, unit) Hashtbl.t =
  match Hashtbl.find_opt wrapper_cache language with
  | Some set -> set
  | None ->
      let set =
        match Embedded_metadata.by_language language with
        | None -> Hashtbl.create 0
        | Some json -> parse_wrapper_set json
      in
      Hashtbl.replace wrapper_cache language set;
      set

let is_list_shape_wrapper ~language ~node_type =
  Hashtbl.mem (load_wrapper_set language) node_type

let list_shape_wrappers ~language =
  let set = load_wrapper_set language in
  Hashtbl.fold (fun k () acc -> k :: acc) set [] |> List.sort String.compare

(* ===================================================================== *)
(* DEL definition — from grammar.json                                    *)
(* ===================================================================== *)

type string_def = { opener : string; closer : string; escape : char option }

type del_definition = {
  bracket_pairs : (string * string) list;
  string_defs : string_def list;
  line_comments : string list;
  block_comments : (string * string) list;
}

let empty_del_definition =
  {
    bracket_pairs = [];
    string_defs = [];
    line_comments = [];
    block_comments = [];
  }

(** Tree-sitter grammar.json wraps content in transparent nodes (TOKEN,
    IMMEDIATE_TOKEN, FIELD, ALIAS, PREC and variants). Walk through them to find
    the inner content. *)
let rec unwrap_node (json : Yojson.Safe.t) : Yojson.Safe.t =
  match json with
  | `Assoc fields -> (
      match List.assoc_opt "type" fields with
      | Some (`String t)
        when t = "TOKEN" || t = "IMMEDIATE_TOKEN" || t = "FIELD" || t = "ALIAS"
             || t = "PREC" || t = "PREC_LEFT" || t = "PREC_RIGHT"
             || t = "PREC_DYNAMIC" -> (
          match List.assoc_opt "content" fields with
          | Some inner -> unwrap_node inner
          | None -> json)
      | _ -> json)
  | _ -> json

(** Extract a STRING literal from a node, transparently unwrapping wrapper
    types. Returns [None] for anything else. *)
let string_value (node : Yojson.Safe.t) : string option =
  match unwrap_node node with
  | `Assoc fields -> (
      match List.assoc_opt "type" fields with
      | Some (`String "STRING") -> (
          match List.assoc_opt "value" fields with
          | Some (`String v) -> Some v
          | _ -> None)
      | _ -> None)
  | _ -> None

(** Walk a JSON tree, calling [f] on every Assoc node. *)
let rec walk_json (f : Yojson.Safe.t -> unit) (json : Yojson.Safe.t) : unit =
  match json with
  | `Assoc fields ->
      f json;
      List.iter (fun (_, v) -> walk_json f v) fields
  | `List xs -> List.iter (walk_json f) xs
  | _ -> ()

let is_alnum c =
  (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9')

let str_exists f s =
  let len = String.length s in
  let rec aux i =
    if i >= len then false else if f s.[i] then true else aux (i + 1)
  in
  aux 0

(** Heuristic: a string-literal or comment delimiter is short, non-alphanumeric,
    and not a structural bracket. Filters out keyword false positives like
    Java's [requires] / [provides] SEQ patterns. *)
let looks_like_delimiter s =
  let len = String.length s in
  len >= 1 && len <= 4
  && (not (str_exists is_alnum s))
  && not (List.mem s [ "("; ")"; "["; "]"; "{"; "}"; "<"; ">"; ","; ";" ])

(** [members_of_seq node] returns the SEQ's members list, or empty if [node]
    isn't a SEQ. Strips wrapper types first. *)
let members_of_seq (node : Yojson.Safe.t) : Yojson.Safe.t list =
  match unwrap_node node with
  | `Assoc fields -> (
      match (List.assoc_opt "type" fields, List.assoc_opt "members" fields) with
      | Some (`String "SEQ"), Some (`List ms) -> ms
      | _ -> [])
  | _ -> []

(** [choice_alternatives node] returns the CHOICE's members list, or [[node]] if
    [node] isn't a CHOICE. Lets callers handle openers that may be wrapped in a
    CHOICE (PHP's line comment is [CHOICE(STRING("//"), PATTERN("#..."))]). *)
let choice_alternatives (node : Yojson.Safe.t) : Yojson.Safe.t list =
  match unwrap_node node with
  | `Assoc fields -> (
      match (List.assoc_opt "type" fields, List.assoc_opt "members" fields) with
      | Some (`String "CHOICE"), Some (`List ms) -> ms
      | _ -> [ node ])
  | _ -> [ node ]

(** [is_repeat_like node] is [true] for REPEAT/REPEAT1 nodes (transparent
    wrappers stripped). Used to require some content between a SEQ's first and
    last STRING for it to look like a string-literal rule. *)
let is_repeat_like (node : Yojson.Safe.t) : bool =
  match unwrap_node node with
  | `Assoc fields -> (
      match List.assoc_opt "type" fields with
      | Some (`String t) -> t = "REPEAT" || t = "REPEAT1"
      | _ -> false)
  | _ -> false

(* ----- Bracket pairs ----- *)

let universal_brackets = [ ("(", ")"); ("[", "]"); ("{", "}") ]

let extract_bracket_pairs (grammar : Yojson.Safe.t) : (string * string) list =
  let candidates = [ ("(", ")"); ("[", "]"); ("{", "}"); ("<", ">") ] in
  let found = Hashtbl.create 4 in
  let visit node =
    match members_of_seq node with
    | [] -> ()
    | members when List.length members >= 2 -> (
        let first = string_value (List.hd members) in
        let last = string_value (List.nth members (List.length members - 1)) in
        match (first, last) with
        | Some o, Some c ->
            List.iter
              (fun (op, cl) ->
                if o = op && c = cl then Hashtbl.replace found (op, cl) ())
              candidates
        | _ -> ())
    | _ -> ()
  in
  walk_json visit grammar;
  List.filter
    (fun pair -> Hashtbl.mem found pair || List.mem pair universal_brackets)
    candidates

(* ----- Comment markers ----- *)

(** [extras_rule_names grammar] returns the names of rules referenced by the
    grammar's [extras] field via SYMBOL nodes. *)
let extras_rule_names (grammar : Yojson.Safe.t) : string list =
  match grammar with
  | `Assoc fields -> (
      match List.assoc_opt "extras" fields with
      | Some (`List xs) ->
          List.filter_map
            (fun e ->
              match e with
              | `Assoc fs -> (
                  match
                    (List.assoc_opt "type" fs, List.assoc_opt "name" fs)
                  with
                  | Some (`String "SYMBOL"), Some (`String n) -> Some n
                  | _ -> None)
              | _ -> None)
            xs
      | _ -> [])
  | _ -> []

(** [find_rule grammar name] returns the rule body for a top-level rule by name,
    or [None] if absent. *)
let find_rule (grammar : Yojson.Safe.t) (name : string) : Yojson.Safe.t option =
  match grammar with
  | `Assoc fields -> (
      match List.assoc_opt "rules" fields with
      | Some (`Assoc rules) -> List.assoc_opt name rules
      | _ -> None)
  | _ -> None

let extract_comment_markers (grammar : Yojson.Safe.t) :
    string list * (string * string) list =
  let line = ref [] in
  let block = ref [] in
  let visit_for_comments node =
    let members = members_of_seq node in
    if members = [] then ()
    else
      let first_member = List.hd members in
      (* The opener may be wrapped in a CHOICE — descend into all
         alternatives. *)
      let openers =
        choice_alternatives first_member |> List.filter_map string_value
      in
      let last_member = List.nth members (List.length members - 1) in
      let closer = string_value last_member in
      List.iter
        (fun first ->
          if not (looks_like_delimiter first) then ()
          else
            match closer with
            | Some last
              when List.length members >= 3 && looks_like_delimiter last ->
                (* Many C-style grammars structure block comments as
                   seq('/*', regex_with_trailing_stars, '/'). The
                   apparent closer is then '/' but the effective
                   closer is '*/'. Detect this and rewrite. *)
                if first = "/*" && last = "/" then
                  block := ("/*", "*/") :: !block
                else block := (first, last) :: !block
            | _ -> line := first :: !line)
        openers
  in
  List.iter
    (fun name ->
      if String.length name >= 7 && String.sub name 0 7 = "comment" then ()
        (* fall through to walk: only descend if name contains "comment" *)
      else if not (str_exists (fun c -> c = 'C' || c = 'c') name) then ()
      else ();
      let lname = String.lowercase_ascii name in
      let contains_comment =
        let n = String.length lname in
        let rec aux i =
          if i + 7 > n then false
          else if String.sub lname i 7 = "comment" then true
          else aux (i + 1)
        in
        aux 0
      in
      if contains_comment then
        match find_rule grammar name with
        | Some rule -> walk_json visit_for_comments rule
        | None -> ())
    (extras_rule_names grammar);
  let dedup xs = List.sort_uniq compare xs in
  (dedup !line, dedup !block)

(* ----- String defs ----- *)

let extract_string_defs (grammar : Yojson.Safe.t)
    ~(comment_markers : (string * string) list) : string_def list =
  let comment_set =
    List.fold_left
      (fun acc pair ->
        Hashtbl.add acc pair ();
        acc)
      (Hashtbl.create 4) comment_markers
  in
  let seen = Hashtbl.create 8 in
  let result = ref [] in
  let visit node =
    match members_of_seq node with
    | members when List.length members >= 2 -> (
        let first = string_value (List.hd members) in
        let last = string_value (List.nth members (List.length members - 1)) in
        match (first, last) with
        | Some o, Some c when looks_like_delimiter o && looks_like_delimiter c
          ->
            (* Skip if this looks like a comment block we've already
               classified, or starts with [/*] (assume block comment
               regardless of the apparent closer). *)
            if Hashtbl.mem comment_set (o, c) then ()
            else if o = "/*" then ()
            else
              (* Require at least one repeat-like content between the
                 two delimiters — actual string literals always have
                 that shape. *)
              let middle =
                List.filteri
                  (fun i _ -> i > 0 && i < List.length members - 1)
                  members
              in
              let has_repeat = List.exists is_repeat_like middle in
              if has_repeat && not (Hashtbl.mem seen (o, c)) then begin
                Hashtbl.add seen (o, c) ();
                result :=
                  { opener = o; closer = c; escape = Some '\\' } :: !result
              end
        | _ -> ())
    | _ -> ()
  in
  walk_json visit grammar;
  List.rev !result

(* ----- Per-language extensions ----- *)

type extensions = {
  extra_strings : string_def list;
  extra_line_comments : string list;
  extra_block_comments : (string * string) list;
}
(** Per-language additions to the auto-derived DEL definition.

    Languages whose string handling is in tree-sitter's external scanner
    (Kotlin's multi-dollar strings, PHP's heredocs, Scala's interpolated
    strings) leave the [string_defs] field empty when auto-derived. Per-language
    extension declarations fill the gap with the simple boundary pairs we care
    about for the DEL lexer.

    Limited scope: we declare the basic "open delimiter, scan to close
    delimiter, with optional escape" cases. More complex constructs (heredoc tag
    matching, prefix-based string interpolation, count-aware multi-dollar
    strings) are deliberately left out at this layer; they'd need specialised
    lexer code rather than declarative data. *)

let no_extensions =
  { extra_strings = []; extra_line_comments = []; extra_block_comments = [] }

let extensions_for_language = function
  | "kotlin" ->
      {
        extra_strings =
          (* Tree-sitter-kotlin's string_literal goes through the
             external scanner. We declare the boundary pairs the
             DEL lexer needs to recognise.

             Kotlin 2.0+ supports multi-dollar string templates:
             [$"..."], [$$"..."], [$$$"..."], etc. The dollar prefix
             only affects interpolation behaviour inside the string;
             the outer boundary is still [$N"] / ["] (or [$N"""] /
             ["""] for triple-quoted). Since we treat string interiors
             as opaque, each variant is just a fresh opener/closer
             pair.

             Variants are listed longest-prefix-first so a
             longest-match lexer correctly recognises [$$"text"] as a
             two-dollar string rather than [$] followed by [$"text"].
             Capped at 4 dollars; trivial to extend if real Kotlin
             code uses more. *)
          [
            (* Triple-quoted variants, longest first. *)
            { opener = "$$$$\"\"\""; closer = "\"\"\""; escape = None };
            { opener = "$$$\"\"\""; closer = "\"\"\""; escape = None };
            { opener = "$$\"\"\""; closer = "\"\"\""; escape = None };
            { opener = "$\"\"\""; closer = "\"\"\""; escape = None };
            { opener = "\"\"\""; closer = "\"\"\""; escape = None };
            (* Single-quote variants, longest first. *)
            { opener = "$$$$\""; closer = "\""; escape = Some '\\' };
            { opener = "$$$\""; closer = "\""; escape = Some '\\' };
            { opener = "$$\""; closer = "\""; escape = Some '\\' };
            { opener = "$\""; closer = "\""; escape = Some '\\' };
            { opener = "\""; closer = "\""; escape = Some '\\' };
          ];
        extra_line_comments = [];
        extra_block_comments = [ ("/*", "*/") ];
        (* Kotlin's multiline_comment is also external. *)
      }
  | "php" ->
      {
        extra_strings =
          [
            (* PHP's encapsed_string_chars (with $-interpolation) is
               external; we declare basic single- and double-quoted
               boundaries. Heredocs and nowdocs (<<<TAG ... TAG)
               need tag-tracking; covered by future specialised
               lexer code if needed. *)
            { opener = "\""; closer = "\""; escape = Some '\\' };
            { opener = "'"; closer = "'"; escape = Some '\\' };
          ];
        extra_line_comments =
          [
            "#";
            (* PHP also accepts # for line comments. The grammar
               expresses this as a regex pattern (#[^?...]) rather
               than a STRING, so it doesn't survive auto-derivation;
               declare explicitly. *)
          ];
        extra_block_comments = [];
      }
  | "scala" ->
      {
        extra_strings =
          [
            (* Scala's _simple_string and interpolated string variants
               are external. We declare basic and multiline string
               boundaries; prefix-based interpolated strings (s"...",
               f"...", raw"...") are out of scope at this layer. *)
            { opener = "\""; closer = "\""; escape = Some '\\' };
            { opener = "\"\"\""; closer = "\"\"\""; escape = None };
          ];
        extra_line_comments = [];
        extra_block_comments = [];
      }
  | _ -> no_extensions

(* ----- Public API: del_definition ----- *)

let del_cache : (string, del_definition) Hashtbl.t = Hashtbl.create 8

let derive_del_definition (json_str : string) : del_definition =
  match Yojson.Safe.from_string json_str with
  | exception _ -> empty_del_definition
  | grammar ->
      let bracket_pairs = extract_bracket_pairs grammar in
      let line_comments, block_comments = extract_comment_markers grammar in
      let string_defs =
        extract_string_defs grammar ~comment_markers:block_comments
      in
      { bracket_pairs; string_defs; line_comments; block_comments }

let apply_extensions (def : del_definition) (ext : extensions) : del_definition
    =
  let merge_unique acc xs =
    List.fold_left
      (fun acc x -> if List.mem x acc then acc else acc @ [ x ])
      acc xs
  in
  {
    bracket_pairs = def.bracket_pairs;
    string_defs = def.string_defs @ ext.extra_strings;
    line_comments = merge_unique def.line_comments ext.extra_line_comments;
    block_comments = merge_unique def.block_comments ext.extra_block_comments;
  }

let del_definition ~language =
  match Hashtbl.find_opt del_cache language with
  | Some def -> def
  | None ->
      let auto =
        match Embedded_grammar_json.by_language language with
        | None -> empty_del_definition
        | Some json -> derive_del_definition json
      in
      let def = apply_extensions auto (extensions_for_language language) in
      Hashtbl.replace del_cache language def;
      def

(* ===================================================================== *)
(* General accessors                                                     *)
(* ===================================================================== *)

let all_languages () = [ "typescript"; "tsx"; "kotlin"; "php"; "scala" ]
