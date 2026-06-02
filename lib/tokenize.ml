(** See [tokenize.mli]. *)

(* True if the "..." at position [i] in [text] is a spread/rest operator
   rather than an ellipsis pattern — i.e. immediately followed by an
   identifier-ish character (so [...rest] / [...$args] are preserved,
   while a standalone [...] is treated as ellipsis). *)
let is_spread_at text i =
  let len = String.length text in
  if i + 3 < len then
    match text.[i + 3] with
    | '$' | '_' -> true
    | c ->
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
  else false

(* Replace each ellipsis "..." with a unique placeholder identifier
   ([__ellipsis_N__]) so the body parses; standalone identifiers parse in
   the positions ellipsis can appear (argument, statement, ...). Returns
   the rewritten body and a mapping from placeholder text to the synthetic
   ellipsis name ([..._N]). Spread/rest operators are left as literal
   "...". *)
let preprocess_ellipsis text =
  let len = String.length text in
  let out = Buffer.create len in
  let mapping = ref [] in
  let counter = ref 0 in
  let i = ref 0 in
  while !i < len do
    if
      !i + 2 < len
      && text.[!i] = '.'
      && text.[!i + 1] = '.'
      && text.[!i + 2] = '.'
    then
      if is_spread_at text !i then begin
        Buffer.add_string out "...";
        i := !i + 3
      end
      else begin
        let n = !counter in
        incr counter;
        let placeholder = Printf.sprintf "__ellipsis_%d__" n in
        let name = Printf.sprintf "..._%d" n in
        mapping := (placeholder, name) :: !mapping;
        Buffer.add_string out placeholder;
        i := !i + 3
      end
    else begin
      Buffer.add_char out text.[!i];
      incr i
    end
  done;
  (Buffer.contents out, !mapping)

(* Classify one leaf into a pattern token. Metavar detection is sigil-free:
   a leaf is a metavar iff its text exactly equals a declared metavar name
   (single or sequence). No `$` prefix is assumed; the preamble is the sole
   authority. Whole-leaf comparison avoids any substring/boundary issue (a
   metavar named [obj] does not match inside [object] — different leaves). *)
let classify_leaf ~single_metavars ~sequence_metavars ~ellipsis_map source
    (node : Tree.pat Tree.t) : Stmatch.pattern_token =
  let text = Tree.text source node in
  if List.mem text single_metavars then Stmatch.Subtree { name = Some text }
  else if List.mem text sequence_metavars then
    Stmatch.Siblings { name = Some text }
  else
    match List.assoc_opt text ellipsis_map with
    | Some name -> Stmatch.Siblings { name = Some name }
    | None -> Stmatch.Concrete { text; node_type = node.node_type }

(* Walk a parsed pattern tree's leaves in document order, classifying each.
   [keep] decides whether a leaf contributes a token (used to restrict to a
   byte range). Extras (comments / whitespace) are skipped, but ERROR nodes
   are descended into (tree-sitter's quirk of flagging unparseable
   top-level content as extra) so bare-keyword/punctuation fragments like
   ["} else {"] still contribute their leaves. *)
let walk_leaves ~single_metavars ~sequence_metavars ~ellipsis_map ~keep source
    root =
  let tokens = ref [] in
  let rec walk (node : Tree.pat Tree.t) =
    if node.is_extra && not (Tree.is_error node) then ()
    else
      match node.children with
      | [] ->
          (* Skip zero-width "missing" leaves. Tree-sitter inserts these
             during error recovery — most visibly a missing statement
             terminator when a complete-statement pattern is written without
             it (a PHP [foo($x)] pattern parses as an [expression_statement]
             with an inserted, zero-width [;]). Such a node has empty text and
             would otherwise emit a phantom [Concrete ""] token that no real
             source leaf can match, silently breaking the pattern. *)
          if keep node && node.start_byte <> node.end_byte then
            tokens :=
              classify_leaf ~single_metavars ~sequence_metavars ~ellipsis_map
                source node
              :: !tokens
      | children ->
          List.iter (fun (c : Tree.pat Tree.child) -> walk c.node) children
  in
  walk root;
  List.rev !tokens

(* Like [walk_leaves] but pairs each token with the 0-based line index of its
   leaf (count of newlines in [source] before its start byte). Ellipsis
   preprocessing preserves newlines, so this index is the same as in the
   pre-preprocessing body — which lets callers map a token to the pattern line
   it came from (and thus its `-`/context role). *)
let walk_leaves_with_lines ~single_metavars ~sequence_metavars ~ellipsis_map
    ~keep source root =
  let line_of off =
    let n = ref 0 in
    for i = 0 to off - 1 do
      if source.[i] = '\n' then incr n
    done;
    !n
  in
  let tokens = ref [] in
  let rec walk (node : Tree.pat Tree.t) =
    if node.is_extra && not (Tree.is_error node) then ()
    else
      match node.children with
      | [] ->
          if keep node && node.start_byte <> node.end_byte then
            let tok =
              classify_leaf ~single_metavars ~sequence_metavars ~ellipsis_map
                source node
            in
            tokens := (tok, line_of node.start_byte) :: !tokens
      | children ->
          List.iter (fun (c : Tree.pat Tree.child) -> walk c.node) children
  in
  walk root;
  List.rev !tokens

let tokenize_with_lines ~ctx ~language ~single_metavars ~sequence_metavars body
    : (Stmatch.pattern_token * int) list =
  let body, ellipsis_map = preprocess_ellipsis body in
  let tree = Tree.parse_as_pattern ~ctx ~language body in
  walk_leaves_with_lines ~single_metavars ~sequence_metavars ~ellipsis_map
    ~keep:(fun _ -> true)
    tree.Tree.source tree.Tree.root

let tokenize ~ctx ~language ~single_metavars ~sequence_metavars body :
    Stmatch.pattern_token list =
  (* Rewrite ellipsis to placeholders before parsing. *)
  let body, ellipsis_map = preprocess_ellipsis body in
  (* Parse the pattern body with the source language's tree-sitter parser.
     We use the parse only as a lexer: the hierarchical structure is
     discarded; we keep the leaves in document order. ERROR nodes in the
     pattern parse are harmless because we never consult node hierarchy on
     the pattern side. *)
  let tree = Tree.parse_as_pattern ~ctx ~language body in
  walk_leaves ~single_metavars ~sequence_metavars ~ellipsis_map
    ~keep:(fun _ -> true)
    tree.Tree.source tree.Tree.root

let tokenize_span ~ctx ~language ~single_metavars ~sequence_metavars ~lo ~hi
    text : Stmatch.pattern_token list =
  (* Tokenize [text] but keep only the leaves whose start byte falls in
     [lo, hi). No ellipsis preprocessing is done — callers use this only on
     ellipsis-free text, so byte offsets are stable (preprocessing would
     shift them). Intended for contextual tokenization: a fragment is
     substituted into a larger scaffold so its leaves get the right
     node-types, then the fragment's own leaves are extracted by span. *)
  let tree = Tree.parse_as_pattern ~ctx ~language text in
  walk_leaves ~single_metavars ~sequence_metavars ~ellipsis_map:[]
    ~keep:(fun (n : Tree.pat Tree.t) -> n.start_byte >= lo && n.start_byte < hi)
    tree.Tree.source tree.Tree.root
