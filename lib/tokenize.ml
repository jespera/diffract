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

(* True if the "..." starting at [i] is alone on its line: nothing but
   spaces/tabs between the preceding newline (or start of text) and [i],
   and nothing but spaces/tabs/CR between [i+3] and the next newline (or
   end of text). *)
let is_own_line_at text i =
  let len = String.length text in
  let rec back j =
    if j < 0 then true
    else
      match text.[j] with
      | ' ' | '\t' -> back (j - 1)
      | '\n' -> true
      | _ -> false
  in
  let rec fwd j =
    if j >= len then true
    else
      match text.[j] with
      | ' ' | '\t' | '\r' -> fwd (j + 1)
      | '\n' -> true
      | _ -> false
  in
  back (i - 1) && fwd (i + 3)

(* Rewrite each ellipsis "..." so the body parses, using one of two
   strategies:

   - An ellipsis *alone on its line* is blanked out (three spaces —
     byte-count preserving, so all leaf offsets and line indices stay
     stable) and its position is recorded in [positional]. The body then
     parses without any artificial token at that spot, which matters in
     grammar-restricted positions: an identifier placeholder is illegal
     between Kotlin imports (only the package header may precede them),
     and degrades the parse so the neighbouring leaves get the wrong
     node types. The walker later injects the [Siblings] wildcard at the
     recorded byte offset.

   - An *inline* ellipsis (e.g. [eval(... x,]) is replaced by a unique
     placeholder identifier ([__ellipsis_N__]), which must occupy a slot
     in its list position for the body to parse ([f(a, , b)] would not).
     The placeholder leaf is mapped back to a [Siblings] wildcard during
     classification via [mapping].

   Spread/rest operators ([...args], [...$rest]) are left as literal
   "...". Returns [(rewritten_body, mapping, positional)] with
   [positional] sorted by byte offset (ascending). *)
let preprocess_ellipsis text =
  let len = String.length text in
  let out = Buffer.create len in
  let mapping = ref [] in
  let positional = ref [] in
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
        let name = Printf.sprintf "..._%d" n in
        if is_own_line_at text !i then begin
          positional := (Buffer.length out, name) :: !positional;
          Buffer.add_string out "   "
        end
        else begin
          let placeholder = Printf.sprintf "__ellipsis_%d__" n in
          mapping := (placeholder, name) :: !mapping;
          Buffer.add_string out placeholder
        end;
        i := !i + 3
      end
    else begin
      Buffer.add_char out text.[!i];
      incr i
    end
  done;
  (Buffer.contents out, !mapping, List.rev !positional)

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
let walk_leaves ~single_metavars ~sequence_metavars ~ellipsis_map
    ?(positional = []) ~keep source root =
  let tokens = ref [] in
  (* Own-line ellipses, to be spliced in at their byte offsets (they have
     no leaf in the parse — their line was blanked). [positional] is
     sorted ascending; emit every pending one whose offset precedes the
     next kept leaf. *)
  let pending = ref positional in
  let flush_before b =
    let rec go () =
      match !pending with
      | (off, name) :: rest when off <= b ->
          tokens := Stmatch.Siblings { name = Some name } :: !tokens;
          pending := rest;
          go ()
      | _ -> ()
    in
    go ()
  in
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
          if keep node && node.start_byte <> node.end_byte then begin
            flush_before node.start_byte;
            tokens :=
              classify_leaf ~single_metavars ~sequence_metavars ~ellipsis_map
                source node
              :: !tokens
          end
      | children ->
          List.iter (fun (c : Tree.pat Tree.child) -> walk c.node) children
  in
  walk root;
  flush_before max_int;
  List.rev !tokens

(* Like [walk_leaves] but pairs each token with the 0-based line index of its
   leaf (count of newlines in [source] before its start byte). Ellipsis
   preprocessing preserves newlines, so this index is the same as in the
   pre-preprocessing body — which lets callers map a token to the pattern line
   it came from (and thus its `-`/context role). *)
let walk_leaves_with_lines ~single_metavars ~sequence_metavars ~ellipsis_map
    ?(positional = []) ~keep source root =
  let line_of off =
    let n = ref 0 in
    for i = 0 to min off (String.length source) - 1 do
      if source.[i] = '\n' then incr n
    done;
    !n
  in
  let tokens = ref [] in
  let pending = ref positional in
  let flush_before b =
    let rec go () =
      match !pending with
      | (off, name) :: rest when off <= b ->
          tokens :=
            (Stmatch.Siblings { name = Some name }, line_of off) :: !tokens;
          pending := rest;
          go ()
      | _ -> ()
    in
    go ()
  in
  let rec walk (node : Tree.pat Tree.t) =
    if node.is_extra && not (Tree.is_error node) then ()
    else
      match node.children with
      | [] ->
          if keep node && node.start_byte <> node.end_byte then begin
            flush_before node.start_byte;
            let tok =
              classify_leaf ~single_metavars ~sequence_metavars ~ellipsis_map
                source node
            in
            tokens := (tok, line_of node.start_byte) :: !tokens
          end
      | children ->
          List.iter (fun (c : Tree.pat Tree.child) -> walk c.node) children
  in
  walk root;
  flush_before max_int;
  List.rev !tokens

let tokenize_with_lines ~ctx ~language ~single_metavars ~sequence_metavars body
    : (Stmatch.pattern_token * int) list =
  let body, ellipsis_map, positional = preprocess_ellipsis body in
  let tree = Tree.parse_as_pattern ~ctx ~language body in
  walk_leaves_with_lines ~single_metavars ~sequence_metavars ~ellipsis_map
    ~positional
    ~keep:(fun _ -> true)
    tree.Tree.source tree.Tree.root

let tokenize ~ctx ~language ~single_metavars ~sequence_metavars body :
    Stmatch.pattern_token list =
  (* Rewrite ellipsis (placeholders or blanked lines) before parsing. *)
  let body, ellipsis_map, positional = preprocess_ellipsis body in
  (* Parse the pattern body with the source language's tree-sitter parser.
     We use the parse only as a lexer: the hierarchical structure is
     discarded; we keep the leaves in document order. ERROR nodes in the
     pattern parse are harmless because we never consult node hierarchy on
     the pattern side. *)
  let tree = Tree.parse_as_pattern ~ctx ~language body in
  walk_leaves ~single_metavars ~sequence_metavars ~ellipsis_map ~positional
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
