(** See [stmatch.mli]. Iterative with explicit checkpoint stack and mutable
    cursor; the [Concrete] branch compares both [text] and [node_type] (see §2.1
    of [docs/universal-tokenizer.md]).

    Bindings are threaded through alongside the cursor: snapshotted on every
    checkpoint push, restored on every checkpoint pop. Non-linearity (the same
    name appearing twice in a pattern) is enforced via {!Cursor.S.subtree_equal}
    — for [Subtree] wildcards a single subtree must match, for [Siblings]
    wildcards each sibling must match the corresponding one in the existing
    binding. See §3.10 of the design doc.

    The shared inner primitive is [drive]: it runs the leaf-walking loop and
    distinguishes pattern-exhaustion (loop completed normally) from
    source-exhaustion (could not advance to consume the next token) from
    mismatch (content disagreement). [match_at] and [match_prefix] are thin
    views on [drive] selecting different success conditions. *)

type pattern_token =
  | Concrete of { text : string; node_type : string }
  | Subtree of { name : string option }
  | Siblings of { name : string option }

module Make (C : Cursor.S) = struct
  type binding =
    | Single of { name : string; cursor : C.t }
    | Sequence of { name : string; cursors : C.t list }

  (* Internal: see comments on the algorithm. *)

  type checkpoint_kind = Cp_subtree | Cp_siblings

  type checkpoint = {
    kind : checkpoint_kind;
    name : string option;
    saved_start : int;
    saved_first : bool;
    saved_cursor : C.t;
    saved_bindings : binding list;
  }

  (* Find the Single binding with the given name, if any. Returns the cursor
     positioned at the previously-bound subtree. *)
  let find_single_binding bindings name =
    List.find_map
      (function Single b when b.name = name -> Some b.cursor | _ -> None)
      bindings

  (* Try to record a Single binding. Returns [Ok new_bindings] if the name is
     new, or if an existing binding has the same name and structurally-equal
     subtree (non-linear match). Returns [Error] if a binding exists but
     differs structurally. *)
  let try_add_single bindings name cursor =
    match find_single_binding bindings name with
    | Some existing ->
        if C.subtree_equal cursor existing then Ok bindings else Error ()
    | None -> Ok (Single { name; cursor } :: bindings)

  (* Look up an existing Sequence binding's cursors, if any. *)
  let find_sequence_binding bindings name =
    List.find_map
      (function Sequence b when b.name = name -> Some b.cursors | _ -> None)
      bindings

  (* Initialize an empty Sequence binding. Caller ensures no existing
     Sequence binding with this name (see the non-linear path). *)
  let add_empty_sequence bindings name =
    Sequence { name; cursors = [] } :: bindings

  (* Append a cursor to the Sequence binding with the given name. The binding
     must exist (we always add an empty Sequence before appending). *)
  let append_to_sequence bindings name cursor =
    List.map
      (function
        | Sequence b when b.name = name ->
            Sequence { name; cursors = b.cursors @ [ cursor ] }
        | other -> other)
      bindings

  (* Driving mode selects what counts as success / failure and whether the
     driver returns the first match or enumerates all of them.
     - [Drive_match_at]: single match; source exhaustion is a failure
       (triggers backtracking).
     - [Drive_match_at_all]: enumerate every match; source exhaustion is a
       failure. After each [Pattern_done] the driver captures the current
       cursor + bindings and triggers backtracking to explore alternative
       [Siblings] counts and [Subtree] descent levels. Returns
       [Drive_enumerated] with the accumulated list.
     - [Drive_match_prefix]: single match; source exhaustion is a success
       (reports how much pattern was left). *)
  type drive_mode = Drive_match_at | Drive_match_at_all | Drive_match_prefix

  type drive_result =
    | Drive_pattern_exhausted of C.t * binding list
        (** Pattern was fully consumed. Source may still have content. *)
    | Drive_source_exhausted of pattern_token list * C.t * binding list
        (** Could not advance the cursor at pattern index [i]; the returned list
            is [pattern[i..n-1]]. Only produced when
            [mode = Drive_match_prefix]. *)
    | Drive_failed  (** All backtracking exhausted; no match path succeeded. *)
    | Drive_enumerated of (C.t * binding list) list
        (** All matches found by [Drive_match_at_all], in discovery order (which
            is leftmost-minimum first, then alternatives via backtracking). Each
            entry's cursor is a cloned snapshot taken at the moment its match
            completed, so subsequent enumeration does not mutate it. *)

  (* The shared driver. Runs the leaf-walking loop, distinguishing the two
     kinds of failure (mismatch vs source-exhausted), and returns a
     [drive_result] selected by [mode].

     [initial_bindings] seeds the binding state from prior matches. When
     non-empty, named wildcards encountered during this run that share a
     name with a seeded binding must bind to a structurally-equal subtree
     (via [subtree_equal]). This is how cross-element non-linearity is
     enforced in partial-mode matching. The returned bindings include the
     seed plus any new bindings recorded during this run, in pattern
     order (seed bindings come first since they originated from earlier
     pattern positions). *)
  let drive ~mode ?(initial_bindings = []) ?(ignore_node_type = false)
      ?(descend = false) ?(spans : (int * int) array option = None)
      (pattern : pattern_token list) (initial_cursor : C.t) : drive_result =
    let pattern = Array.of_list pattern in
    let n = Array.length pattern in
    (* [spans], when supplied, records the source byte range each token
       consumed (index = token position). Filled during [forward_pass]; the
       committed path's values survive backtracking because a re-run only
       overwrites from the restored checkpoint onward with the same committed
       leaves. Surgical transforms read this to locate `-` regions. *)
    let record i sp =
      match spans with Some a when i < Array.length a -> a.(i) <- sp | _ -> ()
    in
    let checkpoints : checkpoint list ref = ref [] in
    let cursor = ref initial_cursor in
    let first = ref true in
    let start = ref 0 in
    (* Bindings are stored in reverse order internally (new bindings
       prepended; final output is List.rev). Seed with the reversed
       initial bindings so the output ends up in natural order: seed
       first, then new additions. *)
    let bindings : binding list ref = ref (List.rev initial_bindings) in
    (* Two internal signals raised by [forward_pass]:
       - [Mismatch]: content disagreement; always triggers backtracking.
       - [Source_exhausted_at i]: could not advance to position [i]; treated
         per [mode]. *)
    let exception Mismatch in
    let exception Source_exhausted_at of int in
    let remaining_from i =
      let rec collect j acc =
        if j < i then acc else collect (j - 1) (pattern.(j) :: acc)
      in
      collect (n - 1) []
    in
    let forward_pass () =
      for i = !start to n - 1 do
        let token = pattern.(i) in
        (* Pre-advance for [Subtree] and [Concrete]. [Siblings] is
           special-cased below. *)
        (match token with
        | Subtree _ | Concrete _ ->
            let can =
              if !first then begin
                first := false;
                true
              end
              else C.move_next_subtree !cursor
            in
            if not can then raise (Source_exhausted_at i)
        | Siblings _ -> ());
        match token with
        | Subtree { name } ->
            let snapshot = !bindings in
            let cur_clone = C.clone !cursor in
            record i (C.byte_range cur_clone);
            (match name with
            | None -> ()
            | Some n -> (
                match try_add_single !bindings n cur_clone with
                | Ok b -> bindings := b
                | Error () -> raise Mismatch));
            checkpoints :=
              {
                kind = Cp_subtree;
                name;
                saved_start = i;
                saved_first = false;
                saved_cursor = cur_clone;
                saved_bindings = snapshot;
              }
              :: !checkpoints
        | Siblings { name } -> (
            (* Non-linear path: if a Sequence binding with this name already
               exists, we must absorb exactly the same items deterministically.
               No checkpoints pushed; failure raises Mismatch (or
               Source_exhausted_at on advance failure). *)
            let existing_seq =
              match name with
              | Some n -> find_sequence_binding !bindings n
              | None -> None
            in
            match existing_seq with
            | Some expected ->
                let saved_first = !first in
                let consumed = ref 0 in
                List.iter
                  (fun exp_cursor ->
                    let advanced =
                      if !consumed = 0 then
                        if saved_first then begin
                          first := false;
                          true
                        end
                        else C.move_next_subtree !cursor
                      else C.move_next_sibling !cursor
                    in
                    if not advanced then raise (Source_exhausted_at i);
                    if not (C.subtree_equal !cursor exp_cursor) then
                      raise Mismatch;
                    incr consumed)
                  expected
            | None ->
                let snapshot = !bindings in
                (match name with
                | None -> ()
                | Some n -> bindings := add_empty_sequence !bindings n);
                let next = C.clone !cursor in
                let can = !first || C.move_next_subtree next in
                if can then begin
                  checkpoints :=
                    {
                      kind = Cp_subtree;
                      name;
                      saved_start = i;
                      saved_first = false;
                      saved_cursor = C.clone next;
                      saved_bindings = snapshot;
                    }
                    :: !checkpoints;
                  checkpoints :=
                    {
                      kind = Cp_siblings;
                      name;
                      saved_start = i;
                      saved_first = true;
                      saved_cursor = next;
                      saved_bindings = !bindings;
                    }
                    :: !checkpoints
                end)
        | Concrete t ->
            let leaf = C.move_first_leaf !cursor in
            if
              C.leaf_text leaf <> t.text
              || ((not ignore_node_type) && C.leaf_node_type leaf <> t.node_type)
            then raise Mismatch
            else record i (C.byte_range !cursor)
      done
    in
    (* Accumulator for Drive_match_at_all. Stays empty in the other modes. *)
    let enumerated : (C.t * binding list) list ref = ref [] in
    (* Deep-clone a binding's cursor(s). Captured results (enumerated, and the
       best prefix below) must not alias checkpoint-saved cursors, which later
       backtracking mutates via [move_first_child] / [move_next_sibling]. *)
    let clone_binding = function
      | Single { name; cursor } -> Single { name; cursor = C.clone cursor }
      | Sequence { name; cursors } ->
          Sequence { name; cursors = List.map C.clone cursors }
    in
    (* Best prefix match found so far in [Drive_match_prefix] mode: the
       source-exhaustion that consumed the MOST pattern (largest token index
       [i], i.e. shortest remaining suffix). We keep exploring via backtracking
       rather than returning at the first source-exhaustion, so a leading
       [Subtree] can descend into a child and consume more of the element
       (e.g. [$K: $V] matching one [pair]: [$K] descends to the key instead of
       greedily taking the whole pair). [Pattern_done] short-circuits — it is
       the best possible. Cursor and bindings are deep-cloned at record time. *)
    let best_prefix : (int * C.t * binding list) option ref = ref None in
    let record_prefix i =
      let better =
        match !best_prefix with Some (bi, _, _) -> i > bi | None -> true
      in
      if better then
        best_prefix :=
          Some (i, C.clone !cursor, List.rev_map clone_binding !bindings)
    in
    let rec check_loop () =
      let outcome =
        try
          forward_pass ();
          `Pattern_done
        with
        | Mismatch -> `Mismatch
        | Source_exhausted_at i -> `Source_at i
      in
      match outcome with
      | `Pattern_done -> (
          match mode with
          | Drive_match_at ->
              Drive_pattern_exhausted (!cursor, List.rev !bindings)
          | Drive_match_prefix ->
              (* In [descend] mode a clean prefix endpoint requires the source
                 subtree to be exhausted here too. If source remains (the
                 pattern consumed only part of this child via a descent), this
                 is NOT a valid prefix match — keep backtracking for a
                 source-clean endpoint or the best recorded source-exhaustion.
                 Without [descend], pattern-exhaustion is reported as-is (the
                 caller checks source-exhaustion). *)
              if descend && C.move_next_subtree (C.clone !cursor) then
                backtrack ()
              else Drive_pattern_exhausted (!cursor, List.rev !bindings)
          | Drive_match_at_all ->
              (* Snapshot the cursor and every binding cursor (see
                 [clone_binding]): they alias checkpoint-saved cursors that
                 later backtracking mutates, so a shallow capture would be
                 corrupted by the next backtrack pass. *)
              let bindings_snapshot = List.rev_map clone_binding !bindings in
              enumerated := (C.clone !cursor, bindings_snapshot) :: !enumerated;
              backtrack ())
      | `Mismatch -> backtrack ()
      | `Source_at i -> (
          match mode with
          | Drive_match_at | Drive_match_at_all -> backtrack ()
          | Drive_match_prefix ->
              if descend then begin
                (* Record this source-exhaustion, then keep exploring: a
                   leading [Subtree] may descend (via its [Cp_subtree]
                   checkpoint) and consume more of the element. The best
                   (most-pattern-consumed) result is returned when checkpoints
                   run out. *)
                record_prefix i;
                backtrack ()
              end
              else
                Drive_source_exhausted
                  (remaining_from i, !cursor, List.rev !bindings))
    and backtrack () =
      match !checkpoints with
      | [] -> (
          match mode with
          | Drive_match_at_all -> Drive_enumerated (List.rev !enumerated)
          | Drive_match_at -> Drive_failed
          | Drive_match_prefix -> (
              (* [descend] mode: backtracking is exhausted, so return the best
                 source-exhaustion recorded (most pattern consumed), or fail.
                 Without [descend], backtracking only happens on [Mismatch],
                 and exhaustion is a plain failure. *)
              match !best_prefix with
              | Some (i, c, b) -> Drive_source_exhausted (remaining_from i, c, b)
              | None -> Drive_failed))
      | cp :: rest -> (
          checkpoints := rest;
          start := cp.saved_start;
          first := cp.saved_first;
          cursor := cp.saved_cursor;
          bindings := cp.saved_bindings;
          match cp.kind with
          | Cp_subtree ->
              assert (not !first);
              if C.move_first_child !cursor then begin
                first := true;
                check_loop ()
              end
              else backtrack ()
          | Cp_siblings ->
              let can =
                if !first then begin
                  first := false;
                  true
                end
                else C.move_next_sibling !cursor
              in
              if can then begin
                (* The cursor is now at the just-consumed sibling. Record it
                   in the Sequence binding if this wildcard is named. *)
                (match cp.name with
                | None -> ()
                | Some n ->
                    bindings := append_to_sequence !bindings n (C.clone !cursor));
                checkpoints :=
                  {
                    kind = Cp_siblings;
                    name = cp.name;
                    saved_start = !start;
                    saved_first = !first;
                    saved_cursor = C.clone !cursor;
                    saved_bindings = !bindings;
                  }
                  :: !checkpoints;
                start := !start + 1;
                check_loop ()
              end
              else backtrack ())
    in
    check_loop ()

  let match_at ?(initial_bindings = []) pattern cursor =
    match drive ~mode:Drive_match_at ~initial_bindings pattern cursor with
    | Drive_pattern_exhausted (c, b) -> Some (c, b)
    | Drive_source_exhausted _ ->
        (* Should not occur with Drive_match_at; backtracking treats source
           exhaustion as failure. *)
        None
    | Drive_failed -> None
    | Drive_enumerated _ ->
        (* Only produced by Drive_match_at_all. *)
        assert false

  let match_at_spans ?(initial_bindings = []) pattern cursor =
    let n = List.length pattern in
    let spans = Array.make (max 1 n) (-1, -1) in
    match
      drive ~mode:Drive_match_at ~initial_bindings ~spans:(Some spans) pattern
        cursor
    with
    | Drive_pattern_exhausted (c, b) -> Some (c, b, spans)
    | _ -> None

  let match_prefix ?(initial_bindings = []) ?(ignore_node_type = false)
      ?(descend = false) ?(spans = None) pattern cursor =
    match pattern with
    | [] ->
        (* Empty pattern can't be a valid prefix-match of a non-empty
           sub-tree: the cursor's current position is still unconsumed
           source content. (The post-check below would mis-classify this
           as "source exhausted" because [move_next_subtree] from the
           cursor's unmoved root fails within the scoped sub-cursor.)
           A sub-cursor always has at least its root node, so empty
           pattern is always a None. *)
        None
    | _ -> (
        match
          drive ~mode:Drive_match_prefix ~initial_bindings ~ignore_node_type
            ~descend ~spans pattern cursor
        with
        | Drive_source_exhausted (remaining, c, b) -> Some (remaining, c, b)
        | Drive_pattern_exhausted (c, b) ->
            (* Pattern fully consumed; require the source sub-tree to also be
               exhausted for match_prefix semantics. If [move_next_subtree] on
               a clone succeeds, the sub-tree has unconsumed content past
               where the pattern stopped — that's not a clean prefix-match. *)
            let probe = C.clone c in
            if C.move_next_subtree probe then None else Some ([], c, b)
        | Drive_failed -> None
        | Drive_enumerated _ ->
            (* Only produced by Drive_match_at_all. *)
            assert false)

  (* Infer the element separator from the source bytes between the first
     two named children. Result is the inter-child bytes trimmed of
     whitespace — typically ["," / ";" / ""]. Returns [""] when there are
     fewer than two named children (no inter-child gap to inspect, no
     stripping needed). *)
  let infer_separator cursor children =
    match children with
    | c1 :: c2 :: _ ->
        let _, c1_end = C.byte_range c1 in
        let c2_start, _ = C.byte_range c2 in
        let raw = C.source_substring cursor c1_end c2_start in
        String.trim raw
    | _ -> ""

  (* Drop a leading [Concrete] token from the pattern whose text equals
     the (non-empty) separator. Lenient: if the pattern doesn't have the
     separator at its head, return the pattern unchanged. This lets users
     write patterns with or without explicit separators — the matcher
     handles both. *)
  let strip_separator separator pattern =
    if separator = "" then pattern
    else
      match pattern with
      | Concrete { text; _ } :: rest when text = separator -> rest
      | _ -> pattern

  (* Strict leaf-by-leaf match: each [Concrete] pattern token must equal the
     corresponding source leaf in both text and node type. Anything else
     (a wildcard token, a length mismatch, a content disagreement) fails.
     Used for the leading/trailing delimiter runs in partial mode — those
     positions are pure structural delimiters and wildcards there have no
     useful meaning. *)
  let strict_leaf_match pattern_tokens leaves =
    let rec loop ts ls =
      match (ts, ls) with
      | [], [] -> true
      | Concrete { text; node_type } :: ts', l :: ls'
        when text = C.leaf_text l && node_type = C.leaf_node_type l ->
          loop ts' ls'
      | _ -> false
    in
    loop pattern_tokens leaves

  (* Split a list into a prefix of length [n] and the rest. Out-of-range
     [n] yields the longest possible prefix and an empty rest. *)
  let split_at_n n xs =
    let rec aux n acc = function
      | xs when n <= 0 -> (List.rev acc, xs)
      | [] -> (List.rev acc, [])
      | x :: rest -> aux (n - 1) (x :: acc) rest
    in
    aux n [] xs

  (* Low-level set-match: consume the whole pattern by repeated
     [match_prefix] calls against the source named children, with set
     semantics (reordering, extras tolerated) and source-derived separator
     stripping between successful element matches. This is the inner
     primitive — it does NOT inspect leading/trailing anonymous runs.

     Exposed so that fixture-based tests (where [Cursor.S] implementations
     don't model the named-vs-anonymous distinction) can exercise the
     set-match logic in isolation. Production code wanting partial mode at
     a container should use [match_partial_at], which composes anon
     delimiter handling on top of this.

     - Each iteration tries to consume the next pattern element by calling
       [match_prefix] against each unused source child in turn. The first
       child whose [match_prefix] succeeds is "locked in" for this element.
     - If a downstream consume call fails after a particular lock-in, the
       loop backtracks and tries the next unused child for this element.
     - Pattern elements may align with source children in any order
       (reordering).
     - Source children that aren't matched by any pattern element are
       extras; they remain in [unused] when the pattern exhausts and are
       silently accepted.
     - Source-derived separator stripping continues to apply between
       successful element matches.

     Cross-element non-linearity: bindings accumulated from earlier
     elements are passed to each [match_prefix] call as [initial_bindings].
     A second occurrence of a named wildcard must bind to a
     structurally-equal subtree, otherwise [match_prefix] returns [None]
     and the driver backtracks to try a different element-to-child
     assignment.

     Complexity caveat: worst-case backtracking is O(n!) for ambiguous
     patterns (e.g. several leading-wildcard elements) over n source
     children. Typical patterns with distinct leading concrete tokens
     per element have effectively linear behaviour because non-matching
     children fail [match_prefix] immediately on the first token. *)
  let match_set_at ?(initial_bindings = []) ?(spans = None) ?(offset = 0)
      pattern cursor =
    let children = C.named_children cursor in
    let separator = infer_separator cursor children in
    (* Record one element's per-token source spans into the global [spans]
       array (indexed by the full pattern) at [base]. Only called on the
       committed path, so abandoned backtracking trials never pollute it. *)
    let record_element base local consumed =
      match spans with
      | Some g ->
          let n = Array.length g in
          for k = 0 to consumed - 1 do
            if base + k < n then g.(base + k) <- local.(k)
          done
      | None -> ()
    in
    let rec consume remaining bindings unused base =
      if remaining = [] then Some (cursor, bindings)
      else
        let rec try_each tried = function
          | [] -> None
          | child :: rest -> (
              let probe = C.clone child in
              let local = Array.make (List.length remaining) (-1, -1) in
              (* [descend]: each element must fully cover one child, so a
                 leading wildcard descends into the child (matching e.g. a
                 key) rather than greedily swallowing the whole child and
                 leaking the rest of the element to the next child. *)
              match
                match_prefix ~initial_bindings:bindings ~descend:true
                  ~spans:(Some local) remaining probe
              with
              | None -> try_each (child :: tried) rest
              | Some (after_match, _, new_bindings) -> (
                  (* [new_bindings] already includes [bindings] (the seed)
                     plus any bindings recorded during this match_prefix
                     call — replace the accumulator with it directly. *)
                  let consumed =
                    List.length remaining - List.length after_match
                  in
                  let after_strip = strip_separator separator after_match in
                  let stripped =
                    List.length after_match - List.length after_strip
                  in
                  let new_unused = List.rev_append tried rest in
                  match
                    consume after_strip new_bindings new_unused
                      (base + consumed + stripped)
                  with
                  | Some _ as result ->
                      record_element base local consumed;
                      result
                  | None -> try_each (child :: tried) rest))
        in
        try_each [] unused
    in
    consume pattern initial_bindings children offset

  (* Partial-mode entry point with source-driven delimiter handling.

     Reads the container's leading and trailing anonymous-leaf runs (e.g.
     [{']/['}'] for an object, ['<']/['/>'] for a JSX self-closing
     element), splits the pattern into a front (strict-matched against
     leading), a middle (set-matched via [match_set_at]), and a back
     (strict-matched against trailing). The delimiter shape comes from
     the source structure — no bracket table, no allowlist, no per-
     language config.

     A genuine set-like container (object literal, JSX element, record
     type, ...) is the only kind of node that has BOTH a leading and a
     trailing anonymous run. Requiring [n_lead > 0 && n_tail > 0] is the
     structural self-filter: nodes without that shape (statements,
     declarations whose children are positional, etc.) yield [None]
     immediately and never spuriously collapse the pattern into a single
     child as a strict subtree match. *)
  let match_partial_at ?(initial_bindings = []) ?(spans = None) pattern cursor =
    let lead = C.leading_anonymous_leaves cursor in
    let tail = C.trailing_anonymous_leaves cursor in
    let n_lead = List.length lead in
    let n_tail = List.length tail in
    if n_lead = 0 || n_tail = 0 then None
    else
      let n_pat = List.length pattern in
      if n_pat < n_lead + n_tail then None
      else
        let front, rest = split_at_n n_lead pattern in
        let middle, back = split_at_n (n_pat - n_lead - n_tail) rest in
        if not (strict_leaf_match front lead) then None
        else if not (strict_leaf_match back tail) then
          None
          (* The leading/trailing delimiter tokens (front/back) are matched
             positionally against anonymous leaves and left without spans; only
             the middle's set-matched elements record source spans, at their
             global offset [n_lead]. *)
        else match_set_at ~initial_bindings ~spans ~offset:n_lead middle cursor

  (* Field-mode entry point: align an ordered pattern leaf-stream to a
     subsequence of a node's named children.

     The pattern is matched left-to-right against the node's named children.
     At each child there are two choices, explored with backtracking:
     - MATCH: consume the prefix of the remaining pattern that exactly covers
       this child's subtree (via [match_prefix]); advance to the next child
       with the leftover pattern.
     - SKIP: leave the pattern untouched and advance to the next child.
     The pattern must be fully consumed for a match; children left over once
     the pattern is exhausted are skipped (tolerated extras).

     This needs no per-language optionality data. The grammar makes the
     required fields (name, parameters, body, ...) syntactically mandatory,
     so the pattern always contains them; they are therefore matched, in
     order, against the corresponding children. The only children a pattern
     can leave unmatched are the ones it omits entirely — exactly the
     optional fields (decorators, annotations, modifier groups, return
     types). MATCH-first means a child whose content the pattern addresses is
     matched in full: a pattern [f ()] cannot match a source [f (a, b)],
     because the parameter list is matched, not skipped. SKIP gives the
     "ignore what I didn't mention" behaviour and, via backtracking, lets a
     concrete element skip earlier same-shaped children to reach the one it
     matches (e.g. [@Get] skipping a leading [@Auth]).

     The walk is over {b all} non-extra children ({!Cursor.S.all_children}),
     not just the named ones, because a declaration interleaves named
     children with anonymous leaves that the pattern addresses (the [fun] /
     [def] / [function] keyword) or omits (the [:] introducing an optional
     return type in Kotlin/Scala — anonymous, yet optional). Both kinds are
     skippable; the grammar's required structure makes that safe. A required
     anonymous keyword the pattern {b does} contain (e.g. [fun]) is matched,
     and cannot be silently dropped, because its pattern token would then
     have no child to bind to and the alignment fails. A pattern's own
     punctuation ([(], [)], [{]) is matched inside the relevant child by
     [match_prefix], not against these top-level leaves.

     One consequence: an anonymous keyword the pattern omits — TS [async] /
     [abstract] — is skipped rather than rejected, so field mode is more
     permissive there than a strict "Tier-3 gap" would be. This falls out of
     the uniform skip rule; it is not special-cased.

     Returns [None] when the pattern cannot be fully consumed. The cursor is
     returned unchanged at the node on success, mirroring {!match_partial_at}. *)
  let match_field_at ?(initial_bindings = []) ?(ignore_node_type = false)
      ?(spans = None) pattern cursor =
    let children = C.all_children cursor in
    let record base local consumed =
      match spans with
      | Some g ->
          let n = Array.length g in
          for k = 0 to consumed - 1 do
            if base + k < n then g.(base + k) <- local.(k)
          done
      | None -> ()
    in
    (* [base] is the global pattern index of [remaining]'s head — advances only
       when a child is MATCHed (consumes pattern), never on a SKIP. *)
    let rec go remaining children bindings base =
      match remaining with
      | [] -> Some (cursor, bindings)
      | _ -> (
          match children with
          | [] -> None
          | child :: rest -> (
              let local = Array.make (List.length remaining) (-1, -1) in
              let matched =
                match
                  match_prefix ~initial_bindings:bindings ~ignore_node_type
                    ~spans:(Some local) remaining (C.clone child)
                with
                | Some (remaining', _, bindings') -> (
                    let consumed =
                      List.length remaining - List.length remaining'
                    in
                    match go remaining' rest bindings' (base + consumed) with
                    | Some _ as r ->
                        record base local consumed;
                        r
                    | None -> None)
                | None -> None
              in
              match matched with
              | Some _ as r -> r
              (* No match here: skip this child (an optional field, or an
                 anonymous leaf the pattern doesn't mention) and try the
                 next. A required child the pattern does address can't be
                 skipped away — its pattern token would find no later home. *)
              | None -> go remaining rest bindings base))
    in
    go pattern children initial_bindings 0

  type match_result = {
    start_byte : int;
    end_byte : int;
    bindings : binding list;
    spans : (int * int) array;
        (** Per pattern-token source byte range, parallel to the pattern token
            list, as produced by {!match_at_spans}. Populated for the strict
            non-overlapping (transform) path; [[||]] otherwise (search, partial,
            field — surgical editing falls back to whole-span there). Used to
            locate `-`-marked regions within the match. *)
  }

  (* Advance [cursor] to the next position in pre-order. Returns true if
     successful, false if the tree is exhausted. *)
  let advance_one cursor =
    C.move_first_child cursor || C.move_next_subtree cursor

  let find_matches_iter ?(overlapping = false) ?(initial_bindings = []) pattern
      source_cursor =
    (* Walk the tree pre-order. At each position, drive enumerates every
       valid binding configuration; we yield each, with span dedup in
       overlapping mode.

       On success the two modes differ:
       - Non-overlapping (default): yield only the first configuration at
         each position, then advance PAST the matched region (to the next
         subtree after the match end). Greedy, left-to-right, no two
         reported matches overlap. Suited to transforms.
       - Overlapping ([overlapping = true]): yield every enumerated
         configuration at each position, then advance ONE node in
         pre-order so nested matches and ancestor-shared first-leaves are
         all explored. Spans are de-duplicated (each distinct
         [(start_byte, end_byte)] is reported once across the whole
         walk). Suited to search.

       [initial_bindings] (default [\[\]]) seeds the binding state for
       every drive call. Cross-walk non-linearity is enforced because the
       seed is checked at every [Subtree] / [Siblings] binding point.

       Stop when no more positions remain. *)
    let cursor = ref (C.clone source_cursor) in
    let exhausted = ref false in
    let seen = ref [] in
    let pending : match_result list ref = ref [] in
    let rec next () =
      match !pending with
      | r :: rest ->
          pending := rest;
          let key = (r.start_byte, r.end_byte) in
          if overlapping && List.mem key !seen then next ()
          else begin
            if overlapping then seen := key :: !seen;
            Seq.Cons (r, next)
          end
      | [] -> (
          if !exhausted then Seq.Nil
          else
            let attempt = C.clone !cursor in
            let start_byte, _ = C.byte_range !cursor in
            let configurations =
              match
                drive ~mode:Drive_match_at_all ~initial_bindings pattern attempt
              with
              | Drive_enumerated lst -> lst
              | _ ->
                  (* drive in Drive_match_at_all always returns
                     Drive_enumerated. *)
                  assert false
            in
            match configurations with
            | [] ->
                if not (advance_one !cursor) then exhausted := true;
                next ()
            | _ ->
                if overlapping then begin
                  pending :=
                    List.map
                      (fun (end_cursor, bindings) ->
                        let _, end_byte = C.byte_range end_cursor in
                        { start_byte; end_byte; bindings; spans = [||] })
                      configurations;
                  if not (advance_one !cursor) then exhausted := true
                end
                else begin
                  (* Non-overlapping (transform) path: commit the first
                     configuration and recover its per-token spans by re-driving
                     the same leftmost match with span recording — the spans
                     surgical editing needs to locate `-` regions. *)
                  let end_cursor_first, bindings_first =
                    List.hd configurations
                  in
                  let _, end_byte = C.byte_range end_cursor_first in
                  let n = List.length pattern in
                  let spans = Array.make (max 1 n) (-1, -1) in
                  (match
                     drive ~mode:Drive_match_at ~initial_bindings
                       ~spans:(Some spans) pattern (C.clone !cursor)
                   with
                  | _ -> ());
                  pending :=
                    [
                      { start_byte; end_byte; bindings = bindings_first; spans };
                    ];
                  let next_cursor = C.clone end_cursor_first in
                  if C.move_next_subtree next_cursor then cursor := next_cursor
                  else exhausted := true
                end;
                next ())
    in
    next

  let find_matches ?(overlapping = false) ?(initial_bindings = []) pattern
      source_cursor =
    List.of_seq
      (find_matches_iter ~overlapping ~initial_bindings pattern source_cursor)

  let find_partial_matches_iter ?(initial_bindings = []) pattern source_cursor =
    (* Walk the tree pre-order; at each node attempt [match_partial_at] (the
       node is treated as the candidate set-like container). It succeeds only
       where the node's named children actually match the pattern's elements,
       so no explicit "is this a set-like container?" test is needed — the
       structural match is the filter. Spans are de-duplicated; the walk
       descends into matches so nested containers are also found.

       [initial_bindings] (default [\[\]]) seeds every [match_partial_at]
       attempt — used by the multi-section driver for cross-section
       non-linearity. *)
    let cursor = ref (C.clone source_cursor) in
    let exhausted = ref false in
    let seen = ref [] in
    let rec next () =
      if !exhausted then Seq.Nil
      else
        let attempt = C.clone !cursor in
        let spans = Array.make (max 1 (List.length pattern)) (-1, -1) in
        match
          match_partial_at ~initial_bindings ~spans:(Some spans) pattern attempt
        with
        | Some (matched_cursor, bindings) ->
            let start_byte, end_byte = C.byte_range matched_cursor in
            let is_dup = List.mem (start_byte, end_byte) !seen in
            if not (advance_one !cursor) then exhausted := true;
            if is_dup then next ()
            else begin
              seen := (start_byte, end_byte) :: !seen;
              Seq.Cons ({ start_byte; end_byte; bindings; spans }, next)
            end
        | None ->
            if not (advance_one !cursor) then begin
              exhausted := true;
              Seq.Nil
            end
            else next ()
    in
    next

  let find_partial_matches ?(initial_bindings = []) pattern source_cursor =
    List.of_seq
      (find_partial_matches_iter ~initial_bindings pattern source_cursor)

  let find_field_matches_iter ?(initial_bindings = []) pattern source_cursor =
    (* Walk pre-order; at each node attempt [match_field_at], treating the
       node as a candidate declaration whose named children the pattern is
       aligned against. The structural match is the filter — a node whose
       children can't be aligned to the pattern fails. Spans are
       de-duplicated; the walk descends into matches.

       Same shape as {!find_partial_matches_iter}. *)
    let cursor = ref (C.clone source_cursor) in
    let exhausted = ref false in
    let seen = ref [] in
    let rec next () =
      if !exhausted then Seq.Nil
      else
        let attempt = C.clone !cursor in
        let spans = Array.make (max 1 (List.length pattern)) (-1, -1) in
        match
          match_field_at ~initial_bindings ~spans:(Some spans) pattern attempt
        with
        | Some (matched_cursor, bindings) ->
            let start_byte, end_byte = C.byte_range matched_cursor in
            let is_dup = List.mem (start_byte, end_byte) !seen in
            if not (advance_one !cursor) then exhausted := true;
            if is_dup then next ()
            else begin
              seen := (start_byte, end_byte) :: !seen;
              Seq.Cons ({ start_byte; end_byte; bindings; spans }, next)
            end
        | None ->
            if not (advance_one !cursor) then begin
              exhausted := true;
              Seq.Nil
            end
            else next ()
    in
    next

  let find_field_matches ?(initial_bindings = []) pattern source_cursor =
    List.of_seq
      (find_field_matches_iter ~initial_bindings pattern source_cursor)

  let find_field_matches_with_iter ?(initial_bindings = []) ~tokens_for
      source_cursor =
    (* Like {!find_field_matches_iter}, but the pattern token list is computed
       per candidate node by [tokens_for]. This lets the caller re-tokenize
       the pattern in each candidate's source context (so context-sensitive
       node-types come out right) without the matcher knowing anything about
       tokenization. A [tokens_for] that returns [[]] makes the node a
       non-match (an empty pattern would otherwise match every node). *)
    let cursor = ref (C.clone source_cursor) in
    let exhausted = ref false in
    let seen = ref [] in
    let rec next () =
      if !exhausted then Seq.Nil
      else
        let attempt = C.clone !cursor in
        let spans = ref [||] in
        let result =
          match tokens_for attempt with
          | [] -> None
          | pattern ->
              let s = Array.make (max 1 (List.length pattern)) (-1, -1) in
              spans := s;
              match_field_at ~initial_bindings ~spans:(Some s) pattern attempt
        in
        match result with
        | Some (matched_cursor, bindings) ->
            let start_byte, end_byte = C.byte_range matched_cursor in
            let is_dup = List.mem (start_byte, end_byte) !seen in
            if not (advance_one !cursor) then exhausted := true;
            if is_dup then next ()
            else begin
              seen := (start_byte, end_byte) :: !seen;
              Seq.Cons ({ start_byte; end_byte; bindings; spans = !spans }, next)
            end
        | None ->
            if not (advance_one !cursor) then begin
              exhausted := true;
              Seq.Nil
            end
            else next ()
    in
    next

  let find_field_matches_with ?(initial_bindings = []) ~tokens_for source_cursor
      =
    List.of_seq
      (find_field_matches_with_iter ~initial_bindings ~tokens_for source_cursor)
end
