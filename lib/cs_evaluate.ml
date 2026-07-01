(** Change-summary evaluation: the per-site safety gate (design §2.3, §3.1).
    Given a rendered pattern and a {!site_info}, classify the rule's behaviour
    at the site as exact / decomposable / unsafe, and compute residual diffs.
    This is where a candidate's *meaning* is defined (design §3.3). Depends on
    {!Cs_types} and the matcher/diff libraries — not on the proposer or
    clustering phases. *)

open Cs_types

(* ── Per-site safety gate (design §2.3, §3.1) ────────────────────── *)

type site_info = {
  si_before : string;  (** full pre-change source of the file *)
  si_after : string;  (** full post-change source of the file *)
  si_language : string;  (** grammar the file parses with *)
  si_regions : (int * int * string) list;
      (** the file's changed regions in before-coordinates, sorted by start,
          disjoint: [(start, end, after_content)]. A zero-width region
          [(p, p, txt)] is an insertion at byte [p]. *)
  si_before_errors : string list;
      (** texts of the before-parse's ERROR nodes (usually empty). The
          well-formedness guard tolerates these in a rule's output — they
          predate the rule — while rejecting any error the rule invents. *)
}

(** A bare separator token — a childless [,]/[;] node. Added or removed alone it
    is pure formatting (a trailing comma, a redundant statement/enum semicolon):
    a real element change carries its own content node, and this token only ever
    sits between elements or before a closer. Used by [changed_regions] under
    [ignore_separators] so formatting-only reflow (which tree-sitter surfaces as
    an added/removed separator plus whitespace) leaves no changed region and is
    filtered out of the residual. *)
let is_separator_token src (n : Tree.src Tree.t) =
  n.children = []
  && match String.trim (Tree.text src n) with "," | ";" -> true | _ -> false

(** Finest-grain changed regions of a diff, each carrying the after-side content
    that replaced it. The walk recurses through [Modified] chains so a region is
    the smallest changed node, not its enclosing scaffold; [Added] children
    become zero-width insertions at the before-position between their siblings.

    With [ignore_separators], an added/removed bare separator token contributes
    no region — the formatting-blind mode (a trailing comma or redundant
    semicolon is trivia). Sound: content changes still carry their own regions,
    so only pure-separator reflow is suppressed. *)
let changed_regions ?(ignore_separators = false) (d : Tree_diff.diff) :
    (int * int * string) list =
  let acc = ref [] in
  let add s e txt = acc := (s, e, txt) :: !acc in
  let after_text (n : Tree.src Tree.t) = Tree.text d.after_source n in
  let rec go (b : Tree.src Tree.t) (a : Tree.src Tree.t)
      (ch : Tree_diff.node_change) =
    match ch with
    | Tree_diff.Unchanged -> ()
    | Tree_diff.Replaced -> add b.start_byte b.end_byte (after_text a)
    | Tree_diff.Modified { child_changes } ->
        let cursor = ref b.start_byte in
        List.iter
          (fun (cc : Tree_diff.child_change) ->
            match cc with
            | Tree_diff.Same { node } -> cursor := node.end_byte
            | Tree_diff.Changed { before; after; change } ->
                go before after change;
                cursor := before.end_byte
            | Tree_diff.Removed { node } ->
                if not (ignore_separators && is_separator_token d.before_source node)
                then add node.start_byte node.end_byte "";
                cursor := node.end_byte
            | Tree_diff.Added { node } ->
                if not (ignore_separators && is_separator_token d.after_source node)
                then add !cursor !cursor (after_text node))
          child_changes
  in
  go d.before_root d.after_root d.root_change;
  List.sort compare !acc

(* Source texts of the outermost ERROR nodes in a parse tree —
   tree-sitter's markers for unparseable stretches. Position-independent
   identities for the gate's well-formedness guard: edits elsewhere in
   the file shift an untouched error's offsets but not its text. *)
let error_texts source (root : Tree.src Tree.t) : string list =
  let rec go acc (n : Tree.src Tree.t) =
    if n.Tree.node_type = "ERROR" then Tree.text source n :: acc
    else
      List.fold_left
        (fun a (c : Tree.src Tree.child) -> go a c.node)
        acc n.Tree.children
  in
  go [] root

(* Multiset inclusion: every element of [xs] consumed from [allowance]. *)
let multiset_covered xs allowance =
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun a ->
      Hashtbl.replace tbl a
        (1 + Option.value ~default:0 (Hashtbl.find_opt tbl a)))
    allowance;
  List.for_all
    (fun x ->
      match Hashtbl.find_opt tbl x with
      | Some n when n > 0 ->
          Hashtbl.replace tbl x (n - 1);
          true
      | _ -> false)
    xs

(* Plain substring membership: does [sub] occur in [s]? Used by the
   deletion-direction guard of the decomposable check. *)
let string_mem ~sub s =
  let n = String.length sub and m = String.length s in
  n = 0
  ||
  let rec at i = i + n <= m && (String.sub s i n = sub || at (i + 1)) in
  at 0

(** Residual diff from a post-rule intermediate to the real after-source, with
    layout-only hunks dropped (§9.1): a hunk is kept only when it touches a
    tree-level changed region of the (intermediate, after) diff. Layout —
    re-indentation, [{ }] vs [{}], line splits — is invisible to the parse tree,
    so a hunk over lines no changed region touches states nothing about the
    change; the summary's reconstruction guarantee is already modulo layout (the
    whole-file gap check, the gate's tree-level re-diff). Returns [""] when the
    gap is entirely layout. Conservative in the keep direction: hunk and region
    line spans are each widened by one line before intersecting, and an
    unparseable side falls back to the unfiltered diff.

    With [ignore_formatting], the changed-region oracle also treats added/removed
    bare separators (trailing commas, redundant semicolons) as trivia, so a hunk
    that is only reflow — re-indentation plus a trailing separator — is dropped
    too, not just pure-whitespace hunks. *)
let residual_diff ?(ignore_formatting = false) ~ctx ~language ~file_path
    ~original ~transformed () =
  if original = transformed then ""
  else
    let keep_hunk =
      try
        let bt = Tree.parse ~ctx ~language original in
        let at = Tree.parse ~ctx ~language transformed in
        let d = Tree_diff.diff ~before:bt ~after:at in
        let regions =
          changed_regions ~ignore_separators:ignore_formatting d
        in
        let line_starts =
          let acc = ref [ 0 ] in
          String.iteri
            (fun i c -> if c = '\n' then acc := (i + 1) :: !acc)
            original;
          Array.of_list (List.rev !acc)
        in
        let line_of byte =
          let lo = ref 0 and hi = ref (Array.length line_starts - 1) in
          while !lo < !hi do
            let mid = (!lo + !hi + 1) / 2 in
            if line_starts.(mid) <= byte then lo := mid else hi := mid - 1
          done;
          !lo
        in
        let region_spans =
          (* Exact line spans; only zero-width insertion regions get a
             one-line widening (the insertion point sits between lines,
             and the textual diff may render the added lines on either
             side of it). Symmetric widening of every span would bridge
             one-line gaps between a real change and an adjacent
             layout-only hunk. *)
          List.map
            (fun (s, e, _) ->
              if e <= s then (line_of s - 1, line_of s)
              else (line_of s, line_of (e - 1)))
            regions
        in
        Some
          (fun ~orig_start ~orig_len ->
            let h_lo = if orig_len = 0 then orig_start - 1 else orig_start in
            let h_hi =
              if orig_len = 0 then orig_start else orig_start + orig_len - 1
            in
            List.exists
              (fun (lo, hi) -> max lo h_lo <= min hi h_hi)
              region_spans)
      with
      | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
      | e ->
          (* Re-parse failed; fall back to the unfiltered diff. *)
          Cs_trace.trace "residual_diff: re-parse failed: %s\n%!"
            (Printexc.to_string e);
          None
    in
    match keep_hunk with
    | None ->
        Text_diff.generate_diff ~context:0 ~file_path ~original ~transformed ()
    | Some keep_hunk ->
        Text_diff.generate_diff ~context:0 ~keep_hunk ~file_path ~original
          ~transformed ()

(** [path → site_info] for every [Modified] file in the changeset. The safety
    gate evaluates rules against these. *)
let build_site_db ~ctx (cs : changeset) : (string, site_info) Hashtbl.t =
  (* The gate re-parses each modified file's before- and after-source once per
     candidate, so the parse cache must hold both for every modified file at
     once or it thrashes on a large changeset. Size it to that working set
     (plus headroom for the transformed intermediates churning through), so the
     cache adapts to the changeset rather than a fixed cap. *)
  let modified =
    List.fold_left
      (fun n -> function Modified _ -> n + 1 | _ -> n)
      0 cs.files
  in
  Context.ensure_parse_cap ctx ((2 * modified) + 128);
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun fc ->
      match fc with
      | Modified { path; language; before_source; after_source } -> (
          try
            let bt = Tree.parse ~ctx ~language before_source in
            let at = Tree.parse ~ctx ~language after_source in
            let d = Tree_diff.diff ~before:bt ~after:at in
            Hashtbl.replace tbl path
              {
                si_before = before_source;
                si_after = after_source;
                si_language = language;
                si_regions = changed_regions d;
                si_before_errors = error_texts before_source bt.Tree.root;
              }
          with
          | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
          | e ->
              (* Tolerate an unparseable file (real corpora contain a few);
                 it simply contributes no site. *)
              Cs_trace.trace "build_site_db: skipping %s: %s\n%!" path
                (Printexc.to_string e))
      | Added _ | Deleted _ -> ())
    cs.files;
  tbl

(* [s, e) and [rs, re) overlap. Zero-width intervals (a pure-removal
   landing zone, an insertion region) overlap when they touch the other
   interval, including at its boundary — a removal at the exact point
   where content must be re-added is a real conflict. Two non-zero-width
   intervals that merely share a boundary do not overlap. *)
let spans_overlap s e rs re =
  max s rs < min e re
  || (s = e && rs <= s && s <= re)
  || (rs = re && s <= rs && rs <= e)

type site_evaluation = {
  ev_exact : bool;
      (** the gate verdict: the candidate fires and fully explains every region
          it touches (no remaining change in its landing zones). *)
  ev_decomposable : bool;
      (** M1.9b: the candidate fires and makes safe-but-*partial* progress
          within a region — [t''] differs from the after inside a landing zone,
          but the rule stays on the geodesic (§2.3): [t''] and the after are
          tree-inclusion comparable ([Tree_inclusion]), so the gap is a pure
          insertion or pure deletion — an honest residual rather than a detour
          (relabel) that must be undone. A decomposable site counts toward
          support and coverage; its in-zone gap is emitted as a
          [rule=]-attributed residual (§4.4) by the re-diff in [summarize].
          Mutually exclusive with [ev_exact]. *)
  ev_fires : int;  (** number of edits the candidate makes at the site *)
  ev_resolved : int list;
      (** indices into [si_regions] of the changed regions the candidate fully
          resolves: regions an edit touches whose t''-image carries no remaining
          change after application. The selector's coverage unit (§3.3). A
          decomposable site lists only the regions it fully resolves; partial
          ones fall to the residual. Empty unless [ev_exact] or
          [ev_decomposable]. *)
  ev_clean : bool;
      (** the candidate, applied alone, reproduces the site's after-source
          (modulo whitespace) — i.e. it leaves no residual here. Used by
          selection to prefer a rule that reconstructs over one that only
          partially resolves the same regions (e.g. an extraction
          [box($H).get() ⤳ $H] over a bare removal [box($H).get()] that deletes
          and defers the rest to a residual). Always false for a
          decomposable-only site. *)
  ev_overfire : bool;
      (** the candidate fired here (produced edits) but at least one edit fell
          outside every changed region — the placement leg failed. Distinct from
          a plain no-match: it marks a context-stripped pattern matching code the
          changeset did not touch. Declaration anchoring keys on this — a body
          rule that over-fires is the one worth re-anchoring under its enclosing
          declaration. *)
}
(** Per-site safety gate: the operational form of the safety property (design
    §2.3) — with [t'' = apply(rule, t)], [d(t,t'') + d(t'',t') = d(t,t')]. Two
    legs (§3.1):

    {b Placement}: every edit the rule would make must intersect a changed
    region of the site's diff. An edit confined to unchanged territory is, by
    construction, a change that must be undone to reach the after-source — the
    over-merged [- import _H0] case, whose application would remove every import
    in the file.

    {b Content}: apply the rule ([t''] = the transformed source) and re-diff
    against the real after-source. No remaining change may overlap the rule's
    landing zones: a zone that still differs from [t'] means the rule wrote
    something other than what the changeset wrote there (claiming [f → h] where
    the change was [f → g]). Comparing via a re-diff rather than reconstructing
    expected text from the region list keeps separator tokens and layout — which
    the node-level regions do not cover — out of the comparison.

    A site where the rule produces no edits fails too — the rendered rule cannot
    fire there at all (the old zero-match applicability failure, e.g. a
    [property_identifier] rendered standalone re-parsing as a bare
    [identifier]).

    M1 emission policy: this is the [exact] classification only — the rule fully
    explains every region it touches; regions it does not touch are other rules'
    or residuals' business. A site where the rule makes safe-but-partial
    progress {e within} a region (the residual case, §4.4) is shed until M1.9b
    can attach residuals to state the gap honestly. *)

let no_fire =
  {
    ev_exact = false;
    ev_decomposable = false;
    ev_fires = 0;
    ev_resolved = [];
    ev_clean = false;
    ev_overfire = false;
  }

(** Evaluate one candidate pattern at one site — the §3.1 gate, keeping the
    information it computes instead of reducing to a boolean. *)
let site_eval ~ctx ~language ~pattern_text (si : site_info) : site_evaluation =
  try
    let edits =
      Matcher.transform_edits ~ctx ~language ~pattern_text
        ~source_text:si.si_before
    in
    if edits = [] then no_fire
    else
      let placement_ok =
        List.for_all
          (fun (ed : Matcher.edit) ->
            List.exists
              (fun (rs, re, _) -> spans_overlap ed.start_byte ed.end_byte rs re)
              si.si_regions)
          edits
      in
      if not placement_ok then { no_fire with ev_overfire = true }
      else begin
        (* Landing zones in t''-coordinates: each edit's span shifted by
           the cumulative length delta of the edits before it
           ([transform_edits] returns them sorted by start). *)
        let zones =
          let delta = ref 0 in
          List.map
            (fun (ed : Matcher.edit) ->
              let zs = ed.start_byte + !delta in
              let ze = zs + String.length ed.replacement in
              delta :=
                !delta
                + String.length ed.replacement
                - (ed.end_byte - ed.start_byte);
              (zs, ze))
            edits
        in
        (* Map a before-coordinate point to t''-coordinates: add the
           length deltas of the edits entirely before it; a point inside
           an edit span clamps into the edit's zone. Approximate inside
           edits — used only for coverage marking, where inaccuracy can
           cost a region its "resolved" mark (it falls to the residual,
           which stays honest) but cannot mis-state anything. *)
        let shift_pt p =
          let delta = ref 0 in
          let result = ref None in
          List.iter
            (fun (ed : Matcher.edit) ->
              match !result with
              | Some _ -> ()
              | None ->
                  if p >= ed.end_byte then
                    delta :=
                      !delta
                      + String.length ed.replacement
                      - (ed.end_byte - ed.start_byte)
                  else if p > ed.start_byte then
                    (* inside the edit span: clamp into its zone *)
                    result :=
                      Some
                        (ed.start_byte + !delta
                        + min (p - ed.start_byte) (String.length ed.replacement)
                        ))
            edits;
          match !result with Some q -> q | None -> p + !delta
        in
        (* Same result as [Matcher.transform], without re-running the
           parse + match: the edits are already in hand. *)
        let t'' = Matcher.apply_edits si.si_before edits in
        let bt = Tree.parse ~ctx ~language t'' in
        let at = Tree.parse ~ctx ~language si.si_after in
        (* Well-formedness: a transform must produce parseable code. A
           removal-only rule that deletes a grammar-required
           sub-expression yields a broken intermediate ([const r = ;]);
           the re-diff over that ERROR-laden tree is unreliable, and it
           once judged such sites "fully explained" — letting a deletion
           rule out-cover the extraction rule and mis-state preserved
           values as deleted-then-readded. Every ERROR in [t''] must
           already exist in the before (pre-dates the rule; real corpora
           do contain the odd unparseable stretch) or in the after (the
           target state itself carries it) — by error text, position
           shifts aside. An error in neither endpoint is one the rule
           invented, and repairing rule-inflicted damage is not a
           residual's job. *)
        if
          not
            (multiset_covered
               (error_texts t'' bt.Tree.root)
               (si.si_before_errors @ error_texts si.si_after at.Tree.root))
        then no_fire
        else begin
          let d = Tree_diff.diff ~before:bt ~after:at in
          let remaining = changed_regions d in
          let exact =
            List.for_all
              (fun (rs, re, _) ->
                List.for_all
                  (fun (zs, ze) -> not (spans_overlap zs ze rs re))
                  zones)
              remaining
          in
          (* M1.9b decomposable: the rule's edits left a gap inside a landing
           zone, but [t''] is on the geodesic between before and after
           (design §2.3) — the rule's change plus the residual change
           compose to the site's change with no detour. Operationally:
           [t''] and the after must be *tree-inclusion comparable* (one
           obtainable from the other by node deletion alone,
           [Tree_inclusion]) — the residual is then a pure insertion
           ([t'' ⊑ after]: the rule under-wrote, e.g. an emptied
           dependency array the site fills) or a pure deletion
           ([after ⊑ t'']: the rule over-wrote through a metavariable,
           e.g. [g(x+1)] where the site keeps only [x]). A detour —
           writing a value in neither before nor after, [f→h] where the
           change is [f→g] — is a relabel, which inclusion forbids in
           both directions. The deletion direction additionally requires
           the deleted content to be before-derived (it reached [t'']
           through a metavariable binding, not an invented template
           literal whose insertion the residual would have to undo).
           Inclusion is checked on the whole file, so a site whose
           remaining gap mixes insertions and deletions — or overlaps
           another rule's pending region — is shed to its residual:
           conservative, honest, never unsafe.

           Inclusion alone validates only the *residual* leg. The rule's
           own leg needs the net-progress guard below: a rule may delete
           content the after still needs and let the residual re-add it —
           inclusion holds ([t'' ⊑ after], the re-add is "pure
           insertion") yet the delete-then-readd is wasted work off the
           geodesic. Soaks hit exactly this: a coarsened rule emptying a
           function body to [{}], whose residual re-inserts the whole
           body — safe by reconstruction, worse than the raw diff by
           size. The guard is the compactness half of the safety story
           (spdiff's largest *common* part, MDL): the in-zone gap the
           rule leaves must be strictly smaller than the change it
           explains, so claiming the site states the change more
           compactly than the raw hunk would. *)
          let net_progress =
            let extent (rs, re, txt) = re - rs + String.length txt in
            let gap =
              List.fold_left
                (fun a ((rs, re, _) as r) ->
                  if
                    List.exists
                      (fun (zs, ze) -> spans_overlap zs ze rs re)
                      zones
                  then a + extent r
                  else a)
                0 remaining
            in
            let explained =
              List.fold_left
                (fun a ((rs, re, _) as r) ->
                  if
                    List.exists
                      (fun (ed : Matcher.edit) ->
                        spans_overlap ed.start_byte ed.end_byte rs re)
                      edits
                  then a + extent r
                  else a)
                0 si.si_regions
            in
            gap < explained
          in
          let decomposable =
            (not exact) && net_progress
            && (Tree_inclusion.included_src ~sub:(t'', bt.Tree.root)
                  ~sup:(si.si_after, at.Tree.root)
               || Tree_inclusion.included_src
                    ~sub:(si.si_after, at.Tree.root)
                    ~sup:(t'', bt.Tree.root)
                  && List.for_all
                       (fun (rs, re, txt) ->
                         txt <> ""
                         || string_mem
                              ~sub:(String.sub t'' rs (re - rs))
                              si.si_before)
                       remaining)
          in
          if not (exact || decomposable) then no_fire
          else
            let resolved =
              (* a region is resolved iff some edit touches it and its
               t''-image carries no remaining change *)
              let idx = ref (-1) in
              List.filter_map
                (fun (rs, re, _) ->
                  incr idx;
                  let touched =
                    List.exists
                      (fun (ed : Matcher.edit) ->
                        spans_overlap ed.start_byte ed.end_byte rs re)
                      edits
                  in
                  if not touched then None
                  else
                    let rs' = shift_pt rs and re' = shift_pt re in
                    if
                      List.for_all
                        (fun (qs, qe, _) -> not (spans_overlap rs' re' qs qe))
                        remaining
                    then Some !idx
                    else None)
                si.si_regions
            in
            {
              ev_exact = exact;
              ev_decomposable = decomposable;
              ev_fires = List.length edits;
              ev_resolved = resolved;
              ev_clean =
                t'' = si.si_after || ws_collapse t'' = ws_collapse si.si_after;
              ev_overfire = false;
            }
        end
      end
  with
  | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
  | e ->
      (* A matcher/transform failure means the candidate does not safely
         apply here — treat as no fire. *)
      Cs_trace.trace "site_eval: %s\n%!" (Printexc.to_string e);
      no_fire

let site_safe ~ctx ~language ~pattern_text (si : site_info) : bool =
  (site_eval ~ctx ~language ~pattern_text si).ev_exact
