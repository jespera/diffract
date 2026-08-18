(** Change-summary selection (design §3.3): one tier of propose → evaluate →
    select. [tier_rules] assembles candidates from every channel ({!Cs_propose},
    {!Cs_cluster}, {!Cs_fusion}), evaluates each against the changeset's sites
    ({!Cs_evaluate} — the gate that defines a rule's meaning), and emits a
    greedy weighted set-cover over the changed regions. *)

open Cs_types
open Cs_pattern
open Cs_evaluate
open Cs_cluster
open Cs_propose
open Cs_fusion

(* A candidate pattern with its evaluated semantics (§3.3): the true
   extension (files where it fires safely, with each site's evaluation)
   and the behavioural support (total fires over the extension). *)
type scored_candidate = {
  sc_pattern : string;
  sc_language : string;
  sc_support : int;
  sc_extension : (string * site_evaluation) list;
}

(** PROPOSE: two-sided clusters from the dendrogram forest. Build one dendrogram
    per LHS (before-side) root node-type rather than one global tree.
    Anti-unification recurses on the before side, so two pairs can only merge
    coherently when their before-roots share a node-type; a before-root mismatch
    collapses to a root hole the cut discards. The AFTER side may differ freely
    — the shared hole binding reconciles it (the extraction case,
    [box(foo()).get()⤳foo()] ∧ [box(42).get()⤳42] → [box(_H0).get()⤳_H0]) — so
    we must NOT split on it. A single pre-grouped cluster skips the dendrogram.
*)
let propose_two_sided_clusters ~safe_instances
    ?(on_reject = fun (_ : edit_pat) (_ : instance list) -> ())
    (initial : cluster list) : cluster list =
  match initial with
  | [] -> []
  | [ c ] ->
      (* Single pre-grouped cluster — no dendrogram needed. Check
         coherence, safety, and min_size directly. *)
      if
        List.length c.instances >= Cs_config.default.min_support
        && has_concrete c.pattern.before
        && has_concrete c.pattern.after
        && has_concrete_edit c.pattern
        && no_junk_passthrough c.pattern
        && hole_frac c.pattern < Cs_config.default.max_hole_fraction
      then begin
        let safe = safe_instances c.pattern c.instances in
        if List.length safe >= Cs_config.default.min_support then
          [ respecialize { c with instances = safe } ]
        else []
      end
      else []
  | _ ->
      let process_bucket bucket =
        (* Bucket cap (see {!Cs_config.dendrogram_bucket_cap} and
           {!Cs_cluster.cap_bucket}): cluster a deterministic file-round-robin
           sample; evaluation extends whatever rules emerge to every site
           they fire at, and uncovered sites re-propose in later tiers. *)
        let bucket =
          cap_bucket
            ~file_of:(fun c ->
              match c.instances with i :: _ -> i.file | [] -> "")
            ~label:"dendrogram bucket" bucket
        in
        let root = build_dendrogram bucket in
        let clusters, _singletons =
          cut_dendrogram ~safe_instances ~on_reject 2 root
        in
        List.map respecialize clusters
      in
      let tbl = Hashtbl.create 16 in
      let order = ref [] in
      List.iter
        (fun c ->
          let s =
            fst (root_sig c.pattern)
            (* LHS root node-type only *)
          in
          match Hashtbl.find_opt tbl s with
          | Some l -> l := c :: !l
          | None ->
              Hashtbl.add tbl s (ref [ c ]);
              order := s :: !order)
        initial;
      List.rev !order
      |> List.concat_map (fun s ->
          process_bucket (List.rev !(Hashtbl.find tbl s)))

(* ── Anchoredness ────────────────────────────────────────────────
   Does the pattern state WHERE the edit applies? 1 iff the matching
   lines ([-] and context) carry at least two word tokens (metavars
   included — a binding is an anchor): [_H0.apiUrl ⤳ _H0.baseUrl],
   [legacyStore.fetch ⤳ store.get]. 0 for a single-token edit — a grep,
   not a rule ([apiUrl ⤳ baseUrl]). Under lexical matching bare forms
   fire freely, and the shorter-pattern tie-breaks would prefer them
   over the anchored statement of the same change; this bit, used BELOW
   coverage and support, breaks those ties toward the anchor. A bare
   form still wins when it explains more (a true global rename), and
   remains available as recall. Context FRAMES deliberately do not add
   a level: a frame that survives selection must pay for its length via
   coverage or the delta/metavar tie-breaks below — junk frames ([{]
   around an edit, [fun _H0() {]) otherwise beat the frameless statement
   of the same edit. Textual, like [match_side_specificity]. *)
let anchoredness pattern_text =
  let is_word c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '_'
  in
  let count_words l seed =
    let n = String.length l in
    let acc = ref seed in
    let i = ref 0 in
    while !i < n do
      if is_word l.[!i] then begin
        let j = ref !i in
        while !j < n && is_word l.[!j] do
          incr j
        done;
        incr acc;
        i := !j
      end
      else incr i
    done;
    !acc
  in
  let lines = String.split_on_char '\n' pattern_text in
  let ats = ref 0 in
  let words = ref 0 in
  List.iter
    (fun l ->
      if String.trim l = "@@" then incr ats
      else if !ats >= 2 && !ats mod 2 = 0 then
        let n = String.length l in
        if n = 0 then ()
        else if l.[0] = '+' then ()
        else if l.[0] = '-' then
          words := count_words (String.sub l 1 (n - 1)) !words
        else words := count_words l !words)
    lines;
  if !words >= 2 then 1 else 0

(* Row-7 tie-break (orthogonality): at equal coverage, support, and
   anchoredness, prefer the candidate whose edit changes FEWER tokens —
   the size of the symmetric difference between the [-] and [+] lines'
   word sets. A fused co-occurrence ([- priority="default"
   + variant="secondary"], four differing tokens) couples two axes that
   general rules state separately ([- _M0="default" + _M0="secondary"],
   two); the finer factoring wins its regions, and the coarser form
   survives only where the axes never occur apart. *)
let delta_token_count pattern_text =
  let is_word c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '_'
  in
  let words l seed =
    let n = String.length l in
    let acc = ref seed in
    let i = ref 0 in
    while !i < n do
      if is_word l.[!i] then begin
        let j = ref !i in
        while !j < n && is_word l.[!j] do
          incr j
        done;
        acc := String.sub l !i (!j - !i) :: !acc;
        i := !j
      end
      else incr i
    done;
    !acc
  in
  let lines = String.split_on_char '\n' pattern_text in
  let ats = ref 0 in
  let minus = ref [] in
  let plus = ref [] in
  List.iter
    (fun l ->
      if String.trim l = "@@" then incr ats
      else if !ats >= 2 && !ats mod 2 = 0 then
        let n = String.length l in
        if n = 0 then ()
        else if l.[0] = '-' then minus := words (String.sub l 1 (n - 1)) !minus
        else if l.[0] = '+' then plus := words (String.sub l 1 (n - 1)) !plus)
    lines;
  List.length (List.filter (fun w -> not (List.mem w !plus)) !minus)
  + List.length (List.filter (fun w -> not (List.mem w !minus)) !plus)

(* Row-6 tie-break (evidence-exactness): at equal coverage, support, and
   anchoredness, the pattern with FEWER metavars wins — [Notification<User>
   ⤳ Notification] over [_H0<User> ⤳ _H0]. A hole claims generality the
   corpus never witnessed; when the sites genuinely vary, the holed form
   has higher support and wins earlier. Counted from the declarations. *)
let metavar_count pattern_text =
  List.fold_left
    (fun n l ->
      match String.split_on_char ' ' (String.trim l) with
      | "metavar" :: _ -> n + 1
      | _ -> n)
    0
    (String.split_on_char '\n' pattern_text)

(** PROPOSE: pick one representative per change-family among the two-sided
    clusters. Multi-level emission hands us several clusters stating the same
    change at nested granularities (statement / declarator / member) with
    identical file sets; Jaccard would fuse them into one self-overlapping
    conjunctive whose sections collide at application time. Score by *evaluated*
    resolved regions over all changed files (semantics, not provenance — a
    tighter candidate whose instances were partly shed during clustering still
    resolves the same regions globally), preferring more resolved regions then
    shorter (tighter) pattern text. *)
let arbitrate_fusion_inputs ~eval_at ~all_files
    (two_sided_clusters : cluster list) : cluster list =
  let cluster_language (c : cluster) =
    match c.instances with i :: _ -> i.language | [] -> ""
  in
  let resolved_of (c : cluster) =
    let pattern_text = render_pattern_body c.pattern in
    let language = cluster_language c in
    all_files
    |> List.concat_map (fun f ->
        let e = eval_at ~language ~pattern_text f in
        List.map (fun i -> (f, i)) e.ev_resolved)
  in
  let scored =
    (* more resolved regions, then anchored over bare (see
       [has_anchoring_context]), then shorter pattern text *)
    List.map (fun c -> (c, resolved_of c)) two_sided_clusters
    |> List.sort (fun (a, ra) (b, rb) ->
        let anch c =
          let t = render_pattern_body c.pattern in
          (-anchoredness t, delta_token_count t, metavar_count t)
        in
        compare
          (-List.length ra, anch a, String.length (render_pattern_body a.pattern))
          (-List.length rb, anch b, String.length (render_pattern_body b.pattern)))
  in
  let claimed : (string * int, unit) Hashtbl.t = Hashtbl.create 32 in
  List.filter_map
    (fun (c, resolved) ->
      let fresh = List.filter (fun k -> not (Hashtbl.mem claimed k)) resolved in
      if fresh = [] then None
      else begin
        List.iter (fun k -> Hashtbl.replace claimed k ()) resolved;
        Some c
      end)
    scored

(* ── Application order ────────────────────────────────────────────
   The tier loop assigns ids in this list's order, and id order IS
   application order (the contract shared by apply_claiming, the
   residual pass, and the round-trip property). Order by SPECIFICITY —
   concrete match-side tokens, descending — so a specific rule applies
   before a broader one whose edits would consume its matches: at a
   file needing both [import android.arch.lifecycle._H0 ⤳ import
   androidx.lifecycle._H0] and the bare [- android + androidx] leaf
   flip (for an arch.core import the specific rule does not cover),
   broad-first flips [android] everywhere, kills the specific rule's
   matches, and forces the next tier to re-derive its content as an
   echo rule against the intermediate. Specific-first lets both fire.
   Support breaks ties (bigger first), then pattern text for
   determinism. *)

(* Concrete tokens on the match side: word tokens ([A-Za-z0-9_]+) of
   body lines that participate in matching (context and [-] lines, not
   [+] replacements), excluding declared metavar names. [-]-line tokens
   count DOUBLE: the matched-and-edited tokens are the rule's true
   discriminator, and a value-specific rewrite ([- priority="default"],
   edit tokens priority+default) must apply before a value-generic one
   ([<Button] ctx + [- priority=_H0]) whose rename would consume its
   match — with flat counts the two tie and support breaks the tie the
   wrong way, starving the specific rule in the application chain.
   Crude — token count, not tree size — but monotone in how much
   concrete anchoring the pattern brings to a match. *)
let match_side_specificity pattern_text =
  let lines = String.split_on_char '\n' pattern_text in
  let metavars = Hashtbl.create 8 in
  List.iter
    (fun l ->
      match String.split_on_char ' ' (String.trim l) with
      | "metavar" :: name :: _ ->
          let name =
            match String.index_opt name ':' with
            | Some i -> String.sub name 0 i
            | None -> name
          in
          Hashtbl.replace metavars name ()
      | _ -> ())
    lines;
  let is_word c =
    (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c = '_'
  in
  let count = ref 0 in
  let ats = ref 0 in
  List.iter
    (fun l ->
      if String.trim l = "@@" then incr ats
      else if !ats >= 2 && !ats mod 2 = 0 then
        if String.length l > 0 && l.[0] = '+' then ()
        else begin
          (* context or [-] line: count its concrete word tokens *)
          let n = String.length l in
          let is_minus = n > 0 && l.[0] = '-' in
          let weight = if is_minus then 2 else 1 in
          let i = ref (if is_minus then 1 else 0) in
          while !i < n do
            if is_word l.[!i] then begin
              let j = ref !i in
              while !j < n && is_word l.[!j] do
                incr j
              done;
              let w = String.sub l !i (!j - !i) in
              if not (Hashtbl.mem metavars w) then count := !count + weight;
              i := !j
            end
            else incr i
          done
        end)
    lines;
  !count

let sort_for_application (rules : rule list) : rule list =
  let scored =
    List.map (fun r -> (match_side_specificity r.pattern_text, r)) rules
  in
  List.sort
    (fun (sa, a) (sb, b) ->
      if sa <> sb then compare sb sa
      else if a.support <> b.support then compare b.support a.support
      else compare a.pattern_text b.pattern_text)
    scored
  |> List.map snd

(* ── Tier environment ─────────────────────────────────────────────
   The evaluation service every phase below shares: the parsed site
   database, the sorted file list, and the memoized per-(pattern, file)
   evaluator (§3.3 — the gate that defines a rule's meaning). PROPOSE's
   internal gates (the dendrogram cut, swap fusion, removal regrouping)
   and EVALUATE share the memo, so proposer-side checks pre-warm the
   evaluator. *)
type tier_env = {
  ctx : Context.t;
  site_db : (string, site_info) Hashtbl.t;
  all_files : string list;  (** [site_db]'s files, sorted *)
  eval_at : language:string -> pattern_text:string -> string -> site_evaluation;
}

let make_tier_env ~ctx (cs : changeset) : tier_env =
  let site_db = build_site_db ~ctx cs in
  let eval_cache : (string * string, site_evaluation) Hashtbl.t =
    Hashtbl.create 256
  in
  (* Literal prefilter (cost control on the gate): a pattern can only fire
     in a file whose bytes contain every one of its required literals
     ({!Matcher.required_literals} — the same sound necessary condition the
     CLI's directory scans use), and [site_eval] returns exactly [no_fire]
     when a pattern produces no edits, so skipping on an absent literal IS
     the evaluation's result, computed by a few substring scans instead of
     a matching walk. Only no-fire evaluations are skippable (a firing
     pattern's literals are present by definition); measured across real
     corpora that is 27–83% of gate evaluations depending on how much the
     callers' own scoping already prunes. A pattern whose literal
     extraction fails gets no prefilter — the evaluation handles it as
     before. *)
  let lits_cache : (string * string, string list) Hashtbl.t =
    Hashtbl.create 64
  in
  let literals_for ~language ~pattern_text =
    let key = (language, pattern_text) in
    match Hashtbl.find_opt lits_cache key with
    | Some lits -> lits
    | None ->
        let lits =
          try Matcher.required_literals ~ctx ~language ~pattern_text with
          | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
          | _ -> []
        in
        Hashtbl.add lits_cache key lits;
        lits
  in
  (* CS_TRACE heartbeat: cache-miss gate evaluations are the pipeline's
     unit of real work (matcher + reparse + rediff per call), so count
     them and tick every 100 — a run that goes quiet for minutes tells
     you which phase is churning through them. Prefilter skips are not
     counted: they do none of that work. *)
  let misses = ref 0 in
  let t0 = Unix.gettimeofday () in
  let eval_at ~language ~pattern_text file =
    if language = "" then no_fire
    else
      let key = (pattern_text, file) in
      match Hashtbl.find_opt eval_cache key with
      | Some e -> e
      | None ->
          let e =
            match Hashtbl.find_opt site_db file with
            | None -> no_fire
            | Some si ->
                if si.si_language <> language then no_fire
                else if
                  not
                    (Matcher.source_may_match
                       ~literals:(literals_for ~language ~pattern_text)
                       si.si_before)
                then no_fire
                else begin
                  incr misses;
                  if !misses mod 100 = 0 then
                    Cs_trace.trace
                      "  eval_at: %d gate evaluations, elapsed %.1fs\n%!"
                      !misses
                      (Unix.gettimeofday () -. t0);
                  site_eval ~ctx ~language ~pattern_text si
                end
          in
          Hashtbl.add eval_cache key e;
          e
  in
  let all_files =
    Hashtbl.fold (fun k _ acc -> k :: acc) site_db []
    |> List.sort String.compare
  in
  { ctx; site_db; all_files; eval_at }

let pattern_safe_at (env : tier_env) ~language ~pattern_text file =
  (* M1.9b/c: a decomposable site is safely explained too (geodesic), so
     it counts when shaping clusters — without it a coarsened candidate's
     own decomposable instances would be shed and the cluster dissolve. *)
  let e = env.eval_at ~language ~pattern_text file in
  e.ev_exact || e.ev_decomposable

let safe_instances (env : tier_env) ep (insts : instance list) =
  let pattern_text = render_pattern_body ep in
  List.filter
    (fun (i : instance) ->
      pattern_safe_at env ~language:i.language ~pattern_text i.file)
    insts

let lang_of (c : cluster) =
  match c.instances with i :: _ -> i.language | [] -> ""

let trace_initial_histogram raw initial =
  if Cs_trace.on () then begin
    Printf.eprintf "initial emissions: %d, clusters after pre-group: %d\n%!"
      (List.length raw) (List.length initial);
    let buckets = [| 0; 0; 0; 0; 0; 0; 0 |] in
    let bucket_of n =
      if n <= 5 then 0
      else if n <= 10 then 1
      else if n <= 20 then 2
      else if n <= 40 then 3
      else if n <= 80 then 4
      else if n <= 160 then 5
      else 6
    in
    List.iter
      (fun c ->
        let s = edit_size c.pattern in
        buckets.(bucket_of s) <- buckets.(bucket_of s) + 1)
      initial;
    Printf.eprintf
      "size hist (edit_size before+after): <=5:%d <=10:%d <=20:%d <=40:%d \
       <=80:%d <=160:%d >160:%d\n\
       %!"
      buckets.(0) buckets.(1) buckets.(2) buckets.(3) buckets.(4) buckets.(5)
      buckets.(6)
  end

(** PROPOSE: §3.2 delta-keyed candidates, pooled by exact pattern identity only
    (no dendrogram participation), gated like any cluster. A pool of ≥ 2
    identical scope-holed pairs is a delta whose support spans anchors;
    evaluation later extends it to every file it fires in. *)
let propose_delta_pooled ?(label = "delta-keyed") (env : tier_env)
    (delta_raw : cluster list) : cluster list =
  (* Re-specialized twin of a pool: anti-unify the pool's instances' own
     [ipat]s — holes survive only where the sites actually differ, every
     position they agree on comes back concrete. For the plain delta channel
     this is the identity ([ipat] is the holed pattern itself); for the deep
     chain variants [ipat] is the site's concrete pattern, so the twin is the
     readable, evidence-exact form of the pooled key ("abstract only the
     parts of the context that vary"). Both are kept as candidates: where
     concreteness breaks the §3.2 re-parse the twin dies at the gate and the
     holed key survives; where both fire, application-order specificity
     prefers the concrete-richer twin. *)
  let with_twins (pools : cluster list) : cluster list =
    List.concat_map
      (fun (c : cluster) ->
        match c.instances with
        | [] | [ _ ] -> [ c ]
        | i :: rest ->
            let twin =
              List.fold_left (fun acc j -> anti_unify_edits acc j.ipat) i.ipat
                rest
            in
            if twin = c.pattern then [ c ]
            else if
              has_concrete twin.before && has_concrete_edit twin
              && no_orphan_after_holes twin
              && no_junk_passthrough twin
            then [ c; { c with pattern = twin } ]
            else [ c ])
      pools
  in
  let delta_clusters =
    pre_group_identical delta_raw |> with_twins
    |> List.filter (fun c ->
        let enough = List.length c.instances >= Cs_config.default.min_support in
        if (not enough) && Cs_trace.on () then
          Printf.eprintf "%s: pool of %d below floor:\n%s\n---\n%!" label
            (List.length c.instances)
            (render_pattern_body c.pattern);
        enough)
    |> List.filter_map (fun c ->
        let safe = safe_instances env c.pattern c.instances in
        if List.length safe >= Cs_config.default.min_support then
          Some { c with instances = safe }
        else begin
          if Cs_trace.on () then
            Printf.eprintf
              "%s: pool gate-shed (%d -> %d safe):\n%s\n---\n%!" label
              (List.length c.instances) (List.length safe)
              (render_pattern_body c.pattern);
          None
        end)
  in
  if Cs_trace.on () then
    Cs_trace.trace "%s: %d raw, %d pooled+safe\n%!" label
      (List.length delta_raw)
      (List.length delta_clusters);
  delta_clusters

(** PROPOSE: AU-intersection candidates — shared sub-edits mined from the
    dendrogram merges the coherence cut rejected. A rejected merge's pattern is
    the aligned intersection of every instance beneath it; a concrete
    before/after divergence inside it is a delta all of them share, buried
    under the per-file holes that sank the merge. {!Cs_pattern.extract_components}
    pulls those out at every enclosing context level; here each component
    becomes a cluster carrying the rejected node's instances (its evidence —
    every one of them exhibits the component, by anti-unification), identical
    components pool across nodes, and the pools are gated like any cluster.
    Support and sites are still evaluation's to decide (§3.3) — instances only
    seed [cand_files]. Instance [ipat] is the component itself (the delta-keyed
    precedent): re-specialization over survivors is then the identity, which is
    right — the component is already exactly as general as its evidence. *)
let propose_intersection (env : tier_env) ~(base : cluster list)
    (mined : (edit_pat * instance list) list) : cluster list =
  (* Redundancy suppression: mining is a recall channel. A base cluster
     that is itself single-delta already carries that delta with
     dendrogram-vetted context; re-proposing the same delta at other
     context levels adds nothing the gate can distinguish on evidence,
     but the extra variants perturb family arbitration and application
     order (the [tsx_remap_overfire_bait] shape). A delta only part of a
     multi-delta base rule is NOT suppressed — the composite fires only
     where all its parts co-occur, so the standalone delta still buys
     recall at partial sites. *)
  let suppressed : (edit_pat, unit) Hashtbl.t = Hashtbl.create 16 in
  List.iter
    (fun (c : cluster) ->
      match minimal_deltas c.pattern with
      | [ d ] -> Hashtbl.replace suppressed d ()
      | _ -> ())
    base;
  let redundant ep =
    match minimal_deltas ep with
    | [ d ] -> Hashtbl.mem suppressed d
    | _ -> false
  in
  (* Structural predicates only — deliberately no hole-fraction cut, the
     delta-channel precedent: an identified component passes per-file
     content through as holes, so its fraction is inherently high, and
     the pattern is exactly as general as its cross-instance evidence.
     Volume is bounded by the per-node cap and identity pooling; meaning
     is decided by the evaluation gate. *)
  let coherent ep =
    has_concrete ep.before && has_concrete_edit ep && no_orphan_after_holes ep
    && no_junk_passthrough ep
  in
  let take n l =
    let rec go n acc = function
      | x :: rest when n > 0 -> go (n - 1) (x :: acc) rest
      | _ -> List.rev acc
    in
    go n [] l
  in
  let raw =
    List.concat_map
      (fun (pat, insts) ->
        extract_components pat
        |> List.filter (fun ep -> coherent ep && not (redundant ep))
        |> take Cs_config.default.intersection_components_cap
        |> List.map (fun ep ->
            {
              pattern = ep;
              instances = List.map (fun i -> { i with ipat = ep }) insts;
            }))
      mined
  in
  let clusters =
    pre_group_identical raw
    |> List.map (fun c ->
        (* Nested rejected merges share instances (a leaf sits under every
           rejected ancestor), so pooling duplicates sites — dedupe on
           identity before support counts anything. *)
        let instances =
          List.sort_uniq
            (fun (a : instance) (b : instance) ->
              compare
                (a.file, a.site_start, a.site_end)
                (b.file, b.site_start, b.site_end))
            c.instances
        in
        { c with instances })
    |> List.filter (fun c ->
        List.length c.instances >= Cs_config.default.min_support)
    |> List.filter_map (fun c ->
        let safe = safe_instances env c.pattern c.instances in
        if List.length safe >= Cs_config.default.min_support then
          Some { c with instances = safe }
        else begin
          if Cs_trace.on () then
            Printf.eprintf
              "intersection: pool gate-shed (%d -> %d safe):\n%s\n---\n%!"
              (List.length c.instances) (List.length safe)
              (render_pattern_body c.pattern);
          None
        end)
  in
  if Cs_trace.on () then begin
    Cs_trace.trace "intersection: %d rejected merges, %d pooled+safe\n%!"
      (List.length mined) (List.length clusters);
    List.iter
      (fun c ->
        Printf.eprintf "intersection: pooled+safe (%d insts):\n%s\n---\n%!"
          (List.length c.instances)
          (render_pattern_body c.pattern))
      clusters
  end;
  clusters

(* ── The anchored stream (§3.2) ───────────────────────────────────
   Anchored variants: support pools on the DELTA. A delta whose distinct
   sites number ≥ 2 may realise as per-site anchored rules of support 1 —
   they are exempted from the min-support thresholds in EVALUATE and
   SELECT (the pool carries the support, the anchors are its site-local
   realisations). Selection's greedy set-cover provides the
   lattice-descent pruning: a general candidate that already covers a
   region wins it first (higher marginal, shorter pattern), so an
   anchored rule is only ever selected for regions no more-general safe
   candidate claims.

   Realisations are gated and evaluated LAZILY: only after round 1, and
   only for delta pools where round 1 left a site uncovered (the
   descent's pruning, made cheap — an anchored rule can never be selected
   over a region a general rule already covered, so there is no point
   safety-checking or evaluating it there). [propose_anchored] only does
   the cheap bookkeeping — the pool's distinct sites, and the exempt
   marking that lowers a pooled delta's min-support to 1 — and
   [live_anchored_candidates] defers the gating to after round 1.

   These clusters are NOT part of the two-sided stream: in fusion-input
   arbitration they would claim regions and knock the general rules out
   of candidacy, and in Jaccard grouping they would fuse into spurious
   conjunctives. *)
type anchored_stream = {
  an_pooled : (string * (int * int) * cluster) list;
      (** (delta key, delta span, realisation), pools ≥ min_support only;
          general merged realisations appended *)
  an_grouped : (string * cluster) list;
      (** identical realisations grouped, keyed by surgical render *)
  an_exempt : (string * string, int * int * string) Hashtbl.t;
      (** (pattern text, language) → (headed flag: 0 for a bare-delimiter root,
          concrete node count, delta needle). Membership = min-support
          exemption; the counts are round 2's generality tie-break (prefer
          head-anchored over bare-delimiter, then fewest concrete nodes); the
          needle (the delta's first before-side text) prefilters evaluation — an
          anchored realisation cannot fire in a file that does not even contain
          its delta text. *)
  an_pool_sites : (string * string, (string * int * int) list ref) Hashtbl.t;
      (** (language, delta key) → the pool's distinct home sites *)
}

let propose_anchored (anchored_raw : (string * (int * int) * cluster) list) :
    anchored_stream =
  let exempt : (string * string, int * int * string) Hashtbl.t =
    Hashtbl.create 16
  in
  let pool_sites : (string * string, (string * int * int) list ref) Hashtbl.t =
    Hashtbl.create 32
  in
  List.iter
    (fun (key, (ds, de), c) ->
      List.iter
        (fun (i : instance) ->
          let pk = (i.language, key) in
          let site = (i.file, ds, de) in
          match Hashtbl.find_opt pool_sites pk with
          | Some l -> if not (List.mem site !l) then l := site :: !l
          | None -> Hashtbl.add pool_sites pk (ref [ site ]))
        c.instances)
    anchored_raw;
  let anchored_pooled =
    List.filter
      (fun (key, _, c) ->
        match Hashtbl.find_opt pool_sites (lang_of c, key) with
        | Some l -> List.length !l >= Cs_config.default.min_support
        | None -> false)
      anchored_raw
  in
  (* §3.2 anchor generalization: distinct realisations of one pooled delta
     that share a structural shape and differ only in named anchor leaves
     anti-unify into the pool's general realisation (e.g. the holed-head JSX
     rule). It joins the same stream — exempt marking, delta-needle
     prefilter, lazy round-2 gating — where the fewest-concrete-nodes
     tie-break prefers it exactly where it is safe, and re-specialization
     collapses the hole back to a literal wherever the surviving sites do
     not vary. *)
  let anchored_pooled =
    let groups :
        ( string * string * string,
          (string * (int * int) * cluster) list ref )
        Hashtbl.t =
      Hashtbl.create 32
    in
    List.iter
      (fun ((key, _, c) as entry) ->
        let gk = (lang_of c, key, anchor_shape_key c.pattern) in
        match Hashtbl.find_opt groups gk with
        | Some l -> l := entry :: !l
        | None -> Hashtbl.add groups gk (ref [ entry ]))
      anchored_pooled;
    let merged =
      Hashtbl.fold
        (fun _ entries acc ->
          let distinct =
            List.sort_uniq compare
              (List.map (fun (_, _, c) -> c.pattern) !entries)
          in
          if List.length distinct < 2 then acc
          else
            match
              generalize_realisations (List.map (fun (_, _, c) -> c) !entries)
            with
            | Some c -> (
                match !entries with
                | (key, span, _) :: _ -> (key, span, c) :: acc
                | [] -> acc)
            | None -> acc)
        groups []
    in
    Cs_trace.trace "anchor-generalize: %d merged realisation(s)\n%!"
      (List.length merged);
    anchored_pooled @ merged
  in
  let needle_of_pat =
    (* Identical patterns carry identical delta keys; the needle (the
       delta's first before-side text) prefilters evaluation. *)
    let key_of_pat : (edit_pat, string) Hashtbl.t = Hashtbl.create 32 in
    List.iter
      (fun (key, _, c) -> Hashtbl.replace key_of_pat c.pattern key)
      anchored_pooled;
    fun p ->
      match Hashtbl.find_opt key_of_pat p with
      | None -> ""
      | Some key -> (
          match String.index_opt key '\x00' with
          | Some i -> String.sub key 0 i
          | None -> (
              match String.index_opt key '\x01' with
              | Some i -> String.sub key 0 i
              | None -> ""))
  in
  (* The grouped anchored clusters (surgical-rendered) and their exempt
     marks. Computed without gating — gating is deferred to
     [live_anchored_candidates] — but the exempt marks are populated now so
     that a general candidate textually coinciding with an anchored
     realisation is treated as exempt (round 2) exactly as before. *)
  let anchored_grouped =
    pre_group_identical (List.map (fun (_, _, c) -> c) anchored_pooled)
    |> List.map (fun c -> (render_pattern_body_surgical c.pattern, c))
  in
  List.iter
    (fun (pattern_text, c) ->
      Hashtbl.replace exempt
        (pattern_text, lang_of c)
        ( (if bare_bracket_root c.pattern then 0 else 1),
          edit_size c.pattern - edit_holes c.pattern,
          needle_of_pat c.pattern ))
    anchored_grouped;
  {
    an_pooled = anchored_pooled;
    an_grouped = anchored_grouped;
    an_exempt = exempt;
    an_pool_sites = pool_sites;
  }

(** PROPOSE: fuse Removed×Added one-sided cluster pairs into swaps, then
    safety-gate them like any other two-sided rule: shed unsafe sites, drop the
    swap when fewer than two fires (removed-side instances) survive. *)
let gate_swap_pairs (env : tier_env) pairs =
  List.filter_map (fun (r, a) -> fuse_swap r a) pairs
  |> List.filter_map (fun (ep, insts) ->
      let pattern_text = render_pattern_body ep in
      let safe =
        List.filter
          (fun (i : one_sided_instance) ->
            pattern_safe_at env ~language:i.os_language ~pattern_text i.os_file)
          insts
      in
      let fires =
        List.length (List.filter (fun i -> i.os_side = Before_side) safe)
      in
      if fires >= Cs_config.default.min_support then Some (ep, safe) else None)

(* ── Candidate registry ───────────────────────────────────────────
   Dedupes candidates on (pattern text, language) and preserves
   first-proposal order — the order EVALUATE runs in, on which
   selection's determinism rests. *)
type registry = {
  reg_tbl : (string * string, unit) Hashtbl.t;
  mutable reg_rev_order : (string * string) list;
}

let reg_create () = { reg_tbl = Hashtbl.create 32; reg_rev_order = [] }

let reg_add reg ~language pattern_text =
  if language <> "" then begin
    let key = (pattern_text, language) in
    if not (Hashtbl.mem reg.reg_tbl key) then begin
      Hashtbl.add reg.reg_tbl key ();
      reg.reg_rev_order <- key :: reg.reg_rev_order
    end
  end

let reg_mem reg key = Hashtbl.mem reg.reg_tbl key
let reg_ordered reg = List.rev reg.reg_rev_order

(** PROPOSE: removal-only clusters that did not pair with an Added cluster in
    M1.6 fusion: their (possibly concretely regrouped) [-]-only bodies.
    Addition-only clusters are not proposed — a [+]-only block has no anchor to
    apply; their changes fall to residuals. *)
let register_removal_candidates (env : tier_env) reg os_clusters pairs =
  let used_removeds = List.map fst pairs in
  List.iter
    (fun c ->
      if c.os_cluster_side = Before_side && not (List.memq c used_removeds) then
        List.iter
          (fun (pattern_text, (insts : one_sided_instance list)) ->
            let language =
              match insts with i :: _ -> i.os_language | [] -> ""
            in
            reg_add reg ~language pattern_text)
          (safe_removal_groups ~ctx:env.ctx ~site_db:env.site_db c))
    os_clusters

(** PROPOSE: declaration-anchored field candidates: re-anchor a two-sided
    cluster's change under its enclosing declaration
    (cs_pattern.build_anchored_decl), rendered as a [match: field] rule. A clean
    but context-stripped body rule ([{ return _H } ⤳ = _H]) over-fires on every
    single-return block, so it fails the placement gate on files where some such
    block was not converted and fragments into signature-anchored pieces; its
    anchored form ([fun _Hf(...) { return _H } ⤳ = _H]) fires only on
    declarations and is safe everywhere. Added as ADDITIONAL candidates:
    selection keeps the bare rule wherever it covers as much (the anchored form
    has more concrete nodes and longer text, so it loses the tie-break) and
    prefers the anchored form only over regions the bare rule's over-fire leaves
    uncovered.

    Gate on over-fire: anchoring pays off exactly when the bare rule fires on
    code the changeset did not touch. A rule already safe everywhere needs no
    anchor, and anchoring it would add a field candidate — expensive to evaluate
    — that only ever loses selection (this keeps corpora without the pattern,
    e.g. gen3, byte-identical and fast). The check reuses the [eval_at] cache
    the bare pattern's general-candidate evaluation populates, so it costs no
    extra parses.

    Returns the field candidates' evaluation scopes: an anchored field candidate
    can only fire in files where its source cluster's bare pattern already
    produces edits (safely or as an over-fire); a file with no such block cannot
    host [fun _Hf(...) { ... }]. So each field candidate's evaluation is scoped
    to that file set rather than the whole changeset — field-mode matching is
    expensive, and evaluating every anchored candidate against all N files is
    what makes the channel blow up on a large corpus (measured on a ~4k-file
    real codebase). *)
let register_field_candidates (env : tier_env) reg
    (base_two_sided : cluster list) : (string * string, string list) Hashtbl.t =
  let field_cand_files : (string * string, string list) Hashtbl.t =
    Hashtbl.create 16
  in
  let anchored_count = ref 0 in
  List.iter
    (fun (c : cluster) ->
      let language = lang_of c in
      if language = "" then ()
      else begin
        (* Evaluate the cluster's (realigned — a misaligned body pair renders
           as an unapplicable orphan) bare pattern once across all files:
           record whether it OVER-FIRES (fires outside a changed region — the
           symptom that a declaration anchor pays off) and the files where it
           produces any edits (the only files the field rule can fire in). *)
        let bare = render_pattern_body (realign_orphan_holes c.pattern) in
        let overfires = ref false in
        let fire_files = ref [] in
        List.iter
          (fun f ->
            let e = env.eval_at ~language ~pattern_text:bare f in
            if e.ev_overfire then overfires := true;
            if e.ev_fires > 0 || e.ev_overfire then
              fire_files := f :: !fire_files)
          env.all_files;
        if not !overfires then ()
        else
          (* [all_files] is sorted; [fire_files] was prepended, so reverse it
             back to sorted order — the field rule's site list then matches
             the order every other candidate produces. *)
          let files = List.rev !fire_files in
          let seen = Hashtbl.create 4 in
          c.instances
          |> List.filteri (fun i _ -> i < Cs_config.default.anchor_sample)
          |> List.iter (fun (inst : instance) ->
              match
                try
                  let t =
                    Tree.parse ~ctx:env.ctx ~language:inst.language
                      inst.before_full_source
                  in
                  match
                    find_enclosing_parent t.Tree.root inst.site_start
                      inst.site_end
                  with
                  | None -> None
                  | Some parent ->
                      build_anchored_decl inst.before_full_source parent
                        ~body_start:inst.site_start ~body_end:inst.site_end
                        c.pattern
                with
                | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
                | _ -> None
              with
              | None -> ()
              | Some ep ->
                  let txt = render_pattern_body_field ep in
                  if not (Hashtbl.mem seen txt) then begin
                    Hashtbl.replace seen txt ();
                    incr anchored_count;
                    reg_add reg ~language txt;
                    (* [reg_add] dedupes by (txt, language), so a txt shared
                       by two clusters is evaluated once. Its scope must
                       therefore be the UNION of both clusters' fire files —
                       a plain replace would drop the earlier cluster's sites
                       to residuals. Merge and re-sort so the scope stays
                       [all_files]-ordered. *)
                    let prev =
                      match
                        Hashtbl.find_opt field_cand_files (txt, language)
                      with
                      | Some fs -> fs
                      | None -> []
                    in
                    let merged =
                      let seen_f = Hashtbl.create 16 in
                      List.iter (fun f -> Hashtbl.replace seen_f f ()) prev;
                      List.iter (fun f -> Hashtbl.replace seen_f f ()) files;
                      List.filter (fun f -> Hashtbl.mem seen_f f) env.all_files
                    in
                    Hashtbl.replace field_cand_files (txt, language) merged
                  end)
      end)
    base_two_sided;
  if Cs_trace.on () then
    Printf.eprintf "anchored field candidates: %d\n%!" !anchored_count;
  field_cand_files

(* ── EVALUATE (§3.3): a candidate's true extension ──────────────── *)
let eval_candidate (env : tier_env) ~(anchored : anchored_stream)
    ~field_cand_files (pattern_text, language) : scored_candidate option =
  (* Needle prefilter for anchored realisations: skip files that do not
     contain the delta's before text — the pattern cannot fire there, and
     exempt candidates are numerous enough that evaluating them everywhere
     dominates runtime. *)
  let needle =
    match Hashtbl.find_opt anchored.an_exempt (pattern_text, language) with
    | Some (_, _, n) when n <> "" -> Some n
    | _ -> None
  in
  let file_plausible f =
    match needle with
    | None -> true
    | Some n -> (
        match Hashtbl.find_opt env.site_db f with
        | Some si -> Matcher.source_may_match ~literals:[ n ] si.si_before
        | None -> true)
  in
  (* Anchored field candidates are scoped to the files their source
     cluster's bare pattern touches (see [register_field_candidates]); all
     other candidates see the whole changeset. *)
  let files =
    match Hashtbl.find_opt field_cand_files (pattern_text, language) with
    | Some fs -> fs
    | None -> env.all_files
  in
  let extension =
    List.filter_map
      (fun f ->
        if not (file_plausible f) then None
        else
          let e = env.eval_at ~language ~pattern_text f in
          (* M1.9b: a decomposable site fires safely (geodesic) and counts
             toward support; its in-zone gap becomes a [rule=]-attributed
             residual. *)
          if (e.ev_exact || e.ev_decomposable) && e.ev_fires > 0 then Some (f, e)
          else None)
      files
  in
  let support = List.fold_left (fun a (_, e) -> a + e.ev_fires) 0 extension in
  (* Anchored realisations of a pooled delta carry the pool's support;
     their own floor is 1 (§3.2 lattice descent). *)
  let floor =
    if Hashtbl.mem anchored.an_exempt (pattern_text, language) then 1
    else Cs_config.default.min_support
  in
  if support < floor then None
  else
    Some
      {
        sc_pattern = pattern_text;
        sc_language = language;
        sc_support = support;
        sc_extension = extension;
      }

(* ── SELECT (§3.3): greedy set-cover over changed regions ─────────
   A candidate's marginal value is the number of still-uncovered
   (file, region) pairs it resolves; it is eligible while that marginal is
   at least [floor]. Reported support stays the global fire count over the
   full extension. Ties break to higher support, then shorter pattern text
   (the tighter statement), then text for determinism. Subsumption is
   inherent: a candidate resolving only covered regions is never
   selected. *)
let select_round ~(anchored : anchored_stream) ~covered ~selected pool floor =
  let marginal sc =
    List.fold_left
      (fun a (f, e) ->
        a
        + List.length
            (List.filter
               (fun i -> not (Hashtbl.mem covered (f, i)))
               e.ev_resolved))
      0 sc.sc_extension
  in
  let remaining = ref pool in
  let picking = ref true in
  while !picking do
    let best =
      List.fold_left
        (fun acc sc ->
          let m = marginal sc in
          if m < floor then acc
          else
            (* Among candidates covering the same marginal regions, prefer
               the one that reconstructs its sites with no residual
               (clean) — e.g. an extraction [box($H).get() ⤳ $H] over a
               bare removal [box($H).get()] that deletes and defers the
               rest to a residual. Then higher support, then shorter
               pattern text, then text. *)
            let clean =
              List.length
                (List.filter (fun (_, e) -> e.ev_clean) sc.sc_extension)
            in
            (* Generality tie-break (round 2): among anchored realisations
               with equal coverage, prefer the one with the FEWEST concrete
               nodes — the most general safe anchor, no site junk.
               Non-exempt candidates score 0 (the best), so round 1 is
               unaffected. *)
            let headed, concrete =
              match
                Hashtbl.find_opt anchored.an_exempt
                  (sc.sc_pattern, sc.sc_language)
              with
              | Some (k, c, _) -> (k, c)
              | None -> (0, 0)
            in
            (* Headed anchors ([import { ... } from './x'] vs a bare
               [{ ... }]) rank ABOVE fewest-concrete: the head names the
               construct and is anchoring knowledge, not site junk — a
               bare delimiter pattern over-matches every other list shape
               the corpus happens not to contain. Candidates that are
               equally headed (the common case) fall through to
               fewest-concrete exactly as before. *)
            let anchored_ctx =
              (* below support, above shortness: an anchored statement of
                 the change beats the frameless/bare forms lexical
                 matching lets fire (see [anchoredness]) — but only when
                 it explains just as much. A bare rename that covers more
                 occurrences (a true global rename) still wins on
                 support. *)
              anchoredness sc.sc_pattern
            in
            (* Support enters twice. FILE COUNT (extension size) ranks
               high: a family spanning more files is a stronger rule, and
               it is grep-robust — under lexical matching a bare token
               rule fires at every occurrence (inflating its FIRES count
               with hits inside regions other rules already explain) but
               reaches no more files than the anchored statement of the
               same change, so they tie here and the shape tie-breaks
               below decide. Raw fires ([sc_support]) drops to a late
               tie-break. Explaining genuinely more is [m]'s job, first
               as always. *)
            let key =
              ( m,
                clean,
                List.length sc.sc_extension,
                anchored_ctx,
                -delta_token_count sc.sc_pattern,
                (* fires above fewest-metavars: a hole whose sites vary
                   fires more, and that generality is earned ([priority=_H0]
                   over the per-value [priority="primary"]); at equal fires
                   the hole is unwitnessed and the concrete form wins
                   (row 6 — [Notification<User>] over [_H0<User>]). *)
                sc.sc_support,
                -metavar_count sc.sc_pattern,
                headed,
                -concrete,
                -String.length sc.sc_pattern,
                sc.sc_pattern )
            in
            match acc with
            | Some (bkey, _) when bkey >= key -> acc
            | _ -> Some (key, sc))
        None !remaining
    in
    match best with
    | None -> picking := false
    | Some (_, sc) ->
        selected := sc :: !selected;
        remaining := List.filter (fun x -> x != sc) !remaining;
        List.iter
          (fun (f, e) ->
            List.iter (fun i -> Hashtbl.replace covered (f, i) ()) e.ev_resolved)
          sc.sc_extension;
        if Cs_trace.on () then
          Printf.eprintf "  selected: support=%d %S\n%!" sc.sc_support
            (String.sub sc.sc_pattern 0 (min 60 (String.length sc.sc_pattern)))
  done

(* ── lazy descent ── Round 1 has marked the regions it covers. Now gate
   and evaluate anchored realisations, but ONLY for delta pools where some
   home site is still uncovered: an anchored rule whose every site is
   already covered would have marginal 0 and never be selected, so
   safety-checking and evaluating it would be wasted work. Returns the
   live realisations as candidates, deduped (via [reg]) against the
   general ones already proposed. *)
let region_covered (env : tier_env) ~covered file ds de =
  match Hashtbl.find_opt env.site_db file with
  | None -> false
  | Some si ->
      let idx = ref (-1) in
      List.iteri
        (fun i (rs, re, _) ->
          if !idx < 0 && rs <= ds && de <= re then idx := i)
        si.si_regions;
      if !idx < 0 then
        List.iteri
          (fun i (rs, re, _) ->
            if !idx < 0 && spans_overlap ds de rs re then idx := i)
          si.si_regions;
      !idx >= 0 && Hashtbl.mem covered (file, !idx)

let live_anchored_candidates (env : tier_env) ~(anchored : anchored_stream)
    ~covered reg : (string * string) list =
  let region_covered = region_covered env ~covered in
  let pool_live key lang =
    match Hashtbl.find_opt anchored.an_pool_sites (lang, key) with
    | Some l -> List.exists (fun (f, ds, de) -> not (region_covered f ds de)) !l
    | None -> false
  in
  let live_pats =
    (* surgical pattern texts whose pool round 1 left partly uncovered *)
    let live = Hashtbl.create 32 in
    List.iter
      (fun (key, _, c) ->
        if pool_live key (lang_of c) then
          Hashtbl.replace live (render_pattern_body_surgical c.pattern) ())
      anchored.an_pooled;
    live
  in
  let anchored_clusters =
    List.filter_map
      (fun (pattern_text, c) ->
        if not (Hashtbl.mem live_pats pattern_text) then None
        else
          let safe =
            List.filter
              (fun (i : instance) ->
                pattern_safe_at env ~language:i.language ~pattern_text i.file)
              c.instances
          in
          if safe = [] then begin
            if Cs_trace.on () then
              Printf.eprintf "anchored: gate rejected everywhere:\n%s\n---\n%!"
                pattern_text;
            None
          end
          else Some (pattern_text, { c with instances = safe }))
      anchored.an_grouped
  in
  if Cs_trace.on () then
    Printf.eprintf "anchored: %d pooled, %d live+safe (gated lazily)\n%!"
      (List.length anchored.an_grouped)
      (List.length anchored_clusters);
  List.filter_map
    (fun ((pattern_text, c) : string * cluster) ->
      let language = lang_of c in
      if language <> "" && not (reg_mem reg (pattern_text, language)) then begin
        reg_add reg ~language pattern_text;
        Some (pattern_text, language)
      end
      else None)
    anchored_clusters

(* ── lazy deep chains ── The deep delta variants ({!Cs_propose.delta_keyed_deep})
   are anchored context-chain forms of a delta. Where round 1 already covers a
   region, they would only re-factor it — and observedly worse: they preempt
   the anchored stream's ellipsis-context realisations and displace
   arity-general rules with clunkier context-chain forms (a decorator-property
   insertion, a constructor-parameter type rename). So, like the anchored
   realisations, they are RECALL: gated and evaluated only when some instance
   sits in a region round 1 left uncovered, and selected in round 2. *)
let live_deep_candidates (env : tier_env) ~(deep_clusters : cluster list)
    ~covered reg : (string * string) list =
  let region_covered = region_covered env ~covered in
  let live =
    List.filter
      (fun (c : cluster) ->
        List.exists
          (fun (i : instance) ->
            not (region_covered i.file i.site_start i.site_end))
          c.instances)
      deep_clusters
  in
  if Cs_trace.on () then
    Cs_trace.trace "deep: %d pooled+safe, %d live (gated lazily)\n%!"
      (List.length deep_clusters) (List.length live);
  List.filter_map
    (fun (c : cluster) ->
      let language = lang_of c in
      let pattern_text = render_pattern_body c.pattern in
      if language <> "" && not (reg_mem reg (pattern_text, language)) then begin
        reg_add reg ~language pattern_text;
        Some (pattern_text, language)
      end
      else None)
    live

(** One tier of the pipeline (§3.3): propose → evaluate → select over a
    changeset, returning the selected rules in application order
    (specificity-descending — see [sort_for_application]), unnumbered
    ([id = ""], [after = []] — the M2 tier loop in [summarize] assigns both).
    Tier 1 runs this on the raw changeset; tier n+1 re-runs it on the
    (intermediate, after) pairs the earlier tiers leave unexplained (design §4.4
    recursive clustering). *)
let tier_rules ~on_file_for ~ctx (cs : changeset) : rule list =
  let env = Cs_trace.timed "site db" (fun () -> make_tier_env ~ctx cs) in
  (* ── PROPOSE: the candidate channels ─────────────────────────── *)
  let raw, delta_raw, deep_raw, anchored_raw =
    Cs_trace.timed "propose: two-sided extract" (fun () ->
        collect_initial_clusters ?on_file:(on_file_for "two-sided") ~ctx cs)
  in
  let base_two_sided, delta_clusters, deep_clusters, intersection_clusters, anchored
      =
    Cs_trace.timed "propose: two-sided cluster" (fun () ->
        let initial =
          Cs_trace.timed "  pre-group" (fun () -> pre_group_identical raw)
        in
        trace_initial_histogram raw initial;
        let mined = ref [] in
        let base_two_sided =
          Cs_trace.timed "  two-sided clusters" (fun () ->
              propose_two_sided_clusters ~safe_instances:(safe_instances env)
                ~on_reject:(fun pat insts ->
                  mined := (pat, insts) :: !mined)
                initial)
        in
        let delta =
          Cs_trace.timed "  delta pool" (fun () ->
              propose_delta_pooled env delta_raw)
        in
        let deep =
          Cs_trace.timed "  deep pool" (fun () ->
              propose_delta_pooled ~label:"deep" env deep_raw)
        in
        let anchored =
          Cs_trace.timed "  anchored" (fun () -> propose_anchored anchored_raw)
        in
        let intersection =
          Cs_trace.timed "  intersection mine" (fun () ->
              (* Suppression base: every candidate another channel already
                 carries — dendrogram clusters, pooled deltas, deep chain
                 variants, and the anchored stream's realisations. *)
              let anchored_clusters =
                List.map (fun (_, _, c) -> c) anchored.an_pooled
              in
              propose_intersection env
                ~base:(base_two_sided @ delta @ deep @ anchored_clusters)
                !mined)
        in
        (base_two_sided, delta, deep, intersection, anchored))
  in
  let two_sided_clusters =
    base_two_sided @ delta_clusters @ intersection_clusters
  in
  let candidates =
    Cs_trace.timed "propose: one-sided extract" (fun () ->
        collect_one_sided_candidates ?on_file:(on_file_for "one-sided") ~ctx cs)
  in
  let os_clusters, pairs, swap_pairs =
    Cs_trace.timed "propose: one-sided cluster+swap gate" (fun () ->
        Cs_trace.trace "  one-sided candidates: %d\n%!" (List.length candidates);
        let os_clusters =
          Cs_trace.timed "  os cluster" (fun () -> cluster_one_sided candidates)
        in
        Cs_trace.trace "  one-sided clusters: %d\n%!" (List.length os_clusters);
        let pairs =
          Cs_trace.timed "  os pair" (fun () ->
              pair_one_sided_clusters os_clusters)
        in
        Cs_trace.trace "  one-sided pairs: %d\n%!" (List.length pairs);
        let swap_pairs =
          Cs_trace.timed "  swap gate" (fun () -> gate_swap_pairs env pairs)
        in
        (os_clusters, pairs, swap_pairs))
  in
  (* Pick one representative per change-family before fusion (see
     [arbitrate_fusion_inputs]) — else nested granularities of the same
     change would fuse into a self-overlapping conjunctive. *)
  let fusion_inputs, group_outputs =
    Cs_trace.timed "propose: fusion" (fun () ->
        let fusion_inputs =
          arbitrate_fusion_inputs ~eval_at:env.eval_at ~all_files:env.all_files
            two_sided_clusters
        in
        let nodes =
          List.map fusion_node_of_two_sided fusion_inputs
          @ List.map
              (fun (ep, insts) -> fusion_node_of_swap ep insts)
              swap_pairs
        in
        let groups = group_by_jaccard nodes in
        (fusion_inputs, List.concat_map materialise_group groups))
  in
  (* ── PROPOSE boundary (§3.3) ─────────────────────────────────────
     Everything above — extraction, clustering, cuts, fusion — only
     *proposes* candidate patterns from here on. Instance bookkeeping
     (which sites a cluster was born from) stays behind this line; a
     rule's sites, support, and coverage are derived by evaluation below,
     from the candidate's behaviour alone. *)
  let reg = reg_create () in
  (* Conjunctive fusions (a singleton group materialises as the node's own
     pattern). *)
  List.iter
    (fun (sections, _sites, language, _support) ->
      reg_add reg ~language
        (String.concat "\n" (List.map render_pattern_body sections)))
    group_outputs;
  (* Every fusion-input cluster and fused swap individually, too: a fused
     form and its components are distinct candidates, and selection
     arbitrates between them on coverage. *)
  List.iter
    (fun (c : cluster) ->
      reg_add reg ~language:(lang_of c) (render_pattern_body c.pattern))
    fusion_inputs;
  List.iter
    (fun (ep, (insts : one_sided_instance list)) ->
      let language = match insts with i :: _ -> i.os_language | [] -> "" in
      reg_add reg ~language (render_pattern_body ep))
    swap_pairs;
  register_removal_candidates env reg os_clusters pairs;
  let field_cand_files = register_field_candidates env reg base_two_sided in
  (* §3.2 anchored realisations are NOT proposed here — they are gated and
     evaluated after round 1, restricted to uncovered delta pools. *)
  let general_cands = reg_ordered reg in
  if Cs_trace.on () then
    Printf.eprintf "general candidates proposed: %d\n%!"
      (List.length general_cands);
  (* ── EVALUATE ──────────────────────────────────────────────────── *)
  let evaluated_general =
    Cs_trace.timed "evaluate: general" (fun () ->
        List.filter_map
          (eval_candidate env ~anchored ~field_cand_files)
          general_cands)
  in
  if Cs_trace.on () then
    Printf.eprintf "general candidates with viable extensions: %d\n%!"
      (List.length evaluated_general);
  (* ── SELECT ────────────────────────────────────────────────────── *)
  let covered : (string * int, unit) Hashtbl.t = Hashtbl.create 64 in
  let selected = ref [] in
  let is_exempt sc =
    Hashtbl.mem anchored.an_exempt (sc.sc_pattern, sc.sc_language)
  in
  (* Round 1: the general candidates. (A general candidate that textually
     coincides with an anchored realisation is exempt and waits for
     round 2.) *)
  Cs_trace.timed "select: round 1" (fun () ->
      select_round ~anchored ~covered ~selected
        (List.filter (fun sc -> not (is_exempt sc)) evaluated_general)
        Cs_config.default.min_support);
  let anchored_cands = live_anchored_candidates env ~anchored ~covered reg in
  let deep_cands = live_deep_candidates env ~deep_clusters ~covered reg in
  let evaluated_anchored =
    Cs_trace.timed "evaluate: anchored" (fun () ->
        List.filter_map
          (eval_candidate env ~anchored ~field_cand_files)
          (anchored_cands @ deep_cands))
  in
  (* Round 2: the anchored realisations and the deep chain variants, plus
     any general candidate that was exempt (a textual coincidence), over
     the regions round 1 left open. *)
  Cs_trace.timed "select: round 2" (fun () ->
      select_round ~anchored ~covered ~selected
        (List.filter is_exempt evaluated_general @ evaluated_anchored)
        1);
  List.rev !selected
  |> List.map (fun sc ->
      {
        id = "";
        pattern_text = sc.sc_pattern;
        support = sc.sc_support;
        language = sc.sc_language;
        sites = List.map fst sc.sc_extension;
        after = [];
      })
  |> sort_for_application
