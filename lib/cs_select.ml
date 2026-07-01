(** Change-summary selection (design §3.3): one tier of propose → evaluate →
    select. [tier_rules] assembles candidates from every channel ({!Cs_propose},
    {!Cs_cluster}, {!Cs_fusion}), evaluates each against the changeset's sites
    ({!Cs_evaluate} — the gate that defines a rule's meaning), and emits a greedy
    weighted set-cover over the changed regions. *)

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
    collapses to a root hole the cut discards. The AFTER side may differ freely —
    the shared hole binding reconciles it (the extraction case,
    [box(foo()).get()⤳foo()] ∧ [box(42).get()⤳42] → [box(_H0).get()⤳_H0]) — so
    we must NOT split on it. A single pre-grouped cluster skips the dendrogram. *)
let propose_two_sided_clusters ~safe_instances (initial : cluster list) :
    cluster list =
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
        let root = build_dendrogram bucket in
        let clusters, _singletons = cut_dendrogram ~safe_instances 2 root in
        List.map respecialize clusters
      in
      let tbl = Hashtbl.create 16 in
      let order = ref [] in
      List.iter
        (fun c ->
          let s = fst (root_sig c.pattern) (* LHS root node-type only *) in
          match Hashtbl.find_opt tbl s with
          | Some l -> l := c :: !l
          | None ->
              Hashtbl.add tbl s (ref [ c ]);
              order := s :: !order)
        initial;
      List.rev !order
      |> List.concat_map (fun s ->
          process_bucket (List.rev !(Hashtbl.find tbl s)))

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
    List.map (fun c -> (c, resolved_of c)) two_sided_clusters
    |> List.sort (fun (a, ra) (b, rb) ->
        compare
          (-List.length ra, String.length (render_pattern_body a.pattern))
          (-List.length rb, String.length (render_pattern_body b.pattern)))
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

(** One tier of the pipeline (§3.3): propose → evaluate → select over a
    changeset, returning the selected rules sorted by support, unnumbered
    ([id = ""], [after = []] — the M2 tier loop in [summarize] assigns both).
    Tier 1 runs this on the raw changeset; tier n+1 re-runs it on the
    (intermediate, after) pairs the earlier tiers leave unexplained (design §4.4
    recursive clustering). *)
let tier_rules ~on_file_for ~ctx (cs : changeset) : rule list =
  let site_db = build_site_db ~ctx cs in
  (* Evaluation of a candidate pattern at one site, memoized on
     (pattern body, file). PROPOSE's internal gates (the dendrogram cut,
     swap fusion, removal regrouping) and EVALUATE share this cache, so
     proposer-side checks pre-warm the evaluator. *)
  let eval_cache : (string * string, site_evaluation) Hashtbl.t =
    Hashtbl.create 256
  in
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
                else site_eval ~ctx ~language ~pattern_text si
          in
          Hashtbl.add eval_cache key e;
          e
  in
  let pattern_safe_at ~language ~pattern_text file =
    (* M1.9b/c: a decomposable site is safely explained too (geodesic), so
       it counts when shaping clusters — without it a coarsened candidate's
       own decomposable instances would be shed and the cluster dissolve. *)
    let e = eval_at ~language ~pattern_text file in
    e.ev_exact || e.ev_decomposable
  in
  let safe_instances ep (insts : instance list) =
    let pattern_text = render_pattern_body ep in
    List.filter
      (fun (i : instance) ->
        pattern_safe_at ~language:i.language ~pattern_text i.file)
      insts
  in
  let raw, delta_raw, anchored_raw =
    collect_initial_clusters ?on_file:(on_file_for "two-sided") ~ctx cs
  in
  let initial = pre_group_identical raw in
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
  end;
  let all_files =
    Hashtbl.fold (fun k _ acc -> k :: acc) site_db []
    |> List.sort String.compare
  in
  let base_two_sided = propose_two_sided_clusters ~safe_instances initial in
  (* §3.2 delta-keyed candidates: pooled by exact pattern identity only
     (no dendrogram participation), gated like any cluster. A pool of
     ≥ 2 identical scope-holed pairs is a delta whose support spans
     anchors; evaluation later extends it to every file it fires in. *)
  let delta_clusters =
    pre_group_identical delta_raw
    |> List.filter (fun c ->
        List.length c.instances >= Cs_config.default.min_support)
    |> List.filter_map (fun c ->
        let safe = safe_instances c.pattern c.instances in
        if List.length safe >= Cs_config.default.min_support then
          Some { c with instances = safe }
        else None)
  in
  if Cs_trace.on () then
    Cs_trace.trace "delta-keyed: %d raw, %d pooled+safe\n%!"
      (List.length delta_raw)
      (List.length delta_clusters);
  (* §3.2 anchored variants: support pools on the DELTA. A delta whose
     distinct sites number ≥ 2 may realise as per-site anchored rules
     of support 1 — they are exempted from the min-support thresholds
     in EVALUATE and SELECT below (the pool carries the support, the
     anchors are its site-local realisations). Selection's greedy
     set-cover provides the lattice-descent pruning: a general
     candidate that already covers a region wins it first (higher
     marginal, shorter pattern), so an anchored rule is only ever
     selected for regions no more-general safe candidate claims. *)
  (* (pattern_text, language) → (concrete node count, delta needle).
     Membership = min-support exemption; the count is round 2's
     generality tie-break; the needle (the delta's first before-side
     text) prefilters evaluation — an anchored realisation cannot fire
     in a file that does not even contain its delta text. *)
  (* §3.2 anchored realisations are gated and evaluated LAZILY: only after
     round 1, and only for delta pools where round 1 left a site uncovered
     (the descent's pruning, made cheap — an anchored rule can never be
     selected over a region a general rule already covered, so there is no
     point safety-checking or evaluating it there). Here we only do the
     cheap bookkeeping — the pool's distinct sites, and the exempt marking
     that lowers a pooled delta's min-support to 1 — and defer the gating
     below to [live_pooled]. *)
  let exempt : (string * string, int * string) Hashtbl.t = Hashtbl.create 16 in
  let lang_of (c : cluster) =
    match c.instances with i :: _ -> i.language | [] -> ""
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
     marks. Computed without gating — gating is deferred to [live_pooled]
     below — but the exempt marks are populated now so that a general
     candidate textually coinciding with an anchored realisation is treated
     as exempt (round 2) exactly as before. *)
  let anchored_grouped =
    pre_group_identical (List.map (fun (_, _, c) -> c) anchored_pooled)
    |> List.map (fun c -> (render_pattern_body_surgical c.pattern, c))
  in
  List.iter
    (fun (pattern_text, c) ->
      Hashtbl.replace exempt
        (pattern_text, lang_of c)
        (edit_size c.pattern - edit_holes c.pattern, needle_of_pat c.pattern))
    anchored_grouped;
  (* Anchored clusters are NOT part of [two_sided_clusters]: in
     fusion-input arbitration they would claim regions and knock the
     general rules out of candidacy, and in Jaccard grouping they would
     fuse into spurious conjunctives. They are added as bare candidates
     below and only become eligible in selection's second round, over
     regions the general candidates left uncovered. *)
  let two_sided_clusters = base_two_sided @ delta_clusters in
  let candidates =
    collect_one_sided_candidates ?on_file:(on_file_for "one-sided") ~ctx cs
  in
  let os_clusters = cluster_one_sided candidates in
  let pairs = pair_one_sided_clusters os_clusters in
  let swap_pairs = List.filter_map (fun (r, a) -> fuse_swap r a) pairs in
  (* Safety-gate fused swaps like any other two-sided rule: shed unsafe
     sites, drop the swap when fewer than two fires (removed-side
     instances) survive. *)
  let swap_pairs =
    List.filter_map
      (fun (ep, insts) ->
        let pattern_text = render_pattern_body ep in
        let safe =
          List.filter
            (fun (i : one_sided_instance) ->
              pattern_safe_at ~language:i.os_language ~pattern_text i.os_file)
            insts
        in
        let fires =
          List.length (List.filter (fun i -> i.os_side = Before_side) safe)
        in
        if fires >= Cs_config.default.min_support then Some (ep, safe) else None)
      swap_pairs
  in
  (* Pick one representative per change-family before fusion (see
     [arbitrate_fusion_inputs]) — else nested granularities of the same change
     would fuse into a self-overlapping conjunctive. *)
  let fusion_inputs =
    arbitrate_fusion_inputs ~eval_at ~all_files two_sided_clusters
  in
  let nodes =
    List.map fusion_node_of_two_sided fusion_inputs
    @ List.map (fun (ep, insts) -> fusion_node_of_swap ep insts) swap_pairs
  in
  let groups = group_by_jaccard nodes in
  let group_outputs = List.concat_map materialise_group groups in
  (* ── PROPOSE boundary (§3.3) ─────────────────────────────────────
     Everything above — extraction, clustering, cuts, fusion — only
     *proposes* candidate patterns from here on. Instance bookkeeping
     (which sites a cluster was born from) stays behind this line; a
     rule's sites, support, and coverage are derived by evaluation
     below, from the candidate's behaviour alone. *)
  let cand_tbl : (string * string, unit) Hashtbl.t = Hashtbl.create 32 in
  let cand_order = ref [] in
  let add_candidate ~language pattern_text =
    if language <> "" then begin
      let key = (pattern_text, language) in
      if not (Hashtbl.mem cand_tbl key) then begin
        Hashtbl.add cand_tbl key ();
        cand_order := key :: !cand_order
      end
    end
  in
  (* Conjunctive fusions (a singleton group materialises as the node's
     own pattern). *)
  List.iter
    (fun (sections, _sites, language, _support) ->
      add_candidate ~language
        (String.concat "\n" (List.map render_pattern_body sections)))
    group_outputs;
  (* Every fusion-input cluster and fused swap individually, too: a
     fused form and its components are distinct candidates, and
     selection arbitrates between them on coverage. *)
  List.iter
    (fun (c : cluster) ->
      let language = match c.instances with i :: _ -> i.language | [] -> "" in
      add_candidate ~language (render_pattern_body c.pattern))
    fusion_inputs;
  List.iter
    (fun (ep, (insts : one_sided_instance list)) ->
      let language = match insts with i :: _ -> i.os_language | [] -> "" in
      add_candidate ~language (render_pattern_body ep))
    swap_pairs;
  (* Removal-only clusters that did not pair with an Added cluster in
     M1.6 fusion: their (possibly concretely regrouped) [-]-only bodies.
     Addition-only clusters are not proposed — a [+]-only block has no
     anchor to apply; their changes fall to residuals. *)
  let used_removeds = List.map fst pairs in
  List.iter
    (fun c ->
      if c.os_cluster_side = Before_side && not (List.memq c used_removeds) then
        List.iter
          (fun (pattern_text, (insts : one_sided_instance list)) ->
            let language =
              match insts with i :: _ -> i.os_language | [] -> ""
            in
            add_candidate ~language pattern_text)
          (safe_removal_groups ~ctx ~site_db c))
    os_clusters;
  (* Declaration-anchored field candidates: re-anchor a two-sided cluster's
     change under its enclosing declaration (cs_pattern.build_anchored_decl),
     rendered as a [match: field] rule. A clean but context-stripped body rule
     ([{ return _H } ⤳ = _H]) over-fires on every single-return block, so it
     fails the placement gate on files where some such block was not converted
     and fragments into signature-anchored pieces; its anchored form
     ([fun _Hf(...) { return _H } ⤳ = _H]) fires only on declarations and is
     safe everywhere. Added as ADDITIONAL candidates: selection keeps the bare
     rule wherever it covers as much (the anchored form has more concrete nodes
     and longer text, so it loses the tie-break) and prefers the anchored form
     only over regions the bare rule's over-fire leaves uncovered.

     Gate on over-fire: anchoring pays off exactly when the bare rule fires on
     code the changeset did not touch. A rule already safe everywhere needs no
     anchor, and anchoring it would add a field candidate — expensive to
     evaluate — that only ever loses selection (this keeps corpora without the
     pattern, e.g. gen3, byte-identical and fast). The check reuses the
     [eval_at] cache the bare pattern's general-candidate evaluation populates,
     so it costs no extra parses. *)
  (* An anchored field candidate can only fire in files where its source
     cluster's bare pattern already produces edits (safely or as an over-fire);
     a file with no such block cannot host [fun _Hf(...) { ... }]. So scope each
     field candidate's evaluation to that file set rather than the whole
     changeset — field-mode matching is expensive, and evaluating every anchored
     candidate against all N files is what makes the channel blow up on a large
     corpus (measured on a ~4k-file real codebase). *)
  let field_cand_files : (string * string, string list) Hashtbl.t =
    Hashtbl.create 16
  in
  let anchored_count = ref 0 in
  (List.iter
       (fun (c : cluster) ->
         let language = lang_of c in
         if language = "" then ()
         else begin
           (* Evaluate the cluster's (realigned — a misaligned body pair renders
              as an unapplicable orphan) bare pattern once across all files:
              record whether it OVER-FIRES (fires outside a changed region — the
              symptom that a declaration anchor pays off) and the files where it
              produces any edits (the only files the field rule can fire in).
              These evals reuse the [eval_at] cache the general candidates warm. *)
           let bare = render_pattern_body (realign_orphan_holes c.pattern) in
           let overfires = ref false in
           let fire_files = ref [] in
           List.iter
             (fun f ->
               let e = eval_at ~language ~pattern_text:bare f in
               if e.ev_overfire then overfires := true;
               if e.ev_fires > 0 || e.ev_overfire then
                 fire_files := f :: !fire_files)
             all_files;
           if not !overfires then ()
           else
             (* [all_files] is sorted; [fire_files] was prepended, so reverse it
                back to sorted order — the field rule's site list then matches the
                order every other candidate produces. *)
             let files = List.rev !fire_files in
             let seen = Hashtbl.create 4 in
             c.instances
             |> List.filteri (fun i _ -> i < Cs_config.default.anchor_sample)
             |> List.iter (fun (inst : instance) ->
                 match
                   (try
                      let t =
                        Tree.parse ~ctx ~language:inst.language
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
                   | _ -> None)
                 with
                 | None -> ()
                 | Some ep ->
                     let txt = render_pattern_body_field ep in
                     if not (Hashtbl.mem seen txt) then begin
                       Hashtbl.replace seen txt ();
                       incr anchored_count;
                       add_candidate ~language txt;
                       Hashtbl.replace field_cand_files (txt, language) files
                     end)
         end)
       base_two_sided);
  if Cs_trace.on () then
    Printf.eprintf "anchored field candidates: %d\n%!" !anchored_count;
  (* §3.2 anchored realisations are NOT proposed here — they are gated and
     evaluated after round 1, restricted to uncovered delta pools (below). *)
  let general_cands = List.rev !cand_order in
  if Cs_trace.on () then
    Printf.eprintf "general candidates proposed: %d\n%!"
      (List.length general_cands);
  (* ── EVALUATE (§3.3): each candidate's true extension ──────────── *)
  let eval_cand (pattern_text, language) =
    (* Needle prefilter for anchored realisations: skip files that
           do not contain the delta's before text — the pattern cannot
           fire there, and exempt candidates are numerous enough that
           evaluating them everywhere dominates runtime. *)
    let needle =
      match Hashtbl.find_opt exempt (pattern_text, language) with
      | Some (_, n) when n <> "" -> Some n
      | _ -> None
    in
    let file_plausible f =
      match needle with
      | None -> true
      | Some n -> (
          match Hashtbl.find_opt site_db f with
          | Some si -> string_mem ~sub:n si.si_before
          | None -> true)
    in
    (* Anchored field candidates are scoped to the files their source cluster's
       bare pattern touches (see the anchoring block); all other candidates see
       the whole changeset. *)
    let files =
      match Hashtbl.find_opt field_cand_files (pattern_text, language) with
      | Some fs -> fs
      | None -> all_files
    in
    let extension =
      List.filter_map
        (fun f ->
          if not (file_plausible f) then None
          else
            let e = eval_at ~language ~pattern_text f in
            (* M1.9b: a decomposable site fires safely (geodesic) and
                 counts toward support; its in-zone gap becomes a
                 [rule=]-attributed residual. *)
            if (e.ev_exact || e.ev_decomposable) && e.ev_fires > 0 then
              Some (f, e)
            else None)
        files
    in
    let support = List.fold_left (fun a (_, e) -> a + e.ev_fires) 0 extension in
    (* Anchored realisations of a pooled delta carry the pool's
           support; their own floor is 1 (§3.2 lattice descent). *)
    let floor = if Hashtbl.mem exempt (pattern_text, language) then 1 else Cs_config.default.min_support in
    if support < floor then None
    else
      Some
        {
          sc_pattern = pattern_text;
          sc_language = language;
          sc_support = support;
          sc_extension = extension;
        }
  in
  let evaluated_general = List.filter_map eval_cand general_cands in
  if Cs_trace.on () then
    Printf.eprintf "general candidates with viable extensions: %d\n%!"
      (List.length evaluated_general);
  (* ── SELECT (§3.3): greedy set-cover over changed regions ────────
     A candidate's marginal value is the number of still-uncovered
     (file, region) pairs it resolves; it is eligible while that
     marginal is at least min_support. Reported support stays the
     global fire count over the full extension. Ties break to higher
     support, then shorter pattern text (the tighter statement), then
     text for determinism. Subsumption is inherent: a candidate
     resolving only covered regions is never selected. *)
  let covered : (string * int, unit) Hashtbl.t = Hashtbl.create 64 in
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
  let selected = ref [] in
  let select_round pool floor =
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
              (* Generality tie-break (round 2): among anchored
                 realisations with equal coverage, prefer the one with
                 the FEWEST concrete nodes — the most general safe
                 anchor, no site junk. Non-exempt candidates score 0
                 (the best), so round 1 is unaffected. *)
              let concrete =
                match
                  Hashtbl.find_opt exempt (sc.sc_pattern, sc.sc_language)
                with
                | Some (c, _) -> c
                | None -> 0
              in
              let key =
                ( m,
                  clean,
                  sc.sc_support,
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
              List.iter
                (fun i -> Hashtbl.replace covered (f, i) ())
                e.ev_resolved)
            sc.sc_extension;
          if Cs_trace.on () then
            Printf.eprintf "  selected: support=%d %S\n%!" sc.sc_support
              (String.sub sc.sc_pattern 0
                 (min 60 (String.length sc.sc_pattern)))
    done
  in
  let is_exempt sc = Hashtbl.mem exempt (sc.sc_pattern, sc.sc_language) in
  (* Round 1: the general candidates. (A general candidate that textually
     coincides with an anchored realisation is exempt — marked above — and
     waits for round 2, exactly as before.) *)
  select_round (List.filter (fun sc -> not (is_exempt sc)) evaluated_general) Cs_config.default.min_support;
  (* ── lazy descent ── Round 1 has marked the regions it covers. Now gate
     and evaluate anchored realisations, but ONLY for delta pools where some
     home site is still uncovered: an anchored rule whose every site is
     already covered would have marginal 0 and never be selected, so
     safety-checking and evaluating it would be wasted work. *)
  let region_covered file ds de =
    match Hashtbl.find_opt site_db file with
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
  in
  let pool_live key lang =
    match Hashtbl.find_opt pool_sites (lang, key) with
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
      anchored_pooled;
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
                pattern_safe_at ~language:i.language ~pattern_text i.file)
              c.instances
          in
          if safe = [] then None
          else Some (pattern_text, { c with instances = safe }))
      anchored_grouped
  in
  if Cs_trace.on () then
    Printf.eprintf "anchored: %d pooled, %d live+safe (gated lazily)\n%!"
      (List.length anchored_grouped)
      (List.length anchored_clusters);
  (* Anchored realisations as candidates, deduped against the general ones
     already proposed; evaluate them (floor 1, via the exempt table). *)
  let anchored_cands =
    List.filter_map
      (fun ((pattern_text, c) : string * cluster) ->
        let language = lang_of c in
        if language <> "" && not (Hashtbl.mem cand_tbl (pattern_text, language))
        then begin
          Hashtbl.add cand_tbl (pattern_text, language) ();
          Some (pattern_text, language)
        end
        else None)
      anchored_clusters
  in
  let evaluated_anchored = List.filter_map eval_cand anchored_cands in
  (* Round 2: the anchored realisations, plus any general candidate that was
     exempt (a textual coincidence), over the regions round 1 left open. *)
  select_round (List.filter is_exempt evaluated_general @ evaluated_anchored) 1;
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
  |> List.sort (fun a b -> compare b.support a.support)
