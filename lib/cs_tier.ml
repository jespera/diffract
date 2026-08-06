(** Change-summary tiering (design §4.4, M2): drive the propose/evaluate/select
    core ({!Cs_select.tier_rules}) over successive tiers — each tier re-runs on
    the (intermediate, after) pairs the earlier tiers leave unexplained — number
    the rules, prune rules an earlier tier's edits consume, account for the
    chain effect per site, and emit the residuals that rules + residuals
    together reproduce the changeset exactly. [summarize] is the public entry
    point. *)

open Cs_types
open Cs_evaluate
open Cs_select

let summarize ?progress ?(ignore_formatting = false) ~ctx (cs : changeset) :
    summary =
  let on_file_for stage =
    match progress with
    | None -> None
    | Some p -> Some (fun ~idx ~total ~path -> p ~stage ~idx ~total ~path)
  in
  (* Apply [path]'s claiming rules to [src] in rule-id order — the
     application contract shared by the tier loop, the residual pass and
     the round-trip property. *)
  let apply_claiming rules path ~language src =
    List.fold_left
      (fun s (r : rule) ->
        if r.language = language && List.mem path r.sites then (
          try
            Matcher.transform ~ctx ~language:r.language
              ~pattern_text:r.pattern_text ~source_text:s
          with
          | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
          | e ->
              Cs_trace.trace "apply_claiming rule %s: %s\n%!" r.id
                (Printexc.to_string e);
              s)
        else s)
      src rules
  in
  (* ── M2 tier loop (§4.4) ─────────────────────────────────────────
     Run propose/evaluate/select; then rebuild the changeset from the
     (intermediate, after) pairs the rules so far leave unexplained —
     including files no rule claims, whose residuals join the global
     pool (§3.3 common factors) — and recurse. Each emitting tier
     strictly shrinks the unexplained gap (the net-progress guard), so
     the loop terminates when a tier emits nothing; the depth cap and
     the no-progress check are backstops, not the intended exit. A
     tier-n rule's per-site [after] lists the earlier rules claiming
     that site: its pattern matched the intermediate those rules
     produce, so id order is application order. *)
  let max_tiers = Cs_config.default.max_tiers in
  let intermediate_key c =
    List.filter_map
      (function
        | Modified { path; before_source; _ } -> Some (path, before_source)
        | Added _ | Deleted _ -> None)
      c.files
  in
  (* Drop tier rules that never fire under id-order application. A
     rule's sites and coverage are evaluated rule-independently against
     the tier's changeset (§3.3), but application composes sequentially —
     an earlier rule's edits can consume a later rule's matches entirely
     (R1 = [f($X,$Y) ⤳ g($X)] rewrites the call that R2 =
     [f($X+1,$Y) ⤳ g($X)] would have matched). Keeping such a rule
     emits a dead pattern whose claimed sites mislead; dropping it leaves
     its regions unexplained for the *next* tier, which re-proposes
     against the actual intermediate ([( $X+1 ) ⤳ ( $X )], after=R1). *)
  let prune_dead (prior : rule list) (tier : rule list) : rule list =
    let fired : (string * string, unit) Hashtbl.t = Hashtbl.create 8 in
    List.iter
      (function
        | Modified { path; language; before_source; _ } ->
            ignore
              (List.fold_left
                 (fun s (r : rule) ->
                   if r.language = language && List.mem path r.sites then begin
                     let s' =
                       try
                         Matcher.transform ~ctx ~language:r.language
                           ~pattern_text:r.pattern_text ~source_text:s
                       with
                       | (Stack_overflow | Out_of_memory | Sys.Break) as e ->
                           raise e
                       | e ->
                           Cs_trace.trace "prune_dead rule %s: %s\n%!" r.id
                             (Printexc.to_string e);
                           s
                     in
                     if s' <> s then
                       Hashtbl.replace fired (r.pattern_text, r.language) ();
                     s'
                   end
                   else s)
                 before_source (prior @ tier))
        | Added _ | Deleted _ -> ())
      cs.files;
    List.filter
      (fun (r : rule) -> Hashtbl.mem fired (r.pattern_text, r.language))
      tier
  in
  let rec tier_loop tier_idx (cur : changeset) (acc : rule list) : rule list =
    let tier =
      Cs_trace.timed (Printf.sprintf "tier %d" tier_idx) (fun () ->
          prune_dead acc (tier_rules ~on_file_for ~ctx cur))
    in
    if tier = [] then acc
    else
      let offset = List.length acc in
      let numbered =
        List.mapi
          (fun i (r : rule) ->
            let id = Printf.sprintf "R%d" (offset + i + 1) in
            let after =
              List.filter_map
                (fun site ->
                  match
                    List.filter (fun (p : rule) -> List.mem site p.sites) acc
                  with
                  | [] -> None
                  | preds -> Some (site, List.map (fun p -> p.id) preds))
                r.sites
            in
            { r with id; after })
          tier
      in
      let acc = acc @ numbered in
      if tier_idx >= max_tiers then acc
      else
        let next_files =
          List.filter_map
            (function
              | Modified
                  { path; moved_to; language; before_source; after_source } ->
                  let inter = apply_claiming acc path ~language before_source in
                  if
                    inter = after_source
                    || ws_collapse inter = ws_collapse after_source
                  then None
                  else
                    Some
                      (Modified
                         {
                           path;
                           moved_to;
                           language;
                           before_source = inter;
                           after_source;
                         })
              | Added _ | Deleted _ -> None)
            cs.files
        in
        let next = { files = next_files } in
        if next_files = [] || intermediate_key next = intermediate_key cur then
          acc
        else tier_loop (tier_idx + 1) next acc
  in
  let combined = tier_loop 1 cs [] in
  (* ── Per-site minimal claiming set ────────────────────────────────
     Selection's sites are evaluated rule-independently (§3.3) and are
     therefore generous: a broad partial rule can claim a file whose
     change a complete rule fully explains on its own; application then
     routes the file through a manufactured intermediate that a later
     tier must re-explain with echo rules duplicating the complete
     rule's content (a bare leaf rename claiming import lines a
     class-name-metavar import rule already rewrites outright).
     Removing a rule at a file is SAFE exactly when the chain without
     it reaches the byte-identical intermediate: the residual — and so
     reconstruction — is unchanged by construction. Walk each file's
     claiming chain in id order, dropping every rule whose removal
     leaves the final intermediate identical; echo rules whose every
     site drops this way die in the chain-effect pass below. A rule
     that makes partial progress no other rule compensates (the
     legitimate §4.4 multi-step factoring) changes the intermediate
     when removed, so it always survives.

     A plain drop only ever considers the file's own claiming chain, so
     it cannot repair the case where a *general* rule explains the site
     outright but a narrower one holds it. Discovery is tier-ordered: if
     the general form is proposed only after an over-anchored variant
     has already claimed a region (daffodil's class-context wrappers
     around [override def run(dstate: DState) {], whose general
     [def _H0( ... ) {] form emerges a tier later), the variant is
     un-droppable — nothing left in the chain compensates — and it
     survives as rule bloat. So when a plain drop fails, try REASSIGNING
     the site: re-test the chain with [r] removed and one non-claiming
     rule added. The acceptance test is unchanged — the byte-identical
     intermediate — and that is the whole safety argument: if the chain
     reaches the same intermediate, the residual and reconstruction are
     the same, so no gate re-evaluation is needed (an edit that
     over-fired or damaged the site could not land on an intermediate
     the accepted chain already produced). Adopters are tried
     most-general-first (descending support) so sites consolidate onto
     the high-support rules, and one adoption is enough to unlock the
     rest: once the general rule is in the kept set, the remaining
     variants at that file drop plainly. A rule that loses every site
     dies in the chain-effect pass below.

     Reassignment therefore runs as a SECOND phase, over the rules the
     plain phase leaves alive. Letting it consider every rule instead
     lets a rule the plain phase had emptied adopt a site and come back
     from the dead, which *raises* the rule count (androidx 26 → 29,
     webxforge 43 → 47 when first tried that way) — the opposite of the
     goal. An adopter must already hold a surviving site of its own; the
     pass consolidates onto rules that are staying, never revives one.

     The prefilter (must produce edits on the file's before-source)
     bounds this to one matcher run per (rule, file); a rule that only
     matches a mid-chain intermediate is therefore not considered as an
     adopter — a missed opportunity, never an unsafe one.

     Both this pass and the chain-effect pass below inform each other:
     this one shortens chains, and the chain pass then retires rules that
     no longer fire anywhere — which shortens chains again, so drops this
     pass had to refuse (its acceptance test ran against a chain still
     containing the retired rules) can become valid. They therefore run
     as a bounded fixpoint at the [refine] call below rather than once
     each. *)
  let minimal_claim (combined : rule list) : rule list =
    let dropped : (string * string, unit) Hashtbl.t = Hashtbl.create 16 in
    let adopted : (string * string, unit) Hashtbl.t = Hashtbl.create 16 in
    let adopted_after : (string * string, string list) Hashtbl.t =
      Hashtbl.create 16
    in
    (* Rule position in [combined] = id order = application order. *)
    let pos : (string, int) Hashtbl.t = Hashtbl.create 32 in
    List.iteri (fun i (r : rule) -> Hashtbl.replace pos r.id i) combined;
    let pos_of id = Option.value ~default:max_int (Hashtbl.find_opt pos id) in
    (* Apply an explicit rule list in the given order, ignoring [r.sites]
       (an adopter is by definition not yet a claimant of the file). *)
    let apply_rules rules ~language src =
      List.fold_left
        (fun s (r : rule) ->
          if r.language <> language then s
          else
            try
              Matcher.transform ~ctx ~language ~pattern_text:r.pattern_text
                ~source_text:s
            with
            | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
            | e ->
                Cs_trace.trace "minimal-claim rule %s: %s\n%!" r.id
                  (Printexc.to_string e);
                s)
        src rules
    in
    let fires_cache : (string * string, bool) Hashtbl.t = Hashtbl.create 64 in
    let can_fire (r : rule) path ~language src =
      match Hashtbl.find_opt fires_cache (r.id, path) with
      | Some b -> b
      | None ->
          let b =
            r.language = language
            &&
            try
              Matcher.transform_edits ~ctx ~language
                ~pattern_text:r.pattern_text ~source_text:src
              <> []
            with
            | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
            | _ -> false
          in
          Hashtbl.add fires_cache (r.id, path) b;
          b
    in
    (* Per-file claiming chain in id (= application) order. *)
    let claiming_at path ~language =
      List.filter
        (fun (r : rule) -> r.language = language && List.mem path r.sites)
        combined
    in
    (* ── Phase 1: plain drops ─────────────────────────────────────── *)
    List.iter
      (function
        | Modified { path; language; before_source; _ } -> (
            match claiming_at path ~language with
            | [] | [ _ ] -> ()
            | claiming ->
                (* Invariant: the kept set always reproduces [full]. *)
                let full = apply_rules claiming ~language before_source in
                let kept = ref claiming in
                List.iter
                  (fun (r : rule) ->
                    if List.length !kept > 1 then begin
                      let without =
                        List.filter (fun (x : rule) -> x != r) !kept
                      in
                      if apply_rules without ~language before_source = full then begin
                        kept := without;
                        Hashtbl.replace dropped (r.id, path) ()
                      end
                    end)
                  claiming)
        | Added _ | Deleted _ -> ())
      cs.files;
    (* ── Phase 2: reassignment onto rules that survive phase 1 ────── *)
    let alive : (string, unit) Hashtbl.t = Hashtbl.create 32 in
    List.iter
      (fun (r : rule) ->
        if List.exists (fun f -> not (Hashtbl.mem dropped (r.id, f))) r.sites
        then Hashtbl.replace alive r.id ())
      combined;
    (* Adopter preference: most general first (highest support), then id
       order for determinism. *)
    let adopter_order =
      List.stable_sort
        (fun (a : rule) (b : rule) ->
          if a.support <> b.support then compare b.support a.support
          else compare (pos_of a.id) (pos_of b.id))
        (List.filter (fun (r : rule) -> Hashtbl.mem alive r.id) combined)
    in
    List.iter
      (function
        | Modified { path; language; before_source; _ } -> (
            match claiming_at path ~language with
            | [] -> ()
            | claiming ->
                let full = apply_rules claiming ~language before_source in
                let live_ids =
                  List.filter_map
                    (fun (r : rule) ->
                      if Hashtbl.mem dropped (r.id, path) then None
                      else Some r.id)
                    claiming
                in
                let kept_ids = ref live_ids in
                let materialize ids =
                  List.filter
                    (fun (r : rule) ->
                      r.language = language && List.mem r.id ids)
                    combined
                in
                let reaches ids =
                  apply_rules (materialize ids) ~language before_source = full
                in
                List.iter
                  (fun (r : rule) ->
                    if List.mem r.id !kept_ids then begin
                      let without =
                        List.filter (fun i -> i <> r.id) !kept_ids
                      in
                      let rec try_adopters = function
                        | [] -> ()
                        | (a : rule) :: tl ->
                            if
                              a.id = r.id || List.mem a.id !kept_ids
                              || not (can_fire a path ~language before_source)
                            then try_adopters tl
                            else if reaches (a.id :: without) then begin
                              kept_ids := a.id :: without;
                              Hashtbl.replace dropped (r.id, path) ();
                              Hashtbl.replace adopted (a.id, path) ();
                              Cs_trace.trace
                                "minimal-claim: %s reassigned to %s at %s\n%!"
                                r.id a.id path;
                              (* The predecessors that shaped the intermediate
                                 at this site carry over, restricted to rules
                                 that really precede the adopter; the chain
                                 pass below drops any that do not fire. *)
                              match List.assoc_opt path r.after with
                              | Some preds ->
                                  let preds =
                                    List.filter
                                      (fun p -> pos_of p < pos_of a.id)
                                      preds
                                  in
                                  if preds <> [] then
                                    Hashtbl.replace adopted_after (a.id, path)
                                      preds
                              | None -> ()
                            end
                            else try_adopters tl
                      in
                      try_adopters adopter_order
                    end)
                  claiming)
        | Added _ | Deleted _ -> ())
      cs.files;
    if Hashtbl.length dropped = 0 && Hashtbl.length adopted = 0 then combined
    else
      List.map
        (fun (r : rule) ->
          let sites =
            List.filter (fun f -> not (Hashtbl.mem dropped (r.id, f))) r.sites
          in
          let gained =
            Hashtbl.fold
              (fun (rid, f) () acc -> if rid = r.id then f :: acc else acc)
              adopted []
          in
          (* Sites stay in the sorted order evaluation produced them in;
             only a rule that gained one is re-sorted. *)
          let sites =
            if gained = [] then sites
            else List.sort_uniq String.compare (sites @ gained)
          in
          let after =
            List.filter
              (fun (site, _) -> not (Hashtbl.mem dropped (r.id, site)))
              r.after
            @ List.filter_map
                (fun f ->
                  match Hashtbl.find_opt adopted_after (r.id, f) with
                  | Some preds when not (List.mem_assoc f r.after) ->
                      Some (f, preds)
                  | _ -> None)
                gained
          in
          { r with sites; after })
        combined
  in
  (* ── Chain-effect accounting (per-site) ──────────────────────────
     A rule's sites and support come from rule-independent evaluation
     (§3.3), but application composes sequentially in id order — an
     earlier rule can consume a later rule's matches at *some* of its
     sites while the later rule stays live at others (the fused-rescue
     shape: [assignee = null ⤳ assignees = emptySet()] is a no-op
     wherever the bare rename already ran, yet is the only safe rule at
     a file the rename cannot claim). Reporting evaluation-time sites
     would list files where the rule never actually edits anything.
     Walk the chain once per file, recording which (rule, file) pairs
     really fire and the final intermediate; then shrink each rule's
     sites, support, and after-attribution to its chain-effective
     extension. Selection, rule ids, and application order are NOT
     revisited — the chain (and so reconstruction) is already fixed;
     this pass only makes the bookkeeping describe it truthfully. A
     chain-pruned rule may legitimately report support below
     min_support: it was selected for coverage it genuinely provides
     at its surviving sites. *)
  let chain_effect (combined : rule list) :
      rule list * (string, string) Hashtbl.t =
    let fires : (string * string, int) Hashtbl.t = Hashtbl.create 32 in
    let inters : (string, string) Hashtbl.t = Hashtbl.create 32 in
    List.iter
      (function
        | Modified { path; language; before_source; _ } ->
            let inter =
              List.fold_left
                (fun s (r : rule) ->
                  if r.language = language && List.mem path r.sites then (
                    try
                      let edits =
                        Matcher.transform_edits ~ctx ~language:r.language
                          ~pattern_text:r.pattern_text ~source_text:s
                      in
                      if edits = [] then s
                      else begin
                        Hashtbl.replace fires (r.id, path) (List.length edits);
                        Matcher.transform ~ctx ~language:r.language
                          ~pattern_text:r.pattern_text ~source_text:s
                      end
                    with
                    | (Stack_overflow | Out_of_memory | Sys.Break) as e ->
                        raise e
                    | e ->
                        Cs_trace.trace "chain-apply rule %s: %s\n%!" r.id
                          (Printexc.to_string e);
                        s)
                  else s)
                before_source combined
            in
            Hashtbl.replace inters path inter
        | Added _ | Deleted _ -> ())
      cs.files;
    let combined =
      List.filter_map
        (fun (r : rule) ->
          let sites =
            List.filter (fun f -> Hashtbl.mem fires (r.id, f)) r.sites
          in
          if sites = [] then None
          else
            let support =
              List.fold_left
                (fun a f ->
                  a + Option.value ~default:0 (Hashtbl.find_opt fires (r.id, f)))
                0 sites
            in
            (* Keep the tier-derived after-attribution, restricted to the
               surviving sites, and within each site to predecessors that
               actually edited there (a no-op predecessor did not shape
               the intermediate this rule matched). *)
            let after =
              List.filter_map
                (fun (site, preds) ->
                  if not (List.mem site sites) then None
                  else
                    match
                      List.filter
                        (fun pid -> Hashtbl.mem fires (pid, site))
                        preds
                    with
                    | [] -> None
                    | preds -> Some (site, preds))
                r.after
            in
            Some { r with sites; support; after })
        combined
    in
    (combined, inters)
  in
  (* Run the two site-bookkeeping passes to a bounded fixpoint: each
     one's output can unlock work for the other (see [minimal_claim]).
     Stopping as soon as the chain pass changes nothing is exact, not a
     heuristic — [minimal_claim] would then be re-run on the input it
     just processed, so it could only repeat its own result. *)
  let combined, inters =
    let sites_key (rules : rule list) =
      List.map (fun (r : rule) -> (r.id, r.sites)) rules
    in
    let rec refine budget rules =
      let claimed = minimal_claim rules in
      let pruned, inters = chain_effect claimed in
      if budget <= 1 || sites_key pruned = sites_key claimed then
        (pruned, inters)
      else refine (budget - 1) pruned
    in
    refine 3 combined
  in
  (* M1.9 residual extraction: for each Modified file, the chain pass
     above already produced the intermediate (claiming rules applied in
     id order); diff it against the real after-source. The gap, if any,
     is the residual — computed against what the rules *actually*
     produce, so rules + residual reproduce the site's change by
     construction. Files no rule claims yield unattributed residuals
     (pure one-off changes); Added/Deleted files appear as [/dev/null]
     residuals (M1.7). Layout-only gaps are skipped — the same tolerance
     the safety gate's tree-level re-diff gives. *)
  let residuals =
    let rules_at f =
      List.filter (fun (r : rule) -> List.mem f r.sites) combined
    in
    let file_op_diff ~added path content =
      let buf = Buffer.create (String.length content + 64) in
      let lines =
        match List.rev (String.split_on_char '\n' content) with
        | "" :: rest -> List.rev rest
        | l -> List.rev l
      in
      let n = List.length lines in
      if added then begin
        Buffer.add_string buf "--- /dev/null\n";
        Buffer.add_string buf (Printf.sprintf "+++ b/%s\n" path);
        Buffer.add_string buf (Printf.sprintf "@@ -0,0 +1,%d @@\n" n)
      end
      else begin
        Buffer.add_string buf (Printf.sprintf "--- a/%s\n" path);
        Buffer.add_string buf "+++ /dev/null\n";
        Buffer.add_string buf (Printf.sprintf "@@ -1,%d +0,0 @@\n" n)
      end;
      List.iter
        (fun line ->
          Buffer.add_char buf (if added then '+' else '-');
          Buffer.add_string buf line;
          Buffer.add_char buf '\n')
        lines;
      Buffer.contents buf
    in
    (* A move whose content is unchanged still has to be stated, or applying
       the summary would leave the file at its old path. git spells this as an
       extended header with no hunks. *)
    let rename_only_diff ~before_path ~after_path =
      Printf.sprintf
        "diff --git a/%s b/%s\n\
         similarity index 100%%\n\
         rename from %s\n\
         rename to %s\n"
        before_path after_path before_path after_path
    in
    List.filter_map
      (fun fc ->
        match fc with
        | Modified { path; moved_to; language; before_source; after_source } ->
            let claiming = rules_at path in
            let inter =
              match Hashtbl.find_opt inters path with
              | Some s -> s
              | None -> before_source
            in
            if
              inter = after_source
              || ws_collapse inter = ws_collapse after_source
            then
              match moved_to with
              | Some ap when ap <> path ->
                  Some
                    {
                      res_file = path;
                      res_moved_to = moved_to;
                      res_rules = List.map (fun (r : rule) -> r.id) claiming;
                      res_diff =
                        rename_only_diff ~before_path:path ~after_path:ap;
                    }
              | _ -> None
            else
              let d =
                (* The residual is stated FROM the before-side path TO the
                   after-side one, so a moved file's hunks land where the file
                   now lives. *)
                residual_diff ~ignore_formatting ~ctx ~language
                  ~before_path:path
                  ~file_path:(Option.value moved_to ~default:path)
                  ~original:inter ~transformed:after_source ()
              in
              if d = "" then None
              else
                Some
                  {
                    res_file = path;
                    res_moved_to = moved_to;
                    res_rules = List.map (fun (r : rule) -> r.id) claiming;
                    res_diff = d;
                  }
        | Added { path; after_source; _ } ->
            Some
              {
                res_file = path;
                res_moved_to = None;
                res_rules = [];
                res_diff = file_op_diff ~added:true path after_source;
              }
        | Deleted { path; before_source; _ } ->
            Some
              {
                res_file = path;
                res_moved_to = None;
                res_rules = [];
                res_diff = file_op_diff ~added:false path before_source;
              })
      cs.files
  in
  (* Parse damage, reported per file (§ [summary.unparsed]). Measured on the
     same source the residual diffs are stated against — the intermediate the
     claiming rules produce — so the line ranges line up with the hunk headers
     next to them. For an unclaimed file that source *is* the before-source. *)
  let unparsed =
    List.filter_map
      (function
        | Modified { path; language; before_source; _ } -> (
            let src =
              match Hashtbl.find_opt inters path with
              | Some s -> s
              | None -> before_source
            in
            match
              try Tree.unparsed_regions (Tree.parse ~ctx ~language src) with
              | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
              | _ -> []
            with
            | [] -> None
            | regions -> Some (path, regions))
        | Added _ | Deleted _ -> None)
      cs.files
  in
  { rules = combined; residuals; unparsed }
