(** Change-summary tiering (design §4.4, M2): drive the propose/evaluate/select
    core ({!Cs_select.tier_rules}) over successive tiers — each tier re-runs on
    the (intermediate, after) pairs the earlier tiers leave unexplained — number
    the rules, prune rules an earlier tier's edits consume, account for the
    chain effect per site, and emit the residuals that rules + residuals together
    reproduce the changeset exactly. [summarize] is the public entry point. *)

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
        if r.language = language && List.mem path r.sites then
          try
            Matcher.transform ~ctx ~language:r.language
              ~pattern_text:r.pattern_text ~source_text:s
          with
          | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
          | e ->
              Cs_trace.trace "apply_claiming rule %s: %s\n%!" r.id
                (Printexc.to_string e);
              s
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
    let tier = prune_dead acc (tier_rules ~on_file_for ~ctx cur) in
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
              | Modified { path; language; before_source; after_source } ->
                  let inter = apply_claiming acc path ~language before_source in
                  if
                    inter = after_source
                    || ws_collapse inter = ws_collapse after_source
                  then None
                  else
                    Some
                      (Modified
                         { path; language; before_source = inter; after_source })
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
     when removed, so it always survives. *)
  let combined =
    let dropped : (string * string, unit) Hashtbl.t = Hashtbl.create 16 in
    List.iter
      (function
        | Modified { path; language; before_source; _ } -> (
            let claiming =
              List.filter
                (fun (r : rule) ->
                  r.language = language && List.mem path r.sites)
                combined
            in
            match claiming with
            | [] | [ _ ] -> ()
            | _ ->
                (* Invariant: [apply_claiming !kept] equals [full] — every
                   accepted drop preserved the intermediate. *)
                let full = apply_claiming claiming path ~language before_source in
                let kept = ref claiming in
                List.iter
                  (fun (r : rule) ->
                    if List.length !kept > 1 then begin
                      let without =
                        List.filter (fun (x : rule) -> x != r) !kept
                      in
                      let inter =
                        apply_claiming without path ~language before_source
                      in
                      if inter = full then begin
                        kept := without;
                        Hashtbl.replace dropped (r.id, path) ()
                      end
                    end)
                  claiming)
        | Added _ | Deleted _ -> ())
      cs.files;
    if Hashtbl.length dropped = 0 then combined
    else
      List.map
        (fun (r : rule) ->
          let sites =
            List.filter (fun f -> not (Hashtbl.mem dropped (r.id, f))) r.sites
          in
          { r with sites })
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
  let fires : (string * string, int) Hashtbl.t = Hashtbl.create 32 in
  let inters : (string, string) Hashtbl.t = Hashtbl.create 32 in
  List.iter
    (function
      | Modified { path; language; before_source; _ } ->
          let inter =
            List.fold_left
              (fun s (r : rule) ->
                if r.language = language && List.mem path r.sites then
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
                  | (Stack_overflow | Out_of_memory | Sys.Break) as e -> raise e
                  | e ->
                      Cs_trace.trace "chain-apply rule %s: %s\n%!" r.id
                        (Printexc.to_string e);
                      s
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
                    List.filter (fun pid -> Hashtbl.mem fires (pid, site)) preds
                  with
                  | [] -> None
                  | preds -> Some (site, preds))
              r.after
          in
          Some { r with sites; support; after })
      combined
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
    List.filter_map
      (fun fc ->
        match fc with
        | Modified { path; language; before_source; after_source } ->
            let claiming = rules_at path in
            let inter =
              match Hashtbl.find_opt inters path with
              | Some s -> s
              | None -> before_source
            in
            if
              inter = after_source
              || ws_collapse inter = ws_collapse after_source
            then None
            else
              let d =
                residual_diff ~ignore_formatting ~ctx ~language ~file_path:path
                  ~original:inter ~transformed:after_source ()
              in
              if d = "" then None
              else
                Some
                  {
                    res_file = path;
                    res_rules = List.map (fun (r : rule) -> r.id) claiming;
                    res_diff = d;
                  }
        | Added { path; after_source; _ } ->
            Some
              {
                res_file = path;
                res_rules = [];
                res_diff = file_op_diff ~added:true path after_source;
              }
        | Deleted { path; before_source; _ } ->
            Some
              {
                res_file = path;
                res_rules = [];
                res_diff = file_op_diff ~added:false path before_source;
              })
      cs.files
  in
  { rules = combined; residuals }
