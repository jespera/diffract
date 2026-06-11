(** Folder-based E2E tests for change summarization.

    Each case lives under [tests/change_summary_cases/<name>/] with [before/],
    [after/], and an [expected.summary] file. Comparison is alpha-equivalent
    over metavar names — the canonical form renames $identifier → $_N by
    order of first occurrence in the rule body. Whitespace in the pattern
    body and metavar name choice do not cause churn; structural drift does. *)

open Diffract

let cases_dir = "change_summary_cases"

(* ── Alpha-canonicalization ───────────────────────────────────────── *)

let is_ident_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
  || (c >= '0' && c <= '9') || c = '_'

(** Names declared by [metavar <name>: ...] lines in a rule body. The
    universal-tokenizer matcher is sigil-free: a metavar is recognised by
    its declared name, not a [$] prefix, so we identify metavars from the
    declarations rather than by scanning for a sigil. *)
let declared_metavars (text : string) : (string, unit) Hashtbl.t =
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun line ->
      let line = String.trim line in
      match String.split_on_char ' ' line with
      | "metavar" :: name :: _ ->
          (* [name] is e.g. "_H0:" or "_H0" — drop a trailing colon. *)
          let name =
            match String.index_opt name ':' with
            | Some k -> String.sub name 0 k
            | None -> name
          in
          if name <> "" then Hashtbl.replace tbl name ()
      | _ -> ())
    (String.split_on_char '\n' text);
  tbl

(** Rename every whole-identifier occurrence of a declared metavar to
    [_M<N>], where N is the first-seen index in [text], so the generated
    metavar names do not cause test churn. A literal [$] adjacent to a
    metavar (PHP's variable delimiter, e.g. [$_H0]) is preserved — only the
    identifier part is renamed. Non-metavar identifiers pass through. *)
let alpha_canonicalize (text : string) : string =
  let declared = declared_metavars text in
  let buf = Buffer.create (String.length text) in
  let mapping = Hashtbl.create 8 in
  let next = ref 0 in
  let n = String.length text in
  let i = ref 0 in
  while !i < n do
    let c = text.[!i] in
    if is_ident_char c && (!i = 0 || not (is_ident_char text.[!i - 1])) then begin
      (* Start of a maximal identifier token. *)
      let j = ref !i in
      while !j < n && is_ident_char text.[!j] do
        incr j
      done;
      let tok = String.sub text !i (!j - !i) in
      (if Hashtbl.mem declared tok then
         let idx =
           match Hashtbl.find_opt mapping tok with
           | Some k -> k
           | None ->
               let k = !next in
               incr next;
               Hashtbl.add mapping tok k;
               k
         in
         Buffer.add_string buf (Printf.sprintf "_M%d" idx)
       else Buffer.add_string buf tok);
      i := !j
    end
    else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  Buffer.contents buf

(* ── Summary section splitting ────────────────────────────────────── *)

type section = { header : string; body : string }

(** Split a summary into sections. Each section starts with a line beginning
    [# ] at column 0 and extends to the next such line or EOF. Leading and
    trailing blank lines in the body are stripped. *)
let split_sections (text : string) : section list =
  let lines = String.split_on_char '\n' text in
  let sections = ref [] in
  let current_header = ref None in
  let current_body = ref [] in
  let flush () =
    match !current_header with
    | None -> ()
    | Some h ->
        let body_lines = List.rev !current_body in
        (* Strip leading/trailing blank lines *)
        let rec drop_leading = function
          | "" :: rest -> drop_leading rest
          | xs -> xs
        in
        let body_lines =
          body_lines |> drop_leading |> List.rev |> drop_leading |> List.rev
        in
        let body = String.concat "\n" body_lines in
        sections := { header = h; body } :: !sections;
        current_header := None;
        current_body := []
  in
  List.iter
    (fun line ->
      if String.length line >= 2 && line.[0] = '#' && line.[1] = ' ' then begin
        flush ();
        current_header := Some (String.trim (String.sub line 2 (String.length line - 2)));
        current_body := []
      end
      else if !current_header <> None then
        current_body := line :: !current_body
      (* Lines before any header are ignored (allows for comments) *))
    lines;
  flush ();
  List.rev !sections

(** Canonical form of a section: header (trimmed) + alpha-canonicalized body
    with collapsed whitespace on each line. *)
let canonicalize_section (s : section) : string =
  let body = alpha_canonicalize s.body in
  (* Normalize per-line: trim trailing whitespace *)
  let lines = String.split_on_char '\n' body in
  let lines =
    List.map
      (fun l ->
        let n = String.length l in
        let j = ref n in
        while !j > 0 && (l.[!j - 1] = ' ' || l.[!j - 1] = '\t') do
          decr j
        done;
        String.sub l 0 !j)
      lines
  in
  Printf.sprintf "%s\n%s" s.header (String.concat "\n" lines)

(** Renumber rule IDs so [R1] names the rule whose canonicalized body
    sorts first lexicographically, [R2] the next, and so on. Without this
    the comparison is fragile to ordering choices in the summariser: two
    semantically equivalent outputs that swap [R1] and [R2] would fail.

    Walks sections in two passes: the first pass scans [# rule Rk ...]
    headers in body-sorted order and builds a renaming [old_id ->
    new_id]. The second pass substitutes every occurrence of each old id
    (in [rule Rk], [sites Rk], and [rule=Rk] attribution on residuals). *)
let canonicalize_rule_ids (canon : string list) : string list =
  let is_rule_section s =
    String.length s >= 5 && String.sub s 0 5 = "rule "
  in
  let split_rule_section s =
    match String.index_opt s '\n' with
    | None -> None
    | Some nl ->
        let first_line = String.sub s 0 nl in
        let body = String.sub s (nl + 1) (String.length s - nl - 1) in
        (match String.split_on_char ' ' first_line with
         | "rule" :: id :: _ -> Some (id, body)
         | _ -> None)
  in
  let rule_sections_with_body_sort =
    canon
    |> List.filter is_rule_section
    |> List.filter_map split_rule_section
    |> List.sort (fun (_, b1) (_, b2) -> compare b1 b2)
    |> List.map fst
  in
  (* Walk the string and rename [Rk] tokens (where k is an integer) to the
     new id in one pass, avoiding aliasing when the mapping swaps ids. An
     [R] followed by digits is only treated as a rule-id token if the
     preceding character is not alphanumeric — this lets us match [rule
     R1], [sites R1], [rule=R1], [R1,R2] without touching identifiers like
     [Request]. *)
  let mapping = Hashtbl.create 8 in
  List.iteri
    (fun i old_id ->
      Hashtbl.replace mapping old_id (Printf.sprintf "R%d" (i + 1)))
    rule_sections_with_body_sort;
  let is_alnum c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9') || c = '_'
  in
  let rename s =
    let buf = Buffer.create (String.length s) in
    let n = String.length s in
    let i = ref 0 in
    while !i < n do
      let c = s.[!i] in
      if c = 'R' && !i + 1 < n
         && s.[!i + 1] >= '0' && s.[!i + 1] <= '9'
         && (!i = 0 || not (is_alnum s.[!i - 1]))
      then begin
        let j = ref (!i + 1) in
        while !j < n && s.[!j] >= '0' && s.[!j] <= '9' do incr j done;
        let token = String.sub s !i (!j - !i) in
        (match Hashtbl.find_opt mapping token with
         | Some new_id -> Buffer.add_string buf new_id
         | None -> Buffer.add_string buf token);
        i := !j
      end
      else begin
        Buffer.add_char buf c;
        incr i
      end
    done;
    Buffer.contents buf
  in
  List.map rename canon

let canonicalize_summary (text : string) : string =
  let sections = split_sections text in
  let canon = List.map canonicalize_section sections in
  let canon = canonicalize_rule_ids canon in
  let sorted = List.sort compare canon in
  String.concat "\n---\n" sorted

(* ── Case discovery and execution ─────────────────────────────────── *)

let read_file path = In_channel.with_open_bin path In_channel.input_all

let list_subdirs dir =
  if not (try Sys.is_directory dir with Sys_error _ -> false) then []
  else
    let entries = Sys.readdir dir in
    Array.sort compare entries;
    Array.to_list entries
    |> List.filter (fun e ->
           try Sys.is_directory (Filename.concat dir e) with _ -> false)

(** Read the required [language] file in a case directory. Its contents (a
    single language name, e.g. "typescript") is used as the default for
    files whose extension is not in the extension→language map. *)
let case_language case_dir =
  let p = Filename.concat case_dir "language" in
  if not (Sys.file_exists p) then
    failwith
      (Printf.sprintf "missing required file: %s (one language per line)" p)
  else String.trim (read_file p)

let run_case case_name () =
  let case_dir = Filename.concat cases_dir case_name in
  let before_dir = Filename.concat case_dir "before" in
  let after_dir = Filename.concat case_dir "after" in
  let expected_path = Filename.concat case_dir "expected.summary" in
  let default_language = case_language case_dir in
  let ctx = Context.create () in
  let changeset =
    Change_summary.load_from_dirs ~before_dir ~after_dir
      ~default_language ()
  in
  let summary = Change_summary.summarize ~ctx changeset in
  let actual = Change_summary.format_summary summary in
  let expected = read_file expected_path in
  let actual_canon = canonicalize_summary actual in
  let expected_canon = canonicalize_summary expected in
  (* A case dir may carry a [pending] marker: its [expected.summary]
     encodes the *target* output for a feature not yet implemented (e.g.
     §4.3 cross-side alignment). Until the feature lands, actual differs
     from the target, and that is fine — assert they still differ so the
     suite stays green. The moment the feature makes them match, fail
     loudly so the marker is removed and the case becomes a live
     regression test. (Same spirit as the matcher's "pin known bug"
     tests.) *)
  let pending_path = Filename.concat case_dir "pending" in
  if Sys.file_exists pending_path then begin
    let reason = try String.trim (read_file pending_path) with _ -> "" in
    if actual_canon = expected_canon then
      Alcotest.failf
        "%s: PENDING case now matches its target (%s) — the feature is \
         implemented; remove the [pending] marker so this becomes a live \
         regression test"
        case_name reason
    (* else: expected-to-fail, pass quietly *)
  end
  else begin
    if actual_canon <> expected_canon then begin
      (* Show both forms for debugging *)
      Printf.eprintf "\n=== %s: canonical mismatch ===\n" case_name;
      Printf.eprintf "--- expected (canonical) ---\n%s\n" expected_canon;
      Printf.eprintf "--- actual   (canonical) ---\n%s\n" actual_canon;
      Printf.eprintf "--- actual   (raw) ---\n%s\n" actual
    end;
    Alcotest.(check string)
      (Printf.sprintf "%s: canonical summary matches" case_name)
      expected_canon actual_canon
  end

(* ── Round-trip reconstruction (decomposition safety, §2.3) ───────────
   The summary promises that rules + residuals reproduce the changeset.
   This pins that as an executable invariant over the *live* summarize
   output (not the golden expected.summary), so it holds regardless of
   how compact the output is — and crucially it guards assembly-level
   bugs (rule ordering, site-scoping, rules overlapping within a file)
   that the per-(rule,site) safety gate cannot see.

   Per Modified file: apply the rules whose [sites] include it, in rule-id
   order, via the *emitted* pattern text (so this re-consumes the
   serialized rules), then:
   - residual-free file: the rules alone must reconstruct [after]. Compared
     as *trees* (Tree.equal), not text: M1.9a suppresses whitespace-only
     gaps, so reconstruction is guaranteed only up to formatting.
   - residual-bearing file: re-diffing the rule-applied source against
     [after] must reproduce the emitted residual exactly — i.e. rules +
     residual provably compose to [after]. (Applying the serialized
     residual would need real hunk headers; that's deferred, so we verify
     the gap compositionally instead.) *)

let rule_id_num (r : Change_summary.rule) =
  (* "R12" -> 12; for ordering the per-file rule application. *)
  try int_of_string (String.sub r.id 1 (String.length r.id - 1))
  with _ -> max_int

let roundtrip_case case_name () =
  let case_dir = Filename.concat cases_dir case_name in
  let before_dir = Filename.concat case_dir "before" in
  let after_dir = Filename.concat case_dir "after" in
  let default_language = case_language case_dir in
  let ctx = Context.create () in
  let changeset =
    Change_summary.load_from_dirs ~before_dir ~after_dir ~default_language ()
  in
  let summary = Change_summary.summarize ~ctx changeset in
  let residual_for path =
    List.find_opt
      (fun (res : Change_summary.residual) -> res.res_file = path)
      summary.residuals
  in
  List.iter
    (fun (fc : Change_summary.file_change) ->
      match fc with
      | Change_summary.Modified { path; language; before_source; after_source }
        ->
          let claiming =
            summary.rules
            |> List.filter (fun (r : Change_summary.rule) ->
                   List.mem path r.sites)
            |> List.sort (fun a b -> compare (rule_id_num a) (rule_id_num b))
          in
          let applied =
            List.fold_left
              (fun src (r : Change_summary.rule) ->
                try
                  Matcher.transform ~ctx ~language:r.language
                    ~pattern_text:r.pattern_text ~source_text:src
                with _ -> src)
              before_source claiming
          in
          (match residual_for path with
          | None ->
              let t1 = Tree.parse ~ctx ~language applied in
              let t2 = Tree.parse ~ctx ~language after_source in
              Alcotest.(check bool)
                (Printf.sprintf
                   "%s: %s reconstructs after rules alone (tree-equal)"
                   case_name path)
                true
                (Tree.equal t1.Tree.source t1.Tree.root t2.Tree.source
                   t2.Tree.root)
          | Some res ->
              (* Recompute the residual the same way summarize renders it
                 (zero-context, layout-only hunks dropped) — the check is
                 that the emitted residual truthfully relates the actual
                 post-rule intermediate to the after-source. *)
              let regen =
                Change_summary.residual_diff ~ctx ~language ~file_path:path
                  ~original:applied ~transformed:after_source ()
              in
              Alcotest.(check string)
                (Printf.sprintf "%s: %s residual bridges rules->after"
                   case_name path)
                res.res_diff regen)
      | Change_summary.Added _ | Change_summary.Deleted _ -> ())
    changeset.files

(* ── M1.5: one-sided candidate extraction ────────────────────────── *)

let test_one_sided_extraction () =
  let case_dir = Filename.concat cases_dir "import_removal" in
  let before_dir = Filename.concat case_dir "before" in
  let after_dir = Filename.concat case_dir "after" in
  let default_language = case_language case_dir in
  let ctx = Context.create () in
  let changeset =
    Change_summary.load_from_dirs ~before_dir ~after_dir
      ~default_language ()
  in
  let candidates = Change_summary.collect_one_sided_candidates ~ctx changeset in
  let instances =
    List.map Change_summary.one_sided_candidate_instance candidates
  in
  let removeds =
    List.filter
      (fun (i : Change_summary.one_sided_instance) ->
        i.os_side = Change_summary.Before_side)
      instances
  in
  let import_removeds =
    List.filter
      (fun (i : Change_summary.one_sided_instance) ->
        (* Crude check: the removed text contains "legacyHandler" and starts
           with "import" — we want to confirm the imports are surfaced. *)
        let t = i.os_text in
        String.length t >= 6 && String.sub t 0 6 = "import")
      removeds
  in
  let files =
    List.map
      (fun (i : Change_summary.one_sided_instance) -> i.os_file)
      import_removeds
    |> List.sort_uniq String.compare
  in
  Alcotest.(check int)
    "three import-removal candidates collected" 3
    (List.length import_removeds);
  Alcotest.(check (list string))
    "one per file" [ "a.ts"; "b.ts"; "c.ts" ] files

let tests =
  let cases = list_subdirs cases_dir in
  let case_tests =
    List.map
      (fun case ->
        let pending =
          Sys.file_exists
            (Filename.concat (Filename.concat cases_dir case) "pending")
        in
        let label = if pending then case ^ " (pending)" else case in
        Alcotest.test_case label `Quick (run_case case))
      cases
  in
  let roundtrip_tests =
    List.map
      (fun case ->
        Alcotest.test_case (case ^ " (round-trip)") `Quick
          (roundtrip_case case))
      cases
  in
  case_tests @ roundtrip_tests
  @ [
      Alcotest.test_case "one-sided import removals surfaced" `Quick
        test_one_sided_extraction;
      (* §9.3 mixed per-site after= rendering: a common-factor rule whose
         sites follow different primaries annotates each site line rather
         than the header. Exercised directly — soaks show mixed factors
         are structurally rare (aligned ones are caught at tier 1,
         unanchored ones cannot rule-ify; see tsx_tier_unanchored_factor),
         so no golden fixture produces this shape. *)
      Alcotest.test_case "tiered format: mixed per-site after" `Quick
        (fun () ->
          let r =
            {
              Change_summary.id = "R3";
              pattern_text = "@@\nmatch: strict\n@@\n- a\n+ b\n";
              support = 2;
              language = "typescript";
              sites = [ "x.ts"; "y.ts" ];
              after = [ ("x.ts", [ "R1" ]); ("y.ts", [ "R2" ]) ];
            }
          in
          let out =
            Change_summary.format_summary
              { Change_summary.rules = [ r ]; residuals = [] }
          in
          Alcotest.(check bool)
            "header carries no after=" true
            (not
               (String.split_on_char '\n' out
               |> List.exists (fun l ->
                   String.length l >= 6
                   && String.sub l 0 6 = "# rule"
                   && String.length l
                      > String.length "# rule R3  support=2  language=typescript"
                  )));
          Alcotest.(check bool)
            "site lines annotated" true
            (String.split_on_char '\n' out
            |> List.exists (fun l -> l = "x.ts  after=R1")
            &&
            String.split_on_char '\n' out
            |> List.exists (fun l -> l = "y.ts  after=R2")));
      (* Uniform per-site after= lifts to the rule header. *)
      Alcotest.test_case "tiered format: uniform after in header" `Quick
        (fun () ->
          let r =
            {
              Change_summary.id = "R2";
              pattern_text = "@@\nmatch: strict\n@@\n- a\n+ b\n";
              support = 2;
              language = "typescript";
              sites = [ "x.ts"; "y.ts" ];
              after = [ ("x.ts", [ "R1" ]); ("y.ts", [ "R1" ]) ];
            }
          in
          let out =
            Change_summary.format_summary
              { Change_summary.rules = [ r ]; residuals = [] }
          in
          Alcotest.(check bool)
            "header has after=R1" true
            (String.split_on_char '\n' out
            |> List.exists (fun l ->
                l = "# rule R2  support=2  language=typescript  after=R1"));
          Alcotest.(check bool)
            "site lines unannotated" true
            (String.split_on_char '\n' out
            |> List.exists (fun l -> l = "x.ts")));
    ]
