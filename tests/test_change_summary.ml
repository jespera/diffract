(** Folder-based E2E tests for change summarization.

    Each case lives under [tests/change_summary_cases/<name>/] with [before/],
    [after/], and an [expected.summary] file. Comparison is alpha-equivalent
    over metavar names — the canonical form renames $identifier → $_N by
    order of first occurrence in the rule body. Whitespace in the pattern
    body and metavar name choice do not cause churn; structural drift does. *)

open Diffract

let cases_dir = "change_summary_cases"

(* ── Alpha-canonicalization ───────────────────────────────────────── *)

(** Replace every [$name] occurrence in [text] with [$_N] where N is the
    first-seen index of [name] in the text. Other characters pass through. *)
let alpha_canonicalize (text : string) : string =
  let buf = Buffer.create (String.length text) in
  let mapping = Hashtbl.create 8 in
  let next = ref 0 in
  let n = String.length text in
  let i = ref 0 in
  let is_ident_char c =
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9') || c = '_'
  in
  while !i < n do
    let c = text.[!i] in
    if c = '$' && !i + 1 < n && is_ident_char text.[!i + 1] then begin
      let j = ref (!i + 1) in
      while !j < n && is_ident_char text.[!j] do
        incr j
      done;
      let name = String.sub text (!i + 1) (!j - !i - 1) in
      let idx =
        match Hashtbl.find_opt mapping name with
        | Some k -> k
        | None ->
            let k = !next in
            incr next;
            Hashtbl.add mapping name k;
            k
      in
      Buffer.add_string buf (Printf.sprintf "$_%d" idx);
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

let canonicalize_summary (text : string) : string =
  let sections = split_sections text in
  let canon = List.map canonicalize_section sections in
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
    List.map (fun case -> Alcotest.test_case case `Quick (run_case case)) cases
  in
  case_tests
  @ [
      Alcotest.test_case "one-sided import removals surfaced" `Quick
        test_one_sided_extraction;
    ]
