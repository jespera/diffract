(** Tests for File_scan — the shared glob/walk used by the CLI and the
    change-summary loader.

    The walk tests double as a regression guard: the CLI's old (now removed)
    copy called [Sys.is_directory] unguarded, so a broken symlink or unreadable
    entry aborted the whole scan. The shared walk must tolerate it. *)

open Diffract
module F = File_scan

(* ── glob_match ─────────────────────────────────────────────────── *)

let check_glob name expected pat file =
  Alcotest.test_case name `Quick (fun () ->
      Alcotest.(check bool) name expected (F.glob_match pat file))

let glob_tests =
  [
    (* matches the basename, not the directory part *)
    check_glob "suffix match" true "*.ts" "foo/bar.ts";
    check_glob "suffix no match" false "*.ts" "foo/bar.tsx";
    check_glob "suffix on basename only" true "*.ts" "a/b/c.d.ts";
    check_glob "prefix match" true "test_*" "x/test_foo.ml";
    check_glob "prefix no match" false "test_*" "x/foo.ml";
    check_glob "prefix+suffix match" true "a*c" "dir/abc";
    check_glob "prefix+suffix wrong prefix" false "a*c" "dir/xbc";
    check_glob "prefix+suffix wrong suffix" false "a*c" "dir/abx";
    check_glob "exact match" true "foo.ts" "p/foo.ts";
    check_glob "exact no match" false "foo.ts" "p/bar.ts";
    (* more than one [*] is not supported: falls back to a literal match *)
    check_glob "multi-star literal match" true "a*b*c" "x/a*b*c";
    check_glob "multi-star not a glob" false "a*b*c" "x/aXbYc";
  ]

(* ── temp tree helpers ──────────────────────────────────────────── *)

let touch path = Out_channel.with_open_bin path (fun _ -> ())

let rec rm_rf path =
  match Sys.is_directory path with
  | true ->
      Array.iter (fun e -> rm_rf (Filename.concat path e)) (Sys.readdir path);
      (try Unix.rmdir path with Sys_error _ | Unix.Unix_error _ -> ())
  | false -> ( try Sys.remove path with Sys_error _ -> ())
  (* a broken symlink raises here; remove the link itself (no follow) *)
  | exception Sys_error _ -> ( try Sys.remove path with Sys_error _ -> ())

(* Build a fixed tree under a fresh temp directory, run [f] on its root, then
   tear it down. Layout:

     root/a.ts
     root/b.txt
     root/sub/c.ts
     root/node_modules/d.ts
     root/dangling          -> (broken symlink) *)
let with_temp_tree f =
  let root = Filename.temp_file "diffract_fscan_" "" in
  Sys.remove root;
  Unix.mkdir root 0o755;
  let mk rel = Filename.concat root rel in
  Fun.protect
    ~finally:(fun () -> rm_rf root)
    (fun () ->
      touch (mk "a.ts");
      touch (mk "b.txt");
      Unix.mkdir (mk "sub") 0o755;
      touch (mk "sub/c.ts");
      Unix.mkdir (mk "node_modules") 0o755;
      touch (mk "node_modules/d.ts");
      Unix.symlink (mk "does/not/exist") (mk "dangling");
      f root)

let basenames paths = List.sort compare (List.map Filename.basename paths)

(* ── walk ───────────────────────────────────────────────────────── *)

let walk_tests =
  [
    Alcotest.test_case "recurses, filters by pred, excludes dirs" `Quick
      (fun () ->
        with_temp_tree (fun root ->
            let got =
              F.walk ~exclude_dirs:[ "node_modules" ]
                ~pred:(F.glob_match "*.ts") root []
              |> basenames
            in
            Alcotest.(check (list string))
              "only .ts outside node_modules" [ "a.ts"; "c.ts" ] got));
    Alcotest.test_case "descends excluded dir when not listed" `Quick (fun () ->
        with_temp_tree (fun root ->
            let got =
              F.walk ~exclude_dirs:[] ~pred:(F.glob_match "*.ts") root []
              |> basenames
            in
            Alcotest.(check (list string))
              "includes node_modules contents" [ "a.ts"; "c.ts"; "d.ts" ] got));
    Alcotest.test_case "tolerates a broken symlink (does not raise)" `Quick
      (fun () ->
        with_temp_tree (fun root ->
            (* pred accepts everything, so the dangling symlink is surfaced as a
               leaf — the point is the walk completes instead of aborting on
               [Sys.is_directory] of a broken link. *)
            let got =
              F.walk ~exclude_dirs:[ "node_modules" ] ~pred:(fun _ -> true) root
                []
              |> basenames
            in
            Alcotest.(check (list string))
              "all entries incl. dangling link"
              [ "a.ts"; "b.txt"; "c.ts"; "dangling" ]
              got));
  ]

let tests = glob_tests @ walk_tests
