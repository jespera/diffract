open Cmdliner
open Cli_common

let print_coverage (changeset : Diffract.Change_summary.changeset)
    (summary : Diffract.Change_summary.summary) =
  let module SS = Set.Make (String) in
  let modified_paths =
    List.fold_left
      (fun acc fc ->
        match fc with
        | Diffract.Change_summary.Modified { path; _ } -> SS.add path acc
        | _ -> acc)
      SS.empty changeset.files
  in
  let covered_paths =
    List.fold_left
      (fun acc (r : Diffract.Change_summary.rule) ->
        List.fold_left (fun a p -> SS.add p a) acc r.sites)
      SS.empty summary.rules
  in
  let uncovered = SS.diff modified_paths covered_paths in
  let total = SS.cardinal modified_paths in
  let cov = SS.cardinal (SS.inter modified_paths covered_paths) in
  Printf.eprintf
    "[summary] file coverage: %d/%d modified files have at least one rule firing\n\
     %!"
    cov total;
  if not (SS.is_empty uncovered) then begin
    Printf.eprintf "[summary] uncovered files (%d):\n%!" (SS.cardinal uncovered);
    SS.iter (fun p -> Printf.eprintf "  %s\n%!" p) uncovered
  end

(* ── summarize subcommand ──────────────────────────────────────────── *)

(* Run function `f` and return result. If `verbose`, then we print
    wall-clock for running function. *)
let phase verbose name f =
  if not verbose then f ()
  else begin
    Printf.eprintf "[%s] start\n%!" name;
    let t0 = Unix.gettimeofday () in
    let result = f () in
    let dt = Unix.gettimeofday () -. t0 in
    Printf.eprintf "[%s] done in %.2fs\n%!" name dt;
    result
  end

let run before_dir after_dir pairs language include_pattern exclude_patterns
    verbose ignore_formatting output_format =
  let ctx = Diffract.Context.create () in
  let exclude_dirs =
    if exclude_patterns = [] then default_excludes else exclude_patterns
  in
  let count_files (cs : Diffract.Change_summary.changeset) =
    List.fold_left
      (fun (m, a, d) fc ->
        match fc with
        | Diffract.Change_summary.Modified _ -> (m + 1, a, d)
        | Diffract.Change_summary.Added _ -> (m, a + 1, d)
        | Diffract.Change_summary.Deleted _ -> (m, a, d + 1))
      (0, 0, 0) cs.files
  in
  try
    let load_result =
      match (pairs, before_dir, after_dir) with
      | Some manifest, _, _ ->
          if not (Sys.file_exists manifest) then
            Error (Printf.sprintf "no such manifest: %s" manifest)
          else
            (* The manifest enumerates the changeset, so --include is optional
               here: it narrows one manifest to a single language's files,
               letting the same checkout serve several runs. *)
            Ok
              (fun () ->
                Diffract.Change_summary.load_from_pairs ~manifest
                  ~include_glob:include_pattern ~language ())
      | None, Some b, Some a ->
          if not (Sys.is_directory b) then
            Error (Printf.sprintf "%s is not a directory" b)
          else if not (Sys.is_directory a) then
            Error (Printf.sprintf "%s is not a directory" a)
          else if include_pattern = None then
            (* Walking directories, --include is mandatory: it scopes which
               files are parsed with the [--language] grammar, instead of
               silently feeding every walked file to it. *)
            Error
              "--include is required when summarizing two directories (e.g., \
               --include '*.kt'): it scopes which files are summarized. Run \
               once per language/extension set."
          else
            Ok
              (fun () ->
                Diffract.Change_summary.load_from_dirs ~before_dir:b
                  ~after_dir:a ~include_glob:include_pattern ~exclude_dirs
                  ~language ())
      | None, _, _ ->
          Error
            "summarize needs either BEFORE_DIR and AFTER_DIR, or --pairs \
             MANIFEST (see scripts/diffract-checkout.sh, which writes one)."
    in
    match load_result with
    | Error msg -> `Error (true, msg)
    | Ok load -> begin
        let changeset = phase verbose "load" load in
        if verbose then begin
          let m, a, d = count_files changeset in
          Printf.eprintf "[load] %d modified, %d added, %d deleted\n%!" m a d;
          Printf.eprintf "[summarize] parsing %d file pair(s)...\n%!" m
        end;
        let progress =
          if verbose then
            Some
              (fun ~stage ~idx ~total ~path ->
                Printf.eprintf "[summarize] (%s %d/%d) %s\n%!" stage idx total
                  path)
          else None
        in
        let summary =
          phase verbose "summarize" (fun () ->
              Diffract.Change_summary.summarize ?progress ~ignore_formatting
                ~ctx changeset)
        in
        if verbose then
          Printf.eprintf "[summarize] %d rule(s)\n%!"
            (List.length summary.rules);
        if verbose then begin
          print_coverage changeset summary
        end;
        let output =
          phase verbose "format" (fun () ->
              match output_format with
              | `Json -> Diffract.Change_summary.format_summary_json summary
              | `Text -> Diffract.Change_summary.format_summary summary
              | `TextMinimal ->
                  Diffract.Change_summary.format_summary ~sites:`Count
                    ~residuals:`Grouped summary)
        in
        print_string output;
        `Ok ()
      end
  with
  | Sys_error msg -> `Error (false, Printf.sprintf "Error reading file: %s" msg)
  | Failure msg -> `Error (false, msg)

let cmd =
  let doc = "Cluster systematic edits across a changeset into spatch rules." in
  let before_dir =
    Arg.(
      value
      & pos 0 (some string) None
      & info [] ~docv:"BEFORE_DIR"
          ~doc:
            "Directory containing the before state. Omit when using \
             $(b,--pairs).")
  in
  let after_dir =
    Arg.(
      value
      & pos 1 (some string) None
      & info [] ~docv:"AFTER_DIR"
          ~doc:
            "Directory containing the after state. Omit when using \
             $(b,--pairs).")
  in
  let pairs =
    let doc =
      "Read the changeset from a change-pair manifest instead of pairing two \
       directories by path. This is the input that can express a $(i,rename): \
       two trees cannot say that a before-file corresponds to a \
       differently-named after-file. $(b,scripts/diffract-checkout.sh) writes \
       one alongside the trees it extracts. $(b,--include) is optional here \
       and narrows the manifest to one language's files."
    in
    Arg.(value & opt (some file) None & info [ "pairs" ] ~docv:"MANIFEST" ~doc)
  in
  let ignore_formatting =
    let doc =
      "Treat formatting as invisible: drop reflow-only residuals \
       (re-indentation plus trailing separators, e.g. a trailing comma a \
       formatter adds) rather than reporting them as unexplained changes. \
       Structural changes are still kept."
    in
    Arg.(value & flag & info [ "ignore-formatting" ] ~doc)
  in
  let output_format =
    (* An enum so further formats (e.g. SARIF for code-review integration)
       can slot in without changing the CLI shape. *)
    let doc =
      "Output format: $(b,text) (the .summary format; default), \
       $(b,text-minimal) (same, but each rule's site list collapses to a \
       one-line file count — a reading mode), or $(b,json) (one object with \
       $(b,rules) and $(b,residuals) arrays, for filtering with e.g. jq)."
    in
    Arg.(
      value
      & opt
          (enum
             [
               ("text", `Text); ("text-minimal", `TextMinimal); ("json", `Json);
             ])
          `Text
      & info [ "format" ] ~docv:"FORMAT" ~doc)
  in
  Cmd.v
    (Cmd.info "summarize" ~doc)
    Term.(
      ret
        (const run $ before_dir $ after_dir $ pairs $ language $ include_pattern
       $ exclude_patterns $ verbose_flag $ ignore_formatting $ output_format))
