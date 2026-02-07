open Cmdliner

let language =
  let doc = "Language grammar to use (typescript, kotlin, ...)." in
  Arg.(value & opt string "typescript" &
       info ["l"; "language"] ~docv:"LANG" ~doc)

let list_languages =
  let doc = "List available languages and exit." in
  Arg.(value & flag & info ["list-languages"] ~doc)

let match_pattern =
  let doc = "Pattern file for matching. Uses @@ delimiters with metavar \
             declarations. See README for pattern format." in
  Arg.(value & opt (some file) None & info ["match"; "m"] ~docv:"PATTERN" ~doc)

let include_pattern =
  let doc = "Glob pattern for files to include when scanning a directory \
             (e.g., '*.ts', '*.py'). Required when target is a directory." in
  Arg.(value & opt (some string) None & info ["include"; "i"] ~docv:"GLOB" ~doc)

let exclude_patterns =
  let doc = "Directory names to exclude when scanning (can be repeated). \
             Defaults to: node_modules, .git, _build, target" in
  Arg.(value & opt_all string [] & info ["exclude"; "e"] ~docv:"DIR" ~doc)

let apply_flag =
  let doc = "Enable transform mode: apply semantic patch and output unified diff." in
  Arg.(value & flag & info ["apply"; "a"] ~doc)

let in_place_flag =
  let doc = "Write changes directly to files (requires --apply)." in
  Arg.(value & flag & info ["in-place"] ~doc)

(* Default directories to skip *)
let default_excludes = ["node_modules"; ".git"; "_build"; "target"; "__pycache__"; ".hg"; ".svn"]

(* Simple glob pattern matching for filenames
   Supports: *.ext, prefix*, *suffix, exact match *)
let glob_match pattern filename =
  let basename = Filename.basename filename in
  if String.contains pattern '*' then
    let parts = String.split_on_char '*' pattern in
    match parts with
    | [prefix; suffix] ->
      String.length basename >= String.length prefix + String.length suffix &&
      String.starts_with ~prefix basename &&
      String.ends_with ~suffix basename
    | [prefix] when String.ends_with ~suffix:"*" pattern ->
      String.starts_with ~prefix basename
    | _ -> basename = pattern  (* fallback to exact match *)
  else
    basename = pattern

(* Recursively find files matching a glob pattern *)
let find_files ~pattern ~exclude_dirs root =
  let rec traverse acc dir =
    let entries = try Sys.readdir dir with Sys_error _ -> [||] in
    Array.fold_left (fun acc entry ->
      let path = Filename.concat dir entry in
      if Sys.is_directory path then begin
        (* Skip excluded directories *)
        if List.mem entry exclude_dirs then
          acc
        else
          traverse acc path
      end else begin
        (* Check if file matches the glob pattern *)
        if glob_match pattern path then
          path :: acc
        else
          acc
      end
    ) acc entries
  in
  traverse [] root |> List.rev

(* Format a match result with file path *)
let format_file_match ~file_path ~source_text (result : Diffract.Match.nested_match_result) =
  let line = result.start_point.row + 1 in
  let matched_text = Diffract.Tree.text source_text result.node in
  (* Truncate and single-line the matched text for display *)
  let preview =
    let text = String.map (fun c -> if c = '\n' then ' ' else c) matched_text in
    if String.length text > 60 then String.sub text 0 57 ^ "..."
    else text
  in
  let buf = Buffer.create 256 in
  Buffer.add_string buf (Printf.sprintf "%s:%d: %s\n" file_path line preview);
  (* Add bindings *)
  let bindings = result.bindings
    |> List.sort_uniq (fun (v1, _) (v2, _) -> String.compare v1 v2) in
  List.iter (fun (var, value) ->
    let value_preview =
      let v = String.map (fun c -> if c = '\n' then ' ' else c) value in
      if String.length v > 40 then String.sub v 0 37 ^ "..." else v
    in
    Buffer.add_string buf (Printf.sprintf "  %s = %s\n" var value_preview)
  ) bindings;
  Buffer.contents buf

(* Match result with parse info *)
type file_match_result = {
  file_path: string;
  source_text: string;
  matches: Diffract.Match.nested_match_result list;
  parse_errors: int;
  error: string option;
}

(* Match pattern against a single file *)
let match_file ~ctx ~language ~pattern_text file_path =
  try
    let source_text = In_channel.with_open_text file_path In_channel.input_all in
    let result = Diffract.Match.search ~ctx ~language ~pattern_text ~source_text in
    { file_path; source_text; matches = result.matches;
      parse_errors = result.parse_error_count; error = None }
  with
  | Failure msg -> { file_path; source_text = ""; matches = [];
                     parse_errors = 0; error = Some msg }
  | Sys_error msg -> { file_path; source_text = ""; matches = [];
                       parse_errors = 0; error = Some msg }

(* Scan a directory for pattern matches *)
let scan_directory ~ctx ~language ~pattern_text ~include_pattern ~exclude_dirs dir_path =
  let files = find_files ~pattern:include_pattern ~exclude_dirs dir_path in
  let total_files = List.length files in
  let total_matches = ref 0 in
  let files_with_matches = ref 0 in
  let files_with_parse_errors = ref [] in
  let errors = ref [] in

  List.iter (fun file_path ->
    let result = match_file ~ctx ~language ~pattern_text file_path in
    match result.error with
    | Some msg ->
      errors := (file_path, msg) :: !errors
    | None ->
      (* Track files with parse errors *)
      if result.parse_errors > 0 then
        files_with_parse_errors := (file_path, result.parse_errors) :: !files_with_parse_errors;
      if result.matches <> [] then begin
        incr files_with_matches;
        total_matches := !total_matches + List.length result.matches;
        List.iter (fun m ->
          print_string (format_file_match ~file_path ~source_text:result.source_text m);
          print_newline ()
        ) result.matches
      end
  ) files;

  (* Print summary *)
  Printf.printf "Found %d match(es) in %d file(s) (scanned %d files)\n"
    !total_matches !files_with_matches total_files;

  (* Report files with parse errors *)
  if !files_with_parse_errors <> [] then begin
    Printf.printf "\nParse errors (%d files):\n" (List.length !files_with_parse_errors);
    List.iter (fun (path, count) ->
      Printf.printf "  %s: %d error(s)\n" path count
    ) (List.rev !files_with_parse_errors)
  end;

  (* Report errors if any *)
  if !errors <> [] then begin
    Printf.printf "\nErrors (%d files):\n" (List.length !errors);
    List.iter (fun (path, msg) ->
      Printf.printf "  %s: %s\n" path msg
    ) (List.rev !errors)
  end

(* Transform a single file and return (had_changes, diff_text) *)
let transform_file ~ctx ~language ~pattern_text ~in_place file_path =
  let result = Diffract.Match.transform_file ~ctx ~language ~pattern_text ~source_path:file_path in
  if result.edits = [] then (false, "")
  else
    let diff = Diffract.Match.generate_diff ~file_path
      ~original:result.original_source ~transformed:result.transformed_source in
    if in_place then
      Out_channel.with_open_text file_path (fun oc ->
        output_string oc result.transformed_source);
    (true, diff)

(* Scan a directory for transforms *)
let scan_directory_transform ~ctx ~language ~pattern_text ~include_pattern ~exclude_dirs ~in_place dir_path =
  let files = find_files ~pattern:include_pattern ~exclude_dirs dir_path in
  let total_files = List.length files in
  let total_edits = ref 0 in
  let files_changed = ref 0 in
  let errors = ref [] in
  List.iter (fun file_path ->
    try
      let result = Diffract.Match.transform_file ~ctx ~language ~pattern_text ~source_path:file_path in
      if result.edits <> [] then begin
        incr files_changed;
        total_edits := !total_edits + List.length result.edits;
        let diff = Diffract.Match.generate_diff ~file_path
          ~original:result.original_source ~transformed:result.transformed_source in
        if not in_place then
          print_string diff
        else
          Out_channel.with_open_text file_path (fun oc ->
            output_string oc result.transformed_source)
      end
    with
    | Failure msg -> errors := (file_path, msg) :: !errors
    | Sys_error msg -> errors := (file_path, msg) :: !errors
  ) files;
  Printf.printf "Transformed %d match(es) in %d file(s) (scanned %d files)\n"
    !total_edits !files_changed total_files;
  if !errors <> [] then begin
    Printf.printf "\nErrors (%d files):\n" (List.length !errors);
    List.iter (fun (path, msg) ->
      Printf.printf "  %s: %s\n" path msg
    ) (List.rev !errors)
  end

let run file1 language list_languages match_pattern include_pattern exclude_patterns apply in_place =
  let ctx = Diffract.Context.create () in
  if list_languages then begin
    let langs = Diffract.available_languages () in
    print_endline "Available languages:";
    List.iter (fun l -> print_endline ("  " ^ l)) langs;
    `Ok ()
  end else if in_place && not apply then
    `Error (true, "--in-place requires --apply")
  else
    let exclude_dirs =
      if exclude_patterns = [] then default_excludes
      else exclude_patterns
    in
    match match_pattern, file1 with
    (* Pattern matching mode *)
    | Some pattern_path, Some source_path ->
      (try
        let pattern_text = In_channel.with_open_text pattern_path In_channel.input_all in
        if apply then begin
          (* Transform mode *)
          if Sys.is_directory source_path then begin
            match include_pattern with
            | None ->
              `Error (true, "Directory scanning requires --include pattern (e.g., --include '*.ts')")
            | Some glob ->
              scan_directory_transform ~ctx ~language ~pattern_text ~include_pattern:glob
                ~exclude_dirs ~in_place source_path;
              `Ok ()
          end else begin
            let (changed, diff) = transform_file ~ctx ~language ~pattern_text ~in_place source_path in
            if not changed then
              print_endline "No matches found"
            else if not in_place then
              print_string diff;
            `Ok ()
          end
        end else if Sys.is_directory source_path then begin
          (* Directory scanning mode *)
          match include_pattern with
          | None ->
            `Error (true, "Directory scanning requires --include pattern (e.g., --include '*.ts')")
          | Some glob ->
            scan_directory ~ctx ~language ~pattern_text ~include_pattern:glob ~exclude_dirs source_path;
            `Ok ()
        end else begin
          (* Single file mode - supports both simple and nested patterns *)
          let source_text = In_channel.with_open_text source_path In_channel.input_all in
          let search_result = Diffract.Match.search ~ctx ~language ~pattern_text ~source_text in
          let results = search_result.matches in
          if results = [] then
            print_endline "No matches found"
          else begin
            (* Check if this is a nested pattern (has contexts) *)
            let has_contexts = List.exists (fun r -> r.Diffract.Match.contexts <> []) results in
            if has_contexts then
              Printf.printf "Found %d nested match(es):\n\n" (List.length results)
            else
              Printf.printf "Found %d match(es):\n\n" (List.length results);
            List.iter (fun result ->
              print_endline (Diffract.Match.format_nested_match source_text result);
              print_newline ()
            ) results
          end;
          (* Report parse errors if any *)
          if search_result.parse_error_count > 0 then
            Printf.printf "\nWarning: %d parse error(s) in source file\n"
              search_result.parse_error_count;
          `Ok ()
        end
      with
      | Sys_error msg ->
        `Error (false, Printf.sprintf "Error reading file: %s" msg)
      | Failure msg ->
        `Error (false, msg))
    | Some _, None ->
      `Error (true, "Pattern matching requires a source file or directory")

    (* Parse mode (single file, no flags) *)
    | None, Some path ->
      (try
        let tree = Diffract.parse_file_tree ~ctx ~language path in
        print_string (Diffract.Tree.format_tree tree);
        let errors = Diffract.Tree.error_count tree in
        if errors > 0 then
          Printf.printf "\n%d parse error(s)\n" errors;
        `Ok ()
      with
      | Sys_error msg ->
        `Error (false, Printf.sprintf "Error reading file: %s" msg)
      | Failure msg ->
        `Error (false, msg))

    (* No arguments *)
    | None, None ->
      `Error (true, "Missing required argument FILE")

let run_term =
  let file1 = Arg.(value & pos 0 (some string) None &
                   info [] ~docv:"FILE" ~doc:"Source file or directory to process.") in
  Term.(ret (const run $ file1 $ language $ list_languages $ match_pattern $ include_pattern $ exclude_patterns $ apply_flag $ in_place_flag))

let cmd =
  let doc = "Parse and search source files using tree-sitter" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) is a tool for working with source code syntax trees. It can:";
    `I ("Parsing", "Output the concrete syntax tree (CST) as S-expressions.");
    `I ("Matching", "Find code patterns using concrete syntax with metavariables.");
    `S Manpage.s_examples;
    `P "$(b,Parsing:)";
    `Pre "  $(tname) example.ts";
    `Pre "  $(tname) --language kotlin example.kt";
    `P "$(b,Pattern matching:)";
    `Pre "  $(tname) --match pattern.txt source.ts";
    `Pre "  $(tname) --match pattern.txt --include '*.ts' src/";
    `Pre "  $(tname) --match pattern.txt --include '*.ts' -e vendor src/";
    `P "$(b,Transform (semantic patch):)";
    `Pre "  $(tname) --apply --match patch.txt source.ts";
    `Pre "  $(tname) --apply --in-place --match patch.txt --include '*.ts' src/";
    `S "PATTERN FORMAT";
    `P "Pattern files use @@ delimiters with metavariable declarations:";
    `Pre "  @@\n  metavar OBJ: single\n  metavar METHOD: single\n  @@\n  OBJ.METHOD()";
    `P "Metavariable names start with a dollar sign. Types: $(b,single) (one node) \
        or $(b,sequence) (zero or more). See README for full documentation.";
    `S "GLOB PATTERNS";
    `P "The --include option supports simple glob patterns:";
    `I ("*.ext", "Files ending with .ext");
    `I ("prefix*", "Files starting with prefix");
    `I ("*suffix", "Files ending with suffix");
    `S Manpage.s_bugs;
    `P "Report bugs at https://github.com/anthropics/diffract/issues"
  ] in
  let info = Cmd.info "diffract" ~version:"0.1.0" ~doc ~man in
  Cmd.v info run_term

let () = exit (Cmd.eval cmd)
