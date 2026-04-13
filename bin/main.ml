open Cmdliner

(* ── Shared arguments ───────────────────────────────────────────────── *)

let language =
  let doc = "Language grammar to use (typescript, kotlin, ...)." in
  Arg.(
    value & opt string "typescript" & info [ "l"; "language" ] ~docv:"LANG" ~doc)

let include_pattern =
  let doc =
    "Glob pattern for files to include when scanning a directory (e.g., \
     '*.ts', '*.py'). Required when target is a directory."
  in
  Arg.(
    value & opt (some string) None & info [ "include"; "i" ] ~docv:"GLOB" ~doc)

let exclude_patterns =
  let doc =
    "Directory names to exclude when scanning (can be repeated). Defaults to: \
     node_modules, .git, _build, target"
  in
  Arg.(value & opt_all string [] & info [ "exclude"; "e" ] ~docv:"DIR" ~doc)

let in_place_flag =
  let doc = "Write changes directly to files instead of printing a diff." in
  Arg.(value & flag & info [ "in-place" ] ~doc)

(* ── File utilities ─────────────────────────────────────────────────── *)

let default_excludes =
  [ "node_modules"; ".git"; "_build"; "target"; "__pycache__"; ".hg"; ".svn" ]

let glob_match pattern filename =
  let basename = Filename.basename filename in
  if String.contains pattern '*' then
    let parts = String.split_on_char '*' pattern in
    match parts with
    | [ prefix; suffix ] ->
        String.length basename >= String.length prefix + String.length suffix
        && String.starts_with ~prefix basename
        && String.ends_with ~suffix basename
    | [ prefix ] when String.ends_with ~suffix:"*" pattern ->
        String.starts_with ~prefix basename
    | _ -> basename = pattern
  else basename = pattern

let find_files ~pattern ~exclude_dirs root =
  let rec traverse acc dir =
    let entries = try Sys.readdir dir with Sys_error _ -> [||] in
    Array.fold_left
      (fun acc entry ->
        let path = Filename.concat dir entry in
        if Sys.is_directory path then
          if List.mem entry exclude_dirs then acc else traverse acc path
        else if glob_match pattern path then path :: acc
        else acc)
      acc entries
  in
  traverse [] root |> List.rev

let format_file_match ~file_path ~source_text
    (result : Diffract.Match.nested_match_result) =
  let line = result.start_point.row + 1 in
  let matched_text = Diffract.Tree.text source_text result.node in
  let preview =
    let text = String.map (fun c -> if c = '\n' then ' ' else c) matched_text in
    if String.length text > 60 then String.sub text 0 57 ^ "..." else text
  in
  let buf = Buffer.create 256 in
  Buffer.add_string buf (Printf.sprintf "%s:%d: %s\n" file_path line preview);
  let bindings =
    result.bindings
    |> List.sort_uniq (fun (v1, _) (v2, _) -> String.compare v1 v2)
  in
  List.iter
    (fun (var, value) ->
      let value_preview =
        let v = String.map (fun c -> if c = '\n' then ' ' else c) value in
        if String.length v > 40 then String.sub v 0 37 ^ "..." else v
      in
      Buffer.add_string buf (Printf.sprintf "  %s = %s\n" var value_preview))
    bindings;
  Buffer.contents buf

let match_file ~ctx ~language ~pattern_text file_path =
  try
    let source_text =
      In_channel.with_open_text file_path In_channel.input_all
    in
    let result =
      Diffract.Match.search ~ctx ~language ~pattern_text ~source_text
    in
    Ok (source_text, result)
  with
  | Failure msg | Sys_error msg -> Error msg

let scan_directory_match ~ctx ~language ~pattern_text ~include_pattern
    ~exclude_dirs dir_path =
  let files = find_files ~pattern:include_pattern ~exclude_dirs dir_path in
  let total_files = List.length files in
  let total_matches = ref 0 in
  let files_with_matches = ref 0 in
  let files_with_parse_errors = ref [] in
  let errors = ref [] in
  List.iter
    (fun file_path ->
      match match_file ~ctx ~language ~pattern_text file_path with
      | Error msg -> errors := (file_path, msg) :: !errors
      | Ok (source_text, result) ->
          if result.Diffract.Match.parse_error_count > 0 then
            files_with_parse_errors :=
              (file_path, result.parse_error_count) :: !files_with_parse_errors;
          if result.matches <> [] then begin
            incr files_with_matches;
            total_matches := !total_matches + List.length result.matches;
            List.iter
              (fun m ->
                print_string (format_file_match ~file_path ~source_text m);
                print_newline ())
              result.matches
          end)
    files;
  Printf.printf "Found %d match(es) in %d file(s) (scanned %d files)\n"
    !total_matches !files_with_matches total_files;
  if !files_with_parse_errors <> [] then begin
    Printf.printf "\nParse errors (%d files):\n"
      (List.length !files_with_parse_errors);
    List.iter
      (fun (path, count) -> Printf.printf "  %s: %d error(s)\n" path count)
      (List.rev !files_with_parse_errors)
  end;
  if !errors <> [] then begin
    Printf.printf "\nErrors (%d files):\n" (List.length !errors);
    List.iter
      (fun (path, msg) -> Printf.printf "  %s: %s\n" path msg)
      (List.rev !errors)
  end

let transform_file ~ctx ~language ~pattern_text ~in_place file_path =
  let source_text = In_channel.with_open_text file_path In_channel.input_all in
  let result =
    Diffract.Match.transform_nested ~ctx ~language ~pattern_text ~source_text
  in
  if result.edits = [] then (false, "")
  else begin
    let diff =
      Diffract.Match.generate_diff ~file_path ~original:result.original_source
        ~transformed:result.transformed_source
    in
    if in_place then
      Out_channel.with_open_text file_path (fun oc ->
          output_string oc result.transformed_source);
    (true, diff)
  end

let scan_directory_apply ~ctx ~language ~pattern_text ~include_pattern
    ~exclude_dirs ~in_place dir_path =
  let files = find_files ~pattern:include_pattern ~exclude_dirs dir_path in
  let total_files = List.length files in
  let total_edits = ref 0 in
  let files_changed = ref 0 in
  let errors = ref [] in
  List.iter
    (fun file_path ->
      try
        let changed, diff =
          transform_file ~ctx ~language ~pattern_text ~in_place file_path
        in
        if changed then begin
          incr files_changed;
          incr total_edits;
          if not in_place then print_string diff
        end
      with
      | Failure msg | Sys_error msg -> errors := (file_path, msg) :: !errors)
    files;
  Printf.printf "Transformed %d file(s) (scanned %d files)\n"
    !files_changed total_files;
  if !errors <> [] then begin
    Printf.printf "\nErrors (%d files):\n" (List.length !errors);
    List.iter
      (fun (path, msg) -> Printf.printf "  %s: %s\n" path msg)
      (List.rev !errors)
  end;
  ignore total_edits

(* ── languages subcommand ───────────────────────────────────────────── *)

let run_languages () =
  let langs = Diffract.available_languages () in
  print_endline "Available languages:";
  List.iter (fun l -> print_endline ("  " ^ l)) langs

let languages_cmd =
  let doc = "List available language grammars." in
  Cmd.v (Cmd.info "languages" ~doc) Term.(const run_languages $ const ())

(* ── parse subcommand ───────────────────────────────────────────────── *)

let run_parse file language =
  let ctx = Diffract.Context.create () in
  try
    let open Diffract.Tree in
    let tree = Diffract.parse_file_tree ~ctx ~language file in
    print_string (format_tree tree);
    let error_nodes = get_errors tree in
    if error_nodes <> [] then begin
      Printf.printf "\n%d parse error(s):\n" (List.length error_nodes);
      List.iter
        (fun node ->
          let row = node.start_point.row + 1 in
          let col = node.start_point.column + 1 in
          let snippet =
            text tree.source node
            |> String.map (fun c -> if c = '\n' then ' ' else c)
            |> fun s ->
            if String.length s > 60 then String.sub s 0 57 ^ "..." else s
          in
          Printf.printf "  %d:%d  %s\n" row col snippet)
        error_nodes
    end;
    `Ok ()
  with
  | Sys_error msg -> `Error (false, Printf.sprintf "Error reading file: %s" msg)
  | Failure msg -> `Error (false, msg)

let parse_cmd =
  let doc = "Display the concrete syntax tree of a source file." in
  let file =
    Arg.(
      required & pos 0 (some file) None
      & info [] ~docv:"FILE" ~doc:"Source file to parse.")
  in
  Cmd.v (Cmd.info "parse" ~doc)
    Term.(ret (const run_parse $ file $ language))

(* ── match subcommand ───────────────────────────────────────────────── *)

let run_match pattern_path target language include_pattern exclude_patterns =
  let ctx = Diffract.Context.create () in
  let exclude_dirs =
    if exclude_patterns = [] then default_excludes else exclude_patterns
  in
  try
    let pattern_text =
      In_channel.with_open_text pattern_path In_channel.input_all
    in
    if Sys.is_directory target then
      match include_pattern with
      | None ->
          `Error
            ( true,
              "Directory target requires --include (e.g., --include '*.ts')" )
      | Some glob ->
          scan_directory_match ~ctx ~language ~pattern_text
            ~include_pattern:glob ~exclude_dirs target;
          `Ok ()
    else begin
      let source_text = In_channel.with_open_text target In_channel.input_all in
      let search_result =
        Diffract.Match.search ~ctx ~language ~pattern_text ~source_text
      in
      let results = search_result.matches in
      if results = [] then print_endline "No matches found"
      else begin
        let has_contexts =
          List.exists (fun r -> r.Diffract.Match.contexts <> []) results
        in
        if has_contexts then
          Printf.printf "Found %d nested match(es):\n\n" (List.length results)
        else Printf.printf "Found %d match(es):\n\n" (List.length results);
        List.iter
          (fun result ->
            print_endline
              (Diffract.Match.format_nested_match source_text result);
            print_newline ())
          results
      end;
      if search_result.parse_error_count > 0 then
        Printf.printf "\nWarning: %d parse error(s) in source file\n"
          search_result.parse_error_count;
      `Ok ()
    end
  with
  | Sys_error msg -> `Error (false, Printf.sprintf "Error reading file: %s" msg)
  | Failure msg -> `Error (false, msg)

let match_cmd =
  let doc = "Find pattern matches in a source file or directory." in
  let pattern =
    Arg.(
      required & pos 0 (some file) None
      & info [] ~docv:"PATTERN" ~doc:"Pattern file.")
  in
  let target =
    Arg.(
      required & pos 1 (some string) None
      & info [] ~docv:"FILE|DIR" ~doc:"Source file or directory to search.")
  in
  Cmd.v (Cmd.info "match" ~doc)
    Term.(
      ret
        (const run_match $ pattern $ target $ language $ include_pattern
       $ exclude_patterns))

(* ── apply subcommand ───────────────────────────────────────────────── *)

let run_apply pattern_path target language include_pattern exclude_patterns
    in_place =
  let ctx = Diffract.Context.create () in
  let exclude_dirs =
    if exclude_patterns = [] then default_excludes else exclude_patterns
  in
  try
    let pattern_text =
      In_channel.with_open_text pattern_path In_channel.input_all
    in
    if Sys.is_directory target then
      match include_pattern with
      | None ->
          `Error
            ( true,
              "Directory target requires --include (e.g., --include '*.ts')" )
      | Some glob ->
          scan_directory_apply ~ctx ~language ~pattern_text
            ~include_pattern:glob ~exclude_dirs ~in_place target;
          `Ok ()
    else begin
      let changed, diff =
        transform_file ~ctx ~language ~pattern_text ~in_place target
      in
      if not changed then print_endline "No matches found"
      else if not in_place then print_string diff;
      `Ok ()
    end
  with
  | Sys_error msg -> `Error (false, Printf.sprintf "Error reading file: %s" msg)
  | Failure msg -> `Error (false, msg)

let apply_cmd =
  let doc =
    "Apply a semantic patch to a source file or directory, printing a unified \
     diff or editing files in place."
  in
  let pattern =
    Arg.(
      required & pos 0 (some file) None
      & info [] ~docv:"PATTERN" ~doc:"Semantic patch file.")
  in
  let target =
    Arg.(
      required & pos 1 (some string) None
      & info [] ~docv:"FILE|DIR" ~doc:"Source file or directory to transform.")
  in
  Cmd.v (Cmd.info "apply" ~doc)
    Term.(
      ret
        (const run_apply $ pattern $ target $ language $ include_pattern
       $ exclude_patterns $ in_place_flag))

(* ── diff subcommand ────────────────────────────────────────────────── *)

let oneline ?(max_len = 120) s =
  let s = String.map (fun c -> if c = '\n' then ' ' else c) s in
  if String.length s > max_len then String.sub s 0 (max_len - 3) ^ "..."
  else s

let rec print_change ~indent ~before_source ~after_source
    (before_n : Diffract.Tree.src Diffract.Tree.t)
    (after_n : Diffract.Tree.src Diffract.Tree.t)
    (change : Diffract.Tree_diff.node_change) =
  let type_label =
    if before_n.node_type = after_n.node_type then before_n.node_type
    else before_n.node_type ^ " → " ^ after_n.node_type
  in
  match change with
  | Diffract.Tree_diff.Unchanged -> ()
  | Diffract.Tree_diff.Replaced ->
      let bt = Diffract.Tree.text before_source before_n in
      let at = Diffract.Tree.text after_source after_n in
      Printf.printf "%s%s  Replaced\n" indent type_label;
      Printf.printf "%s  - %s\n" indent (oneline bt);
      Printf.printf "%s  + %s\n" indent (oneline at)
  | Diffract.Tree_diff.Modified { child_changes } ->
      let line = before_n.start_point.row + 1 in
      Printf.printf "%s%s  Modified  (line %d)\n" indent type_label line;
      let same_run = ref 0 in
      let flush_same () =
        if !same_run > 0 then begin
          Printf.printf "%s  [%d unchanged]\n" indent !same_run;
          same_run := 0
        end
      in
      List.iter
        (function
          | Diffract.Tree_diff.Same _ -> incr same_run
          | Diffract.Tree_diff.Removed { node } ->
              flush_same ();
              Printf.printf "%s  %s  Removed\n" indent node.node_type;
              Printf.printf "%s    - %s\n" indent
                (oneline (Diffract.Tree.text before_source node))
          | Diffract.Tree_diff.Added { node } ->
              flush_same ();
              Printf.printf "%s  %s  Added\n" indent node.node_type;
              Printf.printf "%s    + %s\n" indent
                (oneline (Diffract.Tree.text after_source node))
          | Diffract.Tree_diff.Changed { before; after; change } ->
              flush_same ();
              print_change
                ~indent:(indent ^ "  ")
                ~before_source ~after_source before after change)
        child_changes;
      flush_same ()

let run_diff before_path after_path language =
  let ctx = Diffract.Context.create () in
  try
    let before_source =
      In_channel.with_open_text before_path In_channel.input_all
    in
    let after_source =
      In_channel.with_open_text after_path In_channel.input_all
    in
    let before_tree = Diffract.parse_tree ~ctx ~language before_source in
    let after_tree = Diffract.parse_tree ~ctx ~language after_source in
    let d = Diffract.Tree_diff.diff ~before:before_tree ~after:after_tree in
    Printf.printf "Language: %s\n\n" language;
    (match d.root_change with
    | Diffract.Tree_diff.Unchanged -> print_endline "No changes detected."
    | _ ->
        print_change ~indent:"" ~before_source ~after_source d.before_root
          d.after_root d.root_change);
    `Ok ()
  with
  | Sys_error msg -> `Error (false, Printf.sprintf "Error reading file: %s" msg)
  | Failure msg -> `Error (false, msg)

let diff_cmd =
  let doc = "Show AST-level changes between two versions of a file." in
  let before_file =
    Arg.(
      required & pos 0 (some file) None
      & info [] ~docv:"BEFORE" ~doc:"Before (old) file.")
  in
  let after_file =
    Arg.(
      required & pos 1 (some file) None
      & info [] ~docv:"AFTER" ~doc:"After (new) file.")
  in
  Cmd.v (Cmd.info "diff" ~doc)
    Term.(ret (const run_diff $ before_file $ after_file $ language))

(* ── top-level command ──────────────────────────────────────────────── *)

let cmd =
  let doc = "Parse, search, and transform source files using tree-sitter." in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(tname) works with concrete syntax trees produced by tree-sitter. \
         Choose a subcommand:";
      `I ("$(b,parse)", "Display the syntax tree of a source file.");
      `I ("$(b,match)", "Find pattern matches in a file or directory.");
      `I ("$(b,apply)", "Apply a semantic patch to a file or directory.");
      `I ("$(b,diff)", "Show AST-level changes between two file versions.");
      `I ("$(b,languages)", "List available language grammars.");
      `S Manpage.s_examples;
      `Pre "  $(tname) parse example.ts";
      `Pre "  $(tname) match pattern.pat source.ts";
      `Pre "  $(tname) match --include '*.ts' pattern.pat src/";
      `Pre "  $(tname) apply patch.pat source.ts";
      `Pre "  $(tname) apply --in-place --include '*.ts' patch.pat src/";
      `Pre "  $(tname) diff before.ts after.ts";
    ]
  in
  let info = Cmd.info "diffract" ~version:"0.1.0" ~doc ~man in
  Cmd.group info
    [ languages_cmd; parse_cmd; match_cmd; apply_cmd; diff_cmd ]

let () = exit (Cmd.eval cmd)
