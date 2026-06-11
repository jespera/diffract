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

let debug_tokens_flag =
  let doc =
    "Print how the pattern tokenizes (per section, with declared metavars and \
     any that are ABSENT) and exit, without searching or transforming. Use to \
     diagnose a pattern that matches nothing."
  in
  Arg.(value & flag & info [ "debug-tokens" ] ~doc)

(* Print the tokenized pattern for [--debug-tokens] and return [`Ok ()]. *)
let print_debug_tokens ~ctx ~language ~pattern_text =
  print_string (Diffract.Matcher.debug_tokens ~ctx ~language ~pattern_text);
  `Ok ()

(* Print any static pattern warnings to stderr (e.g. a partial/field section
   that replaces the whole matched span, dropping tolerated/ignored content). *)
let print_pattern_warnings ~pattern_text =
  List.iter
    (fun w -> Printf.eprintf "diffract: %s\n" w)
    (Diffract.Matcher.pattern_warnings pattern_text)

let verbose_flag =
  let doc = "Print phase progress and timing to stderr." in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

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

let transform_file ~ctx ~language ~pattern_text ~in_place file_path =
  let source_text = In_channel.with_open_text file_path In_channel.input_all in
  let transformed =
    Diffract.Matcher.transform ~ctx ~language ~pattern_text ~source_text
  in
  if transformed = source_text then (false, "")
  else begin
    let diff =
      Diffract.Text_diff.generate_diff ~file_path ~original:source_text
        ~transformed ()
    in
    if in_place then
      Out_channel.with_open_text file_path (fun oc ->
          output_string oc transformed);
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
      with Failure msg | Sys_error msg ->
        errors := (file_path, msg) :: !errors)
    files;
  Printf.printf "Transformed %d file(s) (scanned %d files)\n" !files_changed
    total_files;
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
      required
      & pos 0 (some file) None
      & info [] ~docv:"FILE" ~doc:"Source file to parse.")
  in
  Cmd.v (Cmd.info "parse" ~doc) Term.(ret (const run_parse $ file $ language))

(* ── search subcommand (tokenizer-based matcher) ────────────────────── *)

let line_col_of_byte source byte =
  let line = ref 1 and col = ref 1 in
  let n = min byte (String.length source) in
  for i = 0 to n - 1 do
    if source.[i] = '\n' then begin
      incr line;
      col := 1
    end
    else incr col
  done;
  (!line, !col)

let collapse ?(max = 60) s =
  let s = String.map (fun c -> if c = '\n' then ' ' else c) s in
  if String.length s > max then String.sub s 0 (max - 3) ^ "..." else s

(* Format one section's match. If [section_prefix] is given (composite
   case, 2+ sections), the location line is prefixed with e.g. "[§1] " and
   binding lines are indented further. *)
let format_section_match ?section_prefix ~file_path ~source_text
    (r : Diffract.Matcher.M.match_result) =
  let module M = Diffract.Matcher.M in
  let module Cur = Diffract.Tree_sitter_cursor in
  let line, col = line_col_of_byte source_text r.M.start_byte in
  let matched =
    String.sub source_text r.M.start_byte (r.M.end_byte - r.M.start_byte)
  in
  let buf = Buffer.create 128 in
  let header_indent, binding_indent =
    match section_prefix with
    | None -> ("", "  ")
    | Some p -> ("  " ^ p ^ " ", "       ")
  in
  Buffer.add_string buf
    (Printf.sprintf "%s%s:%d:%d: %s\n" header_indent file_path line col
       (collapse matched));
  List.iter
    (fun b ->
      let name, value =
        match b with
        | M.Single { name; cursor } ->
            let s, e = Cur.byte_range cursor in
            (name, String.sub source_text s (e - s))
        | M.Sequence { name; cursors } ->
            let parts =
              List.map
                (fun c ->
                  let s, e = Cur.byte_range c in
                  String.sub source_text s (e - s))
                cursors
            in
            (name, String.concat ", " parts)
      in
      Buffer.add_string buf
        (Printf.sprintf "%s%s = %s\n" binding_indent name
           (collapse ~max:40 value)))
    r.M.bindings;
  Buffer.contents buf

(* Format one composite match. Single-section composites print in the
   flat, headerless format (identical to pre-multi-section output);
   multi-section composites print a "composite match" header and each
   section labelled "[§N]". *)
let format_search_match ~file_path ~source_text
    (c : Diffract.Matcher.composite_match) =
  match c.sections with
  | [ single ] -> format_section_match ~file_path ~source_text single
  | sections ->
      let buf = Buffer.create 256 in
      Buffer.add_string buf (Printf.sprintf "%s: composite match\n" file_path);
      List.iteri
        (fun i r ->
          let prefix = Printf.sprintf "[\xc2\xa7%d]" (i + 1) in
          Buffer.add_string buf
            (format_section_match ~section_prefix:prefix ~file_path ~source_text
               r))
        sections;
      Buffer.contents buf

let search_file ~ctx ~language ~pattern_text file_path =
  try
    let source_text =
      In_channel.with_open_text file_path In_channel.input_all
    in
    (* Parse once; reuse the tree for both matching and parse diagnostics. *)
    let tree = Diffract.Tree.parse ~ctx ~language source_text in
    let results =
      Diffract.Matcher.find_in_tree ~ctx ~language ~pattern_text tree
    in
    Ok (source_text, results, Diffract.Tree.error_count tree)
  with Failure msg | Sys_error msg -> Error msg

let scan_directory_search ~ctx ~language ~pattern_text ~include_pattern
    ~exclude_dirs dir_path =
  let files = find_files ~pattern:include_pattern ~exclude_dirs dir_path in
  let total_files = List.length files in
  let total_matches = ref 0 in
  let files_with_matches = ref 0 in
  let files_with_parse_errors = ref [] in
  let errors = ref [] in
  List.iter
    (fun file_path ->
      match search_file ~ctx ~language ~pattern_text file_path with
      | Error msg -> errors := (file_path, msg) :: !errors
      | Ok (source_text, results, parse_errors) ->
          if parse_errors > 0 then
            files_with_parse_errors :=
              (file_path, parse_errors) :: !files_with_parse_errors;
          if results <> [] then begin
            incr files_with_matches;
            total_matches := !total_matches + List.length results;
            List.iter
              (fun r ->
                print_string (format_search_match ~file_path ~source_text r))
              results
          end)
    files;
  Printf.printf "Found %d match(es) in %d file(s) (scanned %d files)\n"
    !total_matches !files_with_matches total_files;
  if !files_with_parse_errors <> [] then begin
    Printf.printf
      "\n\
       Parse errors (%d files; matches in or near unparseable regions may be \
       missed):\n"
      (List.length !files_with_parse_errors);
    List.iter
      (fun (path, count) -> Printf.printf "  %s: %d error node(s)\n" path count)
      (List.rev !files_with_parse_errors)
  end;
  if !errors <> [] then begin
    Printf.printf "\nErrors (%d files):\n" (List.length !errors);
    List.iter
      (fun (path, msg) -> Printf.printf "  %s: %s\n" path msg)
      (List.rev !errors)
  end

let run_search pattern_path target language include_pattern exclude_patterns
    debug_tokens =
  let ctx = Diffract.Context.create () in
  let exclude_dirs =
    if exclude_patterns = [] then default_excludes else exclude_patterns
  in
  try
    let pattern_text =
      In_channel.with_open_text pattern_path In_channel.input_all
    in
    if debug_tokens then print_debug_tokens ~ctx ~language ~pattern_text
    else begin
      print_pattern_warnings ~pattern_text;
      if Sys.is_directory target then (
        match include_pattern with
        | None ->
            `Error
              ( true,
                "Directory target requires --include (e.g., --include '*.ts')"
              )
        | Some glob ->
            scan_directory_search ~ctx ~language ~pattern_text
              ~include_pattern:glob ~exclude_dirs target;
            `Ok ())
      else begin
        let source_text =
          In_channel.with_open_text target In_channel.input_all
        in
        let tree = Diffract.Tree.parse ~ctx ~language source_text in
        let results =
          Diffract.Matcher.find_in_tree ~ctx ~language ~pattern_text tree
        in
        if results = [] then print_endline "No matches found"
        else begin
          Printf.printf "Found %d match(es):\n\n" (List.length results);
          List.iter
            (fun r ->
              print_string
                (format_search_match ~file_path:target ~source_text r))
            results
        end;
        let parse_errors = Diffract.Tree.error_count tree in
        if parse_errors > 0 then
          Printf.printf
            "\n\
             Warning: %d parse error node(s) in source (matches in or near \
             unparseable regions may be missed)\n"
            parse_errors;
        `Ok ()
      end
    end
  with
  | Sys_error msg -> `Error (false, Printf.sprintf "Error reading file: %s" msg)
  | Failure msg -> `Error (false, msg)

let search_cmd =
  let doc =
    "Find pattern matches using the tokenizer-based matcher (strict mode; \
     supports code fragments and sigil-free metavars)."
  in
  let pattern =
    Arg.(
      required
      & pos 0 (some file) None
      & info [] ~docv:"PATTERN" ~doc:"Pattern file.")
  in
  let target =
    Arg.(
      required
      & pos 1 (some string) None
      & info [] ~docv:"FILE|DIR" ~doc:"Source file or directory to search.")
  in
  Cmd.v (Cmd.info "search" ~doc)
    Term.(
      ret
        (const run_search $ pattern $ target $ language $ include_pattern
       $ exclude_patterns $ debug_tokens_flag))

(* ── apply subcommand ───────────────────────────────────────────────── *)

let run_apply pattern_path target language include_pattern exclude_patterns
    in_place debug_tokens =
  let ctx = Diffract.Context.create () in
  let exclude_dirs =
    if exclude_patterns = [] then default_excludes else exclude_patterns
  in
  try
    let pattern_text =
      In_channel.with_open_text pattern_path In_channel.input_all
    in
    if debug_tokens then print_debug_tokens ~ctx ~language ~pattern_text
    else begin
      print_pattern_warnings ~pattern_text;
      if Sys.is_directory target then (
        match include_pattern with
        | None ->
            `Error
              ( true,
                "Directory target requires --include (e.g., --include '*.ts')"
              )
        | Some glob ->
            scan_directory_apply ~ctx ~language ~pattern_text
              ~include_pattern:glob ~exclude_dirs ~in_place target;
            `Ok ())
      else begin
        let changed, diff =
          transform_file ~ctx ~language ~pattern_text ~in_place target
        in
        if not changed then print_endline "No matches found"
        else if not in_place then print_string diff;
        `Ok ()
      end
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
      required
      & pos 0 (some file) None
      & info [] ~docv:"PATTERN" ~doc:"Semantic patch file.")
  in
  let target =
    Arg.(
      required
      & pos 1 (some string) None
      & info [] ~docv:"FILE|DIR" ~doc:"Source file or directory to transform.")
  in
  Cmd.v (Cmd.info "apply" ~doc)
    Term.(
      ret
        (const run_apply $ pattern $ target $ language $ include_pattern
       $ exclude_patterns $ in_place_flag $ debug_tokens_flag))

(* ── diff subcommand ────────────────────────────────────────────────── *)

let oneline ?(max_len = 120) s =
  let s = String.map (fun c -> if c = '\n' then ' ' else c) s in
  if String.length s > max_len then String.sub s 0 (max_len - 3) ^ "..." else s

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
              print_change ~indent:(indent ^ "  ") ~before_source ~after_source
                before after change)
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

(* ── summarize subcommand ──────────────────────────────────────────── *)

let run_summarize before_dir after_dir language include_pattern exclude_patterns
    verbose =
  let ctx = Diffract.Context.create () in
  let exclude_dirs =
    if exclude_patterns = [] then default_excludes else exclude_patterns
  in
  let phase name f =
    if not verbose then f ()
    else begin
      Printf.eprintf "[%s] start\n%!" name;
      let t0 = Unix.gettimeofday () in
      let result = f () in
      let dt = Unix.gettimeofday () -. t0 in
      Printf.eprintf "[%s] done in %.2fs\n%!" name dt;
      result
    end
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
    if not (Sys.is_directory before_dir) then
      `Error (false, Printf.sprintf "%s is not a directory" before_dir)
    else if not (Sys.is_directory after_dir) then
      `Error (false, Printf.sprintf "%s is not a directory" after_dir)
    else begin
      let changeset =
        phase "load" (fun () ->
            Diffract.Change_summary.load_from_dirs ~before_dir ~after_dir
              ~include_glob:include_pattern ~exclude_dirs
              ~default_language:language ())
      in
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
        phase "summarize" (fun () ->
            Diffract.Change_summary.summarize ?progress ~ctx changeset)
      in
      if verbose then
        Printf.eprintf "[summarize] %d rule(s)\n%!" (List.length summary.rules);
      if verbose then begin
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
          "[summary] file coverage: %d/%d modified files have at least one \
           rule firing\n\
           %!"
          cov total;
        if not (SS.is_empty uncovered) then begin
          Printf.eprintf "[summary] uncovered files (%d):\n%!"
            (SS.cardinal uncovered);
          SS.iter (fun p -> Printf.eprintf "  %s\n%!" p) uncovered
        end
      end;
      let output =
        phase "format" (fun () ->
            Diffract.Change_summary.format_summary summary)
      in
      print_string output;
      `Ok ()
    end
  with
  | Sys_error msg -> `Error (false, Printf.sprintf "Error reading file: %s" msg)
  | Failure msg -> `Error (false, msg)

let summarize_cmd =
  let doc = "Cluster systematic edits across a changeset into spatch rules." in
  let before_dir =
    Arg.(
      required
      & pos 0 (some string) None
      & info [] ~docv:"BEFORE_DIR" ~doc:"Directory containing the before state.")
  in
  let after_dir =
    Arg.(
      required
      & pos 1 (some string) None
      & info [] ~docv:"AFTER_DIR" ~doc:"Directory containing the after state.")
  in
  Cmd.v
    (Cmd.info "summarize" ~doc)
    Term.(
      ret
        (const run_summarize $ before_dir $ after_dir $ language
       $ include_pattern $ exclude_patterns $ verbose_flag))

let diff_cmd =
  let doc = "Show AST-level changes between two versions of a file." in
  let before_file =
    Arg.(
      required
      & pos 0 (some file) None
      & info [] ~docv:"BEFORE" ~doc:"Before (old) file.")
  in
  let after_file =
    Arg.(
      required
      & pos 1 (some file) None
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
      `I
        ( "$(b,search)",
          "Find pattern matches in a file or directory (handles code fragments \
           and sigil-free metavars)." );
      `I ("$(b,apply)", "Apply a semantic patch to a file or directory.");
      `I ("$(b,diff)", "Show AST-level changes between two file versions.");
      `I
        ( "$(b,summarize)",
          "Cluster systematic edits across a changeset into spatch rules." );
      `I ("$(b,languages)", "List available language grammars.");
      `S Manpage.s_examples;
      `Pre "  $(tname) parse example.ts";
      `Pre "  $(tname) search pattern.pat source.ts";
      `Pre "  $(tname) search --include '*.ts' pattern.pat src/";
      `Pre "  $(tname) apply patch.pat source.ts";
      `Pre "  $(tname) apply --in-place --include '*.ts' patch.pat src/";
      `Pre "  $(tname) diff before.ts after.ts";
    ]
  in
  let info = Cmd.info "diffract" ~version:"0.1.0" ~doc ~man in
  Cmd.group info
    [ languages_cmd; parse_cmd; search_cmd; apply_cmd; diff_cmd; summarize_cmd ]

let () = exit (Cmd.eval cmd)
