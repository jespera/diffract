open Cmdliner

let language =
  let doc = "Language grammar to use (typescript, kotlin, ...)." in
  Arg.(value & opt string "typescript" &
       info ["l"; "language"] ~docv:"LANG" ~doc)

let list_languages =
  let doc = "List available languages and exit." in
  Arg.(value & flag & info ["list-languages"] ~doc)

let flat_diff =
  let doc = "Show flattened diff (one change per line) instead of hierarchical." in
  Arg.(value & flag & info ["flat"] ~doc)

let antiunify_diff =
  let doc = "Show anti-unified changes (only differing parts shown as [before → after])." in
  Arg.(value & flag & info ["antiunify"; "a"] ~doc)

let match_pattern =
  let doc = "Pattern file for matching. Pattern format: @@ on first line, \
             metavars (e.g., $msg $fn) on next lines, @@ to close, then the \
             pattern code using those metavars." in
  Arg.(value & opt (some file) None & info ["match"; "m"] ~docv:"PATTERN" ~doc)

let run file1 file2 language list_languages flat_diff antiunify_diff match_pattern =
  if list_languages then begin
    let langs = Diffract.available_languages () in
    print_endline "Available languages:";
    List.iter (fun l -> print_endline ("  " ^ l)) langs;
    `Ok ()
  end else
    match match_pattern, file1, file2 with
    | Some pattern_path, Some source_path, None ->
      (* Pattern matching mode - supports both simple and nested patterns *)
      (try
        let pattern_text = In_channel.with_open_text pattern_path In_channel.input_all in
        let source_text = In_channel.with_open_text source_path In_channel.input_all in
        let results = Diffract.Match.find_nested_matches ~language ~pattern_text ~source_text in
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
        `Ok ()
      with
      | Sys_error msg ->
        `Error (false, Printf.sprintf "Error reading file: %s" msg)
      | Failure msg ->
        `Error (false, msg))
    | Some _, None, _ ->
      `Error (true, "Pattern matching requires a source file")
    | Some _, Some _, Some _ ->
      `Error (true, "Pattern matching only takes one source file")
    | None, None, _ ->
      `Error (true, "Missing required argument FILE")
    | None, Some path, None ->
      (* Single file mode: parse and print S-expression *)
      (try
        let sexp = Diffract.parse_file_to_sexp ~language path in
        print_endline sexp;
        `Ok ()
      with
      | Sys_error msg ->
        `Error (false, Printf.sprintf "Error reading file: %s" msg)
      | Failure msg ->
        `Error (false, msg))
    | None, Some before_path, Some after_path ->
      (* Diff mode: compare two files *)
      (try
        let result = Diffract.diff_files ~language ~before_path ~after_path in
        let flat = Diffract.Diff.flatten_changes result.changes in
        if flat = [] then
          print_endline "No changes"
        else if antiunify_diff then begin
          (* Show anti-unified changes - concrete with [before → after] for diffs *)
          List.iter (fun change ->
            let open Diffract.Diff in
            let ctx = change_context change in
            let node_type = change_node_type change in
            let parent = match ctx.parent_type with Some p -> p | None -> "root" in
            let ann = Diffract.Antiunify.antiunify_change result change in
            Printf.printf "%s in %s:\n" node_type parent;
            Printf.printf "  %s\n" (Diffract.Antiunify.to_string ann)
          ) flat
        end else if flat_diff then begin
          List.iter (fun change ->
            let open Diffract.Diff in
            let ctx = change_context change in
            let node_type = change_node_type change in
            let parent = match ctx.parent_type with Some p -> p | None -> "root" in
            let before_text = change_text_before result change in
            let after_text = change_text_after result change in
            Printf.printf "%s in %s:\n" node_type parent;
            (match before_text with
             | Some t -> Printf.printf "  - %s\n" (String.escaped t)
             | None -> ());
            (match after_text with
             | Some t -> Printf.printf "  + %s\n" (String.escaped t)
             | None -> ())
          ) flat
        end else
          print_endline (Diffract.Diff.to_string result);
        `Ok ()
      with
      | Sys_error msg ->
        `Error (false, Printf.sprintf "Error reading file: %s" msg)
      | Failure msg ->
        `Error (false, msg))

let run_term =
  let file1 = Arg.(value & pos 0 (some file) None &
                   info [] ~docv:"FILE1" ~doc:"Source file to parse, or 'before' file for diff.") in
  let file2 = Arg.(value & pos 1 (some file) None &
                   info [] ~docv:"FILE2" ~doc:"Optional 'after' file for diff.") in
  Term.(ret (const run $ file1 $ file2 $ language $ list_languages $ flat_diff $ antiunify_diff $ match_pattern))

let cmd =
  let doc = "Parse source files to S-expressions or diff two files" in
  let man = [
    `S Manpage.s_description;
    `P "$(tname) parses source files using tree-sitter grammars and outputs \
        the concrete syntax tree (CST) as S-expressions.";
    `P "When two files are provided, $(tname) computes and displays the \
        structural differences between them.";
    `S Manpage.s_examples;
    `P "Parse a TypeScript file:";
    `Pre "  $(tname) example.ts";
    `P "Parse a Kotlin file:";
    `Pre "  $(tname) --language kotlin example.kt";
    `P "Diff two TypeScript files:";
    `Pre "  $(tname) before.ts after.ts";
    `P "Diff with flattened output:";
    `Pre "  $(tname) --flat before.ts after.ts";
    `P "Diff with anti-unified output (shows [before → after] for changes):";
    `Pre "  $(tname) --antiunify before.ts after.ts";
    `P "Match a pattern against source (pattern file uses @@ delimiters for metavars):";
    `Pre "  $(tname) --match pattern.txt source.ts";
    `S Manpage.s_bugs;
    `P "Report bugs at https://github.com/example/diffract/issues"
  ] in
  let info = Cmd.info "diffract" ~version:"0.1.0" ~doc ~man in
  Cmd.v info run_term

let () = exit (Cmd.eval cmd)
