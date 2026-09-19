open Cmdliner
open Cli_common

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
    (* Lead with the regions. The per-node list below is the detail for
       debugging a grammar; the region count is what tells you how much of the
       file is unreachable to a pattern (tree-sitter nests many ERROR nodes
       inside one garbled span). *)
    (match unparsed_regions tree with
    | [] -> ()
    | rs ->
        Printf.printf "\n%d unparsed region(s): lines %s\n" (List.length rs)
          (format_regions rs));
    if error_nodes <> [] then begin
      Printf.printf "\n%d parse error node(s):\n" (List.length error_nodes);
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
    [
      languages_cmd;
      parse_cmd;
      Search_command.cmd;
      Apply_command.cmd;
      Diff_command.cmd;
      Summarize_command.cmd;
    ]

let () = exit (Cmd.eval cmd)
