open Cmdliner
open Cli_common

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

let run before_path after_path language =
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

let cmd =
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
    Term.(ret (const run $ before_file $ after_file $ language))
