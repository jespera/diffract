open Cmdliner
open Cli_common
open Pattern_command_common

let in_place_flag =
  let doc = "Write changes directly to files instead of printing a diff." in
  Arg.(value & flag & info [ "in-place" ] ~doc)

(* Regions the grammar could not read, as a display string, or [None] for a
   clean parse. A pattern cannot match inside such a region, so a silent
   "no matches" there is misleading. Costs one extra parse per file
   ([Matcher.transform] has no tree-taking variant to borrow a tree from), which
   measures as noise next to the matcher's backtracking walk: applying a
   def-level pattern over 312 Scala files ran 2.4s either way. *)
let unparsed_note ~ctx ~language source_text =
  match
    Diffract.Tree.unparsed_regions
      (Diffract.Tree.parse ~ctx ~language source_text)
  with
  | [] -> None
  | rs -> Some (Diffract.Tree.format_regions rs)

let transform_source ~ctx ~language ~pattern_text ~in_place ~file_path
    source_text =
  let unparsed = unparsed_note ~ctx ~language source_text in
  let transformed =
    Diffract.Matcher.transform ~ctx ~language ~pattern_text ~source_text
  in
  if transformed = source_text then (false, "", unparsed)
  else begin
    let diff =
      Diffract.Text_diff.generate_diff ~file_path ~original:source_text
        ~transformed ()
    in
    if in_place then
      Out_channel.with_open_text file_path (fun oc ->
          output_string oc transformed);
    (true, diff, unparsed)
  end

let scan_directory_apply ~ctx ~language ~pattern_text ~include_pattern
    ~exclude_dirs ~in_place ~verbose dir_path =
  let files = find_files ~pattern:include_pattern ~exclude_dirs dir_path in
  let total_files = List.length files in
  let literals = prefilter_literals ~ctx ~language ~pattern_text in
  let skipped = ref 0 in
  let total_edits = ref 0 in
  let files_changed = ref 0 in
  let errors = ref [] in
  let unparsed_files = ref [] in
  (* Diffs are buffered and printed after the scan so they land together at
     the end, below any per-file progress log (like summarize's output). *)
  let output = Buffer.create 1024 in
  List.iteri
    (fun i file_path ->
      if verbose then
        Printf.eprintf "[apply] (%d/%d) %s\n%!" (i + 1) total_files file_path;
      try
        let source_text =
          In_channel.with_open_text file_path In_channel.input_all
        in
        if not (Diffract.Matcher.source_may_match ~literals source_text) then
          incr skipped
        else begin
          let changed, diff, unparsed =
            transform_source ~ctx ~language ~pattern_text ~in_place ~file_path
              source_text
          in
          (match unparsed with
          | Some lines ->
              unparsed_files := (file_path, lines) :: !unparsed_files
          | None -> ());
          if changed then begin
            incr files_changed;
            incr total_edits;
            if not in_place then Buffer.add_string output diff
          end
        end
      with Failure msg | Sys_error msg ->
        errors := (file_path, msg) :: !errors)
    files;
  if verbose && literals <> [] then
    Printf.eprintf "[apply] prefilter: skipped %d of %d files (literals: %s)\n%!"
      !skipped total_files
      (String.concat ", " literals);
  Buffer.output_buffer stdout output;
  Printf.printf "Transformed %d file(s) (scanned %d files)\n" !files_changed
    total_files;
  (* One line by default: a scan can turn up dozens of damaged files (62 of 312
     on one Scala corpus), and a warning that scrolls the real output off the
     screen is its own kind of silence. [--verbose] lists them. *)
  if !unparsed_files <> [] then begin
    let fs = List.rev !unparsed_files in
    Printf.eprintf
      "\n\
       Warning: %d of %d file(s) contain regions the grammar could not parse; \
       matches there are missed%s\n"
      (List.length fs) total_files
      (if verbose then ":" else " (--verbose lists them)");
    if verbose then
      List.iter (fun (p, lines) -> Printf.eprintf "  %s  lines=%s\n" p lines) fs;
    flush stderr
  end;
  if !errors <> [] then begin
    Printf.printf "\nErrors (%d files):\n" (List.length !errors);
    List.iter
      (fun (path, msg) -> Printf.printf "  %s: %s\n" path msg)
      (List.rev !errors)
  end;
  ignore total_edits

let run pattern_path target language include_pattern exclude_patterns
    in_place debug_tokens verbose =
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
              ~include_pattern:glob ~exclude_dirs ~in_place ~verbose target;
            `Ok ())
      else begin
        let source_text =
          In_channel.with_open_text target In_channel.input_all
        in
        let changed, diff, unparsed =
          transform_source ~ctx ~language ~pattern_text ~in_place
            ~file_path:target source_text
        in
        if not changed then print_endline "No matches found"
        else if not in_place then print_string diff;
        (match unparsed with
        | Some lines ->
            Printf.eprintf
              "\n\
               Warning: the grammar could not parse lines %s; matches there \
               are missed\n"
              lines;
            flush stderr
        | None -> ());
        `Ok ()
      end
    end
  with
  | Sys_error msg -> `Error (false, Printf.sprintf "Error reading file: %s" msg)
  | Failure msg -> `Error (false, msg)

let cmd =
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
        (const run $ pattern $ target $ language $ include_pattern
       $ exclude_patterns $ in_place_flag $ debug_tokens_flag $ verbose_flag))
