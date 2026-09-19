open Cmdliner
open Cli_common
open Pattern_command_common

let explain_flag =
  let doc =
    "On a search that finds nothing, explain why: report locations whose \
     tokens occur as text but in a different syntactic role than the pattern \
     (a strict, structural matcher rejects those). Off by default. Adding does \
     a second, text-only pass, so it only runs when asked and only when there \
     were no matches."
  in
  Arg.(value & flag & info [ "explain" ] ~doc)

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

(* [--explain] support: the (file, line, col) of each text-only match's start —
   locations whose tokens match as text but whose syntactic role differs from
   the pattern. Empty unless the pattern is a single strict section (see
   {!Diffract.Matcher.text_only_find_in_tree}). *)
let text_only_locations ~ctx ~language ~pattern_text ~file_path ~source_text
    tree =
  Diffract.Matcher.text_only_find_in_tree ~ctx ~language ~pattern_text tree
  |> List.filter_map (fun (c : Diffract.Matcher.composite_match) ->
      match c.sections with
      | r :: _ ->
          let line, col =
            line_col_of_byte source_text r.Diffract.Matcher.M.start_byte
          in
          Some (file_path, line, col)
      | [] -> None)

(* Print the [--explain] hint to stderr for a search that found nothing. *)
let print_no_match_hint locations =
  match locations with
  | [] -> ()
  | _ ->
      let n = List.length locations in
      Printf.eprintf
        "\n\
         No structural matches, but %d location(s) contain the pattern's \
         tokens as text\n\
         in a different syntactic role (diffract matches on structure, not \
         text). E.g.:\n"
        n;
      List.iteri
        (fun i (f, l, c) -> if i < 3 then Printf.eprintf "  %s:%d:%d\n" f l c)
        locations;
      if n > 3 then Printf.eprintf "  ... and %d more\n" (n - 3);
      Printf.eprintf
        "Hint: for a plain text search, ripgrep is simpler; to match \
         structurally,\n\
         write the pattern in that context (e.g. as a type, not an expression).\n"

let search_source ~ctx ~language ~pattern_text source_text =
  try
    (* Parse once; reuse the tree for both matching and parse diagnostics. *)
    let tree = Diffract.Tree.parse ~ctx ~language source_text in
    let results =
      Diffract.Matcher.find_in_tree ~ctx ~language ~pattern_text tree
    in
    Ok (results, Diffract.Tree.unparsed_regions tree)
  with Failure msg | Sys_error msg -> Error msg

let scan_directory_search ~ctx ~language ~pattern_text ~include_pattern
    ~exclude_dirs ~explain ~verbose dir_path =
  let files = find_files ~pattern:include_pattern ~exclude_dirs dir_path in
  let total_files = List.length files in
  let literals = prefilter_literals ~ctx ~language ~pattern_text in
  let skipped = ref 0 in
  let total_matches = ref 0 in
  let files_with_matches = ref 0 in
  let files_with_parse_errors = ref [] in
  let errors = ref [] in
  (* Matches are formatted eagerly (so sources aren't retained) but printed
     after the scan, so they land together at the end, below any per-file
     progress log (like summarize's output). *)
  let output = Buffer.create 1024 in
  List.iteri
    (fun i file_path ->
      if verbose then
        Printf.eprintf "[search] (%d/%d) %s\n%!" (i + 1) total_files file_path;
      match In_channel.with_open_text file_path In_channel.input_all with
      | exception Sys_error msg -> errors := (file_path, msg) :: !errors
      | source_text when
          not (Diffract.Matcher.source_may_match ~literals source_text) ->
          incr skipped
      | source_text -> (
          match search_source ~ctx ~language ~pattern_text source_text with
          | Error msg -> errors := (file_path, msg) :: !errors
          | Ok (results, unparsed) ->
              if unparsed <> [] then
                files_with_parse_errors :=
                  (file_path, unparsed) :: !files_with_parse_errors;
              if results <> [] then begin
                incr files_with_matches;
                total_matches := !total_matches + List.length results;
                List.iter
                  (fun r ->
                    Buffer.add_string output
                      (format_search_match ~file_path ~source_text r))
                  results
              end))
    files;
  if verbose && literals <> [] then
    Printf.eprintf
      "[search] prefilter: skipped %d of %d files (literals: %s)\n%!" !skipped
      total_files
      (String.concat ", " literals);
  Buffer.output_buffer stdout output;
  Printf.printf "Found %d match(es) in %d file(s) (scanned %d files)\n"
    !total_matches !files_with_matches total_files;
  (* One line by default, as in [scan_directory_apply]. *)
  if !files_with_parse_errors <> [] then begin
    Printf.printf "\nParse errors: %d of %d files; matches there are missed%s\n"
      (List.length !files_with_parse_errors)
      total_files
      (if verbose then ":" else " (--verbose lists them)");
    if verbose then
      List.iter
        (fun (path, rs) ->
          Printf.printf "  %s  lines=%s\n" path
            (Diffract.Tree.format_regions rs))
        (List.rev !files_with_parse_errors)
  end;
  if !errors <> [] then begin
    Printf.printf "\nErrors (%d files):\n" (List.length !errors);
    List.iter
      (fun (path, msg) -> Printf.printf "  %s: %s\n" path msg)
      (List.rev !errors)
  end;
  (* --explain: only when the whole scan found nothing, make a second,
     text-only pass to locate tokens that match as text but in a different
     syntactic role. Re-parses the files, so it runs only on this empty case. *)
  if explain && !total_matches = 0 then
    print_no_match_hint
      (List.concat_map
         (fun file_path ->
           match In_channel.with_open_text file_path In_channel.input_all with
           | exception Sys_error _ -> []
           (* Text-only matches need the same literal tokens as text, so the
              prefilter applies to this pass too. *)
           | source_text when
               not (Diffract.Matcher.source_may_match ~literals source_text) ->
               []
           | source_text ->
               let tree = Diffract.Tree.parse ~ctx ~language source_text in
               text_only_locations ~ctx ~language ~pattern_text ~file_path
                 ~source_text tree)
         files)

let run pattern_path target language include_pattern exclude_patterns
    debug_tokens explain verbose =
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
              ~include_pattern:glob ~exclude_dirs ~explain ~verbose target;
            `Ok ())
      else begin
        let source_text =
          In_channel.with_open_text target In_channel.input_all
        in
        let tree = Diffract.Tree.parse ~ctx ~language source_text in
        let results =
          Diffract.Matcher.find_in_tree ~ctx ~language ~pattern_text tree
        in
        if results = [] then begin
          print_endline "No matches found";
          if explain then
            print_no_match_hint
              (text_only_locations ~ctx ~language ~pattern_text
                 ~file_path:target ~source_text tree)
        end
        else begin
          Printf.printf "Found %d match(es):\n\n" (List.length results);
          List.iter
            (fun r ->
              print_string
                (format_search_match ~file_path:target ~source_text r))
            results
        end;
        (* Report the regions, not the raw ERROR-node count: tree-sitter nests
           many ERROR nodes inside one garbled span, so a count of 23 for a
           single unreadable declaration reads as catastrophe. Line ranges say
           what is actually unreachable. *)
        (match Diffract.Tree.unparsed_regions tree with
        | [] -> ()
        | rs ->
            Printf.printf
              "\n\
               Warning: the grammar could not parse %d region(s) (lines %s); \
               matches there are missed\n"
              (List.length rs)
              (Diffract.Tree.format_regions rs));
        `Ok ()
      end
    end
  with
  | Sys_error msg -> `Error (false, Printf.sprintf "Error reading file: %s" msg)
  | Failure msg -> `Error (false, msg)

let cmd =
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
        (const run $ pattern $ target $ language $ include_pattern
       $ exclude_patterns $ debug_tokens_flag $ explain_flag $ verbose_flag))
