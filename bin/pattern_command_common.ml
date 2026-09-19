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

(* Sort scan results lexicographically by path so search/apply output is
   reproducible: [File_scan.walk] returns entries in [readdir] order, which is
   filesystem-dependent. *)
let find_files ~pattern ~exclude_dirs root =
  let pred path = Diffract.File_scan.glob_match pattern path in
  Diffract.File_scan.walk ~exclude_dirs ~pred root [] |> List.sort compare

(* Literal prefilter for directory scans: texts that must occur verbatim in
   any file a match can be found in ({!Diffract.Matcher.required_literals}),
   so a file lacking one is skipped without parsing. On a malformed pattern
   return no literals — the per-file path then surfaces the error exactly as
   it did before the prefilter existed. *)
let prefilter_literals ~ctx ~language ~pattern_text =
  try Diffract.Matcher.required_literals ~ctx ~language ~pattern_text
  with Failure _ -> []
