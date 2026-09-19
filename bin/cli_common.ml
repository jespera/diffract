open Cmdliner

let language =
  let doc =
    "Language grammar to use (typescript, kotlin, ...). Required - there is no \
     default, so a file is never parsed with an unintended grammar."
  in
  Arg.(
    required
    & opt (some string) None
    & info [ "l"; "language" ] ~docv:"LANG" ~doc)

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

let verbose_flag =
  let doc = "Print per-file progress (and phase timing) to stderr." in
  Arg.(value & flag & info [ "v"; "verbose" ] ~doc)

let default_excludes =
  [ "node_modules"; ".git"; "_build"; "target"; "__pycache__"; ".hg"; ".svn" ]

let debug_tokens_flag =
  let doc =
    "Print how the pattern tokenizes (per section, with declared metavars and \
     any that are ABSENT) and exit, without searching or transforming. Use to \
     diagnose a pattern that matches nothing."
  in
  Arg.(value & flag & info [ "debug-tokens" ] ~doc)
