(** Filesystem scanning shared by the CLI and the change-summary loader.

    Previously the [glob_match]/walk pair was duplicated in [bin/main.ml] and
    [Cs_io]. The copies had drifted: the CLI's walker called [Sys.is_directory]
    unguarded, so a broken symlink or unreadable directory aborted the whole
    scan, while the loader's copy tolerated it. This module is the single,
    guarded implementation both share. *)

(** Match [filename]'s basename against a simple glob [pattern]. Supports a
    single [*] wildcard:
    - ["*.ts"] — suffix match (any basename ending in [.ts])
    - ["test_*"] — prefix match (any basename starting with [test_])
    - ["a*c"] — prefix [a] and suffix [c]
    A pattern with no [*] is an exact basename match. A pattern with more than
    one [*] falls back to an exact (literal) match. *)
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

(** Recursively collect files under [root] for which [pred path] holds,
    accumulating onto [acc]. Directories whose entry name is in [exclude_dirs]
    are skipped (not descended into).

    Robust to a hostile filesystem: an unreadable directory ([Sys.readdir]
    raising) contributes nothing, and an entry whose type cannot be determined
    ([Sys.is_directory] raising — e.g. a broken symlink or a permission error)
    is treated as a non-directory rather than aborting the walk.

    The result order follows the (left) fold over each directory's entries,
    which is [Sys.readdir]'s order reversed. Callers that need a stable order
    should sort the result. *)
let rec walk ~exclude_dirs ~pred root acc =
  let entries = try Sys.readdir root with Sys_error _ -> [||] in
  Array.fold_left
    (fun acc entry ->
      let path = Filename.concat root entry in
      if try Sys.is_directory path with Sys_error _ -> false then
        if List.mem entry exclude_dirs then acc
        else walk ~exclude_dirs ~pred path acc
      else if pred path then path :: acc
      else acc)
    acc entries
