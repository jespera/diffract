(** Line-based unified diff between two strings. Matcher-independent. *)

(** [generate_diff ~file_path ~original ~transformed] produces a unified diff
    (with [--- a/], [+++ b/] headers and standard [@@ -l,n +l,m @@] hunk
    headers, [context] lines of context — default 3) describing the line-level
    changes from [original] to [transformed]. Returns the empty string when the
    two are identical. Change-summary residual sections use [~context:0] so the
    hunks are stable against diff-presentation tweaks (design §9.1).

    [keep_hunk], when given, filters hunks: it receives each hunk's
    original-side position ([orig_start] — 0-based line index; [orig_len] —
    number of original lines the hunk spans, 0 for a pure insertion) and the
    hunk is omitted when it returns [false]. Returns the empty string when every
    hunk is filtered out. Change-summary uses this to drop layout-only hunks
    (hunks touching no tree-level changed region).

    [before_path], when it differs from [file_path], marks a file that MOVED:
    the diff then carries git's extended header ([diff --git], [rename from],
    [rename to]) ahead of the [---]/[+++] pair, which is what makes [git apply]
    perform the move as well as the content change. Defaults to [file_path],
    leaving output for unmoved files byte-identical. *)
val generate_diff :
  ?context:int ->
  ?keep_hunk:(orig_start:int -> orig_len:int -> bool) ->
  ?before_path:string ->
  file_path:string ->
  original:string ->
  transformed:string ->
  unit ->
  string
