(** Line-based unified diff between two strings. Matcher-independent. *)

val generate_diff :
  ?context:int ->
  file_path:string ->
  original:string ->
  transformed:string ->
  unit ->
  string
(** [generate_diff ~file_path ~original ~transformed] produces a unified diff
    (with [--- a/], [+++ b/] headers and [@@ ... @@] hunks, [context] lines of
    context — default 3) describing the line-level changes from [original] to
    [transformed]. Returns the empty string when the two are identical.
    Change-summary residual sections use [~context:0] so the hunks are stable
    against diff-presentation tweaks (design §9.1). *)
