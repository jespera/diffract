(** Line-based unified diff between two strings. Matcher-independent. *)

val generate_diff :
  file_path:string -> original:string -> transformed:string -> string
(** [generate_diff ~file_path ~original ~transformed] produces a unified diff
    (with [--- a/], [+++ b/] headers and [@@ ... @@] hunks, three lines of
    context) describing the line-level changes from [original] to [transformed].
    Returns the empty string when the two are identical. *)
