(** Diffract - structural diff and pattern inference for source code *)

(** Re-export modules for convenient access *)
module Tree = Tree
module Pattern = Pattern
module Diff = Diff
module Abstract = Abstract
module Antiunify = Antiunify
module Match = Match

(** {1 Parsing} *)

val parse_tree : language:string -> string -> Tree.tree
(** [parse_tree ~language source] parses source and returns the tree representation. *)

val parse_file_tree : language:string -> string -> Tree.tree
(** [parse_file_tree ~language path] parses a file and returns the tree representation. *)

(** {1 S-expression output} *)

val parse_to_sexp : language:string -> string -> string
(** [parse_to_sexp ~language source] parses source and returns S-expression string. *)

val parse_file_to_sexp : language:string -> string -> string
(** [parse_file_to_sexp ~language path] parses a file and returns S-expression string. *)

(** {1 Language support} *)

val available_languages : unit -> string list
(** Returns a list of available language names. *)

(** {1 Diff} *)

val diff : language:string -> before:string -> after:string -> Diff.diff_result
(** [diff ~language ~before ~after] parses both sources and computes their difference. *)

val diff_files : language:string -> before_path:string -> after_path:string -> Diff.diff_result
(** [diff_files ~language ~before_path ~after_path] diffs two files. *)
