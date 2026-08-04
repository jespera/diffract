(** Diffract - structural pattern matching for source code *)

(** Re-export modules for convenient access *)
module Context = Context

module Tree = Tree
module Tree_diff = Tree_diff
module Leaf_metric = Leaf_metric
module Cursor = Cursor
module Stmatch = Stmatch
module Tree_sitter_cursor = Tree_sitter_cursor
module Tokenize = Tokenize
module Matcher = Matcher
module Text_diff = Text_diff
module Change_summary = Change_summary

(** Internal change-summary modules, exposed for unit testing of the pattern
    layer (anti-unification, rendering). Not part of the stable public API. *)
module Cs_types = Cs_types

module Cs_pattern = Cs_pattern
module File_scan = File_scan

(** {1 Parsing} *)

(** [parse_tree ~ctx ~language source] parses source and returns the tree
    representation. *)
val parse_tree :
  ctx:Context.t -> language:string -> string -> Tree.src Tree.tree

(** [parse_file_tree ~ctx ~language path] parses a file and returns the tree
    representation. *)
val parse_file_tree :
  ctx:Context.t -> language:string -> string -> Tree.src Tree.tree

(** {1 S-expression output} *)

(** [parse_to_sexp ~ctx ~language source] parses source and returns S-expression
    string. *)
val parse_to_sexp : ctx:Context.t -> language:string -> string -> string

(** [parse_file_to_sexp ~ctx ~language path] parses a file and returns
    S-expression string. *)
val parse_file_to_sexp : ctx:Context.t -> language:string -> string -> string

(** {1 Language support} *)

(** Returns a list of available language names. *)
val available_languages : unit -> string list
