(** Diffract - structural pattern matching for source code *)

(** Re-export modules for convenient access *)
module Context = Context
module Tree = Tree
module Match = Match

(** {1 Parsing} *)

val parse_tree : ctx:Context.t -> language:string -> string -> Tree.src Tree.tree
(** [parse_tree ~ctx ~language source] parses source and returns the tree representation. *)

val parse_file_tree : ctx:Context.t -> language:string -> string -> Tree.src Tree.tree
(** [parse_file_tree ~ctx ~language path] parses a file and returns the tree representation. *)

(** {1 S-expression output} *)

val parse_to_sexp : ctx:Context.t -> language:string -> string -> string
(** [parse_to_sexp ~ctx ~language source] parses source and returns S-expression string. *)

val parse_file_to_sexp : ctx:Context.t -> language:string -> string -> string
(** [parse_file_to_sexp ~ctx ~language path] parses a file and returns S-expression string. *)

(** {1 Language support} *)

val available_languages : unit -> string list
(** Returns a list of available language names. *)
