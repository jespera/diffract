(* OCaml externals for tree-sitter helper functions *)

external parser_new : unit -> nativeint = "tsh_parser_new"
external parser_delete : nativeint -> unit = "tsh_parser_delete"

external parser_set_language : nativeint -> nativeint -> bool
  = "tsh_parser_set_language"

external parse_to_sexp : nativeint -> string -> string = "tsh_parse_to_sexp"
