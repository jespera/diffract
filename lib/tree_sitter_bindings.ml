(* Tree-sitter bindings using native OCaml externals for core functions
   and ctypes for dynamic language loading *)

(* Native externals for parser operations *)
external parser_new : unit -> nativeint = "ts_helper_parser_new"
external parser_delete : nativeint -> unit = "ts_helper_parser_delete"

external parser_set_language : nativeint -> nativeint -> bool
  = "ts_helper_parser_set_language"

external parse_to_sexp : nativeint -> string -> string
  = "ts_helper_parse_to_sexp"
