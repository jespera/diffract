(** Diffract - structural pattern matching for source code *)

module Bindings = Tree_sitter_bindings
module Tree = Tree
module Match = Match

(** Parse source code and return the tree representation *)
let parse_tree ~language source =
  Tree.parse ~language source

(** Parse a file and return the tree representation *)
let parse_file_tree ~language path =
  Tree.parse_file ~language path

(** Parse and return S-expression string (original API) *)
let parse_to_sexp ~language source =
  let lang = Languages.get language in
  let parser = Bindings.parser_new () in

  if parser = 0n then
    failwith "Failed to create parser";

  let cleanup () = Bindings.parser_delete parser in

  if not (Bindings.parser_set_language parser lang) then begin
    cleanup ();
    failwith "Failed to set language"
  end;

  let sexp =
    try Bindings.parse_to_sexp parser source
    with e ->
      cleanup ();
      raise e
  in
  cleanup ();
  sexp

(** Parse file and return S-expression string *)
let parse_file_to_sexp ~language path =
  let source = In_channel.with_open_text path In_channel.input_all in
  parse_to_sexp ~language source

let available_languages () = Languages.list_available ()
