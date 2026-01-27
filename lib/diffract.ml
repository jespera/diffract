(** Diffract - structural diff and pattern inference for source code *)

module Bindings = Tree_sitter_bindings
module Tree = Tree
module Pattern = Pattern
module Diff = Diff
module Abstract = Abstract
module Antiunify = Antiunify
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

(** Diff two source strings *)
let diff ~language ~before ~after =
  let before_tree = Tree.parse ~language before in
  let after_tree = Tree.parse ~language after in
  Diff.diff ~before_source:before_tree.source ~before_root:before_tree.root
            ~after_source:after_tree.source ~after_root:after_tree.root

(** Diff two files *)
let diff_files ~language ~before_path ~after_path =
  let before = In_channel.with_open_text before_path In_channel.input_all in
  let after = In_channel.with_open_text after_path In_channel.input_all in
  diff ~language ~before ~after
