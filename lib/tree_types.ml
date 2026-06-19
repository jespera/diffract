(** Bare tree record types, factored out of {!Tree} so that lower-level modules
    ([Context]) can name a parsed tree without depending on [Tree] (which
    depends on [Context]). [Tree] re-exports these as its own [point]/[child]/
    [t]/[tree], so callers keep using [Tree.t] etc. The phantom kind markers
    ([src]/[pat]/[any]) stay in [Tree] — they are an mli-level abstraction and
    nothing below [Tree] needs them. *)

type point = { row : int; column : int }
(** Position in source code *)

type 'kind child = { field_name : string option; node : 'kind t }
(** A child node with optional field name *)

and 'kind t = {
  node_type : string;
  is_named : bool;
  is_extra : bool;
      (** True iff tree-sitter generated this node from one of the grammar's
          [extras] rules — typically comments and (rarely) whitespace tokens.
          Such nodes can appear anywhere between tokens without being part of
          the syntactic structure. *)
  hash : int;
  start_byte : int;
  end_byte : int;
  start_point : point;
  end_point : point;
  children : 'kind child list;
  named_children : 'kind t list;
}
(** A tree node with all data copied from tree-sitter. The [hash] field is a
    precomputed structural hash that excludes positional information. *)

type 'kind tree = { root : 'kind t; source : string }
(** A complete parsed tree with source. The 'kind parameter is a phantom type —
    it's never used at runtime. *)
