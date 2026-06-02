(** See [cursor.mli]. *)

module type S = sig
  type t
  type leaf

  val move_first_leaf : t -> leaf
  val move_first_child : t -> bool
  val move_next_subtree : t -> bool
  val move_next_sibling : t -> bool
  val clone : t -> t
  val narrow : t -> t
  val leaf_text : leaf -> string
  val leaf_node_type : leaf -> string
  val subtree_equal : t -> t -> bool
  val is_named : t -> bool
  val byte_range : t -> int * int
  val named_children : t -> t list
  val all_children : t -> t list
  val leading_anonymous_leaves : t -> leaf list
  val trailing_anonymous_leaves : t -> leaf list
  val source_substring : t -> int -> int -> string
end
