(** [Cursor.S] implementation over tree-sitter parses ([Tree.src Tree.t]).

    Used in production to drive the matcher against real source files. Tests
    that don't need a real parse should use [Test_cursor] instead — this module
    pulls in tree-sitter as a transitive dependency.

    Navigation walks [Tree.children] (all children including unnamed leaves like
    punctuation), skipping nodes whose [is_extra] flag is set (typically
    comments and grammar-declared whitespace tokens). The source string is
    carried inside the cursor so that [leaf_text] can extract leaf bytes without
    a separate parameter. *)

include Cursor.S

val of_tree : Tree.src Tree.tree -> t
(** [of_tree tree] returns a cursor positioned at the tree's root. The source
    string is taken from [tree.source]. *)

val of_node : source:string -> Tree.src Tree.t -> t
(** [of_node ~source node] returns a cursor positioned at [node]. Use when
    matching against a subtree rather than a full file. *)

val node_type : t -> string
(** [node_type c] is the tree-sitter node type of the node at [c]'s current
    position (e.g. ["method_definition"]). Unlike {!Cursor.S.leaf_node_type}
    this works for interior nodes, not just leaves. Used by field-mode
    source-context tokenization to key its per-context cache. *)

val source : t -> string
(** [source c] is the full source string the cursor was built over. Used by
    field-mode source-context tokenization to build the context string into
    which the pattern is spliced. *)
