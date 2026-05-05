(** Per-language grammar metadata derived from each tree-sitter
    grammar's [node-types.json]. *)

val list_shape_wrappers : language:string -> string list
(** Returns the list of node types that are "list-shape wrappers" in
    [language]'s grammar — named nodes that carry no field-named
    children. The grammar's [node-types.json] is the authoritative
    source: a node type is a wrapper iff its entry has [named: true]
    and its [fields] dict is empty or absent.

    The criterion is a structural superset of "true wrappers" worth
    peeling. The peel decision in [Tree.unwrap_root] applies further
    runtime guards (single-named-child + byte-range equality) that
    filter the candidate set to the wrappers actually safe to peel.

    Memoized; first call per language parses the embedded JSON.
    Returns [] for languages whose metadata is missing or malformed
    (e.g. an unrecognised language name). *)

val is_list_shape_wrapper : language:string -> node_type:string -> bool
(** [is_list_shape_wrapper ~language ~node_type] is [true] iff
    [node_type] appears in [list_shape_wrappers ~language]. O(1) lookup
    after the first call per language. *)

val all_languages : unit -> string list
(** Returns the list of language names for which embedded metadata is
    available. *)
