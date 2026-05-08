(** Per-language grammar metadata derived from each tree-sitter
    grammar's [node-types.json] and [grammar.json]. *)

(** {1 List-shape wrappers (from node-types.json)} *)

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

(** {1 DEL definition (from grammar.json)} *)

type string_def = {
  opener : string;
  closer : string;
  escape : char option;
}
(** A string literal's lexical shape: opening and closing delimiters,
    plus the escape character if any. Interior content is treated as
    opaque by the DEL lexer. *)

type del_definition = {
  bracket_pairs : (string * string) list;
      (** Pairs of opening and closing bracket strings, e.g. [("(",")")]. *)
  string_defs : string_def list;
      (** String literals derivable from [grammar.json]. Languages whose
          string handling is in external scanners (Kotlin multi-dollar,
          PHP heredocs, Scala interpolated) typically have an empty or
          partial list here; per-language extensions augment it. *)
  line_comments : string list;
      (** Opening markers for line comments, e.g. ["//"]. *)
  block_comments : (string * string) list;
      (** Pairs of opening and closing markers for block comments,
          e.g. [("/*","*/")]. *)
}
(** The lexical metadata needed by the DEL pattern lexer: which
    characters open and close brackets, strings, and comments.
    Auto-derived from each language's [grammar.json] where possible;
    per-language extensions may add string definitions for cases
    handled by external scanners. *)

val del_definition : language:string -> del_definition
(** [del_definition ~language] returns the DEL definition for the
    given language. Memoized; first call per language parses the
    embedded [grammar.json]. Returns an empty definition for unknown
    languages. *)

(** {1 General accessors} *)

val all_languages : unit -> string list
(** Returns the list of language names for which embedded metadata is
    available. *)
