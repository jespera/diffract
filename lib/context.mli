(** Execution context threaded through the library.

    Holds caches and configuration that would otherwise be global mutable state.
    Create once at program start and pass to library entry points. *)

type parse_memo = {
  mutable cur : (string * string, unit Tree_types.tree) Hashtbl.t;
  mutable prev : (string * string, unit Tree_types.tree) Hashtbl.t;
  mutable cap : int;
}
(** Internal: a bounded two-generation memo of parsed trees, keyed on
    [(language, source)]. The value type lives in {!Tree_types} so [Context]
    (below [Tree]) can name it. Manipulated only by [Tree.parse_internal]. *)

type t = {
  lang_cache : (string, nativeint) Hashtbl.t;
  parse_memo : parse_memo;
}

val create : ?parse_cache_cap:int -> unit -> t
(** [create ()] returns a fresh context with empty caches. [parse_cache_cap]
    bounds the parse memo (default 512 entries per generation). *)

val parse_memo_find :
  t -> string * string -> unit Tree_types.tree option
(** Internal: look up a parsed tree by [(language, source)], promoting a
    [prev]-generation hit. Used by [Tree.parse_internal]. *)

val parse_memo_add : t -> string * string -> unit Tree_types.tree -> unit
(** Internal: record a parsed tree, rotating generations when [cur] is full. *)

val ensure_parse_cap : t -> int -> unit
(** [ensure_parse_cap ctx n] raises the parse cache's per-generation cap to at
    least [n] (never lowers it). A caller that will repeatedly re-parse a fixed
    working set of [n] inputs calls this so the cache fits it and does not
    thrash, however large the changeset. *)
