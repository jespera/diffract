(** Execution context threaded through the library.

    Holds caches and configuration that would otherwise be global mutable state.
    Create once at program start and pass to library entry points. *)

(** Internal: a bounded two-generation memo of parsed trees, keyed on
    [(language, source)]. The value type lives in {!Tree_types} so [Context]
    (below [Tree]) can name it. Manipulated only by [Tree.parse_internal]. *)
type parse_memo = {
  mutable cur : (string * string, unit Tree_types.tree) Hashtbl.t;
  mutable prev : (string * string, unit Tree_types.tree) Hashtbl.t;
  mutable cap : int;
}

type t = {
  lang_cache : (string, nativeint) Hashtbl.t;
  parse_memo : parse_memo;
  interned : (string, string) Hashtbl.t;
      (** Internal: canonical copies of grammar-vocabulary strings (node types,
          field names). Manipulated only via {!intern}. *)
}

(** [create ()] returns a fresh context with empty caches. [parse_cache_cap]
    bounds the parse memo (default 512 entries per generation). *)
val create : ?parse_cache_cap:int -> unit -> t

(** [intern ctx s] returns a canonical string equal to [s], reusing a previous
    one when possible.

    The FFI allocates a fresh OCaml string for every node's type and every
    child's field name, but a grammar has only a few hundred distinct names
    between them, so without canonicalization each node carries its own copy —
    ~43 MB of duplicates per parse of a 312-file corpus, re-allocated on each of
    the thousands of reparses the change-summary pipeline performs. The table is
    bounded by the loaded grammars' vocabularies, so it needs no eviction, and
    scoping it to the context keeps it out of global state and gives one table
    per domain for callers that parse concurrently. *)
val intern : t -> string -> string

(** Internal: look up a parsed tree by [(language, source)], promoting a
    [prev]-generation hit. Used by [Tree.parse_internal]. *)
val parse_memo_find : t -> string * string -> unit Tree_types.tree option

(** Internal: record a parsed tree, rotating generations when [cur] is full. *)
val parse_memo_add : t -> string * string -> unit Tree_types.tree -> unit

(** [ensure_parse_cap ctx n] raises the parse cache's per-generation cap to at
    least [n] (never lowers it). A caller that will repeatedly re-parse a fixed
    working set of [n] inputs calls this so the cache fits it and does not
    thrash, however large the changeset. *)
val ensure_parse_cap : t -> int -> unit
