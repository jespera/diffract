(** Execution context threaded through the library.

    Holds caches and configuration that would otherwise be global mutable state.
    Create once at program start and pass to library entry points. *)

type t = { lang_cache : (string, nativeint) Hashtbl.t }

val create : unit -> t
(** [create ()] returns a fresh context with empty caches. *)
