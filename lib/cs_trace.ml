(** Change-summary diagnostic tracing, gated on the [CS_TRACE] environment
    variable. Replaces the inlined [if Sys.getenv_opt "CS_TRACE" <> None then
    Printf.eprintf ...] checks that were scattered through the pipeline. *)

let enabled = lazy (Sys.getenv_opt "CS_TRACE" <> None)

(** [true] when [CS_TRACE] is set — guard trace-only computation with this. *)
let on () = Lazy.force enabled

(** Gated [eprintf]: prints to stderr when {!on}, otherwise discards the
    arguments. Use for trace one-liners, e.g.
    [Cs_trace.trace "selected %d rules\n%!" n]. *)
let trace fmt =
  if Lazy.force enabled then Printf.eprintf fmt
  else Printf.ifprintf stderr fmt
