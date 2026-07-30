(** Change-summary diagnostic tracing, gated on the [CS_TRACE] environment
    variable. Replaces the inlined
    [if Sys.getenv_opt "CS_TRACE" <> None then Printf.eprintf ...] checks that
    were scattered through the pipeline. *)

let enabled = lazy (Sys.getenv_opt "CS_TRACE" <> None)

(** [true] when [CS_TRACE] is set — guard trace-only computation with this. *)
let on () = Lazy.force enabled

(** Gated [eprintf]: prints to stderr when {!on}, otherwise discards the
    arguments. Use for trace one-liners, e.g.
    [Cs_trace.trace "selected %d rules\n%!" n]. *)
let trace fmt =
  if Lazy.force enabled then Printf.eprintf fmt else Printf.ifprintf stderr fmt

(** [timed label f] runs [f ()]; when tracing is on, prints
    ["[t] <label>: <wall seconds>"] to stderr on completion (and on
    exception, marked as such). Wrap pipeline phases with this so a slow
    corpus tells you *where* the time goes — zero overhead beyond the
    closure when tracing is off. *)
let timed label f =
  if not (Lazy.force enabled) then f ()
  else begin
    let t0 = Unix.gettimeofday () in
    match f () with
    | r ->
        Printf.eprintf "[t] %s: %.2fs\n%!" label (Unix.gettimeofday () -. t0);
        r
    | exception e ->
        Printf.eprintf "[t] %s: %.2fs (raised %s)\n%!" label
          (Unix.gettimeofday () -. t0)
          (Printexc.to_string e);
        raise e
  end
