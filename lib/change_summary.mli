(** Change summary: cluster systematic edits across a changeset into
    spatch-style rules.

    M1 scope — rules only, no residuals or tiered attribution. See
    [docs/change-summary-design.md]. *)

type file_change =
  | Modified of {
      path : string;
      language : string;
      before_source : string;
      after_source : string;
    }
  | Added of { path : string; language : string; after_source : string }
  | Deleted of { path : string; language : string; before_source : string }

type changeset = { files : file_change list }

type rule = {
  id : string;  (** e.g. "R1", "R2" *)
  pattern_text : string;  (** .pat-style body (match + replace) *)
  support : int;  (** number of sites this rule fires at *)
  language : string;
      (** grammar the rule's pattern body is written in; all instances share *)
  sites : string list;
      (** distinct files where the rule fires, sorted lexicographically *)
}

type summary = { rules : rule list }

type side = Before_side | After_side

type one_sided_instance = {
  os_file : string;
  os_line : int;
  os_language : string;
  os_text : string;
  os_side : side;
}
(** A single Added or Removed subtree at a site. Scaffolding for M1.6
    fusion; not emitted as rules in M1. *)

type one_sided_candidate
(** Opaque — pairs a [one_sided_instance] with its structural shape for
    later clustering. *)

val one_sided_candidate_instance : one_sided_candidate -> one_sided_instance

val collect_one_sided_candidates :
  ?on_file:(idx:int -> total:int -> path:string -> unit) ->
  ctx:Context.t ->
  changeset ->
  one_sided_candidate list
(** Extracts every [Added]/[Removed] subtree across [Modified] files in the
    changeset. M1.5 plumbing; M1.6 will cluster and fuse these.
    [on_file], if provided, is called once per [Modified] file just before
    parsing it. *)

val summarize :
  ?progress:(stage:string -> idx:int -> total:int -> path:string -> unit) ->
  ctx:Context.t ->
  changeset ->
  summary
(** [summarize ~ctx cs] runs the clustering pipeline and returns the summary.
    In M1 scope: only Modified files contribute change pairs; Added/Deleted
    files are carried in the changeset but not clustered yet.
    [progress], if provided, is called once per [Modified] file just before
    parsing. [stage] identifies which pass is running ([{"two-sided";
    "one-sided"}]); [idx] is 1-based and [total] is the count of [Modified]
    files. *)

val format_summary : summary -> string
(** [format_summary s] serialises [s] in the [.summary] format defined in
    §9 of the design doc. *)

val load_from_dirs :
  before_dir:string ->
  after_dir:string ->
  ?include_glob:string option ->
  ?exclude_dirs:string list ->
  ?ext_language:(string * string) list ->
  default_language:string ->
  unit ->
  changeset
(** [load_from_dirs ~before_dir ~after_dir ~default_language ()] pairs files
    under [before_dir] and [after_dir] by relative path. Files only in
    [before_dir] become [Deleted]; only in [after_dir] become [Added];
    differing contents become [Modified]. Extension lookup via [ext_language]
    (defaults to .tsx/.ts) falls back to [default_language]. *)
