(** Change summary: cluster systematic edits across a changeset into
    spatch-style rules, recursively — residuals re-cluster into tiered rules
    (M2). See [docs/change-summary-design.md]. *)

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
  support : int;
      (** number of edits the rule makes in the applied chain, summed over
          [sites] *)
  language : string;
      (** grammar the rule's pattern body is written in; all instances share *)
  sites : string list;
      (** distinct files where the rule actually edits something when the
          summary's rules are applied in id order, sorted lexicographically —
          not merely where its pattern matches the original source (an earlier
          rule may consume its matches at some files; those are not listed) *)
  after : (string * string list) list;
      (** M2 per-site tier attribution: [(site, earlier rule ids)] — at that
          site the rule's pattern matches the intermediate produced by applying
          those earlier rules, so rule-id order is application order. Empty for
          tier-1 rules; a site absent from the list has no predecessors. *)
}

(** The change at a site that the rules do not explain (design §4.4, M1.9). For
    every Modified file, applying its claiming rules and diffing against the
    after-source either yields nothing (fully explained) or this gap. Rules +
    residuals together account for the whole changeset — the Covering
    desideratum of §2.3. *)
type residual = {
  res_file : string;  (** relative path of the file the gap lives in *)
  res_rules : string list;
      (** ids of the rules applied before the gap was measured, in application
          order; [[]] means no emitted rule claims the file — a pure one-off
          change (or a file-level add/delete) *)
  res_diff : string;
      (** unified diff (zero context) from the post-rule intermediate to the
          real after-source; for added/deleted files the absent side is
          [/dev/null] *)
}

type summary = {
  rules : rule list;
  residuals : residual list;
  unparsed : (string * (int * int) list) list;
      (** files the grammar could not fully read, with the 1-based inclusive
          line ranges it failed on, in the coordinates the residual diffs use; a
          rule cannot match inside such a region, so a change there falls to a
          residual however systematic it is. Empty on a clean parse. *)
}

type side = Before_side | After_side

(** A single Added or Removed subtree at a site. Scaffolding for M1.6 fusion;
    not emitted as rules in M1. The byte range is used to suppress removal-only
    rule emission for subtrees already covered by a two-sided rule's site. *)
type one_sided_instance = {
  os_file : string;
  os_line : int;
  os_language : string;
  os_text : string;
  os_side : side;
  os_start_byte : int;
  os_end_byte : int;
}

(** Opaque — pairs a [one_sided_instance] with its structural shape for later
    clustering. *)
type one_sided_candidate

val one_sided_candidate_instance : one_sided_candidate -> one_sided_instance

(** Extracts every [Added]/[Removed] subtree across [Modified] files in the
    changeset. M1.5 plumbing; M1.6 will cluster and fuse these. [on_file], if
    provided, is called once per [Modified] file just before parsing it. *)
val collect_one_sided_candidates :
  ?on_file:(idx:int -> total:int -> path:string -> unit) ->
  ctx:Context.t ->
  changeset ->
  one_sided_candidate list

(** Residual diff from a post-rule intermediate ([original]) to the real
    after-source ([transformed]): a zero-context unified diff with layout-only
    hunks dropped — a hunk is kept only when it touches a tree-level changed
    region of the (original, transformed) diff, since layout (re-indentation,
    spacing, line splits) is invisible to the parse tree and states nothing
    about the change. Returns [""] when the gap is entirely layout. This is the
    renderer [summarize] uses for residuals; exposed so tests can recompute a
    residual the same way. *)
val residual_diff :
  ?ignore_formatting:bool ->
  ctx:Context.t ->
  language:string ->
  file_path:string ->
  original:string ->
  transformed:string ->
  unit ->
  string

(** [summarize ~ctx cs] runs the clustering pipeline and returns the summary:
    rules plus residuals (M1.9). Only Modified files contribute change pairs;
    Added/Deleted files appear as unattributed [/dev/null] residuals (M1.7).
    [progress], if provided, is called once per [Modified] file just before
    parsing. [stage] identifies which pass is running
    ([{"two-sided"; "one-sided"}]); [idx] is 1-based and [total] is the count of
    [Modified] files.

    [ignore_formatting] (default [false]): when set, the residual filter treats
    trailing separators (commas, redundant semicolons) as trivia in addition to
    whitespace, so reflow-only leftovers — re-indentation plus a trailing comma,
    the shape a formatter like ktlint/prettier produces — are dropped from the
    residuals rather than reported as unexplained changes. Structural changes
    (e.g. an inserted brace block) are still kept. *)
val summarize :
  ?progress:(stage:string -> idx:int -> total:int -> path:string -> unit) ->
  ?ignore_formatting:bool ->
  ctx:Context.t ->
  changeset ->
  summary

(** [format_summary s] serialises [s] in the [.summary] format defined in §9 of
    the design doc. [?sites] (default [`Full]) controls the [# sites] blocks:
    [`Count] replaces each file list with a one-line count
    ([# sites R1  8 file(s)]) — the CLI's [--format text-minimal], a reading
    mode; the full text remains the canonical format. *)
val format_summary : ?sites:[ `Full | `Count ] -> summary -> string

(** [format_summary_json s] serialises [s] as a single compact JSON object, for
    filtering with tools like [jq]:
    [{"rules": [{"id", "support", "language", "pattern", "sites": [{"file",
     "after"?}]}], "residuals": [{"file", "rules", "diff"}]}]. A site's optional
    ["after"] array carries the per-site tier attribution (the earlier rule ids
    whose output the rule's pattern matched there). The [.summary] text
    ([format_summary]) remains the canonical format; this is a projection of the
    same data. *)
val format_summary_json : summary -> string

(** [load_from_dirs ~before_dir ~after_dir ~default_language ()] pairs files
    under [before_dir] and [after_dir] by relative path. Files only in
    [before_dir] become [Deleted]; only in [after_dir] become [Added]; differing
    contents become [Modified]. Extension lookup via [ext_language] (defaults to
    .tsx/.ts) falls back to [default_language]. *)
val load_from_dirs :
  before_dir:string ->
  after_dir:string ->
  ?include_glob:string option ->
  ?exclude_dirs:string list ->
  ?ext_language:(string * string) list ->
  default_language:string ->
  unit ->
  changeset
