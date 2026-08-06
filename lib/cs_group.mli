(** Grouping of repeated residual edits — the reading-mode digest. See
    {!Cs_group} for why this is a rendering pass and not a pipeline phase. *)

open Cs_types

(** The differing runs of a word-level alignment, as [(removed, added)] text
    pairs. Two hunks are "the same edit" iff their signatures are equal.
    Whitespace-only runs are dropped, so a reflowed line groups with the
    unreflowed form of the same change. Exposed for testing. *)
val signature : string -> string -> (string * string) list

(** One-line rendering of a signature, e.g. ["akka -> org.apache.pekko"] or
    ["(insert) import Foo"]. Long runs are elided at a UTF-8 boundary. *)
val describe : (string * string) list -> string

type group = {
  g_edit : string;  (** {!describe} of the shared signature *)
  g_count : int;  (** hunks in the group (always ≥ 2) *)
  g_files : int;  (** distinct files those hunks span *)
  g_exemplar : string list * string list;
      (** removed and added lines of one real hunk. Printed beneath the group
          because the edit alone is ambiguous: pekko renames [akka] to
          [org/apache/pekko] in paths, [org.apache.pekko] in quoted class names
          and [pekko] in doc comments, which read as contradictions until you
          see one line of each. Chosen over a derived context {e label}, which
          would mean classifying context heuristically and then asserting the
          guess as fact. *)
}

type rename_edit = {
  re_edit : string;
  re_count : int;
  re_exemplar : string * string;  (** a real (before, after) path pair *)
}

type digest = {
  dg_renames : (int * rename_edit list) option;
      (** [(files moved, distinct path edits)]; [None] when nothing moved *)
  dg_groups : group list;  (** most frequent first, ties broken by text *)
  dg_grouped : int;  (** hunks the groups account for *)
  dg_total : int;  (** hunks across all content residuals *)
  dg_rest : (residual * string list) list;
      (** each residual paired with the bodies of its hunks that no group
          covers, verbatim; residuals left with none are dropped. Every hunk
          therefore appears exactly once in a rendered digest — inside a group
          or printed in full. *)
}

(** [digest residuals] partitions rename-only residuals from content ones and
    groups the latter's hunks by edit signature. Deterministic: counting and
    exemplar choice both follow residual-list then file order. *)
val digest : residual list -> digest
