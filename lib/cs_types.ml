(** Change-summary shared types: the public API surface ([file_change],
    [changeset], [rule], [residual], [summary]) plus the internal pattern
    representation ([pat_node], [edit_pat], [instance], [cluster]) and one-sided
    fusion scaffolding shared across the pipeline phases. See
    docs/change-summary-design.md. *)

(* ── Public types ────────────────────────────────────────────────── *)

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
  id : string;
  pattern_text : string;
  support : int;
      (** number of edits the rule makes in the applied chain, summed over
          [sites] — chain-effective, not evaluation-time (see [sites]) *)
  language : string;
  sites : string list;
      (** distinct file paths where the rule actually edits something when the
          summary's rules are applied in id order — not merely where its pattern
          matches the original source. An earlier rule can consume a later
          rule's matches at some files; those files are not listed (M2
          chain-effect accounting). *)
  after : (string * string list) list;
      (** M2 per-site tier attribution: [(site, earlier rule ids)] — the rule's
          pattern at [site] matches the intermediate produced by applying those
          earlier rules, so it must be applied after them (rule-id order is
          application order). Empty for tier-1 rules; a site absent from the
          list has no predecessors. Global residual clustering makes this
          per-site: one tier-2 rule may follow different primaries at different
          sites (design §3.3 "common factors", §9.3). *)
}

type residual = {
  res_file : string;
  res_rules : string list;
  res_diff : string;
}

type summary = { rules : rule list; residuals : residual list }

(** Collapse whitespace runs to single spaces and trim. Layout-only differences
    are presentational, not a statement about the change — the same tolerance
    the safety gate's tree-level re-diff gives. *)
let ws_collapse s =
  let b = Buffer.create (String.length s) in
  let pend = ref false in
  String.iter
    (fun c ->
      if c = ' ' || c = '\t' || c = '\n' || c = '\r' then pend := true
      else begin
        if !pend && Buffer.length b > 0 then Buffer.add_char b ' ';
        pend := false;
        Buffer.add_char b c
      end)
    s;
  Buffer.contents b

(* ── Internal pattern representation ─────────────────────────────── *)

type pat_node =
  | Hole of int
  | Leaf of { node_type : string; value : string }
  | PNode of {
      node_type : string;
      is_named : bool;
      children : pat_child list;
      template : template_part list;
          (** Inter-child source text and child placeholders, used by the
              renderer to reconstruct the node's surface syntax. Captures source
              bytes (e.g. the quote delimiters of a string literal) that the
              grammar consumes silently — i.e. that fall inside the node's byte
              range but aren't exposed as child nodes. *)
    }

and pat_child = { field_name : string option; child : pat_node }
and template_part = Lit of string | Slot of int

type edit_pat = { before : pat_node; after : pat_node }

type instance = {
  before_text : string;
  after_text : string;
  before_full_source : string;
      (** Full pre-change source of the file containing this site, used by the
          applicability check to verify the rendered pattern actually matches in
          real code. *)
  file : string;
  line : int;
  language : string;
  site_start : int;
  site_end : int;
  ipat : edit_pat;
      (** The instance's own fully-concrete pattern (no holes) — what the site's
          change pair anti-unifies *from*. Kept so a cluster can be
          re-specialized over its surviving instances after covering and safety
          shedding: a hole the survivors no longer vary on collapses back to the
          literal. *)
}

type cluster = { pattern : edit_pat; instances : instance list }
type side = Before_side | After_side

type one_sided_instance = {
  os_file : string;
  os_line : int;
  os_language : string;
  os_text : string;
  os_side : side;
  os_start_byte : int;
  os_end_byte : int;
}

type one_sided_candidate = {
  os_pat : pat_node;
  os_instance : one_sided_instance;
}
(** Internal candidate for M1.6 fusion: carries the structural shape alongside
    its site metadata. Not emitted as rules in M1. *)

let one_sided_candidate_instance (c : one_sided_candidate) : one_sided_instance
    =
  c.os_instance

type one_sided_cluster = {
  os_cluster_pattern : pat_node;
  os_cluster_side : side;
  os_cluster_instances : one_sided_instance list;
}
(** A cluster of Added-only or Removed-only candidates that share a common
    pat_node shape. Internal — built by one-sided clustering ({!Cs_cluster}),
    consumed by M1.6b fusion ({!Cs_fusion}). *)
