(** Change-summary tuning parameters, gathered in one place.

    These were previously hardcoded literals scattered across the pipeline (the
    {e same} hole-fraction threshold appeared in five separate spots). Collecting
    them here gives each knob one documented home and a single source of truth.

    This is an {e internal} seam — there is deliberately no CLI surface for it
    (the [summarize] subcommand takes no tuning flags). The record exists so the
    constants are discoverable and so a future caller could thread an override
    through if one is ever needed; today every site reads {!default}. *)

type options = {
  min_support : int;
      (** a rule must fire at [>= min_support] sites to be emitted (singletons
          fall to residuals). Design §2.3 "Supported". Anchored lattice-descent
          realisations are exempt — their support is counted on the delta pool
          (§3.2). *)
  max_hole_fraction : float;
      (** coherence cut (§2.3 "Coherent", §4.1): a candidate pattern's holes,
          as a fraction of its shell size, must stay below this. Also the
          dendrogram cut's coherence bound. *)
  jaccard_threshold : float;
      (** M1.6 fusion (§4.2): two candidate clusters fuse into a conjunctive
          rule when their file-set Jaccard overlap is [> jaccard_threshold]. *)
  max_tiers : int;
      (** M2 recursive residual clustering (§4.4): cap on tier depth. A
          backstop — each emitting tier strictly shrinks the gap by the
          net-progress guard, so this is rarely reached. *)
  cost_byte_limit : int;
      (** §3.2 cost containment: change pairs whose before/after spans exceed
          this many bytes (roughly more than a statement) emit no anchored
          variants. *)
  max_selectors_per_pair : int;
      (** §3.2 cost containment: at most this many path selectors are
          enumerated per change pair when building anchored variants. *)
  selector_depth_limit : int;
      (** §3.2 cost containment: maximum descent depth when enumerating anchored
          path selectors. *)
  emission_threshold : float;
      (** legacy change-density parameter for [collect_change_pairs_multi];
          retained for compatibility (multi-level emission no longer gates on
          it). *)
  anchor_sample : int;
      (** declaration anchoring: how many of a two-sided cluster's instances to
          re-anchor under their enclosing declaration when proposing a
          [match: field] candidate. Distinct anchor shapes (e.g. [fun]) dedupe,
          and evaluation finds the full support of each from one representative,
          so a small sample suffices; the cap bounds the re-parse cost on large
          clusters. *)
}

(** The values the pipeline ships with — the behaviour every golden test pins. *)
let default =
  {
    min_support = 2;
    max_hole_fraction = 0.35;
    jaccard_threshold = 0.7;
    max_tiers = 5;
    cost_byte_limit = 1500;
    max_selectors_per_pair = 8;
    selector_depth_limit = 5;
    emission_threshold = 0.5;
    anchor_sample = 12;
  }
