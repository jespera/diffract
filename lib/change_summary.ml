(** Change summary: infer the spatch-style rules behind a before/after changeset
    (design docs/change-summary-design.md).

    This module is a thin facade over the pipeline phases, each in its own
    module:
    - {!Cs_types}    — shared types (the public API surface + internal pattern
                       representation)
    - {!Cs_pattern}  — tree → [pat_node] conversion, rendering, anti-unification,
                       coherence predicates
    - {!Cs_propose}  — change-pair extraction and the candidate channels (§3.1–3.2)
    - {!Cs_cluster}  — anti-unification dendrogram, coarsening, one-sided clustering (§4.1)
    - {!Cs_evaluate} — the per-site safety gate that defines a rule's meaning (§3.3)
    - {!Cs_fusion}   — conjunctive multi-section fusion of co-occurring changes (§4.2)
    - {!Cs_select}   — one tier: propose → evaluate → greedy set-cover (§3.3)
    - {!Cs_tier}     — the tiered loop, chain-effect accounting, residual emission (§4.4)
    - {!Cs_io}       — [.summary] formatting and the directory-pair loader *)

include Cs_types

let summarize = Cs_tier.summarize
let residual_diff = Cs_evaluate.residual_diff
let collect_one_sided_candidates = Cs_propose.collect_one_sided_candidates
let format_summary = Cs_io.format_summary
let load_from_dirs = Cs_io.load_from_dirs
