#!/usr/bin/env bash
# Run the fast corpora and regenerate their baselines; review with `git diff`.
# drupal-attr (slow, known-fail stress corpus) is run directly when wanted:
#   ./evaluation/drupal-attr.sh > evaluation/drupal-attr.baseline
set -euo pipefail
dir="$(cd "$(dirname "$0")" && pwd)"
for c in androidx webxforge; do
  "$dir/$c.sh" | tee "$dir/$c.baseline"
done
