#!/usr/bin/env bash
# Run the fast corpora and regenerate their baselines; review with `git diff`.
# The slow corpora are run directly when wanted:
#   ./evaluation/drupal-attr.sh > evaluation/drupal-attr.baseline   (known-fail stress)
#   ./evaluation/symfony-str.sh > evaluation/symfony-str.baseline   (~3 min/format)
set -euo pipefail
dir="$(cd "$(dirname "$0")" && pwd)"
for c in androidx webxforge sentry-prop-rename sentry-prop-manual finagle; do
  "$dir/$c.sh" | tee "$dir/$c.baseline"
done
