#!/usr/bin/env bash
#
# diffract-checkout.sh — materialize a git change-pair as before/ and after/
# trees for `diffract summarize`.
#
# Given two refs (commits, branches, tags) and a destination directory, extract
# every file that differs between them into <dest>/before/<path> and
# <dest>/after/<path>, preserving directory structure. Added files appear only
# under after/, deleted files only under before/ — exactly the shape summarize
# expects.
#
# Usage:
#   scripts/diffract-checkout.sh [-C REPO] BEFORE_REF AFTER_REF DEST [PATHSPEC...]
#
#   -C REPO     run against the git repo at REPO (default: current directory)
#   PATHSPEC    optional git pathspecs to restrict which files are copied,
#               e.g.  '*.ts' 'src/'   (anything `git diff -- ...` accepts)
#
# Examples:
#   # Everything that changed in the last commit, TS files only:
#   scripts/diffract-checkout.sh HEAD~1 HEAD /tmp/cs -- '*.ts'
#   diffract summarize -l typescript -i '*.ts' /tmp/cs/before /tmp/cs/after
#
#   # Compare two branches in another repo:
#   scripts/diffract-checkout.sh -C ~/proj main feature /tmp/cs '*.kt'

set -euo pipefail

repo="."
while getopts "C:h" opt; do
  case "$opt" in
    C) repo="$OPTARG" ;;
    h) sed -n '2,30p' "$0"; exit 0 ;;
    *) echo "try -h for usage" >&2; exit 2 ;;
  esac
done
shift $((OPTIND - 1))

if [ "$#" -lt 3 ]; then
  echo "usage: $0 [-C REPO] BEFORE_REF AFTER_REF DEST [PATHSPEC...]" >&2
  exit 2
fi

before_ref="$1"; after_ref="$2"; dest="$3"; shift 3
# A lone leading "--" before pathspecs is conventional; drop it.
if [ "${1:-}" = "--" ]; then shift; fi

git -C "$repo" rev-parse --git-dir >/dev/null 2>&1 || {
  echo "error: '$repo' is not a git repository" >&2; exit 1; }
for ref in "$before_ref" "$after_ref"; do
  git -C "$repo" rev-parse --verify --quiet "$ref^{commit}" >/dev/null || {
    echo "error: '$ref' is not a valid commit-ish in '$repo'" >&2; exit 1; }
done

mkdir -p "$dest/before" "$dest/after"

# Pull one blob out of a ref into the tree, creating parent dirs. A missing
# path (e.g. asking for a deleted file's after-state) is skipped silently —
# the caller already knows from the diff status which side exists.
extract() {
  local ref="$1" path="$2" out="$3"
  mkdir -p "$(dirname "$out")"
  git -C "$repo" show "$ref:$path" >"$out" 2>/dev/null || rm -f "$out"
}

count=0
# --no-renames so a rename shows as delete+add (distinct before/after paths),
# which is what summarize reasons over. -z keeps paths with spaces intact.
while IFS= read -r -d '' status && IFS= read -r -d '' path; do
  case "$status" in
    A*) extract "$after_ref"  "$path" "$dest/after/$path" ;;
    D*) extract "$before_ref" "$path" "$dest/before/$path" ;;
    *)  extract "$before_ref" "$path" "$dest/before/$path"
        extract "$after_ref"  "$path" "$dest/after/$path" ;;
  esac
  count=$((count + 1))
done < <(git -C "$repo" diff --no-renames -z --name-status \
           "$before_ref" "$after_ref" -- "$@")

echo "checked out $count changed file(s) into $dest/{before,after}" >&2
