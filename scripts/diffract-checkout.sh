#!/usr/bin/env bash
#
# diffract-checkout.sh — materialize a git change-pair for `diffract summarize`.
#
# Given two refs (commits, branches, tags) and a destination directory, extract
# every file that differs between them into <dest>/before/<path> and
# <dest>/after/<path>, preserving directory structure, and write a manifest
# <dest>/pairs.tsv naming which before-file corresponds to which after-file.
#
# The manifest is what carries RENAMES. Two directory trees cannot express "this
# before-file is that differently-named after-file", so without it a renamed
# file looks like an unrelated deletion plus an unrelated addition and the
# systematic edits inside it yield no rules. Contents on disk are the same
# either way, so the trees remain usable on their own:
#
#   diffract summarize --pairs <dest>/pairs.tsv -l kotlin      # rename-aware
#   diffract summarize <dest>/before <dest>/after -l kotlin -i '*.kt'
#
# Manifest format — tab-separated, one record per changed file, paths are
# logical (repo-relative; field 1 lives under before/, field 2 under after/):
#
#   pair<TAB>src/old/Thing.kt<TAB>src/new/Thing.kt    renamed
#   pair<TAB>src/Widget.kt<TAB>src/Widget.kt          modified in place
#   add<TAB>src/New.kt                                added
#   del<TAB>src/Gone.kt                               deleted
#
# Blank lines and lines starting with '#' are comments.
#
# Usage:
#   scripts/diffract-checkout.sh [-C REPO] [-M PCT] BEFORE_REF AFTER_REF DEST [PATHSPEC...]
#
#   -C REPO     run against the git repo at REPO (default: current directory)
#   -M PCT      rename-detection similarity threshold, percent (default: 50,
#               git's own default). Lower it when renames are being missed: a
#               thoroughly-renamed file scores LOWER, because git compares
#               content, so heavily-rewritten renames fall below the default.
#   PATHSPEC    optional git pathspecs restricting which files are copied,
#               e.g.  '*.ts' 'src/'   (anything `git diff -- ...` accepts)
#
# Examples:
#   scripts/diffract-checkout.sh HEAD~1 HEAD /tmp/cs -- '*.ts'
#   diffract summarize --pairs /tmp/cs/pairs.tsv -l typescript
#
#   scripts/diffract-checkout.sh -C ~/proj -M 40 main feature /tmp/cs '*.kt'

set -euo pipefail

repo="."
sim=50
# Pair count above which the changeset is worth a word of warning (see the note
# printed at the end). Not a limit — the checkout itself is cheap either way.
big=500
while getopts "C:M:h" opt; do
  case "$opt" in
    C) repo="$OPTARG" ;;
    M) sim="$OPTARG" ;;
    h) awk 'NR==1{next} /^#/{sub(/^# ?/,""); print; next} {exit}' "$0"; exit 0 ;;
    *) echo "try -h for usage" >&2; exit 2 ;;
  esac
done
shift $((OPTIND - 1))

if [ "$#" -lt 3 ]; then
  echo "usage: $0 [-C REPO] [-M PCT] BEFORE_REF AFTER_REF DEST [PATHSPEC...]" >&2
  exit 2
fi

case "$sim" in
  ''|*[!0-9]*) echo "error: -M expects a percentage, got '$sim'" >&2; exit 2 ;;
esac

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
manifest="$dest/pairs.tsv"

# Pull one blob out of a ref into the tree, creating parent dirs. A missing
# path (e.g. asking for a deleted file's after-state) is skipped silently —
# the caller already knows from the diff status which side exists.
extract() {
  local ref="$1" path="$2" out="$3"
  mkdir -p "$(dirname "$out")"
  git -C "$repo" show "$ref:$path" >"$out" 2>/dev/null || rm -f "$out"
}

{
  printf '# diffract change-pair manifest\n'
  printf '# repo: %s\n' "$(cd "$repo" && pwd -P)"
  printf '# refs: %s..%s (-M%s%%)\n' "$before_ref" "$after_ref" "$sim"
} >"$manifest"

count=0; renames=0
# -M detects renames; the record then carries TWO paths, so the field count
# depends on the status letter and must be read conditionally. -z keeps paths
# with spaces (and tabs, and newlines) intact, which the non-z form would
# C-quote. Note the extracted trees are identical to a --no-renames run: only
# git's classification differs, not which blobs exist on either side.
while IFS= read -r -d '' status; do
  IFS= read -r -d '' path || break
  case "$status" in
    R*|C*)
      IFS= read -r -d '' newpath || break
      extract "$before_ref" "$path"    "$dest/before/$path"
      extract "$after_ref"  "$newpath" "$dest/after/$newpath"
      printf 'pair\t%s\t%s\n' "$path" "$newpath" >>"$manifest"
      renames=$((renames + 1))
      ;;
    A*)
      extract "$after_ref" "$path" "$dest/after/$path"
      printf 'add\t%s\n' "$path" >>"$manifest"
      ;;
    D*)
      extract "$before_ref" "$path" "$dest/before/$path"
      printf 'del\t%s\n' "$path" >>"$manifest"
      ;;
    M*|T*)
      # T is a type change (file <-> symlink <-> submodule). Both sides exist,
      # so extract and pair it like a modification, but say so: summarize will
      # be comparing things that are not both source files.
      if [ "${status#T}" != "$status" ]; then
        echo "warning: $path changed type ($status); pairing it anyway" >&2
      fi
      extract "$before_ref" "$path" "$dest/before/$path"
      extract "$after_ref"  "$path" "$dest/after/$path"
      printf 'pair\t%s\t%s\n' "$path" "$path" >>"$manifest"
      ;;
    *)
      # U (unmerged), X (unknown), B (broken pairing) should not arise from a
      # diff of two commits. Fail loudly rather than silently shortening the
      # changeset — a manifest that quietly omits files is worse than none.
      echo "error: unexpected diff status '$status' for '$path'" >&2
      echo "       (U/X/B need a conflicted index or -B; this script expects neither)" >&2
      exit 1
      ;;
  esac
  count=$((count + 1))
done < <(git -C "$repo" diff "-M${sim}%" -z --name-status \
           "$before_ref" "$after_ref" -- "$@")

echo "checked out $count changed file(s) into $dest/{before,after}" >&2
echo "wrote $manifest ($renames rename(s) detected at -M${sim}%)" >&2

# `summarize` holds parsed trees for the whole changeset at once, so both its
# runtime and its memory grow with the file count — a few hundred pairs is
# minutes and gigabytes. Whole-repo migrations run to thousands, which is why
# this script takes pathspecs: say so here rather than letting the user find
# out from a run that has to be killed.
if [ "$count" -gt "$big" ]; then
  echo "note: $count pairs is a large changeset for 'summarize' (expect long" >&2
  echo "      runtimes and multi-gigabyte memory). Consider narrowing it with a" >&2
  echo "      pathspec — one module at a time — if a systematic change repeats" >&2
  echo "      across the tree, a slice shows the same rules for a fraction of" >&2
  echo "      the cost." >&2
fi

echo "  diffract summarize --pairs $manifest -l LANG" >&2
