#!/usr/bin/env bash
# Publish the generated experiments site (experiments/sites/) to the `pages`
# branch as a single *orphan* commit — no parent, so the branch never
# accumulates history and the regenerated, often-churning doc-gen4 sites stay
# out of main's history entirely. (Mirrors the L4YAML gh-pages publish.)
#
#   ./scripts/publish-pages.sh                # build the orphan locally; do NOT push
#   PAGES_PUSH=1 ./scripts/publish-pages.sh   # also force-push it (CI publishes this way)
#
# Env knobs:
#   PAGES_BRANCH    target branch (default: pages) — the live GitHub Pages source.
#   PAGES_SITE      site directory to publish (default: experiments/sites).
#   PAGES_PUSH      "1" to force-push (default: 0, build only).
#   PAGES_PUSH_URL  explicit push URL. CI passes a token URL:
#                     https://x-access-token:${GITHUB_TOKEN}@github.com/<owner>/<repo>.git
#   PAGES_REMOTE    if PAGES_PUSH_URL is unset, resolve the URL from this remote
#                   (default: origin — on the CI runner actions/checkout's `origin`
#                   is the github.com repo; run locally with PAGES_REMOTE=github).
#
# Two choices that differ from a naive `commit-tree` on the checkout's own .git:
#
#  (1) NO Git LFS, and the big internal artifacts are EXCLUDED. GitHub Pages does
#      not resolve LFS objects — it serves the pointer stub as the file, breaking
#      every LFS-tracked asset — and GitHub never garbage-collects unreferenced
#      LFS objects, so the old workflow's `git lfs track "*.json"/"*.jsonl"/
#      "*.sqlite"` both broke the served JSON and piled up LFS storage without
#      bound. We publish plain git blobs and drop the two artifacts that only
#      existed to justify LFS — the multi-hundred-MB classification-cache.jsonl
#      "entries" caches and the api-temp/ doc-gen4 SQLite build dirs. Neither is
#      fetched by the browsed site, and both are regenerable, so they are
#      excluded from the publish (NOT deleted from the persistent sites cache).
#
#  (2) The orphan commit is built in a THROWAWAY git dir under $TMPDIR, never in
#      the checkout's .git. The site is multiple GB; building it via commit-tree
#      in the checkout would deposit that much into the self-hosted runner's
#      persistent .git on every run and re-bloat it. The throwaway dir is removed
#      on exit, so the main repo's .git stays pristine.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

SITE="${PAGES_SITE:-$ROOT/experiments/sites}"
BRANCH="${PAGES_BRANCH:-pages}"

if [ ! -d "$SITE" ] || [ -z "$(ls -A "$SITE" 2>/dev/null || true)" ]; then
  echo "error: site directory '$SITE' is missing or empty — generate it first (experiments/run.sh run ...)" >&2
  exit 1
fi

# GitHub Pages must serve doc-gen4 assets in dash/underscore-prefixed dirs
# verbatim (no Jekyll), so drop a .nojekyll marker at the site root.
touch "$SITE/.nojekyll"

# Scrub any leftover state from the OLD throwaway-git-init deploy (a nested
# experiments/sites/.git) and any stray LFS .gitattributes, so the fresh orphan
# carries neither.
rm -rf "$SITE/.git" "$SITE/.gitattributes"

SRC_REF="$(git -C "$ROOT" rev-parse --short HEAD 2>/dev/null || echo unknown)"

# Build the single orphan commit in a throwaway git dir.
TMP="$(mktemp -d -t ghpages.XXXXXX)"
trap 'rm -rf "$TMP"' EXIT
GD="$TMP/git"
git --git-dir="$GD" --work-tree="$SITE" init -q

# Stage the whole site EXCEPT regenerable internal artifacts and anything
# GitHub would reject as a plain blob. Pathspec excludes (glob magic → **
# spans directories) keep them out of the commit WITHOUT deleting them from
# the persistent sites cache.
EXCLUDES=(
  ':(exclude,glob)**/*.jsonl'      # classification-cache "entries" (>100 MB)
  ':(exclude,glob)**/*.sqlite'     # doc-gen4 build databases
  ':(exclude,glob)**/api-temp/**'  # doc-gen4 intermediate SQLite/BMP dirs
  ':(exclude,glob)**/.backups/**'  # pipeline backup copies (never served)
)

# GitHub hard-rejects any plain blob >100 MB. Exclude such files explicitly so
# the push can never fail on size (the combined table-data.json programmatic
# export can reach hundreds of MB; the browsed site does not fetch it — the
# HTML tables carry their own data, and the per-module table-data/*.json files
# stay published). Report exactly what was dropped — no silent truncation.
mapfile -d '' -t BIGFILES < <(cd "$SITE" && find . -type f -size +100M -print0)
if [ "${#BIGFILES[@]}" -gt 0 ]; then
  echo "note: excluding ${#BIGFILES[@]} file(s) >100 MB (GitHub's non-LFS blob limit) from the publish:"
  for f in "${BIGFILES[@]}"; do
    rel="${f#./}"
    printf '  - %s (%s)\n' "$rel" "$(du -h "$SITE/$rel" | cut -f1)"
    EXCLUDES+=( ":(exclude,literal)$rel" )
  done
fi

( cd "$SITE"
  git --git-dir="$GD" --work-tree="$SITE" add -A -- . "${EXCLUDES[@]}"
)

git --git-dir="$GD" --work-tree="$SITE" \
  -c user.name="${GIT_AUTHOR_NAME:-github-actions[bot]}" \
  -c user.email="${GIT_AUTHOR_EMAIL:-github-actions[bot]@users.noreply.github.com}" \
  commit -q --no-gpg-sign -m "Deploy experiments site (source ${SRC_REF})"

echo "Built orphan commit for branch '${BRANCH}' with $(git --git-dir="$GD" ls-files | wc -l) files (source ${SRC_REF})."

if [ "${PAGES_PUSH:-0}" = "1" ]; then
  PUSH_URL="${PAGES_PUSH_URL:-}"
  if [ -z "$PUSH_URL" ]; then
    REMOTE="${PAGES_REMOTE:-origin}"
    PUSH_URL="$(git -C "$ROOT" remote get-url "$REMOTE" 2>/dev/null || true)"
    if [ -z "$PUSH_URL" ]; then
      echo "error: cannot resolve a push URL — set PAGES_PUSH_URL, or PAGES_REMOTE to a valid remote" >&2
      exit 1
    fi
  fi
  echo "Force-pushing orphan to ${BRANCH} ..."
  git --git-dir="$GD" push --force "$PUSH_URL" "HEAD:refs/heads/${BRANCH}"
  echo "Published to ${BRANCH}."
else
  echo "Build only (PAGES_PUSH!=1). Push with:  PAGES_PUSH=1 PAGES_REMOTE=github $0"
fi
