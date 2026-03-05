#!/bin/bash
# Sync changes from internal repo to public GitHub
# This script helps manage the dual-config setup

set -e

INTERNAL_DIR="/home/nfr/projects/tlr/lean/doc-verification-bridge.internal"
PUBLIC_DIR="/home/nfr/projects/tlr/lean/doc-verification-bridge"

echo "🔄 Syncing doc-verification-bridge: Internal → Public GitHub"
echo "=================================================="
echo ""

# Step 1: Ensure we're in the public repo
cd "$PUBLIC_DIR"

# Step 2: Check git status
echo "📊 Checking git status..."
if [[ -n $(git status --porcelain) ]]; then
    echo "⚠️  WARNING: Public repo has uncommitted changes:"
    git status --short
    echo ""
    read -p "Continue anyway? (y/N): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 1
    fi
fi

# Step 3: Fetch from internal origin
echo ""
echo "📥 Fetching from internal origin (JPL)..."
git fetch origin

# Step 4: Show what will be merged
echo ""
echo "📋 Changes to be merged:"
git log HEAD..origin/main --oneline --graph --decorate
echo ""

# Step 5: Ask for confirmation
read -p "Proceed with merge? (y/N): " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Aborted."
    exit 1
fi

# Step 6: Merge changes
echo ""
echo "🔀 Merging changes from origin/main..."
git merge origin/main --no-edit

# Step 7: Handle config file
echo ""
echo "📝 Updating config.toml from config.public.toml..."
if [[ -f "experiments/config.public.toml" ]]; then
    cp experiments/config.public.toml experiments/config.toml
    git add experiments/config.toml
    echo "✅ config.toml updated with public configuration"
else
    echo "⚠️  WARNING: config.public.toml not found!"
    echo "You may need to manually manage experiments/config.toml"
fi

# Step 8: Show status
echo ""
echo "📊 Current status:"
git status

# Step 9: Ask about pushing
echo ""
echo "🚀 Ready to push to public GitHub?"
echo "   This will push to: $(git remote get-url github)"
read -p "Push now? (y/N): " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo ""
    echo "📤 Pushing to public GitHub..."
    git push github main
    echo ""
    echo "✅ Successfully synced to public GitHub!"
else
    echo ""
    echo "ℹ️  Changes merged but not pushed."
    echo "   Push manually with: git push github main"
fi

echo ""
echo "✨ Done!"
