#!/bin/bash
# Experiment runner script for doc-verification-bridge evaluation
# Usage: ./run.sh [run|serve|clean] [options]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DVB_DIR="$(dirname "$SCRIPT_DIR")"
cd "$SCRIPT_DIR"

# Build the experiments executable if needed
build_experiments() {
    echo "Building experiments executable..."
    (cd "$DVB_DIR" && lake build experiments)
}

case "${1:-help}" in
    run)
        build_experiments
        shift
        # Pass remaining arguments (--resume, --update, etc.)
        echo "Starting experiment pipeline..."
        "$DVB_DIR/.lake/build/bin/experiments" run "$@" --config "$SCRIPT_DIR/config.toml"
        ;;
    refresh)
        build_experiments
        echo "Refreshing summary page..."
        "$DVB_DIR/.lake/build/bin/experiments" refresh --config "$SCRIPT_DIR/config.toml"
        ;;
    serve)
        build_experiments
        echo "Starting HTTP server..."
        "$DVB_DIR/.lake/build/bin/experiments" serve --config "$SCRIPT_DIR/config.toml"
        ;;
    clean)
        echo "Cleaning experiment artifacts..."
        rm -rf repos sites results.json
        echo "Done."
        ;;
    *)
        echo "Usage: $0 <command> [options]"
        echo ""
        echo "Commands:"
        echo "  run              Clone, build, and analyze all projects"
        echo "  refresh          Regenerate summary page from existing coverage data"
        echo "  serve            Start HTTP server to view results"
        echo "  clean            Remove all generated artifacts"
        echo ""
        echo "Run Options:"
        echo "  --resume             Skip completed projects, restart incomplete/failed"
        echo "  --update             Update git repos and regenerate docs"
        echo "  --reanalyze          Re-run unified-doc only (skip build, requires existing build)"
        echo "  --reclassify         Re-run classification only (skip build AND doc-gen4)"
        echo "  --docgen-only        Re-run doc-gen4 + MkDocs (loads classification from cache)"
        echo "  --mkdocs-only        Regenerate MkDocs only (loads classification from cache)"
        echo "  --projects <names>   Only run specified projects (space-separated)"
        echo ""
        echo "Examples:"
        echo "  $0 run                            # Fresh run of all projects"
        echo "  $0 run --resume                   # Continue after interruption"
        echo "  $0 run --update                   # Update repos and regenerate"
        echo "  $0 run --reanalyze --projects mathlib4  # Re-analyze without rebuild"
        echo "  $0 run --reclassify --projects mathlib4 # Re-classify (uses existing doc-gen4)"
        echo "  $0 run --docgen-only --projects mathlib4 # Re-gen doc-gen4 (uses cached classification)"
        echo "  $0 run --mkdocs-only --projects mathlib4 # Regenerate MkDocs (uses cache)"
        echo "  $0 run --projects mathlib4        # Run only mathlib4"
        echo "  $0 run --projects batteries mm0   # Run batteries and mm0"
        echo "  $0 run --update --projects mathlib4  # Update only mathlib4"
        echo "  $0 refresh                        # Regenerate summary only"
        echo "  $0 serve                          # Start HTTP server"
        echo "  $0 clean                          # Remove all artifacts"
        exit 1
        ;;
esac
