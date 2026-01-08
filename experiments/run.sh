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
    serve)
        build_experiments
        echo "Starting HTTP servers for all projects..."
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
        echo "  serve            Start HTTP servers for viewing results"
        echo "  clean            Remove all generated artifacts"
        echo ""
        echo "Run Options:"
        echo "  --resume             Skip completed projects, restart incomplete/failed"
        echo "  --update             Update git repos and regenerate docs"
        echo "  --projects <names>   Only run specified projects (space-separated)"
        echo ""
        echo "Examples:"
        echo "  $0 run                            # Fresh run of all projects"
        echo "  $0 run --resume                   # Continue after interruption"
        echo "  $0 run --update                   # Update repos and regenerate"
        echo "  $0 run --projects mathlib4        # Run only mathlib4"
        echo "  $0 run --projects batteries mm0   # Run batteries and mm0"
        echo "  $0 run --update --projects mathlib4  # Update only mathlib4"
        echo "  $0 serve                          # Start HTTP server"
        echo "  $0 clean                          # Remove all artifacts"
        exit 1
        ;;
esac
