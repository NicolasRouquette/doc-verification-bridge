#!/bin/bash
# Link checker script for doc-verification-bridge generated sites
# Generated from config.toml project definitions

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SITES_DIR="${SCRIPT_DIR}/sites"
CONFIG_FILE="${SCRIPT_DIR}/config.toml"

# Extract project names from config.toml
# Looks for lines like: name = "project-name"
read_projects_from_config() {
    if [[ ! -f "$CONFIG_FILE" ]]; then
        echo "Error: config.toml not found at $CONFIG_FILE" >&2
        exit 1
    fi
    grep -E '^name\s*=' "$CONFIG_FILE" | sed -E 's/^name\s*=\s*"([^"]+)"/\1/'
}

# Read projects into array
mapfile -t PROJECTS < <(read_projects_from_config)

# External dependency patterns to ignore (these link to docs not included in project)
IGNORE_PATTERNS=(
    'Init/.*\.html'
    'Std/.*\.html'
    'Lean/.*\.html'
    'Lake/.*\.html'
    'Batteries/.*\.html'  # For non-batteries projects
    'Mathlib/.*\.html'    # For non-mathlib projects
    'Qq/.*\.html'
    'Aesop/.*\.html'      # For non-aesop projects
    'ImportGraph/.*\.html'
    'ProofWidgets/.*\.html'
    'Cli/.*\.html'
    'UnicodeBasic/.*\.html'
    'Plausible/.*\.html'
    'TensorLib/.*\.html'
    'LeanSearchClient/.*\.html'
    'Verso/.*\.html'
    'BibtexQuery/.*\.html'
    'MD4Lean/.*\.html'
    'SubVerso/.*\.html'
)

usage() {
    echo "Usage: $0 [OPTIONS] [PROJECT...]"
    echo ""
    echo "Check links in generated documentation sites."
    echo ""
    echo "Options:"
    echo "  -h, --help       Show this help message"
    echo "  -a, --all        Check all projects (default if no projects specified)"
    echo "  -v, --verbose    Verbose output"
    echo "  -q, --quiet      Only show errors"
    echo "  -l, --list       List available projects"
    echo "  --no-ignore      Don't ignore external dependency links"
    echo "  --summary        Show summary only (no individual errors)"
    echo ""
    echo "Examples:"
    echo "  $0                     # Check all existing project sites"
    echo "  $0 batteries mathlib4  # Check specific projects"
    echo "  $0 -l                  # List available projects"
    echo "  $0 --verbose PhysLean  # Verbose check of PhysLean"
}

list_projects() {
    echo "Available projects (from config.toml):"
    echo ""
    for project in "${PROJECTS[@]}"; do
        if [[ -d "${SITES_DIR}/${project}/site" ]]; then
            echo "  ✓ ${project}"
        else
            echo "  ✗ ${project} (not built)"
        fi
    done
}

# Build ignore pattern arguments for linkchecker
build_ignore_args() {
    local args=""
    for pattern in "${IGNORE_PATTERNS[@]}"; do
        args+=" --ignore-url='${pattern}'"
    done
    echo "$args"
}

check_project() {
    local project="$1"
    local site_dir="${SITES_DIR}/${project}/site"
    
    if [[ ! -d "$site_dir" ]]; then
        echo "⚠️  Skipping ${project}: site directory not found"
        return 1
    fi
    
    echo "🔍 Checking ${project}..."
    
    local ignore_args=""
    if [[ "$NO_IGNORE" != "true" ]]; then
        ignore_args=$(build_ignore_args)
    fi
    
    local verbose_args=""
    if [[ "$VERBOSE" == "true" ]]; then
        verbose_args="--verbose"
    fi
    
    local output_args=""
    if [[ "$QUIET" == "true" ]]; then
        output_args="--no-warnings"
    fi
    
    # Run linkchecker
    local cmd="linkchecker ${verbose_args} ${output_args} ${ignore_args} '${site_dir}'"
    
    if [[ "$VERBOSE" == "true" ]]; then
        echo "  Running: $cmd"
    fi
    
    # Execute with eval to handle the quoted ignore patterns
    if eval "$cmd"; then
        echo "✅ ${project}: All links valid"
        return 0
    else
        echo "❌ ${project}: Link errors found"
        return 1
    fi
}

# Parse arguments
PROJECTS_TO_CHECK=()
CHECK_ALL=false
VERBOSE=false
QUIET=false
NO_IGNORE=false
SUMMARY_ONLY=false

while [[ $# -gt 0 ]]; do
    case $1 in
        -h|--help)
            usage
            exit 0
            ;;
        -a|--all)
            CHECK_ALL=true
            shift
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -q|--quiet)
            QUIET=true
            shift
            ;;
        -l|--list)
            list_projects
            exit 0
            ;;
        --no-ignore)
            NO_IGNORE=true
            shift
            ;;
        --summary)
            SUMMARY_ONLY=true
            shift
            ;;
        -*)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
        *)
            PROJECTS_TO_CHECK+=("$1")
            shift
            ;;
    esac
done

# Check if linkchecker is installed
if ! command -v linkchecker &> /dev/null; then
    echo "Error: linkchecker is not installed"
    echo "Install with: pip install linkchecker"
    exit 1
fi

# If no projects specified, check all existing ones
if [[ ${#PROJECTS_TO_CHECK[@]} -eq 0 ]]; then
    for project in "${PROJECTS[@]}"; do
        if [[ -d "${SITES_DIR}/${project}/site" ]]; then
            PROJECTS_TO_CHECK+=("$project")
        fi
    done
fi

if [[ ${#PROJECTS_TO_CHECK[@]} -eq 0 ]]; then
    echo "No project sites found in ${SITES_DIR}/"
    echo "Run the doc-verification-bridge experiments first."
    exit 1
fi

echo "========================================"
echo "Link Checker for doc-verification-bridge"
echo "========================================"
echo ""
echo "Sites directory: ${SITES_DIR}"
echo "Projects to check: ${PROJECTS_TO_CHECK[*]}"
echo ""

# Track results
PASSED=()
FAILED=()
SKIPPED=()

for project in "${PROJECTS_TO_CHECK[@]}"; do
    echo "----------------------------------------"
    if check_project "$project"; then
        PASSED+=("$project")
    else
        if [[ -d "${SITES_DIR}/${project}/site" ]]; then
            FAILED+=("$project")
        else
            SKIPPED+=("$project")
        fi
    fi
    echo ""
done

# Summary
echo "========================================"
echo "Summary"
echo "========================================"
echo ""
echo "✅ Passed: ${#PASSED[@]}"
for p in "${PASSED[@]}"; do echo "   - $p"; done
echo ""
echo "❌ Failed: ${#FAILED[@]}"
for p in "${FAILED[@]}"; do echo "   - $p"; done
echo ""
echo "⚠️  Skipped: ${#SKIPPED[@]}"
for p in "${SKIPPED[@]}"; do echo "   - $p"; done

# Exit with error if any failed
if [[ ${#FAILED[@]} -gt 0 ]]; then
    exit 1
fi
