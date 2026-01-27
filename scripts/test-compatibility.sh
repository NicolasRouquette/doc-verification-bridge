#!/bin/bash
# Test that Compatibility.lean compiles across all supported Lean 4 versions
# Usage: ./scripts/test-compatibility.sh#
# This tests standalone files that verify cross-version idioms:
# - Compatibility.lean: String operations (no imports)
# - CompatibilityLean.lean: Options API, Core.Context (import Lean)
#
# Files like UnifiedBasic.lean can't be tested here because they have
# dependencies (DocGen4, etc.) that would need lake builds per version.
# Instead, we test the idioms they use in CompatibilityLean.lean.
# Don't use set -e since lean returns non-zero on warnings

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DVB_DIR="$(dirname "$SCRIPT_DIR")"
COMPAT_FILE="$DVB_DIR/DocVerificationBridge/Compatibility.lean"
COMPAT_LEAN_FILE="$DVB_DIR/DocVerificationBridge/CompatibilityLean.lean"

# Supported Lean 4 versions (from Experiments.lean: minSupportedVersion to maxSupportedVersion)
# Note: We test representative versions, not every patch release
VERSIONS=(
    "v4.24.0"
    "v4.25.0"
    "v4.26.0"
    "v4.27.0"
    "v4.28.0-rc1"
)

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "Testing compatibility files across Lean 4 versions..."
echo ""

# Check if elan is available
if ! command -v elan &> /dev/null; then
    echo -e "${RED}Error: elan not found. Please install elan first.${NC}"
    exit 1
fi

# Track results
PASSED=0
FAILED=0
SKIPPED=0

# Function to test a single file
test_file() {
    local VERSION="$1"
    local TOOLCHAIN="$2"
    local FILE="$3"
    local FILENAME=$(basename "$FILE")
    
    # Create a temporary directory for the test
    TMPDIR=$(mktemp -d)
    
    # Copy the file
    cp "$FILE" "$TMPDIR/"
    
    # Write toolchain file
    echo "$TOOLCHAIN" > "$TMPDIR/lean-toolchain"
    
    # Try to compile
    echo -n "  [$FILENAME] "
    
    # Run lean directly on the file
    if OUTPUT=$(cd "$TMPDIR" && lean "$FILENAME" 2>&1); then
        echo -e "${GREEN}PASSED${NC}"
        rm -rf "$TMPDIR"
        return 0
    else
        # Check if it's just deprecation warnings (which are OK)
        if echo "$OUTPUT" | grep -q "error:"; then
            echo -e "${RED}FAILED${NC}"
            echo "    Error output:"
            echo "$OUTPUT" | grep -A2 "error:" | sed 's/^/      /'
            rm -rf "$TMPDIR"
            return 1
        else
            echo -e "${GREEN}PASSED${NC} (with warnings)"
            rm -rf "$TMPDIR"
            return 0
        fi
    fi
}

for VERSION in "${VERSIONS[@]}"; do
    TOOLCHAIN="leanprover/lean4:$VERSION"
    
    echo "[$VERSION]"
    
    # Check if toolchain is installed (suppress broken pipe errors)
    if ! elan show 2>/dev/null | grep -q "$TOOLCHAIN" 2>/dev/null; then
        echo "  Not installed, installing..."
        if ! elan toolchain install "$TOOLCHAIN" 2>&1 | head -5; then
            echo -e "  ${YELLOW}SKIPPED${NC} (toolchain not available)"
            ((SKIPPED++))
            continue
        fi
    fi
    
    # Test both compatibility files
    VERSION_PASSED=true
    
    # Test 1: Compatibility.lean (standalone, no imports)
    if ! test_file "$VERSION" "$TOOLCHAIN" "$COMPAT_FILE"; then
        VERSION_PASSED=false
    fi
    
    # Test 2: CompatibilityLean.lean (requires `import Lean`)
    if ! test_file "$VERSION" "$TOOLCHAIN" "$COMPAT_LEAN_FILE"; then
        VERSION_PASSED=false
    fi
    
    if $VERSION_PASSED; then
        ((PASSED++))
    else
        ((FAILED++))
    fi
done

echo ""
echo "=========================================="
echo "SUMMARY"
echo "=========================================="
echo -e "Versions Passed:  ${GREEN}$PASSED${NC}"
echo -e "Versions Failed:  ${RED}$FAILED${NC}"
echo -e "Versions Skipped: ${YELLOW}$SKIPPED${NC}"
echo ""

if [ $FAILED -gt 0 ]; then
    echo -e "${RED}Some versions failed!${NC}"
    exit 1
else
    echo -e "${GREEN}All tested versions passed!${NC}"
    exit 0
fi
