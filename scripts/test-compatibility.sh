#!/bin/bash
# Test that Compatibility.lean compiles across all supported Lean 4 versions
# Usage: ./scripts/test-compatibility.sh

# Don't use set -e since lean returns non-zero on warnings

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DVB_DIR="$(dirname "$SCRIPT_DIR")"
COMPAT_FILE="$DVB_DIR/DocVerificationBridge/Compatibility.lean"

# Supported Lean 4 versions (from Experiments.lean: minSupportedVersion to maxSupportedVersion)
# Note: We test representative versions, not every patch release
VERSIONS=(
    "v4.24.0"
    "v4.25.0"
    "v4.26.0"
    "v4.27.0-rc1"  # Latest RC, v4.27.0 not released yet
)

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "Testing Compatibility.lean across Lean 4 versions..."
echo "File: $COMPAT_FILE"
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

for VERSION in "${VERSIONS[@]}"; do
    TOOLCHAIN="leanprover/lean4:$VERSION"
    
    # Check if toolchain is installed (suppress broken pipe errors)
    if ! elan show 2>/dev/null | grep -q "$TOOLCHAIN" 2>/dev/null; then
        echo -e "${YELLOW}[$VERSION]${NC} Not installed, installing..."
        if ! elan toolchain install "$TOOLCHAIN" 2>&1 | head -5; then
            echo -e "${YELLOW}[$VERSION]${NC} SKIPPED (toolchain not available)"
            ((SKIPPED++))
            continue
        fi
    fi
    
    # Create a temporary directory for the test
    TMPDIR=$(mktemp -d)
    trap "rm -rf $TMPDIR" EXIT
    
    # Copy the file
    cp "$COMPAT_FILE" "$TMPDIR/"
    
    # Write toolchain file
    echo "$TOOLCHAIN" > "$TMPDIR/lean-toolchain"
    
    # Try to compile
    echo -n "[$VERSION] Testing... "
    
    # Run lean directly on the file
    if OUTPUT=$(cd "$TMPDIR" && lean Compatibility.lean 2>&1); then
        echo -e "${GREEN}PASSED${NC}"
        ((PASSED++))
    else
        # Check if it's just deprecation warnings (which are OK)
        if echo "$OUTPUT" | grep -q "error:"; then
            echo -e "${RED}FAILED${NC}"
            echo "  Error output:"
            echo "$OUTPUT" | grep -A2 "error:" | sed 's/^/    /'
            ((FAILED++))
        else
            echo -e "${GREEN}PASSED${NC} (with warnings)"
            ((PASSED++))
        fi
    fi
    
    # Cleanup temp dir
    rm -rf "$TMPDIR"
    trap - EXIT
done

echo ""
echo "=========================================="
echo "SUMMARY"
echo "=========================================="
echo -e "Passed:  ${GREEN}$PASSED${NC}"
echo -e "Failed:  ${RED}$FAILED${NC}"
echo -e "Skipped: ${YELLOW}$SKIPPED${NC}"
echo ""

if [ $FAILED -gt 0 ]; then
    echo -e "${RED}Some versions failed!${NC}"
    exit 1
else
    echo -e "${GREEN}All tested versions passed!${NC}"
    exit 0
fi
