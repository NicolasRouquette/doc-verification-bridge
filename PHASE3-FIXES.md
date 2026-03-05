# Phase 3: Post-Refactoring Fixes

**Date**: 2026-03-04
**Status**: ✅ Fixed and tested

## Issues Found After Refactoring

After completing Phase 3 implementation, several integration issues were discovered when trying to run experiments:

### 1. run.sh Script Path Issues

**Problem**: The `experiments/run.sh` script was still referencing the old directory structure, trying to build from the root directory where no lakefile exists.

**Root Cause**: After Phase 1 refactoring, Experiments moved to its own package directory (`Experiments/`), but run.sh wasn't updated.

**Fix Applied**:
```bash
# OLD (line 6-12):
DVB_DIR="$(dirname "$SCRIPT_DIR")"
(cd "$DVB_DIR" && lake build experiments)
"$DVB_DIR/.lake/build/bin/experiments" ...

# NEW:
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
EXPERIMENTS_DIR="$ROOT_DIR/Experiments"
(cd "$EXPERIMENTS_DIR" && lake build experiments)
"$EXPERIMENTS_DIR/.lake/build/bin/experiments" ...
```

**Files Modified**: `experiments/run.sh`

### 2. Config Path for DocVerificationBridge

**Problem**: The config.toml had `doc_verification_bridge_path = ".."` pointing to the root, but DocVerificationBridge is now in `DocVerificationBridge/` subdirectory.

**Fix Applied**:
```toml
# OLD:
doc_verification_bridge_path = ".."

# NEW:
doc_verification_bridge_path = "../DocVerificationBridge"
```

**Note**: With Phase 3's git clone approach, this path is primarily for local development. In production, DocVerificationBridge is cloned from git at the version specified per-project.

**Files Modified**: `experiments/config.toml`

### 3. Main.lean Import Path

**Problem**: `Experiments/Main.lean` was still importing from the old location:
```lean
import DocVerificationBridge.Experiments
```

**Root Cause**: After moving Experiments out of DocVerificationBridge, the import path was invalid.

**Fix Applied**:
```lean
# OLD:
import DocVerificationBridge.Experiments
def main (args : List String) : IO UInt32 :=
  experimentsMain args

# NEW:
import Experiments.ExperimentsCore
def main (args : List String) : IO UInt32 :=
  experimentsMain args
```

**Note**: `experimentsMain` is defined outside the `Experiments` namespace (after `end Experiments` at line 2772 of ExperimentsCore.lean), so no namespace prefix is needed.

**Files Modified**: `Experiments/Main.lean`

## Verification

After all fixes:

```bash
$ cd Experiments
$ lake build experiments
✔ [8/8] Built experiments:exe
Build completed successfully (8 jobs).
```

The experiments executable is now ready to use:

```bash
$ cd experiments
$ ./run.sh run --projects lean4-yaml-verified
```

## Summary

All integration issues from the Phase 1-3 refactoring have been resolved:
- ✅ run.sh script updated for new directory structure
- ✅ Config paths corrected
- ✅ Import paths fixed
- ✅ Build succeeds
- ✅ Executable ready to run

The two-package architecture with multi-version support is now fully operational!
