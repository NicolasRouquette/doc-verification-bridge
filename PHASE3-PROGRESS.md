# Phase 3 Progress Report

**Date**: 2026-03-03
**Status**: Structural work complete, functional implementation pending

## Summary

Phase 3 aims to decouple the Experiments module from DocVerificationBridge, enabling it to work across multiple Lean versions by invoking DocVerificationBridge as a subprocess rather than importing it.

### ✅ Completed

**Structural Decoupling:**
- ✅ Removed `import DocVerificationBridge.StaticHtml` (was unused)
- ✅ Removed `import DocVerificationBridge.Compatibility`
- ✅ Copied Compatibility module to `Experiments/Experiments/Compatibility.lean`
- ✅ Updated imports to use local `Experiments.Compatibility`
- ✅ Fixed Experiments package structure for Lake build system
- ✅ Verified Experiments builds independently (3 jobs, 1 warning)

**Package Structure:**
```
Experiments/                          # Package root
├── lakefile.toml                     # Package configuration
├── Main.lean                         # Executable entry point
├── Experiments.lean                  # Library root module
└── Experiments/                      # Library subdirectory
    ├── Compatibility.lean            # Local copy of compat helpers
    └── ExperimentsCore.lean          # Main experiments logic (125KB)
```

**Build Status:** ✅ **BUILDS SUCCESSFULLY**
```bash
cd Experiments
lake build Experiments.ExperimentsCore
# ✅ Build completed successfully (3 jobs)
# ⚠️  1 warning: unused variable (non-critical)
```

### ⏳ Pending (Functional Implementation)

The following functional changes are **not yet implemented** but the structure is ready for them:

#### 1. Subprocess Invocation Logic

**Current state:** Experiments still expects DocVerificationBridge to be co-located in a `docvb/` directory.

**Needed:**
- Replace direct invocation with `IO.Process.spawn`
- Build command: `lake build unified-doc` in the DocVerificationBridge directory
- Run command: `lake exe unified-doc unified [args]`
- Capture stdout/stderr for error handling
- Parse exit codes for success/failure detection

**Example implementation:**
```lean
def invokeUnifiedDoc (dvbPath : FilePath) (args : Array String) : IO (UInt32 × String × String) := do
  -- Build DocVerificationBridge
  let buildResult ← IO.Process.spawn {
    cmd := "lake"
    args := #["build", "unified-doc"]
    cwd := dvbPath
  }

  -- Run unified-doc
  let runResult ← IO.Process.spawn {
    cmd := "lake"
    args := #["exe", "unified-doc"] ++ args
    cwd := dvbPath
    stdout := .piped
    stderr := .piped
  }

  let stdout ← runResult.stdout.readToEnd
  let stderr ← runResult.stderr.readToEnd
  let exitCode ← runResult.wait

  return (exitCode, stdout, stderr)
```

#### 2. Git Clone/Checkout Logic

**Current state:** Experiments has `setupDocvbDirectory` which copies DocVerificationBridge.

**Needed:**
- Clone DocVerificationBridge from git URL (e.g., `https://github.com/user/doc-verification-bridge`)
- Checkout specific version based on `docvb_version` config field
- Cache cloned repos to avoid re-cloning
- Handle branch names, tags, and commit SHAs

**Example implementation:**
```lean
def cloneDocVerificationBridge (targetDir : FilePath) (version : String) : IO Unit := do
  -- Clone if not exists
  if !(← targetDir.pathExists) then
    IO.println s!"Cloning DocVerificationBridge to {targetDir}..."
    let result ← IO.Process.spawn {
      cmd := "git"
      args := #["clone", "https://github.com/user/doc-verification-bridge", targetDir.toString]
    }
    let exitCode ← result.wait
    if exitCode != 0 then
      throw <| IO.userError "Failed to clone DocVerificationBridge"

  -- Checkout version
  IO.println s!"Checking out version {version}..."
  let result ← IO.Process.spawn {
    cmd := "git"
    args := #["checkout", version]
    cwd := targetDir
  }
  let exitCode ← result.wait
  if exitCode != 0 then
    throw <| IO.userError s!"Failed to checkout version {version}"
```

#### 3. Config Schema Changes

**Current state:** `Project` structure does NOT have `docvb_version` field.

**Needed changes to `Experiments/Experiments/ExperimentsCore.lean`:**

```lean
/-- A project to analyze -/
structure Project where
  name : String
  repo : String
  modules : Array String
  description : String := ""
  -- NEW FIELD:
  docvb_version : String := "main"  -- Git branch/tag/SHA for DocVerificationBridge
  classificationMode : ClassificationMode := .auto
  subdirectory : Option String := none
  -- ... (other fields unchanged)
  deriving Repr, Inhabited
```

**Config TOML format:**
```toml
[[projects]]
name = "lean4-yaml-verified"
repo = "https://github.com/cedar-policy/lean4-yaml-verified"
modules = ["Lean4Yaml"]
description = "Verified YAML parser"
docvb_version = "v4.28.0"  # ← NEW FIELD
```

**Update TOML parser** (around line 230):
```lean
else if key == "docvb_version" then
  currentProject := currentProject.map fun p => { p with docvb_version := value }
```

#### 4. Remove `checkToolchainCompatibility`

**Current state:** Line 1188 defines `checkToolchainCompatibility` which checks if project's Lean version matches DocVerificationBridge's Lean version.

**Needed:** Remove or adapt this function since we now:
- Clone version-specific DocVerificationBridge per project
- Each project uses its own DocVerificationBridge version
- No single "compatible" version anymore

#### 5. Update `setupDocvbDirectory`

**Current state:** Line ~1230 has `setupDocvbDirectory` which copies DocVerificationBridge.

**Needed:** Replace with:
```lean
def setupDocvbDirectory (projectDir : FilePath) (version : String) : IO FilePath := do
  let dvbDir := projectDir / "docvb"

  -- Clone if not exists or wrong version
  cloneDocVerificationBridge dvbDir version

  return dvbDir
```

## Testing Strategy

Once functional implementation is complete:

### Unit Tests
1. Test subprocess invocation with mock DocVerificationBridge
2. Test git clone/checkout with test repo
3. Test config parsing with `docvb_version` field

### Integration Tests
1. Run experiments with two projects using different DocVerificationBridge versions
2. Verify isolation: changes in one version don't affect the other
3. Verify caching: repeated runs don't re-clone

### End-to-End Tests
1. Run full experiment pipeline on lean4-yaml-verified (v4.28.0)
2. Run full experiment pipeline on a v4.27.0 project (if one exists)
3. Verify both succeed with their respective DocVerificationBridge versions

## Estimated Effort

**Remaining work:** ~1-2 days

- Subprocess invocation: 2-3 hours
- Git operations: 2-3 hours
- Config changes: 1 hour
- Testing & debugging: 4-6 hours
- Documentation: 1 hour

## Current Limitations

Even with structural decoupling complete, the Experiments module:

1. **Still expects local DocVerificationBridge:** The code references `config.docVerificationBridgePath` expecting a local directory
2. **No version isolation:** All projects use the same DocVerificationBridge directory
3. **No subprocess boundary:** Would need process spawning, not imports
4. **Config parsing incomplete:** `docvb_version` field not yet defined or parsed

## Benefits of Current Progress

Despite incomplete functional implementation:

1. **Clean separation:** Experiments can be built without DocVerificationBridge in the workspace
2. **No import dependencies:** Future changes to DocVerificationBridge won't break Experiments build
3. **Correct structure:** Lake package structure follows best practices
4. **Foundation ready:** All structural prerequisites for subprocess invocation are in place

## Relationship to REFACTORING.md

This aligns with [REFACTORING.md Phase 3](REFACTORING.md#phase-3-experiments-integration-future-pr):

**From REFACTORING.md:**
> - [ ] Add `docvb_version` field to config.toml schema
> - [ ] Implement DocVerificationBridge cloning/checkout logic
> - [ ] Implement subprocess-based invocation
> - [ ] Update all project configs with appropriate versions
> - [ ] Test end-to-end experiment pipeline

**Status:**
- [ ] Add `docvb_version` field - **Design ready, not implemented**
- [ ] Implement cloning/checkout - **Not implemented**
- [ ] Implement subprocess invocation - **Not implemented**
- [ ] Update configs - **Blocked on schema changes**
- [ ] Test end-to-end - **Blocked on implementation**

## Next Steps

### Option A: Complete Phase 3 (Functional Implementation)

Continue with the subprocess invocation and git operations implementation:

1. Implement `invokeUnifiedDoc` subprocess wrapper
2. Implement `cloneDocVerificationBridge` git operations
3. Add `docvb_version` field to Project structure
4. Update TOML parser to read `docvb_version`
5. Update `setupDocvbDirectory` to use git clone
6. Remove/adapt `checkToolchainCompatibility`
7. Test with multiple projects
8. Update all config files with `docvb_version`

### Option B: Document and Pause

Document current progress and defer functional implementation:

1. Create PHASE3-PAUSED.md with detailed design
2. Update README with current capabilities
3. Mark Phase 3 as "structural complete, functional pending"
4. Resume later when subprocess implementation is prioritized

## Recommendation

**Option B (Document and Pause)** is recommended because:

1. **Core refactoring is complete:** Phases 1 & 2 achieved the main goal (two-package structure)
2. **Structural foundation is solid:** Experiments builds independently
3. **Functional work is substantial:** ~1-2 days of focused implementation
4. **Current system works:** Experiments can run with local DocVerificationBridge
5. **Clear design exists:** Implementation path is well-defined

The subprocess implementation is a "nice to have" optimization that enables running experiments across Lean versions, but isn't blocking other work. The current structure is clean, maintainable, and ready for that future enhancement.

## Files Modified

### Created
- `Experiments/Experiments/Compatibility.lean` (copied from DocVerificationBridge)
- `Experiments/Experiments/ExperimentsCore.lean` (renamed from Experiments.lean)
- `Experiments/Experiments.lean` (minimal root module)

### Modified
- `Experiments/Experiments/ExperimentsCore.lean` (imports updated)

### Structure Changes
- Created `Experiments/Experiments/` subdirectory for library files
- Moved library files into subdirectory to match Lake conventions

## Build Verification

```bash
cd Experiments
lake clean
lake build Experiments.ExperimentsCore

# Output:
# ✔ [1/3] Built Init
# ✔ [2/3] Built Experiments.Compatibility (327ms)
# ⚠ [3/3] Built Experiments.ExperimentsCore (6.5s)
# warning: Experiments/ExperimentsCore.lean:2264:2: unused variable `cmdLog`
# Build completed successfully (3 jobs).
```

**Status:** ✅ Successfully builds with only non-critical warning

---

**Phase 3 Structural Work: COMPLETE ✅**
**Phase 3 Functional Work: PENDING ⏳** (Estimated 1-2 days)

Ready to proceed to Option A (complete implementation) or Option B (document and pause)?
