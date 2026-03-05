# Required Updates for Experiment Success

**Based on**: Experiment run 2026-03-04
**Current Success Rate**: 1/5 projects (20%)
**Target Success Rate**: 5/5 projects (100%)

## Summary

The experiments revealed that 4 out of 5 projects failed due to Lean 4.29.0 API incompatibilities. All failures are related to breaking changes in doc-gen4 v4.29.0-rc3+. This document outlines the required updates to achieve full experiment success.

## Critical Path Updates

### 🔴 CRITICAL: Update 1 - Implement DB-Based doc-gen4 API

**Priority**: HIGHEST (Blocks 3/4 failures: batteries, mathlib4, cslib)
**Estimated Effort**: 2-3 days
**Files to Modify**:
- `DocVerificationBridge/DocVerificationBridge/SourceLinkerCompat.lean`
- `DocVerificationBridge/lakefile.toml` (v4.29.0 branch)

**Problem**:
doc-gen4 v4.29.0 replaced `htmlOutputResults` with `htmlOutputResultsParallel`, which requires a SQLite database backend instead of in-memory `AnalyzerResult`.

**Solution**:
Implement a new compatibility layer that:
1. Creates a temporary SQLite database
2. Populates it from AnalyzerResult using doc-gen4's DB schema
3. Builds LinkingContext from the analyzer result
4. Calls the new parallel API
5. Returns results in the expected format

**Implementation Outline**:
```lean
-- For Lean 4.29.0+ in SourceLinkerCompat.lean
def htmlOutputResultsCompat429
    (baseConfig : SiteBaseContext)
    (result : AnalyzerResult)
    (sourceUrl? : Option String)
    (sourceLinkerFn? : Option SourceLinkerFn)
    (declarationDecorator? : Option DeclarationDecoratorFn)
    : IO (Array System.FilePath) := do

  -- Step 1: Create temporary database
  let dbPath := baseConfig.buildDir / "docvb-temp.db"
  IO.FS.createDirAll baseConfig.buildDir
  let db ← DocGen4.DB.Schema.getDb dbPath

  -- Step 2: Populate database from AnalyzerResult
  -- Research needed: Study DocGen4.DB.Schema to understand insertion API
  for (modName, module) in result.moduleInfo do
    -- Example (actual API may differ):
    -- ← DocGen4.DB.insertModule db modName module
    sorry -- TODO: Implement based on doc-gen4 DB API

  -- Step 3: Build LinkingContext
  let linkCtx : DocGen4.Output.LinkingContext := {
    name2ModIdx := result.name2ModIdx
    moduleNames := result.moduleNames
  }

  -- Step 4: Call new API
  let (files, _) ← DocGen4.Output.htmlOutputResultsParallel
    baseConfig dbPath linkCtx
    result.moduleNames sourceLinkerFn? declarationDecorator?

  return files
```

**Research Tasks**:
1. Study doc-gen4 v4.29.0-rc3 source code:
   - `DocGen4/Output.lean` - New API signatures
   - `DocGen4/DB/Schema.lean` - Database creation and schema
   - `DocGen4/DB/*.lean` - Insertion functions
2. Understand AnalyzerResult → DB mapping
3. Test with incremental builds

**Testing**:
```bash
# Test on v4.29.0 branch
cd DocVerificationBridge
git checkout v4.29.0
lake build unified-doc

# Test with cslib
cd ../experiments
./run.sh run --projects cslib
```

**Validation**:
- [ ] Builds successfully on v4.29.0 branch
- [ ] cslib experiment passes
- [ ] mathlib4 experiment passes
- [ ] batteries experiment passes (after Update 3)
- [ ] Backward compatibility: lean4-yaml-verified still works

---

### 🔴 CRITICAL: Update 2 - Fix Conditional Import in Main.lean

**Priority**: HIGH (Blocks 1/4 failures: cslib)
**Estimated Effort**: 4-6 hours
**Files to Modify**:
- `DocVerificationBridge/Main.lean`
- `DocVerificationBridge/DocVerificationBridge/Unified.lean` (optional)

**Problem**:
Main.lean unconditionally imports `DocVerificationBridge.Unified`, but the temporary workaround excludes SourceLinkerCompat.lean from Lean 4.29.0+ builds, causing "bad import" errors.

**Solution Option A - Create Separate Modules**:
```lean
-- Main.lean
#if DECORATOR_SUPPORT
import DocVerificationBridge.Unified
#else
import DocVerificationBridge.UnifiedBasic
#end

def main : IO Unit := do
  #if DECORATOR_SUPPORT
  Unified.run
  #else
  UnifiedBasic.run
  #end
```

Create `UnifiedBasic.lean` that doesn't depend on SourceLinkerCompat.

**Solution Option B - Graceful Feature Detection** (RECOMMENDED):
```lean
-- Unified.lean
import DocVerificationBridge.Core
-- Conditional import
#if DECORATOR_SUPPORT
import DocVerificationBridge.SourceLinkerCompat
import DocVerificationBridge.VerificationDecorator
#end

def runAnalysis (config : Config) : IO Unit := do
  let result ← analyzeProject config

  #if DECORATOR_SUPPORT
  -- Use custom decorator if available
  let files ← htmlOutputWithDecorator result config.decorator
  #else
  -- Use standard doc-gen4 output
  let files ← DocGen4.Output.htmlOutput result
  #end

  generateReport files result
```

**Recommended Approach**: Option B
- More maintainable (single code path)
- User-friendly (graceful degradation)
- Easier to test

**Implementation Steps**:
1. Wrap decorator-dependent code in `#if DECORATOR_SUPPORT`
2. Provide fallback implementations for `#else` branches
3. Set DECORATOR_SUPPORT flag in lakefile.toml based on Lean version
4. Test both paths

**Testing**:
```bash
# Test without decorator (Lean 4.29.0)
cd DocVerificationBridge
git checkout v4.29.0
lake build unified-doc  # Should succeed

# Test with decorator (Lean 4.28.0)
git checkout v4.28.0
lake build unified-doc  # Should succeed with decorator
```

**Validation**:
- [ ] Builds on v4.29.0 without SourceLinkerCompat
- [ ] Builds on v4.28.0 with SourceLinkerCompat
- [ ] cslib experiment passes
- [ ] Generated output is valid (may lack decorator features)

---

### 🟡 MEDIUM: Update 3 - Handle RC Version Mismatches

**Priority**: MEDIUM (Blocks 1/4 failures: batteries)
**Estimated Effort**: 2-3 hours
**Files to Modify**:
- `Experiments/Experiments/ExperimentsCore.lean` (around line 700-800)

**Problem**:
batteries requires doc-gen4 v4.29.0-rc4, but only v4.29.0-rc3 exists. Current code fails with "revision not found".

**Solution**:
Implement smart fallback logic for RC versions:

```lean
def findBestDocGen4Version (targetVersion : String) : IO String := do
  let repo := "https://github.com/leanprover/doc-gen4"

  -- Try exact match first
  if ← gitTagExists repo targetVersion then
    return targetVersion

  -- Handle RC version fallback for 4.29.0+
  if let some (major, minor, patch, rcNum) := parseRCVersion targetVersion then
    if major ≥ 4 && minor ≥ 29 then
      -- Try progressively older RCs
      for i in [rcNum - 1 : 1 : -1] do
        let candidate := s!"v{major}.{minor}.{patch}-rc{i}"
        if ← gitTagExists repo candidate then
          IO.println s!"⚠️  Warning: {targetVersion} not found, using {candidate}"
          return candidate

      -- For Lean 4.29.0+, fall back to main branch
      IO.println s!"⚠️  Warning: No matching RC found, using main branch"
      return "main"

  -- For older versions or stable releases, fail with clear error
  throw <| IO.userError s!"doc-gen4 version {targetVersion} not found"

-- Helper function
def parseRCVersion (v : String) : Option (Nat × Nat × Nat × Nat) := do
  -- Parse "v4.29.0-rc4" → (4, 29, 0, 4)
  sorry -- Implementation details
```

**Alternative Approach** (Simpler):
For v4.29.0 RC versions, always use the highest available RC or main:

```lean
def normalizeDocGen4Version (v : String) : IO String := do
  if v.startsWith "v4.29.0-rc" then
    -- Always use rc3 (known to exist) for any 4.29.0-rc request
    return "v4.29.0-rc3"
  else
    return v
```

**Implementation Steps**:
1. Add version fallback logic before cloning doc-gen4
2. Log warnings when falling back
3. Test with batteries project
4. Document fallback behavior

**Testing**:
```bash
cd experiments
./run.sh run --projects batteries
# Should see warning about RC fallback, then proceed
```

**Validation**:
- [ ] batteries experiment passes with RC fallback
- [ ] Warning message logged
- [ ] Other projects unaffected

---

### 🟢 LOW: Update 4 - Fix contracts Package Resolution

**Priority**: LOW (Blocks 1/4 failures: contracts, but likely stale cache)
**Estimated Effort**: 1 hour
**Files to Modify**: None (cleaning/documentation only)

**Problem**:
Module disambiguation error for `DocVerificationBridge.Compatibility` suggests stale cache or duplicate package definitions.

**Solution**:
Add cleanup procedure to experiments runner:

```bash
# Add to experiments/run.sh or create separate script
function cleanProject() {
  local project=$1
  echo "Cleaning $project..."
  rm -rf "repos/$project/docvb/.lake"
  rm -rf "repos/$project/docvb/lake-packages"
  rm -f "repos/$project/docvb/lake-manifest.json"
}

# Or add --clean flag to run.sh
./run.sh run --clean --projects contracts
```

**Implementation Steps**:
1. Add `--clean` flag to experiments runner
2. Document cleanup procedure in experiments/README.md
3. Test contracts with clean build
4. If still fails, investigate lakefile.toml

**Testing**:
```bash
cd experiments
./run.sh run --clean --projects contracts
```

**Validation**:
- [ ] contracts experiment passes
- [ ] No module disambiguation errors
- [ ] Cleanup procedure documented

---

## Implementation Plan

### Phase 1: Foundation (Day 1-2)
1. **Research doc-gen4 DB API** (4-6 hours)
   - Study v4.29.0-rc3 source code
   - Document DB schema and insertion functions
   - Create test harness

2. **Implement DB-based API** (8-12 hours)
   - Write htmlOutputResultsCompat429
   - Test with simple project
   - Verify output correctness

### Phase 2: Integration (Day 3)
3. **Fix Conditional Import** (4-6 hours)
   - Implement Option B (graceful feature detection)
   - Update Main.lean
   - Test both paths

4. **Handle RC Mismatches** (2-3 hours)
   - Add version fallback logic
   - Test with batteries
   - Document behavior

### Phase 3: Validation (Day 4)
5. **Run Full Experiment Suite** (2-3 hours)
   - Test all 5 projects
   - Document results
   - Fix any issues

6. **Documentation & Cleanup** (2-3 hours)
   - Update STATUS.md
   - Update LEAN-4.29-STATUS.md
   - Document workarounds

### Phase 4: Release (Day 5)
7. **Merge and Deploy** (2-3 hours)
   - Merge v4.29.0 branch to internal
   - Tag release
   - Update documentation

---

## Success Criteria

### Minimum Success (80% - 4/5 projects)
- [ ] lean4-yaml-verified: ✅ (already passing)
- [ ] cslib: ✅ (Update 1 & 2)
- [ ] mathlib4: ✅ (Update 1)
- [ ] batteries: ✅ (Update 1 & 3)
- [ ] contracts: ❓ (may need investigation)

### Full Success (100% - 5/5 projects)
- [ ] All above passing
- [ ] contracts: ✅ (Update 4 or investigation)

### Quality Metrics
- [ ] No compiler warnings
- [ ] Backward compatibility maintained (4.24.0 - 4.28.0)
- [ ] Forward compatibility achieved (4.29.0+)
- [ ] Comprehensive test coverage
- [ ] Documentation complete

---

## Risk Assessment

### High Risk Items
1. **doc-gen4 DB API complexity** - May take longer than estimated
   - **Mitigation**: Start early, allocate buffer time
   - **Contingency**: Contact doc-gen4 maintainers for guidance

2. **Backward compatibility** - Changes may break older versions
   - **Mitigation**: Test all supported versions
   - **Contingency**: Use version-specific code paths

### Medium Risk Items
3. **contracts investigation** - Issue may be deeper than cache
   - **Mitigation**: Budget extra time for investigation
   - **Contingency**: Document as known issue, fix later

### Low Risk Items
4. **RC version handling** - Straightforward logic
5. **Conditional import** - Well-understood pattern

---

## Resource Requirements

### Time
- **Estimated Total**: 4-5 days
- **Critical Path**: Updates 1 & 2 (blocking)
- **Buffer**: +1 day for unknowns

### Expertise
- **Lean 4 programming**: Intermediate to advanced
- **SQLite**: Basic (for DB API)
- **doc-gen4 internals**: Need to learn
- **Git/version control**: Basic

### Tools
- Lean 4 toolchain (4.24.0 - 4.29.0)
- Lake build system
- SQLite3 (for testing DB operations)
- doc-gen4 source code (for reference)

---

## Next Steps

### Immediate (Today)
1. Read this document thoroughly
2. Review LEAN-4.29-STATUS.md for context
3. Set up development environment
4. Begin Research phase (doc-gen4 DB API)

### This Week
5. Implement Updates 1 & 2 (critical path)
6. Test with cslib and mathlib4
7. Implement Update 3 (batteries)
8. Run full experiment suite

### Next Week
9. Fix any remaining issues
10. Update all documentation
11. Prepare release notes
12. Merge to main branch

---

## Appendix: Quick Reference

### Key Files

**Core Implementation**:
- `DocVerificationBridge/DocVerificationBridge/SourceLinkerCompat.lean`
- `DocVerificationBridge/Main.lean`
- `DocVerificationBridge/DocVerificationBridge/Unified.lean`

**Experiments Runner**:
- `Experiments/Experiments/ExperimentsCore.lean`
- `experiments/run.sh`
- `experiments/config.toml`

**Documentation**:
- `STATUS.md` - Overall project status
- `LEAN-4.29-STATUS.md` - Detailed 4.29.0 analysis
- `experiments/EXPERIMENT-ANALYSIS.md` - This analysis
- `REQUIRED-UPDATES.md` - This document

### Commands

**Build DocVerificationBridge**:
```bash
cd DocVerificationBridge
lake build unified-doc
```

**Run Experiments**:
```bash
cd experiments
./run.sh run --projects <project-name>
./run.sh run  # All projects
```

**Switch Versions**:
```bash
git checkout v4.29.0  # For Lean 4.29.0 work
git checkout v4.28.0  # For Lean 4.28.0 work
git checkout internal # For latest development
```

**Clean Build**:
```bash
lake clean
rm -rf .lake lake-packages
```

### Contact/Resources

- **doc-gen4 Repository**: https://github.com/leanprover/doc-gen4
- **Lean 4 Repository**: https://github.com/leanprover/lean4
- **Lean Zulip**: https://leanprover.zulipchat.com/

---

**Document Status**: ✅ Complete
**Last Updated**: 2026-03-04
**Next Review**: After implementing Update 1 & 2
