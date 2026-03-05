# Experiment Results Analysis

**Generated**: 2026-03-04
**Location**: `/home/nfr/projects/tlr/lean/doc-verification-bridge.internal/experiments/sites/`

## Executive Summary

**Success Rate**: 1/5 projects (20%)
**Projects Tested**: batteries, contracts, cslib, lean4-yaml-verified, mathlib4
**Successful**: lean4-yaml-verified
**Failed**: batteries, contracts, cslib, mathlib4

### Key Finding
All failures are related to **Lean 4.29.0 API incompatibility**. The doc-verification-bridge project requires updates to support the breaking changes introduced in doc-gen4 v4.29.0-rc3+.

## Detailed Results

### ✅ Successful: lean4-yaml-verified
- **Status**: COMPLETED
- **Definitions**: 644 total (0 with sorry)
- **Theorems**: 634 total (0 with sorry)
- **Bridging Theorems**: 208 (32% of theorems)
- **Unclassified**: 51 (8% of theorems)
- **Generated Site**: Successfully created with full classification

**Ontology Breakdown**:
- Mathematical Abstractions (Prop + Types): 0
- Computational Datatypes (Data + Types): 42
- Mathematical Definitions (Prop + Defs): 20 (3%)
- Computational Operations (Data + Defs): 624 (96%)

**Theorem Classification**:
- Mathematical Properties: 375 (59%)
- Bridging Properties: 208 (32%)
- Computational Properties: 0
- Soundness Properties: 0
- Completeness Properties: 0
- Unclassified: 51 (8%)

### ❌ Failed: batteries
- **Error**: `lake update failed`
- **Root Cause**: doc-gen4 revision not found for v4.29.0-rc4
- **Lean Version**: v4.29.0-rc4
- **DocVB Version**: v4.29.0-rc3 (compatible range)

**Error Details**:
```
error: /home/nfr/projects/tlr/lean/doc-verification-bridge.internal/experiments/repos/batteries/docvb/../.lake/packages/doc-gen4: revision not found 'v4.29.0-rc4'
```

**Analysis**: The project requires doc-gen4 v4.29.0-rc4, but only v4.29.0-rc3 exists in the doc-gen4 repository. The experiments framework needs to handle RC version mismatches more gracefully.

### ❌ Failed: mathlib4
- **Error**: `docvb build failed`
- **Root Cause**: Unknown identifier `DocGen4.htmlOutputResults`
- **Lean Version**: v4.29.0-rc4
- **DocVB Version**: v4.29.0-rc3 (compatible range)

**Error Details**:
```
error: DocVerificationBridge/Unified.lean:230:13: Unknown identifier `DocGen4.htmlOutputResults`
error: DocVerificationBridge/Unified.lean:231:2: Type mismatch
  htmlOutputIndex baseConfig
has type
  Array JsonModule → Array (TacticInfo Html) → IO Unit
but is expected to have type
  IO PUnit
```

**Analysis**: This is the **core API incompatibility** identified in LEAN-4.29-STATUS.md. The doc-gen4 v4.29.0 API changed from `htmlOutputResults` to `htmlOutputResultsParallel` with a completely different signature requiring a SQLite database backend.

**Build Progress**: Built 206/211 jobs before failure, showing that most dependencies compiled successfully.

### ❌ Failed: cslib
- **Error**: `docvb build failed`
- **Root Cause**: Missing file `DocVerificationBridge/Unified.lean`
- **Lean Version**: v4.29.0-rc3
- **DocVB Version**: unknown (from cache)

**Error Details**:
```
error: no such file or directory (error code: 4294967294)
  file: /home/nfr/projects/tlr/lean/doc-verification-bridge.internal/experiments/repos/cslib/docvb/DocVerificationBridge/Unified.lean
error: Main.lean: bad import 'DocVerificationBridge.Unified'
```

**Analysis**: The `useDecoratorSupport` flag disabled SourceLinkerCompat.lean for Lean >= 4.29.0, but Main.lean still unconditionally imports DocVerificationBridge.Unified, which depends on the excluded module. This is the **conditional import issue** identified in LEAN-4.29-STATUS.md.

**Build Progress**: Built 140/140 dependencies successfully, failure only in DocVB-specific modules.

### ❌ Failed: contracts
- **Error**: `docvb build failed`
- **Root Cause**: Module disambiguation failure
- **Lean Version**: v4.28.0
- **DocVB Version**: v4.28.0 (exact match)

**Error Details**:
```
error: DocVerificationBridge/Types.lean: could not disambiguate the module `DocVerificationBridge.Compatibility`; multiple packages provide distinct definitions:
  docvb (hash: 148ba428749c5611)
  doc-verification-bridge (hash: a178e310187980ec)
```

**Analysis**: This is a **package resolution conflict**. The contracts project's docvb directory has a conflicting module structure where `DocVerificationBridge.Compatibility` exists in two different packages with different hashes. This suggests the docvb setup for this project may have stale or duplicate package definitions.

**Build Progress**: Built 169/174 jobs, failing only on DocVerificationBridge.Types due to the import ambiguity.

## Root Cause Analysis

### Primary Issue: Lean 4.29.0 API Breaking Changes

**Affected Projects**: batteries, cslib, mathlib4 (3/4 failures)

The doc-gen4 library underwent significant API changes in v4.29.0-rc3:

1. **Function renamed**: `htmlOutputResults` → `htmlOutputResultsParallel`
2. **Database requirement**: New API requires a pre-populated SQLite database instead of in-memory `AnalyzerResult`
3. **New context type**: Uses `LinkingContext` instead of full analyzer result
4. **Return type changed**: Now returns `(Array System.FilePath × Array JsonModule)` instead of just file paths

**Impact**: DocVerificationBridge's compatibility layer (SourceLinkerCompat.lean) needs a complete rewrite to:
- Create and populate a temporary SQLite database from AnalyzerResult
- Build LinkingContext from the analyzer result
- Handle the new return type
- Call the parallel version of the API

### Secondary Issue: Conditional Import Handling

**Affected Projects**: cslib

The temporary workaround (commit `aed31a4`) disables decorator support for Lean >= 4.29.0 by excluding SourceLinkerCompat.lean from the build. However, Main.lean unconditionally imports DocVerificationBridge.Unified, which depends on the excluded module.

**Fix Required**: Main.lean needs conditional compilation directives or a different module structure that gracefully handles missing features.

### Tertiary Issue: Package Resolution

**Affected Projects**: contracts

The contracts project has a module disambiguation problem where DocVerificationBridge.Compatibility exists in two different packages. This suggests either:
- Stale cache from previous builds
- Incorrect lakefile configuration
- Duplicate package installations

**Fix Required**: Clean docvb directory and rebuild, or fix lakefile dependencies.

## Required Updates to doc-verification-bridge

### 1. High Priority: Implement DB-Based API (for Lean 4.29.0+)

**File**: `DocVerificationBridge/DocVerificationBridge/SourceLinkerCompat.lean`

**Changes Needed**:
```lean
-- Old (Lean 4.24.0 - 4.28.0)
def htmlOutputResults
    (baseConfig : SiteBaseContext)
    (result : AnalyzerResult)
    (sourceUrl? : Option String)
    (sourceLinkerFn? : Option SourceLinkerFn)
    (declarationDecorator? : Option DeclarationDecoratorFn)
    : IO (Array System.FilePath)

-- New (Lean 4.29.0+) - needs implementation
def htmlOutputResultsCompat
    (baseConfig : SiteBaseContext)
    (result : AnalyzerResult)
    (sourceUrl? : Option String)
    (sourceLinkerFn? : Option SourceLinkerFn)
    (declarationDecorator? : Option DeclarationDecoratorFn)
    : IO (Array System.FilePath) := do
  -- 1. Create temporary SQLite database
  let dbPath := baseConfig.buildDir / "docvb-temp.db"
  let db ← DocGen4.DB.Schema.getDb dbPath

  -- 2. Populate database from AnalyzerResult
  --    (Need to understand doc-gen4's DB schema)
  for (modName, module) in result.moduleInfo do
    -- Insert module data
    ...

  -- 3. Build LinkingContext
  let linkCtx : LinkingContext := {
    name2ModIdx := result.name2ModIdx
    moduleNames := result.moduleNames
  }

  -- 4. Call new parallel API
  let (files, _) ← htmlOutputResultsParallel
    baseConfig dbPath linkCtx sourceLinkerFn? declarationDecorator?

  return files
```

**Research Needed**:
- Study doc-gen4 v4.29.0 DB schema (DocGen4.DB.Schema module)
- Understand how to convert AnalyzerResult to DB format
- Test with both RC and stable versions

### 2. High Priority: Fix Conditional Import

**File**: `DocVerificationBridge/Main.lean`

**Option A - Conditional Compilation**:
```lean
#if DECORATOR_SUPPORT
import DocVerificationBridge.Unified
#else
import DocVerificationBridge.UnifiedBasic  -- New: Unified without custom features
#end
```

**Option B - Graceful Feature Detection**:
```lean
import DocVerificationBridge.Unified  -- Modified to handle missing features

-- In Unified.lean:
#if DECORATOR_SUPPORT
def runWithDecorator ... := ...
#else
def runWithDecorator ... := runBasic ...  -- Fallback to basic mode
#end
```

**Recommended**: Option B is more maintainable and user-friendly.

### 3. Medium Priority: Handle RC Version Mismatches

**File**: `Experiments/Experiments/ExperimentsCore.lean`

**Current Behavior**: Exact version match required for doc-gen4 tags
**Issue**: v4.29.0-rc4 requested but only v4.29.0-rc3 exists

**Improvement**:
```lean
def findBestDocGen4Version (leanVersion : String) : IO String := do
  -- Try exact match first
  if ← gitTagExists repo leanVersion then
    return leanVersion

  -- Try without RC suffix for newer RCs
  if leanVersion.startsWith "v4.29.0-rc" then
    let rcNum := extractRCNumber leanVersion
    -- Try progressively older RCs
    for i in [rcNum-1:1:-1] do
      let candidate := s!"v4.29.0-rc{i}"
      if ← gitTagExists repo candidate then
        IO.println s!"Warning: Using {candidate} for requested {leanVersion}"
        return candidate
    -- Fall back to main for v4.29.0+
    return "main"

  return leanVersion
```

### 4. Low Priority: Fix contracts Package Resolution

**Scope**: Project-specific issue

**Steps**:
1. Clean contracts docvb directory
2. Rebuild from scratch
3. Verify lakefile.toml dependencies
4. Document proper cleanup procedure in experiments README

## Testing Strategy

### Phase 1: Fix Core API (v4.29.0 support)
1. Implement DB-based SourceLinkerCompat for v4.29.0 branch
2. Test with cslib (Lean 4.29.0-rc3)
3. Test with mathlib4 (Lean 4.29.0-rc4)
4. Verify backward compatibility with lean4-yaml-verified (Lean 4.28.0)

### Phase 2: Fix Conditional Import
1. Implement Option B (graceful feature detection) in Main.lean
2. Test cslib builds successfully
3. Verify all 4.29.0+ projects work

### Phase 3: Handle Version Mismatches
1. Implement smart RC version fallback
2. Test batteries with fallback to older RC
3. Document fallback behavior

### Phase 4: Comprehensive Testing
- Re-run all 5 projects
- Verify 4/5 or 5/5 success rate
- Document any remaining issues

## Estimated Effort

| Task | Priority | Effort | Dependencies |
|------|----------|--------|--------------|
| Implement DB-based API | HIGH | 2-3 days | Research doc-gen4 DB schema |
| Fix conditional import | HIGH | 4-6 hours | None |
| Handle RC mismatches | MEDIUM | 2-3 hours | None |
| Fix contracts issue | LOW | 1 hour | None |
| Testing & validation | HIGH | 1 day | All above |
| **Total** | | **4-5 days** | |

## Recommendations

### Immediate Actions
1. **Prioritize DB-based API implementation** - Blocks 3/4 failures
2. **Fix conditional import** - Quick win for cslib
3. **Document workarounds** - For users on 4.29.0+ projects

### Short-term Actions
1. Implement RC version fallback for batteries
2. Fix contracts package resolution
3. Re-run full experiment suite

### Long-term Actions
1. Set up CI/CD to catch API changes early
2. Create integration tests for each Lean version
3. Monitor doc-gen4 releases for future API changes
4. Consider upstream contribution to doc-gen4 for better API stability

## Success Metrics

**Current State**: 1/5 projects (20%)
**After Core Fixes**: Expected 4/5 projects (80%)
**After All Fixes**: Expected 5/5 projects (100%)

## Appendix: Error Reference

### Error Patterns

**Pattern 1: Missing API Function**
```
error: Unknown identifier `DocGen4.htmlOutputResults`
```
→ Lean 4.29.0 API breaking change

**Pattern 2: Missing File**
```
error: no such file or directory
  file: DocVerificationBridge/Unified.lean
```
→ Conditional compilation excluded required file

**Pattern 3: Module Disambiguation**
```
error: could not disambiguate the module `DocVerificationBridge.Compatibility`
```
→ Package resolution conflict

**Pattern 4: Missing Git Tag**
```
error: revision not found 'v4.29.0-rc4'
```
→ Version mismatch between project and available tags

### Build Statistics

| Project | Lean Version | Jobs Completed | Jobs Total | Completion % | Failure Point |
|---------|--------------|----------------|------------|--------------|---------------|
| lean4-yaml-verified | 4.28.0 | N/A | N/A | 100% | ✅ Success |
| batteries | 4.29.0-rc4 | 0 | ~100 | 0% | lake update |
| mathlib4 | 4.29.0-rc4 | 206 | 211 | 98% | DocVB build |
| cslib | 4.29.0-rc3 | 140 | 140 | 100% deps | DocVB main |
| contracts | 4.28.0 | 169 | 174 | 97% | DocVB Types |

**Key Insight**: Dependencies build successfully (97-100% completion), indicating the failures are isolated to doc-verification-bridge integration, not project-level issues.

## References

- [LEAN-4.29-STATUS.md](../LEAN-4.29-STATUS.md) - Detailed 4.29.0 API analysis
- [STATUS.md](../STATUS.md) - Overall project status
- [experiments/sites/index.html](./sites/index.html) - Generated results dashboard
- Commit `aed31a4` - Temporary workaround disabling decorator support
- Commit `bd30b0d` - Documentation of incomplete 4.29.0 support
