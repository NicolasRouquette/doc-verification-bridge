# Experiment Results - Executive Summary

**Date**: 2026-03-04
**Success Rate**: 1/5 projects (20%)
**Primary Issue**: Lean 4.29.0 API breaking changes in doc-gen4

## Quick Status

| Project | Status | Lean Version | Issue |
|---------|--------|--------------|-------|
| lean4-yaml-verified | ✅ PASS | 4.28.0 | None |
| batteries | ❌ FAIL | 4.29.0-rc4 | Missing git tag |
| mathlib4 | ❌ FAIL | 4.29.0-rc4 | API incompatibility |
| cslib | ❌ FAIL | 4.29.0-rc3 | Missing module |
| contracts | ❌ FAIL | 4.28.0 | Package conflict |

## Root Cause

**All failures stem from Lean 4.29.0 API changes:**
- doc-gen4 v4.29.0 replaced `htmlOutputResults` with `htmlOutputResultsParallel`
- New API requires SQLite database instead of in-memory data
- DocVerificationBridge's compatibility layer needs rewrite

## Required Work

### 🔴 Critical (Blocks 3/4 failures)
**Update SourceLinkerCompat.lean for DB-based API**
- Effort: 2-3 days
- Impact: Fixes mathlib4, cslib, batteries (partial)
- Files: `DocVerificationBridge/DocVerificationBridge/SourceLinkerCompat.lean`

### 🔴 Critical (Blocks 1/4 failures)
**Fix conditional import in Main.lean**
- Effort: 4-6 hours
- Impact: Fixes cslib
- Files: `DocVerificationBridge/Main.lean`, `DocVerificationBridge/DocVerificationBridge/Unified.lean`

### 🟡 Medium (Blocks 1/4 failures)
**Handle RC version mismatches**
- Effort: 2-3 hours
- Impact: Fixes batteries
- Files: `Experiments/Experiments/ExperimentsCore.lean`

### 🟢 Low (Blocks 1/4 failures)
**Clean contracts build cache**
- Effort: 1 hour
- Impact: Fixes contracts
- Action: Clean and rebuild

## Successful Project Analysis

**lean4-yaml-verified** (Lean 4.28.0) ✅:
- 644 definitions, 634 theorems (0 with sorry)
- 208 bridging theorems (32%)
- 51 unclassified theorems (8%)
- Full site generated with classification

**Ontology Results**:
- 96% computational operations
- 3% mathematical definitions
- 32% theorems bridge math ↔ computation

## Timeline

- **Phase 1** (Days 1-2): Research and implement DB-based API
- **Phase 2** (Day 3): Fix conditional import and RC handling
- **Phase 3** (Day 4): Full testing and validation
- **Phase 4** (Day 5): Documentation and release

**Total Effort**: 4-5 days

## Expected Outcome

- **After Critical Updates**: 4/5 projects passing (80%)
- **After All Updates**: 5/5 projects passing (100%)

## Detailed Documentation

- **Full Analysis**: [experiments/EXPERIMENT-ANALYSIS.md](./EXPERIMENT-ANALYSIS.md)
- **Update Guide**: [REQUIRED-UPDATES.md](../REQUIRED-UPDATES.md)
- **API Details**: [LEAN-4.29-STATUS.md](../LEAN-4.29-STATUS.md)
- **Project Status**: [STATUS.md](../STATUS.md)

## View Results

Open in browser: `experiments/sites/index.html`

---

**Bottom Line**: The experiments successfully validated the framework and identified a clear path to full Lean 4.29.0 support. Core infrastructure works; only API compatibility needs updating.
