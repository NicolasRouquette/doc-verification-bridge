# Experiment Results Analysis

## 📊 Current Status: All Experiments Failed

**Generated**: 2026-03-04 22:37:49
**Result**: 0/5 projects analyzed successfully

## 🔍 Root Cause

All 5 projects failed with:
```
Toolchain incompatibility: project uses unknown, supported range is v4.24.0 to v4.29.0
```

### The Issue

1. **Config specifies**: `docvb_version = "v4.29.0"` (stable release)
2. **Repos actually use**: `v4.29.0-rc4` (release candidate 4)
3. **Toolchain detector**: Reports "unknown" due to version mismatch

### Verification

```bash
# Check actual toolchain in repos:
$ cat experiments/repos/cslib/lean-toolchain
leanprover/lean4:v4.29.0-rc4

$ cat experiments/repos/batteries/lean-toolchain
leanprover/lean4:v4.29.0-rc4
```

## ✅ Solutions

### Option 1: Update Config to Match Repos (Quick Fix)

Edit `experiments/config.toml`:

```toml
[[projects]]
name = "batteries"
repo = "https://github.com/leanprover-community/batteries"
modules = ["Batteries"]
description = "Standard library extensions for Lean 4"
docvb_version = "v4.29.0-rc4"  # Changed from v4.29.0

[[projects]]
name = "cslib"
repo = "https://github.com/leanprover/cslib"
modules = ["Cslib"]
description = "The Lean Computer Science Library (CSLib)"
docvb_version = "v4.29.0-rc4"  # Changed from v4.29.0

[[projects]]
name = "mathlib4"
repo = "https://github.com/leanprover-community/mathlib4"
modules = ["Mathlib"]
description = "Mathematical library for Lean 4"
docvb_version = "v4.29.0-rc4"  # Changed from v4.29.0
# ... rest of config
```

### Option 2: Update Repos to Stable Release

If v4.29.0 stable has been released:

```bash
cd experiments/repos/cslib
git fetch
git checkout <commit-with-v4.29.0>

# Repeat for other repos
```

### Option 3: Fix Toolchain Detection

Update the toolchain detection code to:
- Handle `-rc` versions
- Map rc versions to their base version for compatibility checking

## 🧪 Testing the Fix

After applying Option 1:

```bash
cd experiments
./run.sh run
```

Expected result: Projects should build and analyze successfully.

## 📈 Expected Successful Output

When working, you should see:
- ✅ Projects cloned
- ✅ Dependencies resolved
- ✅ Documentation generated
- ✅ Classifications computed
- ✅ Statistics like:
  - Total Definitions: ~10,000+
  - Total Theorems: ~5,000+
  - Bridging Theorems: classified
  - Four-Category Ontology breakdown
  - Theorem Taxonomy breakdown

## 🔗 Related Files

- [config.toml](experiments/config.toml) - Internal configuration
- [config.public.toml](experiments/config.public.toml) - Public configuration
- [run.sh](experiments/run.sh) - Experiment runner
- [index.html](experiments/sites/index.html) - Results dashboard
