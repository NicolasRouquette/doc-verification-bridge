# DocVerificationBridge Refactoring Plan

## Goal

Split the repository into two independent Lake packages to support multiple Lean versions:

1. **DocVerificationBridge** (version-dependent): Core analysis library that depends on doc-gen4 APIs
2. **Experiments** (version-agnostic): Orchestration module that runs experiments across multiple projects

## Why?

DocVerificationBridge depends on Lean and doc-gen4 APIs that have breaking changes between versions (v4.27.0, v4.28.0, v4.29.0-rc2+). A single codebase cannot support all versions simultaneously.

The solution: **process isolation** via git branches and subprocess invocation:
- DocVerificationBridge uses git branches/tags (v4.27.0, v4.28.0, main) with version-specific code
- Experiments orchestrates by cloning the appropriate DocVerificationBridge version per project
- Communication happens via CLI/filesystem (not Lean imports)

## Repository Structure

### Before (Current)
```
doc-verification-bridge.internal/
├── lakefile.lean
├── DocVerificationBridge.lean
├── UnifiedMain.lean
├── ExperimentsMain.lean
├── DocVerificationBridge/
│   ├── Experiments.lean
│   ├── Unified.lean
│   ├── UnifiedBasic.lean
│   ├── VerificationDecorator.lean
│   ├── VerificationDecoratorStandard.lean
│   ├── SourceLinkerCompat.lean
│   ├── SourceLinkerCompatCustom.lean
│   ├── SourceLinkerCompatStandard.lean
│   └── ... (all other modules)
└── ...
```

### After (Target)
```
doc-verification-bridge/
├── README.md (explains two-package structure)
├── REFACTORING.md (this document)
├── DocVerificationBridge/
│   ├── lakefile.toml
│   ├── DocVerificationBridge.lean
│   ├── Main.lean (renamed from UnifiedMain.lean)
│   └── DocVerificationBridge/
│       ├── Unified.lean (version-specific, see below)
│       ├── VerificationDecorator.lean (version-specific, see below)
│       ├── SourceLinkerCompat.lean (version-specific, see below)
│       └── ... (all other core modules)
└── Experiments/
    ├── lakefile.toml
    ├── Main.lean (renamed from ExperimentsMain.lean)
    ├── Experiments.lean (moved from DocVerificationBridge/)
    └── Config.lean (new: TOML parsing, config types)
```

## Step 1: Initial Restructuring (Current Branch)

This step creates the two-package structure on the current branch. We keep ALL existing files (including version-specific variants) for now.

### Actions:

1. **Create root directory structure:**
   ```bash
   mkdir -p DocVerificationBridge/DocVerificationBridge
   mkdir -p Experiments
   ```

2. **Move DocVerificationBridge files:**
   ```bash
   # Move all files EXCEPT Experiments.lean
   mv DocVerificationBridge/*.lean DocVerificationBridge/DocVerificationBridge/
   mv DocVerificationBridge.lean DocVerificationBridge/
   mv UnifiedMain.lean DocVerificationBridge/Main.lean
   ```

3. **Move Experiments files:**
   ```bash
   mv DocVerificationBridge/Experiments.lean Experiments/
   mv ExperimentsMain.lean Experiments/Main.lean
   ```

4. **Create lakefile.toml for DocVerificationBridge:**
   - Based on current `lakefile.lean`
   - Defines `DocVerificationBridge` library
   - Defines `unified-doc` executable
   - Depends on doc-gen4 @ v4.28.0 (for current branch)

5. **Create lakefile.toml for Experiments:**
   - Defines `Experiments` library
   - Defines `experiments` executable
   - Minimal dependencies (NO doc-gen4, NO DocVerificationBridge)
   - May depend on TOML parsing library

6. **Update imports:**
   - Experiments files should NOT import DocVerificationBridge
   - Instead, interact via CLI subprocess calls
   - May need to extract some shared types to a common format (JSON/TOML)

7. **Create root README.md:**
   - Explains two-package architecture
   - Documents version compatibility
   - Provides build instructions per package

### Files to Keep in Both Locations Temporarily:
- `Unified.lean` + `UnifiedBasic.lean` (cleaned per branch in Step 2)
- `VerificationDecorator.lean` + `VerificationDecoratorStandard.lean` (cleaned per branch in Step 2)
- `SourceLinkerCompat.lean` + `SourceLinkerCompatCustom.lean` + `SourceLinkerCompatStandard.lean` (cleaned per branch in Step 2)

## Step 2: Per-Branch Cleanup (Future Work)

After Step 1 is complete, checkout each branch and clean up version-specific files.

### Branch: `v4.27.0`

**Renames:**
- `UnifiedBasic.lean` → `Unified.lean`
- `VerificationDecoratorStandard.lean` → `VerificationDecorator.lean`
- `SourceLinkerCompatStandard.lean` → `SourceLinkerCompat.lean`

**Deletes:**
- `Unified.lean` (the v4.28.0 version)
- `VerificationDecorator.lean` (the v4.28.0 version)
- `SourceLinkerCompatCustom.lean`
- `SourceLinkerCompat.lean` (the v4.29.0 version)

**Update:**
- `lakefile.toml`: Change doc-gen4 dependency to `@ "v4.27.0"`

### Branch: `v4.28.0`

**Renames:**
- `SourceLinkerCompatCustom.lean` → `SourceLinkerCompat.lean`

**Deletes:**
- `UnifiedBasic.lean`
- `VerificationDecoratorStandard.lean`
- `SourceLinkerCompatStandard.lean`
- `SourceLinkerCompat.lean` (the v4.29.0 version)

**Keep:**
- `Unified.lean` (correct for this version)
- `VerificationDecorator.lean` (correct for this version)

**Update:**
- `lakefile.toml`: Change doc-gen4 dependency to `@ "v4.28.0"`

### Branch: `main` (v4.29.0-rc2+)

**Deletes:**
- `UnifiedBasic.lean`
- `VerificationDecoratorStandard.lean`
- `SourceLinkerCompatStandard.lean`
- `SourceLinkerCompatCustom.lean`

**Keep:**
- `Unified.lean` (correct for this version)
- `VerificationDecorator.lean` (correct for this version)
- `SourceLinkerCompat.lean` (correct for this version)

**Update:**
- `lakefile.toml`: Keep current doc-gen4 dependency (v4.28.0 for now, will update when v4.29.0 is released)

## Configuration Format Changes

### Add to `config.toml` (Experiments config):

For each project, add a `docvb_version` field specifying which DocVerificationBridge version to use:

```toml
[[project]]
name = "lean4-yaml-verified"
repo = "https://github.com/cedar-policy/lean4-yaml-verified"
modules = ["Lean4Yaml"]
description = "Verified YAML parser"
docvb_version = "v4.28.0"  # ← NEW FIELD
```

### Version Mapping Strategy:

The `docvb_version` field can be:
- A git tag: `"v4.28.0"`, `"v4.27.0"`
- A git branch: `"main"`, `"v4.28.0"`
- A git commit SHA: `"abc123..."`

The Experiments module will:
1. Read the project's `lean-toolchain` file to determine Lean version
2. Clone/checkout DocVerificationBridge at the specified `docvb_version`
3. Build that version's `unified-doc` executable
4. Run it via subprocess

## Implementation Phases

### Phase 1: Restructure (This PR)
- [x] Create REFACTORING.md plan
- [ ] Create two-package directory structure
- [ ] Create lakefiles for both packages
- [ ] Move files to new locations
- [ ] Update imports
- [ ] Create root README
- [ ] Verify builds on current branch

### Phase 2: Per-Branch Cleanup (Future PRs)
- [ ] Checkout v4.27.0 branch
  - [ ] Rename version-specific files
  - [ ] Delete unused variants
  - [ ] Update lakefile dependencies
  - [ ] Test build
- [ ] Checkout v4.28.0 branch
  - [ ] Rename version-specific files
  - [ ] Delete unused variants
  - [ ] Update lakefile dependencies
  - [ ] Test build
- [ ] Checkout main branch
  - [ ] Delete unused variants
  - [ ] Update lakefile dependencies
  - [ ] Test build

### Phase 3: Experiments Integration (Future PR)
- [ ] Add `docvb_version` field to config.toml schema
- [ ] Implement DocVerificationBridge cloning/checkout logic
- [ ] Implement subprocess-based invocation
- [ ] Update all project configs with appropriate versions
- [ ] Test end-to-end experiment pipeline

## Migration Guide

### For DocVerificationBridge Users

**Old way (single package):**
```bash
cd doc-verification-bridge.internal
lake build unified-doc
lake exe unified-doc unified MyModule
```

**New way (DocVerificationBridge package):**
```bash
cd doc-verification-bridge/DocVerificationBridge
lake build unified-doc
lake exe unified-doc unified MyModule
```

### For Experiments Users

**Old way:**
```bash
cd doc-verification-bridge.internal
lake build experiments
lake exe experiments --config experiments/config.toml
```

**New way:**
```bash
cd doc-verification-bridge/Experiments
lake build experiments
lake exe experiments --config config.toml
```

## Benefits

1. **Version Isolation**: Each Lean version gets its own DocVerificationBridge branch with compatible code
2. **Simplified Code**: No more version-specific file variants in the same branch
3. **Process Isolation**: Experiments module is version-agnostic, works across all Lean versions
4. **Maintainability**: Clear separation between core analysis (version-dependent) and orchestration (version-agnostic)
5. **Flexibility**: Easy to add support for new Lean versions by creating a new branch

## Risks & Mitigation

### Risk: Import hell during transition
**Mitigation**: Do refactoring in small, testable steps. Verify builds after each step.

### Risk: Breaking existing workflows
**Mitigation**: Document migration guide clearly. Keep old structure temporarily for backwards compatibility.

### Risk: Config format changes break existing experiments
**Mitigation**: Make `docvb_version` optional initially with sensible defaults based on project's Lean version.

## Timeline

- **Week 1**: Step 1 (Initial Restructuring) - THIS PHASE
- **Week 2**: Step 2 (Per-Branch Cleanup) - verify all branches build
- **Week 3**: Step 3 (Experiments Integration) - full end-to-end testing

## Notes

- This refactoring was motivated by the need to fix the "Grammable" inference bug, which exists in all branches
- The fix needs to be applied to all version branches (v4.27.0, v4.28.0, main)
- The two-package structure makes it easier to maintain version-specific fixes going forward
