# Doc Verification Bridge

A tool for analyzing Lean 4 projects to classify theorems and track verification coverage using the Four-Category Ontology.

## Repository Structure

This repository contains **two independent Lake packages**:

```
doc-verification-bridge/
├── REFACTORING.md              # Detailed refactoring plan
├── README.md                    # This file
├── DocVerificationBridge/       # Core analysis library (version-dependent)
│   ├── lakefile.toml
│   ├── DocVerificationBridge.lean
│   ├── Main.lean               # unified-doc CLI
│   └── DocVerificationBridge/   # Core modules
│       ├── Types.lean
│       ├── Inference.lean
│       ├── Classify.lean
│       ├── Report.lean
│       ├── StaticHtml.lean
│       ├── TableData.lean
│       ├── Unified.lean
│       ├── Cache.lean
│       ├── Attributes.lean
│       ├── Compatibility.lean
│       ├── SourceLinkerCompat.lean
│       ├── VerificationDecorator.lean
│       └── ... (other modules)
└── Experiments/                 # Multi-project orchestration (version-agnostic)
    ├── lakefile.toml
    ├── Main.lean               # experiments CLI
    ├── Experiments.lean        # Library root module
    └── Experiments/
        ├── Compatibility.lean  # Local copy of compat helpers
        └── ExperimentsCore.lean # Pipeline orchestration + git clone + subprocess
```

### Why Two Packages?

DocVerificationBridge depends on Lean and doc-gen4 APIs that have **breaking changes** between versions (v4.27.0, v4.28.0, v4.29.0-rc2+). A single codebase cannot support all versions simultaneously.

**Solution**: Version-specific git branches + process isolation:
- **DocVerificationBridge** has version-specific branches (v4.27.0, v4.28.0, main)
- **Experiments** orchestrates by cloning the appropriate DocVerificationBridge version per project
- Communication happens via CLI/subprocess (not Lean imports)

## DocVerificationBridge Package

**Purpose**: Core analysis library that classifies theorems and generates verification reports.

**Dependencies**: Lean 4, doc-gen4 (version-specific)

**Executables**:
- `unified-doc`: Combined doc-gen4 + verification pipeline

### Building

```bash
cd DocVerificationBridge
lake build unified-doc
```

### Usage

```bash
# Generate documentation with auto-classification
lake exe unified-doc unified \
  --repo https://github.com/owner/repo \
  --project "MyProject" \
  --auto \
  MyModule

# Generate with explicit annotations only
lake exe unified-doc unified \
  --repo https://github.com/owner/repo \
  --annotated \
  MyModule
```

### Supported Lean Versions

- **v4.27.0 branch**: Lean 4.24.0 - 4.27.0
- **v4.28.0 branch**: Lean 4.28.0
- **main branch**: Lean 4.28.0+ (will update to 4.29.0-rc2 when stable)

## Experiments Package

**Purpose**: Orchestration module for running verification analysis across multiple projects.

**Dependencies**: Lean 4 (minimal, no doc-gen4)

**Executables**:
- `experiments`: Multi-project pipeline runner

### Building

```bash
cd Experiments
lake build experiments
```

### Usage

```bash
# Run all experiments from config
lake exe experiments --config config.toml

# Resume incomplete experiments
lake exe experiments --config config.toml --resume

# Re-run analysis only (skip build)
lake exe experiments --config config.toml --reanalyze
```

### Configuration Format

Create a `config.toml` in the `Experiments/` directory:

```toml
# Experiments configuration
repos_dir = "experiments/repos"
sites_dir = "experiments/sites"
max_parallel_jobs = 4

[[project]]
name = "my-project"
repo = "https://github.com/owner/my-project"
modules = ["MyProject"]
description = "Project description"
docvb_version = "v4.28.0"  # Which DocVerificationBridge version to use
```

The `docvb_version` field specifies which DocVerificationBridge branch/tag to use for that project. This allows mixing projects with different Lean versions in the same experiment run.

## Four-Category Ontology

DocVerificationBridge classifies declarations into four categories based on E.J. Lowe's ontology:

|                    | **Mathematical** (Universal) | **Computational** (Particular) |
|--------------------|------------------------------|--------------------------------|
| **Substantial**    | Mathematical Abstraction     | Computational Datatype         |
| **Non-substantial**| Mathematical Definition      | Computational Operation        |

**Theorem Kinds**:
- **Computational Property**: Proves computational definitions satisfy laws
- **Mathematical Property**: Proves abstract mathematical properties
- **Bridging Property**: Connects Prop specifications with Bool computations
- **Soundness Property**: Proves user types correctly embed into external specs
- **Completeness Property**: Proves external specs can be represented by user types

## Classification Modes

### Auto Mode (Default)

Uses heuristic-based type analysis to automatically infer categories:
- Types: Prop-based → Mathematical Abstraction, Data-carrying → Computational Datatype
- Definitions: Returns Prop → Mathematical Definition, Returns non-Prop → Computational Operation
- Theorems: Automatically infers `assumes`, `proves`, `validates` from type structure

```bash
lake exe unified-doc unified --auto MyModule
```

### Annotated Mode

Only classifies declarations with explicit `@[api_*]` annotations:

```lean
@[api_type .mathematicalAbstraction]
class MapLike (α : Type) where ...

@[api_def .computationalOperation]
def lookup (m : Map α β) (key : α) : Option β := ...

@[api_theorem .bridgingProperty
  (assumes := #[`wellFormed])
  (proves := #[`isCorrect])
  (validates := #[`checkInvariant])]
theorem lookup_correct : wellFormed m → ... := ...
```

```bash
lake exe unified-doc unified --annotated MyModule
```

## Development

### Running Tests

```bash
# Test DocVerificationBridge
cd DocVerificationBridge
lake build
lake exe unified-doc unified --repo https://github.com/test/project TestModule

# Test Experiments
cd Experiments
lake build
lake exe experiments --config test-config.toml
```

### Adding Support for New Lean Versions

1. Create a new git branch from main: `git checkout -b v4.XX.0`
2. Update `DocVerificationBridge/lakefile.toml` with new Lean/doc-gen4 versions
3. Fix any API breakages in the core modules
4. Test with projects using that Lean version
5. Tag the branch: `git tag v4.XX.0`
6. Update experiment configs to use `docvb_version = "v4.XX.0"` for appropriate projects

## Security

Compiling untrusted Lean 4 code executes arbitrary code at build time (tactics, macros, elaborators, `initialize` blocks). The project uses a two-phase sandboxed build via [Bubblewrap](https://github.com/containers/bubblewrap) to mitigate this:

1. **Dependency fetch phase**: Network allowed, compilation blocked
2. **Compilation phase**: Network completely disabled (blocks exfiltration), filesystem restricted

See [SECURITY.md](SECURITY.md) for the full threat model, attack vector analysis, sandboxing strategy evaluation (Docker, Firejail, Bubblewrap, VMs, Landlock), and implementation details in [`scripts/sandbox-lake.sh`](scripts/sandbox-lake.sh).

## Project Status

### Refactoring: Complete

The three-phase refactoring is complete (see [REFACTORING.md](REFACTORING.md), [PHASE1-COMPLETE.md](PHASE1-COMPLETE.md), [PHASE2-COMPLETE.md](PHASE2-COMPLETE.md), [PHASE3-PROGRESS.md](PHASE3-PROGRESS.md)):

- [x] **Phase 1**: Two-package structure, lakefiles, file moves (2026-03-03)
- [x] **Phase 2**: Per-branch cleanup — internal + v4.28.0 branches cleaned (2026-03-03)
- [x] **Phase 3**: Experiments decoupling — subprocess invocation, git clone/checkout, `docvb_version` config (2026-03-03)
  - Implementation complete; end-to-end testing pending

### Current Phase: Inference Improvements

Current work focuses on improving classification accuracy for verification coverage tracking.

### Planned: Existential Binder Type Tracing

**Problem:** When a theorem concludes with `∃ vy : ValidYaml, vy.input = ... ∧ ...`, the `collectHeadConstants` function in [Inference.lean](DocVerificationBridge/DocVerificationBridge/Inference.lean) strips the existential binder and recurses into the body only. The field projections (`.input`, `.value`) are traced into `proves`, but the binder type (`ValidYaml`) is discarded. This means the parent structure never appears in any theorem's `proves` list, so its `verifiedBy` stays empty.

**Root cause** — the `Exists` branch in `collectHeadConstants`:
```lean
| .const ``Exists _, #[_, body] =>
  lambdaTelescope body fun _ innerBody =>
    collectHeadConstants innerBody sourceDesc internalPrefixes
```

The first argument to `Exists` is the binder type (e.g., `ValidYaml`), but it is matched as `_` and ignored.

**Fix** — also extract head constants from the binder type:
```lean
| .const ``Exists _, #[binderType, body] =>
  let binderNames ← collectHeadConstantsFromTerm binderType sourceDesc
  let bodyNames ← lambdaTelescope body fun _ innerBody =>
    collectHeadConstants innerBody sourceDesc internalPrefixes
  return binderNames ++ bodyNames
```

Uses `collectHeadConstantsFromTerm` (not `collectHeadConstants`) because the binder type is a `Type`-level expression, not `Prop`. Existing filtering (`shouldFilter`, `isInternalName`) prevents stdlib types like `Nat` from leaking in.

**Impact** — for `parse_produces_valid_yaml`:
- Before: `proves = [ValidYaml.input, stripAnnotations, ValidYaml.value, ...]`
- After: `proves = [ValidYaml, ValidYaml.input, stripAnnotations, ValidYaml.value, ...]`
- `computeVerifiedByMap` then automatically populates `ValidYaml.verifiedBy`

| File | Change |
|------|--------|
| [Inference.lean](DocVerificationBridge/DocVerificationBridge/Inference.lean) | Extract `binderType` from `Exists` in `collectHeadConstants` (~5 lines) |

### Planned: Def-as-Witness Detection

**Problem:** A `def` returning a specification structure (e.g., `def scan_produces_valid_tokens : ... → ValidTokenStream`) is classified as `computationalOperation` because its return type is `Type`, not `Prop`. The bridge never populates `proves` for definitions — only theorems get inference. These "def-as-witness" patterns are invisible to verification coverage tracking.

**Heuristic** — "Structure-instance witness": A `def` is a witness if its return type (after stripping ∀-args) is an application of an internal structure/inductive classified as `computationalDatatype`.

**Solution** — add a `witnessOf` field to `DefData`:
```lean
structure DefData where
  category : DefCategory
  hasSorry : Bool
  witnessOf : Array Name := #[]  -- structures this def constructs instances of
```

New helper `extractWitnessTargets` checks whether the return type head is an internal inductive:
```lean
def extractWitnessTargets (type : Expr) (internalPrefixes : Array String) : MetaM (Array Name) := do
  let env ← getEnv
  forallTelescope type fun _params body => do
    let body ← whnf body
    match body.getAppFn with
    | .const name _ =>
      if isInternalName env internalPrefixes name then
        match env.find? name with
        | some (.inductInfo _) => return #[name]
        | _ => return #[]
      else return #[]
    | _ => return #[]
```

Then `computeVerifiedByMap` includes `witnessOf` in the reverse index alongside theorem `proves`/`validates`.

**Impact:**
- `scan_produces_valid_tokens` → `witnessOf = [ValidTokenStream]` → fills `ValidTokenStream.verifiedBy`
- `toYamlValue_nodeToValue` → `witnessOf = [NodeToValue]` → adds to `NodeToValue.verifiedBy`

| File | Change |
|------|--------|
| [Types.lean](DocVerificationBridge/DocVerificationBridge/Types.lean) | Add `witnessOf : Array Name := #[]` to `DefData` |
| [Classify.lean](DocVerificationBridge/DocVerificationBridge/Classify.lean) | Add `extractWitnessTargets`, use in `classifyConstantLight`/`classifyConstant` (~20 lines) |
| [TableData.lean](DocVerificationBridge/DocVerificationBridge/TableData.lean) | Include `witnessOf` in `computeVerifiedByMap` reverse index (~8 lines) |
| [TableData.lean](DocVerificationBridge/DocVerificationBridge/TableData.lean) | Add `witnessOf` to `DefinitionTableEntry` JSON serialization (~3 lines) |

### Previous Improvements

- **Bubblewrap sandboxing** ([SECURITY.md](SECURITY.md), [`scripts/sandbox-lake.sh`](scripts/sandbox-lake.sh))
  - Two-phase build isolation: network allowed during fetch, disabled during compilation
  - Filesystem restrictions deny access to sensitive credentials
  - Unprivileged, minimal attack surface (~2000 lines of C)

- **Three-phase refactoring** ([REFACTORING.md](REFACTORING.md))
  - Phase 1: Two-package structure (DocVerificationBridge + Experiments)
  - Phase 2: Per-branch cleanup of version-specific file variants
  - Phase 3: Experiments decoupled — git clone, subprocess invocation, `docvb_version` config

- **Fixed "Grammable" inference bug** ([Inference.lean:389-535](DocVerificationBridge/DocVerificationBridge/Inference.lean#L389-L535))
  - Theorems can now correctly appear in both `assumes` and `proves`
  - Uses separate deduplication for hypotheses vs conclusion
  - Critical for soundness/completeness proof chains

## Contributing

See [REFACTORING.md](REFACTORING.md) for the refactoring plan and contribution guidelines.

## License

Apache 2.0 (see LICENSE file)

## Authors

- Nicolas Rouquette (California Institute of Technology)
- Contributors

## Acknowledgments

This tool builds on:
- [doc-gen4](https://github.com/leanprover/doc-gen4) - Lean 4 documentation generator
- [lean4-cli](https://github.com/leanprover/lean4-cli) - CLI framework
- E.J. Lowe's *Four-Category Ontology* (Oxford, 2006) - Philosophical foundation
