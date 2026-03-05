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
    └── Experiments.lean        # Pipeline orchestration
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

## Project Status

### Current Phase: Refactoring (Step 1)

We are currently in **Phase 1** of the refactoring (see [REFACTORING.md](REFACTORING.md) for full plan):

- [x] Create two-package structure
- [x] Create lakefiles for both packages
- [x] Move files to new locations
- [ ] **TODO**: Refactor Experiments imports to use subprocess invocation (Phase 3)
- [ ] **TODO**: Per-branch cleanup of version-specific files (Phase 2)

**Note**: The Experiments module currently still imports DocVerificationBridge modules directly. This will be refactored in Phase 3 to use subprocess invocation, making it truly version-agnostic.

### Recent Improvements

- **Fixed "Grammable" inference bug** ([Inference.lean:389-535](DocVerificationBridge/DocVerificationBridge/Inference.lean#L389-L535))
  - Theorems can now correctly appear in both `assumes` and `proves`
  - Uses separate deduplication for hypotheses vs conclusion
  - Critical for soundness/completeness proof chains

## Contributing

See [REFACTORING.md](REFACTORING.md) for the detailed refactoring plan and contribution guidelines.

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
