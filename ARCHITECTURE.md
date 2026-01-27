# doc-verification-bridge: Architecture Documentation

## Overview

`doc-verification-bridge` is a Lean 4 documentation tool that automatically classifies theorems and definitions according to their role in bridging mathematical specifications and computational implementations. It extends [doc-gen4](https://github.com/leanprover/doc-gen4) with semantic classification capabilities and verification coverage tracking.

**Core Purpose:** Enable formal verification practitioners to understand at a glance which theorems prove mathematical properties, which validate computational implementations, and which bridge the gap between specifications and executable code.

## Philosophical Foundation: Four-Category Ontology

The project is grounded in E.J. Lowe's *Four-Category Ontology* (Oxford, 2006), adapted for formal verification:

|                    | **Mathematical** (Universal, Prop) | **Computational** (Particular, Data) |
|--------------------|-------------------------------------|--------------------------------------|
| **Substantial**    | Mathematical Abstractions (Kinds)   | Computational Datatypes (Objects)    |
| **Non-substantial**| Mathematical Definitions (Attributes) | Computational Operations (Modes)   |

This creates four fundamental categories:
- **Mathematical Abstractions**: Abstract types, type classes, Prop-based structures
- **Computational Datatypes**: Concrete data structures with computational representation
- **Mathematical Definitions**: Prop-returning predicates, relations, properties
- **Computational Operations**: Computable functions, Bool predicates, algorithms

**Bridging theorems** prove the relations between these levels, enabling verification that computational implementations correctly realize their mathematical specifications.

---

## Project Structure

```
doc-verification-bridge/
├── lakefile.lean                    # Lake build configuration
├── lean-toolchain                   # Lean version specification
├── DocVerificationBridge.lean       # Main library entry point
├── UnifiedMain.lean                 # Unified CLI executable
├── ExperimentsMain.lean            # Batch analysis executable
│
├── DocVerificationBridge/          # Core library modules
│   ├── Types.lean                  # Core type definitions
│   ├── Attributes.lean             # Annotation syntax (@[api_*])
│   ├── Inference.lean              # Automatic assumes/proves/validates inference
│   ├── Classify.lean               # Declaration classification engine
│   ├── Report.lean                 # Markdown report generation (legacy)
│   ├── Cache.lean                  # Classification cache (JSON/JSONL)
│   ├── Unified.lean                # Unified pipeline orchestration
│   ├── StaticHtml.lean             # Static HTML generation
│   ├── VerificationDecorator.lean  # Doc-gen4 badge integration
│   ├── Experiments.lean            # Batch experiment runner
│   ├── Compatibility.lean          # Cross-version compatibility layer
│   └── SourceLinker*.lean          # Source link generators
│
├── experiments/                    # Batch analysis framework
│   ├── config.toml                # Project configurations
│   └── sites/                     # Generated documentation sites
│
├── scripts/                        # Utility scripts
│   ├── sandbox-lake.sh            # Isolated Lake environment
│   └── test-compatibility.sh      # Compatibility testing
│
└── paper/                          # NFM 2026 paper submission
```

---

## Architecture: Core Components

### 1. Type System (`Types.lean`)

Defines the fundamental data structures for classification:

#### Category Types
- **`TypeCategory`**: Classifies `structure`, `inductive`, `class` declarations
  - `.mathematicalAbstraction`: Prop-based types, type classes
  - `.computationalDatatype`: Concrete data structures

- **`DefCategory`**: Classifies `def` declarations (auto-detected from return type)
  - `.mathematicalDefinition`: Returns `Prop` (predicates, relations)
  - `.computationalOperation`: Returns non-`Prop` (algorithms, accessors)

- **`TheoremKind`**: Classifies `theorem`/`lemma` declarations
  - `.mathematicalProperty`: Proves abstract properties within Prop layer
  - `.computationalProperty`: Proves algebraic laws (BEq, Hashable, etc.)
  - `.bridgingProperty`: Links Bool computations ↔ Prop specifications
  - `.soundnessProperty`: Proves user type embeds into external type
  - `.completenessProperty`: Proves external type representable by user type

#### Metadata Structures
- **`APIMeta`**: Complete metadata for a declaration
  - `kind: DeclKind` (type/def/theorem)
  - `module: Name` (source module)
  - `coverage: CoverageStatus` (verification status)

- **`TheoremData`**: Theorem-specific metadata
  - `assumes`: Preconditions (hypothesis names)
  - `proves`: Conclusions (what the theorem establishes)
  - `validates`: Bool functions validated (bridging theorems only)
  - `dependsOn`: Other theorems used in proof
  - `axiomDeps`: Axioms the proof relies on

#### Coverage Tracking
- **`CoverageStatus`**: Tracks verification state
  - `.complete`: Fully verified, no axioms
  - `.axiomDependent`: Verified but uses axioms
  - `.partialCoverage`: Some properties verified
  - `.unverified`: No verification yet

### 2. Inference Engine (`Inference.lean`)

Automatically infers `assumes`, `proves`, and `validates` relationships by analyzing theorem types.

#### Algorithm

Given a theorem type like:
```lean
∀ (g : Graph) (h₁ : IsAcyclic g) (h₂ : IsConnected g), NoSelfLoop g ∧ IsTree g
```

The inference engine:

1. **Hypothesis Analysis → `assumes`**
   - Collects Prop-typed parameters whose head constant is a known definition
   - Example: `IsAcyclic`, `IsConnected`

2. **Conclusion Analysis → `proves`**
   - Extracts head constants from the conclusion
   - For compound conclusions (`∧`, `∨`, `↔`), recursively extracts all head constants
   - Example: `NoSelfLoop`, `IsTree`

3. **Bool Pattern Detection → `validates`**
   - Identifies `BoolFunc x = true/false` patterns
   - Indicates bridging between Bool computations and Prop specifications

4. **Proof Term Traversal → `dependsOn`**
   - Analyzes proof term to find applied theorems/lemmas
   - Creates dependency graph showing which foundational lemmas are used

#### Filtering Logic
- **Excluded Names**: Logical connectives (`And`, `Or`, `Not`, `Iff`, etc.)
- **Auxiliary Definitions**: Compiler-generated helpers (`.rec`, `.match_`, etc.)
- **Internal vs External**: Distinguishes project-local names from dependencies

### 3. Classification Engine (`Classify.lean`)

Orchestrates the automatic classification process using doc-gen4's infrastructure.

#### Two-Phase Strategy

**Phase 1: Lightweight Classification (Sequential in MetaM)**
- Enumerate all declarations from doc-gen4's analysis
- Classify types based on `isProp` check
- Classify definitions by return type
- Infer theorem kinds using `inferTheoremAnnotations`
- Check for `sorry` usage
- Collect tasks for proof dependency extraction

**Phase 2: Proof Dependency Extraction (Parallel in IO)**
- Process proof terms to find theorem dependencies
- Can run sequentially or with worker threads (`--proof-dep-workers N`)
- Supports blacklisting slow theorems (`--proof-dep-blacklist`)
- Optional: skip entirely for faster analysis (`--skip-proof-deps`)

#### Blacklist Filtering
Excludes from reports:
- Internal names (compiler-generated)
- Names without source locations
- Auxiliary recursors, matchers, `noConfusion`
- Private definitions (`_private`, `_root_`)

### 4. Annotation System (`Attributes.lean`)

Provides optional manual control for precise classification when automatic inference is insufficient.

#### Annotation Attributes

**`@[api_type]`** — For types (category must be specified):
```lean
@[api_type { category := .mathematicalAbstraction }]
structure PackageRegistrySpec where ...
```

**`@[api_def]`** — For definitions (category auto-detected):
```lean
@[api_def]  -- Returns Prop → mathematicalDefinition
def IsAcyclic (g : Graph) : Prop := ...
```

**`@[api_theorem]` / `@[api_lemma]`** — For proofs:
```lean
@[api_theorem {
  theoremKind := .bridgingProperty,
  bridgingDirection := .sound,
  proves := #[`IsPositive],
  validates := #[`isPositiveBool]
}]
theorem isPositiveBool_sound : isPositiveBool n = true → IsPositive n := ...
```

#### Validation Rules
The annotation system validates at compile time:
- **ACE1-ACE19**: Errors for invalid configurations
- **ACW9-ACW19**: Warnings for potential improvements
- Warnings can be suppressed: `suppress := #["ACW15"]`

### 5. Unified Pipeline (`Unified.lean`)

Orchestrates the combined doc-gen4 + verification documentation generation.

#### Pipeline Stages

```
[1/7] Load Modules
  ↓ Use doc-gen4's envOfImports
  ↓ Process with DocGen4.Process.process
  
[2/7] Build Git File Cache
  ↓ Scan source directory for .lean files
  ↓ Enable accurate source links
  
[3/7] Classification
  ↓ Auto mode: Heuristic-based inference
  ↓ Annotated mode: Explicit @[api_*] only
  ↓ Optional: Load/save cache for large projects
  
[4/7] Generate doc-gen4 API docs (temp location)
  ↓ Standard doc-gen4 HTML generation
  
[5/7] Generate Static HTML Site
  ↓ Index page with verification stats
  ↓ Per-module verification reports
  ↓ Badge decoration system
  
[6/7] Copy API docs into site
  ↓ Place under /api/ subdirectory
  
[7/7] Create Dependency Stubs
  ↓ Stub pages for transitive dependencies
```

#### Configuration Options

**`UnifiedConfig`** supports:
- **Source Links**: `repoUrl`, `platform` (GitHub/GitLab), `branch`
- **Performance**: `proofDepWorkers`, `htmlWorkers`, `skipProofDeps`
- **Caching**: `loadClassificationCache`, `saveClassificationCache`
- **Project Metadata**: `projectName`, `projectDescription`, `projectModules`
- **External Docs**: Maps dependency modules to external doc URLs

### 6. Static HTML Generation (`StaticHtml.lean`)

Generates Python-free static HTML documentation.

#### Output Structure
```
site/
├── index.html              # Home page with project overview
├── modules/
│   ├── index.html         # Module list with stats
│   └── Module.Name.html   # Per-module verification report
└── api/                   # doc-gen4 API documentation (copied)
    ├── index.html
    ├── declarations/
    └── Module.Name.html   # Enhanced with verification badges
```

#### Badge System (`VerificationDecorator.lean`)

Integrates with doc-gen4 via `DeclarationDecoratorFn` hook (PR #344):
- **Bidirectional Navigation**: Badges link API docs ↔ verification reports
- **Visual Classification**: Emoji badges indicate category
  - 🔷 Mathematical abstraction
  - 🔶 Computational datatype
  - 🔹 Mathematical definition
  - 🔸 Computational operation
  - 📐 Mathematical property
  - ⚙️ Computational property
  - ⬇️ Soundness theorem
  - ⬆️ Completeness theorem
  - ⇕ Equivalence (iff) theorem

#### Parallel HTML Generation
- Sequential by default
- `--html-workers N` enables parallel file writing
- Useful for large projects (10,000+ declarations)

### 7. Classification Cache (`Cache.lean`)

Enables fast iteration on large projects by caching classification results.

#### Cache Format

**Split-file design for streaming I/O:**
- **`<path>.json`**: Small metadata file (version, entry count)
- **`<path>.jsonl`**: Pure JSON Lines format (one entry per line)

This avoids stack overflow when serializing/deserializing 280K+ entries (e.g., mathlib4).

#### Usage Pattern
```bash
# First run: Generate and save cache
lake exe unified-doc unified --auto \
  --save-classification /tmp/cache \
  --output docs Mathlib

# Subsequent runs: Load from cache (fast iteration)
lake exe unified-doc unified --auto \
  --load-classification /tmp/cache \
  --output docs Mathlib
```

### 8. Batch Experiments (`Experiments.lean`)

Automated pipeline for analyzing multiple Lean 4 projects in parallel.

#### Configuration (`experiments/config.toml`)

```toml
[settings]
repos_dir = "./repos"
sites_dir = "./sites"
max_parallel_jobs = 4
proof_dep_workers = 50
html_workers = 20

[[projects]]
name = "batteries"
repo = "https://github.com/leanprover-community/batteries"
modules = ["Batteries"]
classification_mode = "auto"
```

#### Run Modes
- **`fresh`**: Process all from scratch
- **`resume`**: Skip completed, restart incomplete
- **`reanalyze`**: Skip build, only re-run analysis
- **`reclassify`**: Skip build & doc-gen4, only re-classify
- **`htmlOnly`**: Skip classification, only regenerate HTML

#### Workflow
1. Clone repositories to `repos_dir`
2. Create nested `docbuild` directories (doc-gen4 pattern)
3. Run `lake build` on each project
4. Execute `unified-doc` with project-specific settings
5. Generate meta-summary page with sortable statistics

---

## Data Flow

### Automatic Classification Mode (`--auto`)

```
Source Code (.lean files)
    ↓
doc-gen4 Environment Loading
    ↓
DocGen4.Process.process → AnalyzerResult
    ↓
Classification Engine (Classify.lean)
    ├─→ Types: isProp check → TypeCategory
    ├─→ Defs: return type → DefCategory  
    └─→ Theorems: Inference.lean
         ├─→ Type analysis → assumes/proves/validates
         ├─→ Pattern matching → TheoremKind
         └─→ Proof term traversal → dependsOn
    ↓
NameMap APIMeta (classification results)
    ↓
Static HTML Generation
    ├─→ Index page (project overview)
    ├─→ Module pages (per-module reports)
    └─→ Badge decoration (doc-gen4 integration)
```

### Annotated Mode (`--annotated`)

```
Source Code with @[api_*] attributes
    ↓
Attribute Registration (compile-time)
    ↓ Environment Extensions
    ↓
Classification: Extract annotations
    ↓ getApiTypeAttr, getApiDefAttr, getApiTheoremAttr
    ↓
NameMap APIMeta (only annotated declarations)
    ↓
Static HTML Generation
```

---

## Two Operating Modes

### Mode 1: Automatic Classification (`--auto`)
- **Zero annotations required**: Works on any Lean 4 codebase
- **Heuristic-based**: Analyzes type signatures and proof structure
- **Best for**: Quick overview, existing codebases, exploration

### Mode 2: Annotated Classification (`--annotated`)
- **Explicit annotations**: Uses `@[api_type]`, `@[api_def]`, `@[api_theorem]`
- **Exact control**: Precise classification as specified by author
- **Best for**: Production documentation, precise control, published work

---

## Integration with doc-gen4

doc-verification-bridge extends doc-gen4 through:

1. **Shared Module Loading**: Reuses `DocGen4.envOfImports` and `DocGen4.Process.process`
2. **Badge Decoration**: Custom `DeclarationDecoratorFn` hook (PR #344)
3. **Directory Structure**: Copies doc-gen4 output under `/api/`
4. **Source Links**: Compatible with doc-gen4's source URL system

**Key Difference from doc-gen4:**
- doc-gen4: Generates API reference (types, signatures)
- doc-verification-bridge: Adds semantic classification (ontological categories, verification coverage)

---

## Performance Optimizations

### For Large Projects (e.g., mathlib4)

#### Parallel Proof Dependency Extraction
```bash
--proof-dep-workers 50  # Use up to 50 worker threads
```
- Phase 1 (MetaM): Sequential type analysis
- Phase 2 (IO): Parallel proof term traversal
- Speedup: ~10-50x for projects with complex proofs

#### Skip Proof Dependencies Entirely
```bash
--skip-proof-deps  # Fastest, but no dependsOn data
```

#### Parallel HTML Generation
```bash
--html-workers 20  # Use 20 parallel workers for file writing
```

#### Classification Caching
```bash
# Save cache
--save-classification /tmp/cache

# Load cache (skip classification step)
--load-classification /tmp/cache
```

#### Blacklist Slow Theorems
```bash
--proof-dep-blacklist "SlowTheorem1,SlowTheorem2"
```

---

## CLI Executables

### 1. `unified-doc` (UnifiedMain.lean)

Main executable with three subcommands:

#### `unified` — Combined doc-gen4 + verification (recommended)
```bash
lake exe unified-doc unified --auto \
  --repo https://github.com/org/repo \
  --project "Project Name" \
  --output site \
  MyProject.Core
```

#### `docgen4` — Pure doc-gen4 HTML
```bash
lake exe unified-doc docgen4 --output docs MyProject
```

#### `verify` — Verification report only
```bash
lake exe unified-doc verify --auto --output docs MyProject
```

### 2. `experiments` (ExperimentsMain.lean)

Batch analysis runner:
```bash
lake exe experiments run --mode fresh
lake exe experiments run --mode resume
lake exe experiments summary  # Generate meta-summary page
```

---

## Extension Points

### Adding New Theorem Kinds

1. Extend `TheoremKind` enum in `Types.lean`
2. Add pattern detection in `Inference.suggestTheoremKind`
3. Update HTML badge in `StaticHtml.emitBadgeHtml`
4. Add validation rules in `Attributes.lean`

### Custom Report Formats

Implement alternative generators alongside `StaticHtml.lean`:
- Export classification as JSON: Already supported via `Cache.lean`
- Generate LaTeX: Process `NameMap APIMeta` entries
- Create GraphViz dependency graphs: Use `dependsOn` fields

### Integration with Other Tools

**Input**: Classification cache (`.jsonl` format)
```json
{"name":"MyTheorem","kind":"mathematicalProperty","proves":["MyDef"]}
```

**Output**: doc-gen4-compatible HTML with badge hooks

---

## Design Principles

### 1. Minimal Disruption
- **No source modifications required** in automatic mode
- **Opt-in annotations** for precise control
- **Compatible with doc-gen4** workflow

### 2. Scalability
- **Parallel processing** for large codebases
- **Caching** for fast iteration
- **Streaming I/O** for memory efficiency

### 3. Correctness
- **Pure Lean 4 implementation** (no external scripts for core logic)
- **Type-safe classification** with validated enums
- **Compile-time validation** for annotations

### 4. Extensibility
- **Modular architecture** (clear separation of concerns)
- **Standard formats** (JSON Lines for interoperability)
- **Hook-based integration** with doc-gen4

---

## Dependencies

### Required
- **Lean 4**: v4.28.0-rc1 (specified in `lean-toolchain`)
- **doc-gen4**: Main branch (for HTML generation and analysis infrastructure)
- **Cli**: lean4-cli (for command-line interface)

### Optional
- **Python 3**: Only for serving documentation locally (`python3 -m http.server`)

---

## Compatibility

### Cross-Version Support (`Compatibility.lean`)

Provides compatibility layer for:
- String trimming (`trimCompat`)
- File system operations
- Environment extensions

### Testing (`scripts/test-compatibility.sh`)

Validates compatibility across Lean 4 versions.

---

## Future Directions

### Planned Enhancements
1. **Interactive proof explorer**: Navigate dependsOn graph visually
2. **Coverage metrics**: Quantitative verification completeness scores
3. **Refinement suggestions**: Automated detection of missing bridging theorems
4. **IDE integration**: VS Code extension for in-editor classification

### Research Applications
- **Formalization patterns**: Analyze common verification structures
- **Proof reuse**: Identify widely-used foundational lemmas
- **Ontological analysis**: Study distribution of theorem kinds across projects

---

## References

- **Four-Category Ontology**: E.J. Lowe, *The Four-Category Ontology* (Oxford, 2006)
- **doc-gen4**: https://github.com/leanprover/doc-gen4
- **NFM 2026 Paper**: See `paper/doc_verification_bridge_NFM2026.pdf`
- **Repository**: https://github.com/NicolasRouquette/doc-verification-bridge

---

## License

Apache 2.0
