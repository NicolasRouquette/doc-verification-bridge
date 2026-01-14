# doc-verification-bridge

**Linking theorems to what they assume, prove, validate, and depend on**

A doc-gen4 plugin that automatically classifies theorems by their role in bridging mathematical specifications and computational implementations.

## Overview

When writing verified software, proofs exist at different conceptual levels:

| Classification | Role |
|----------------|------|
| **Mathematical Property** | Proves within the specification layer |
| **Computational Property** | Proves within the implementation layer |
| **Bridging Property** | Connects spec ↔ impl (soundness/completeness) |
| **Soundness Property** | Shows impl → spec (embedding) |
| **Completeness Property** | Shows spec → impl (representation) |

This tool:
1. **Automatically infers** `assumes`/`proves`/`dependsOn` relationships from theorem types and proof terms
2. **Classifies** definitions by their ontological category (mathematical vs computational)
3. **Identifies** bridging theorems that connect Bool computations to Prop specifications
4. **Tracks proof dependencies** — which theorems a proof uses
5. **Generates** documentation showing verification coverage and proof relationships

## Installation

Add to your `lakefile.lean`:

```lean
require «doc-verification-bridge» from git
  "https://github.com/YOUR_ORG/doc-verification-bridge" @ "main"
```

---

## Using Annotations in Your Code

To use the `@[api_type]`, `@[api_def]`, `@[api_theorem]`, and `@[api_lemma]` attributes in your Lean files, import the attributes module:

```lean
import DocVerificationBridge.Attributes
```

This gives you access to all the annotation attributes for tracking API coverage. If you're migrating from a local `Meta.APICoverage` implementation, simply change:

```lean
-- Before
import Meta.APICoverage

-- After
import DocVerificationBridge.Attributes
```

The annotation syntax is identical. For example:

```lean
import DocVerificationBridge.Attributes

@[api_type { category := .mathematicalAbstraction, coverage := .complete }]
inductive PathWithLength {α : Type*} (r : α → α → Prop) : α → α → Nat → Prop
  | single {a b} : r a b → PathWithLength r a b 1
  | cons {a b c n} : r a b → PathWithLength r b c n → PathWithLength r a c (n + 1)

@[api_theorem {
  theoremKind := .soundnessProperty,
  assumes := #[`PathWithLength],
  proves := #[`Relation.TransGen]
}]
theorem PathWithLength_soundness {a b : α} {n : Nat} (h : PathWithLength r a b n) :
    Relation.TransGen r a b := by
  induction h <;> simp_all [Relation.TransGen.single, Relation.TransGen.trans]
```

### Adding as a Dependency (Path-based)

For local development, add doc-verification-bridge as a path-based dependency in your `lakefile.toml`:

```toml
[[require]]
name = "doc-verification-bridge"
path = "../path/to/doc-verification-bridge"
```

Or in `lakefile.lean`:

```lean
require «doc-verification-bridge» from "../path/to/doc-verification-bridge"
```

---

## Running on External Projects

To analyze a project you haven't modified (e.g., `batteries`, `mathlib4`), you need a **nested docbuild directory** similar to doc-gen4. This is required because:

1. The target project's compiled modules must be available to import
2. doc-verification-bridge and its dependencies (doc-gen4, Cli) must be in scope
3. The nested project approach allows both requirements without modifying the target

### Setup Instructions

1. **Create a `docbuild` subdirectory** inside the target project:

```bash
cd /path/to/target-project  # e.g., batteries
mkdir docbuild
cd docbuild
```

2. **Create `lakefile.toml`** with the following content:

```toml
name = "docbuild"
reservoir = false
version = "0.1.0"
packagesDir = "../.lake/packages"

[[require]]
name = "batteries"       # Replace with your target library name
path = "../"

[[require]]
name = "doc-verification-bridge"
git = "https://github.com/YOUR_ORG/doc-verification-bridge"
rev = "main"
```

3. **Copy the `lean-toolchain`** from the parent project:

```bash
cp ../lean-toolchain .
```

4. **Update dependencies:**

```bash
lake update doc-verification-bridge
```

5. **Build the target project** (if not already built):

```bash
cd ..
lake build
cd docbuild
```

6. **Run doc-verification-bridge:**

```bash
lake exe doc-verification-bridge --output docs Batteries Batteries
```

### Example: Analyzing batteries

```bash
# From batteries root
cd /path/to/batteries
mkdir -p docbuild
cd docbuild

# Create lakefile.toml
cat > lakefile.toml << 'EOF'
name = "docbuild"
reservoir = false
version = "0.1.0"
packagesDir = "../.lake/packages"

[[require]]
name = "batteries"
path = "../"

[[require]]
name = "doc-verification-bridge"
git = "https://github.com/YOUR_ORG/doc-verification-bridge"
rev = "main"
EOF

cp ../lean-toolchain .
lake update doc-verification-bridge

# Run with automatic classification (default)
lake exe unified-doc unified --auto --output docs Batteries

# Or with annotation-based classification
lake exe unified-doc unified --annotated --output docs Batteries
```

The generated documentation will be in `docbuild/docs/`.

---

## Performance Tuning for Large Projects

For very large projects like mathlib4, proof dependency extraction can be slow because it traverses proof terms to find which lemmas each theorem uses.

### CLI Flags

| Flag | Effect |
|------|--------|
| `--skip-proof-deps` | Skip proof dependency extraction entirely (fastest, no `dependsOn` data) |
| `--proof-dep-workers N` | Use up to N worker threads for parallel proof extraction |
| `--save-classification PATH` | Save classification results to cache (creates PATH.json + PATH.jsonl) |
| `--load-classification PATH` | Load classification from cache, skip classification phase |
| `--mkdocs-workers N` | Use N parallel workers for MkDocs file generation |

**Example:**
```bash
# Skip proof deps entirely (fastest)
lake exe unified-doc unified --auto --skip-proof-deps --output docs Mathlib

# Parallel proof extraction with 8 workers
lake exe unified-doc unified --auto --proof-dep-workers 8 --output docs Mathlib

# Parallel MkDocs generation with 20 workers (speeds up file writing for large projects)
lake exe unified-doc unified --auto --mkdocs-workers 20 --output docs Mathlib

# Combined: parallel proof extraction + parallel MkDocs generation
lake exe unified-doc unified --auto --proof-dep-workers 50 --mkdocs-workers 20 --output docs Mathlib

# Save classification to cache (for large projects like mathlib4)
# Creates /tmp/mathlib-cache.json (metadata) and /tmp/mathlib-cache.jsonl (entries)
lake exe unified-doc unified --auto --save-classification /tmp/mathlib-cache --output docs Mathlib

# Load from cache and regenerate MkDocs only (fast iteration)
lake exe unified-doc unified --auto --load-classification /tmp/mathlib-cache --mkdocs-workers 20 --output docs Mathlib
```

### Classification Cache Format

The classification cache uses a split format for streaming I/O with large projects:

- **`<path>.json`**: Small metadata file with version and entry count
- **`<path>.jsonl`**: Pure [JSON Lines](https://jsonlines.org/) with one entry per line

This enables standard JSONL tooling (`jq`, `wc -l`, `head`, `tail`) and avoids stack overflow when serializing/deserializing 280K+ entries.

### How Parallel Extraction Works

When `--proof-dep-workers N` is specified with N > 0, the classifier uses a two-phase approach:
1. **Phase 1 (Sequential in MetaM)**: Extract type information, infer theorem kinds, and detect `sorry` usage
2. **Phase 2 (Parallel in IO)**: Extract proof dependencies using worker threads

This provides good speedup while maintaining correctness, since proof term traversal is pure and can be safely parallelized.

> **Tip:** For batch analysis of multiple projects, see the [experiments pipeline](experiments/README.md) which handles this configuration via TOML (`skip_proof_deps` and `proof_dep_workers` fields).

---

## Two Modes of Operation

doc-verification-bridge supports two complementary classification modes:

| Mode | Flag | Effort | Precision | Best For |
|------|------|--------|-----------|----------|
| **Automatic** | `--auto` | Zero annotations | Good (heuristic-based) | Quick overview, existing codebases |
| **Annotated** | `--annotated` | Explicit annotations | Exact | Production documentation, precise control |

---

## Mode 1: Automatic (No Annotations Required)

Run doc-verification-bridge on any Lean 4 project without modifying source code.

> **Batch Analysis:** To automatically analyze multiple Lean 4 projects in parallel,
> see the [experiments/README.md](experiments/README.md) for an automated pipeline
> that clones, builds, and generates documentation for configured repositories.

> **Note:** Run these commands from inside the `docbuild` directory after completing
> the [setup instructions](#running-on-external-projects) above.

```bash
lake exe unified-doc unified --auto --output docs MyProject.Core MyProject.Theorems
```

With source links:
```bash
lake exe unified-doc unified --auto \
  --repo https://github.com/org/repo \
  --output docs \
  MyProject.Core MyProject.Theorems
```

For projects with a single top-level module:
```bash
lake exe unified-doc unified --auto --output docs --project "Batteries" Batteries
```

### How Automatic Inference Works

The inference engine analyzes theorem types to classify names into `assumes`, `proves`, `validates`, and `dependsOn`:

#### H1: Predicate Hypotheses → `assumes`

```lean
theorem foo (h : IsAcyclic g) : ...
--            ^^^^^^^^^^^^^ predicate hypothesis → assumes
```

#### H2: Equation Hypotheses → `proves`

```lean
theorem map_val (h : strings.mapM Identifier.mk? = some ids) : ...
--                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ equation → proves
```

When a hypothesis characterizes a function's behavior, the theorem proves a property *about* that function.

#### H3: Conclusion Analysis → `proves`

```lean
theorem foo ... : NoSelfLoop g ∧ IsTree g
--                ^^^^^^^^^^^^^^^^^^^^^^^ conclusion → proves
```

#### H4: Bool Functions in Hypotheses/Conclusions → `validates`

```lean
theorem sound : isAcyclicBool g = true → IsAcyclic g
--              ^^^^^^^^^^^^^                        → validates
```

#### H5: Proof Term Analysis → `dependsOn`

```lean
theorem bar : P := by
  apply foo  -- bar depends on foo
  exact baz  -- bar depends on baz
```

The tool examines the proof term to find all theorems and lemmas used in the proof.
This creates a "depends on" relationship: if theorem A uses theorem B in its proof,
then A depends on B. This is useful for understanding proof structure and identifying
which foundational lemmas are most widely used.

#### Theorem Kind Inference

| Pattern | Inferred Kind |
|---------|---------------|
| `BoolFunc = true → PropSpec` | `bridgingProperty` (sound) |
| `PropSpec → BoolFunc = true` | `bridgingProperty` (complete) |
| `BoolFunc = true ↔ PropSpec` | `bridgingProperty` (iff) |
| `UserType → ExternalType` | `soundnessProperty` |
| `ExternalType → ∃..., UserType` | `completenessProperty` |
| Internal properties only | `mathematicalProperty` |
| Algebraic laws (BEq, Hashable) | `computationalProperty` |

---

## Mode 2: Annotated Mode (Precise Control)

For production documentation, use explicit annotations to ensure accuracy.
Run with `--annotated` flag to only classify declarations with explicit `@[api_*]` attributes:

```bash
lake exe unified-doc unified --annotated --output docs MyProject
```

### Three Attributes for the Four-Category Ontology

| Attribute | For | Category Detection |
|-----------|-----|-------------------|
| `@[api_type]` | `structure`, `inductive`, `class` | **User specifies** |
| `@[api_def]` | `def` | **Auto-detected** from return type |
| `@[api_theorem]` / `@[api_lemma]` | `theorem`, `lemma` | N/A (use `theoremKind`) |

### `@[api_type]` — For Types

Category **must** be specified (abstract vs concrete can't be auto-detected):

```lean
@[api_type { category := .mathematicalAbstraction }]
structure PackageRegistrySpec where ...

@[api_type { category := .computationalDatatype }]
inductive DependencyKind where ...
```

### `@[api_def]` — For Definitions

Category is **auto-detected** from return type:

```lean
@[api_def]  -- Returns Prop → mathematicalDefinition
def IsAcyclic (g : Graph) : Prop := ...

@[api_def]  -- Returns Option Value → computationalOperation
def lookup (m : Map) (k : Key) : Option Value := ...

@[api_def { coverage := .complete }]  -- Can specify other fields
def isEmpty (xs : List α) : Bool := ...
```

### `@[api_theorem]` / `@[api_lemma]` — For Proofs

Use `theoremKind` to classify:

```lean
@[api_theorem {
  theoremKind := .bridgingProperty,
  bridgingDirection := .sound,
  proves := #[`IsPositive],
  validates := #[`isPositiveBool]
}]
theorem isPositiveBool_sound : isPositiveBool n = true → IsPositive n := ...

@[api_lemma {
  theoremKind := .mathematicalProperty,
  proves := #[`IsAcyclic]
}]
lemma acyclic_of_tree : IsTree g → IsAcyclic g := ...
```

### Annotation Fields

#### `assumes` / `proves` / `validates`

| Field | Meaning | Example |
|-------|---------|---------|
| `assumes` | Preconditions the theorem relies on | `assumes := #[\`IsWellFormed]` |
| `proves` | What the theorem establishes | `proves := #[\`IsAcyclic]` |
| `validates` | Bool functions validated (bridging only) | `validates := #[\`isAcyclicBool]` |

#### `bridgingDirection` (for `bridgingProperty`)

| Direction | Pattern | Meaning |
|-----------|---------|---------|
| `.sound` | `comp = true → prop` | "If algorithm says yes, spec agrees" |
| `.complete` | `prop → comp = true` | "If spec says yes, algorithm finds it" |
| `.iff` | `comp = true ↔ prop` | Full decidability |

---

## Four-Category Ontology

Based on E.J. Lowe's *Four-Category Ontology* (Oxford, 2006):

|                    | **Mathematical** (Universal, Prop) | **Computational** (Particular, data) |
|--------------------|-------------------------------------|--------------------------------------|
| **Substantial**    | `mathematicalAbstraction` — Kinds   | `computationalDatatype` — Objects    |
| **Non-substantial**| `mathematicalDefinition` — Attributes | `computationalOperation` — Modes   |

### Theorem Classification Summary

| Pattern | TheoremKind | proves | validates |
|---------|-------------|--------|-----------|
| `UserType → ExternalType` | `soundnessProperty` | ExternalType | ∅ |
| `ExternalType → ∃..., UserType` | `completenessProperty` | UserType | ∅ |
| Internal closure properties | `mathematicalProperty` | internal types | ∅ |
| `BoolFunc = true ↔ PropSpec` | `bridgingProperty` | PropSpec | BoolFunc |
| Algebraic laws (reflexivity, etc.) | `computationalProperty` | internal types | ∅ |

**Key distinctions:**
- **soundnessProperty**: Every UserType is a valid ExternalType ("PathWithLength is a valid TransGen")
- **completenessProperty**: Every ExternalType can be represented as UserType
- **bridgingProperty**: Links Bool computations to Prop specifications
- **mathematicalProperty**: Internal closure/structural properties
- **computationalProperty**: Algebraic laws about BEq, Hashable, Ord instances

---

## Validation Rules

The annotation system validates at compile time:

| Code | Severity | Description |
|------|----------|-------------|
| ACE1 | ❌ error | `theoremKind` without `proves` |
| ACE2 | ❌ error | Unresolved `proves` reference |
| ACE3 | ❌ error | Unresolved `validates` reference |
| ACE13 | ❌ error | Missing `category` on type |
| ACE18 | ❌ error | `validates` on non-bridging theorem |
| ACE19 | ❌ error | Missing `validates` on bridging theorem |
| ACW9 | ⚠️ warning | Inference suggests different values |
| ACW10 | ⚠️ warning | `proves` without `theoremKind` |
| ACW19 | ⚠️ warning | Naming convention suggestion |

### Suppressing Warnings

```lean
@[api_theorem {
  theoremKind := .mathematicalProperty,
  proves := #[`MyDef],
  suppress := #["ACW15", "ACW19"]  -- Use string array
}]
theorem myTheorem : ... := ...
```

---

## Output

Generates MkDocs-compatible documentation with:
- Cross-referenced definitions and theorems
- Source code links with line numbers
- Coverage status (✅ complete, ⚠️ axiom-dependent, 🔄 partial, ❌ unverified)
- Theorem classification breakdown
- Bidirectional `verifiedBy` links

### Example Output Structure

```
docs/
├── mkdocs.yml
└── docs/
    ├── index.md
    ├── API_Coverage.md
    └── stylesheets/
        └── extra.css
```

Serve locally:
```bash
cd docs && mkdocs serve
```

---

## Comparison with Other Tools

| Feature | doc-gen4 | blueprint | doc-verification-bridge |
|---------|----------|-----------|-------------------------|
| **Purpose** | API docs | Proof dependency graphs | Semantic coverage tracking |
| **Tracks** | Types, signatures | Theorem dependencies | Ontological categories |
| **Semantic classification** | ❌ | ❌ | ✅ Four-Category Ontology |
| **Spec↔Impl links** | ❌ | ❌ | ✅ `proves`/`validates` |
| **Coverage metrics** | ❌ | ✅ (sorry tracking) | ✅ (unverified/partial/complete) |

---

## License

Apache 2.0
