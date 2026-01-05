# doc-verification-bridge

**Theorem classification for specification-implementation refinement**

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
1. **Automatically infers** `assumes`/`proves` relationships from theorem types
2. **Classifies** definitions by their ontological category (mathematical vs computational)
3. **Identifies** bridging theorems that connect Bool computations to Prop specifications
4. **Generates** documentation showing verification coverage and proof relationships

## Installation

Add to your `lakefile.lean`:

```lean
require «doc-verification-bridge» from git
  "https://github.com/YOUR_ORG/doc-verification-bridge" @ "main"
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
lake exe doc-verification-bridge --output docs Batteries
```

The generated documentation will be in `docbuild/docs/`.

---

## Two Modes of Operation

doc-verification-bridge supports two complementary modes:

| Mode | Effort | Precision | Best For |
|------|--------|-----------|----------|
| **Automatic** | Zero annotations | Good (heuristic-based) | Quick overview, existing codebases |
| **Manual** | Explicit annotations | Exact | Production documentation, precise control |

---

## Mode 1: Automatic (No Annotations Required)

Run doc-verification-bridge on any Lean 4 project without modifying source code.

> **Note:** Run these commands from inside the `docbuild` directory after completing
> the [setup instructions](#running-on-external-projects) above.

```bash
lake exe doc-verification-bridge --output docs MyProject.Core MyProject.Theorems
```

With source links:
```bash
lake exe doc-verification-bridge \
  --repo https://github.com/org/repo \
  --output docs \
  MyProject.Core MyProject.Theorems
```

For projects with a single top-level module:
```bash
lake exe doc-verification-bridge --output docs --project "Batteries" Batteries
```

### How Automatic Inference Works

The inference engine analyzes theorem types to classify names into `assumes`, `proves`, or `validates`:

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

## Mode 2: Manual Annotations (Precise Control)

For production documentation, use explicit annotations to ensure accuracy.

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
