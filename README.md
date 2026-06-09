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

DocVerificationBridge depends on Lean and doc-gen4 APIs that have **breaking changes** between versions. A single codebase cannot support all versions simultaneously.

#### Supported Version Range

| Constant | Value | Location |
|----------|-------|----------|
| **Min supported** | 4.24.0 | `Experiments/Experiments/ExperimentsCore.lean` → `minSupportedVersion` |
| **Max supported** | 4.30.0.0 | `Experiments/Experiments/ExperimentsCore.lean` → `maxSupportedVersion` |
| **Current toolchain** | v4.29.0 | `DocVerificationBridge/lakefile.toml` → `leanVersion` |

> **To add a new Lean version**: update `maxSupportedVersion` in ExperimentsCore.lean,
> then follow the steps in [Adding Support for New Lean Versions](#adding-support-for-new-lean-versions).

**Solution**: Version-specific git branches + process isolation:
- **DocVerificationBridge** has version-specific branches (v4.27.0, v4.28.0, v4.29.0, main)
- **Experiments** orchestrates by cloning the appropriate DocVerificationBridge version per project
- Communication happens via CLI/subprocess (not Lean imports)

## DocVerificationBridge Package

**Purpose**: Core analysis library that classifies theorems and generates verification reports.

**Dependencies**: Lean 4, doc-gen4 (version-specific)

**Executables**:
- `unified-doc` — combined doc-gen4 + verification HTML pipeline
- `unified-dep-table` — per-namespace dependency tables and dead-code analysis

### Building

```bash
cd DocVerificationBridge
lake build           # builds the library and both executables
```

### CLI Tools

#### `unified-doc` — combined HTML documentation

Generates doc-gen4 HTML output side-by-side with a verification coverage
report based on the Four-Category Ontology classification.

```bash
lake exe unified-doc unified \
  --repo https://github.com/owner/repo \
  --project "MyProject" \
  --auto \
  MyModule

# Annotation-only mode
lake exe unified-doc unified --annotated MyModule
```

Subcommands: `unified` (full pipeline), `docgen4` (HTML only), `verify`
(verification report only).  See `lake exe unified-doc --help` for the
full flag list, or the [Classification Modes](#classification-modes)
section below.

#### `unified-dep-table` — per-namespace dependency table

Produces a Markdown or JSON table covering every classified declaration
in a chosen namespace, with forward proof-term dependencies, an inverse
caller map, and `hasSorry` annotations.  The actionable output is the
**deletion candidates** section — declarations that carry `sorry` and
have no in-scope callers — plus a complementary **dead non-sorry**
section that surfaces unreferenced helper machinery.

```bash
# Fresh classification + table for one namespace.  Run from the project
# being analyzed so `lake env` exposes its olean tree.
cd /path/to/some-lean-project
lake env /path/to/unified-dep-table fresh \
  --namespace MyProject.Subnamespace \
  --external-only \
  --proof-dep-workers 4 \
  --output deps.md \
  MyProject

# Reuse a saved classification cache (much faster than `fresh`)
unified-dep-table cached \
  --namespace MyProject.Subnamespace \
  --classification cache.json \
  --output deps.md
```

The `--external-only` flag is the key for cleanup work: it suppresses
callers from inside the same namespace, which surfaces transitively-dead
clusters that look "alive" with intra-namespace references but have no
out-of-namespace consumers.  Without it, the deletion-candidate filter
only catches singleton dead leaves, missing the more common case of
mutually-referencing dead theorem groups.

### Using DocVerificationBridge as a Library

The CLI tools are thin wrappers; the same analysis is available
programmatically by importing the library.

#### Adding as a dependency

```toml
# In your project's lakefile.toml:
[[require]]
name = "DocVerificationBridge"
git = "https://github.com/NicolasRouquette/doc-verification-bridge.git"
rev = "main"
subDir = "DocVerificationBridge"
```

Then `import DocVerificationBridge` in your Lean modules.  Toolchain
constraint: your project's `lean-toolchain` must match what
`DocVerificationBridge/lean-toolchain` pins, since classifying a project
requires its olean files to be ABI-compatible with DVB's own.

#### Key entry points

| Module | What it gives you |
|---|---|
| `DocVerificationBridge.Types` | `APIMeta`, `DeclKind`, `TheoremData`, `DefData` — the schema for every classified declaration |
| `DocVerificationBridge.Classify` | `classifyAllDeclarations env modPrefix` — produces a `NameMap APIMeta` covering every declaration whose source file matches `modPrefix`, including extracted proof-term dependencies |
| `DocVerificationBridge.Inference` | `extractProofDependencies` (proof-term refs), `inferTheoremAnnotations` (heuristic annotations), `collectProofDependencies` (low-level walker) |
| `DocVerificationBridge.Cache` | `saveClassification` / `loadClassification` — persist a `NameMap APIMeta` to JSON for reuse |
| `DocVerificationBridge.DependencyTable` | `Table.build`, `Table.toMarkdown`, `Table.toJson`, `Table.deletionCandidates`, `Table.deadDeclarations` — the dep-table analysis used by `unified-dep-table` |

#### Example: programmatic dependency table

```lean
import Lean
import DocVerificationBridge

open Lean DocVerificationBridge

def main (args : List String) : IO UInt32 := do
  let modName := args.head!.toName
  Lean.initSearchPath (← Lean.findSysroot)
  let env ← importModules #[{ module := modName }] {}

  -- Classify every declaration whose source file is under `modName`.
  let coreCtx : Core.Context :=
    { fileName := "<demo>", fileMap := default, maxHeartbeats := 0 }
  let coreState : Core.State := { env }
  let (result, _) ←
    (classifyAllDeclarations env modName).run' {} |>.toIO coreCtx coreState

  -- Build a dep table restricted to `modName`, suppressing in-namespace
  -- callers so transitively-dead clusters surface together.
  let table := DependencyTable.build result.entries modName (externalOnly := true)
  IO.println s!"{table.deletionCandidates.size} deletion candidates"
  IO.println s!"{table.deadDeclarations.size} total dead declarations"
  IO.FS.writeFile "deps.md" table.toMarkdown
  return 0
```

This is the same code path `unified-dep-table` follows internally — use
it when you want to embed dependency analysis in larger tooling
(blueprint regeneration, CI gates, etc.).

### `DependencyAnalysis` API — auxiliary-name policy

`DependencyAnalysis.isAuxiliaryName` and `shouldFilter`
(`DocVerificationBridge/DependencyAnalysis/Inference.lean`) classify
compiler-generated companion declarations so downstream cross-reference
and call-graph tools can exclude them. Behaviour is governed by an
`AuxiliaryNamePolicy`:

```lean
structure AuxiliaryNamePolicy where
  lastComponentMatchers : List (String → Bool) := []
  subHelperMatchers     : List (String → Bool) := []
```

A name is auxiliary if **any** `lastComponentMatchers` predicate
accepts its last component, or if walking up through
`subHelperMatchers`-recognised components leads to such a match.
The walk stops at any unrecognised component, so user names like
`Foo.go`, `Foo.bar.go`, or `Foo.below.helper` are not filtered.

Default + customization:

```lean
-- Use the defaults (tracks Lean v4.30.0-rc2's elaborator):
#eval isAuxiliaryName `Foo.brecOn_1.go    -- true
#eval isAuxiliaryName `Foo.bar.go         -- false

-- Or pass a custom policy:
def myPolicy : AuxiliaryNamePolicy :=
  defaultPolicy
    |>.addBases ["myaux"]                 -- extend with a project base
    |>.addMatcher (·.endsWith "Helper")   -- arbitrary string predicate
    |>.removeBases ["recOn"]              -- exclude a default
    |>.removeMatcher ["eq_def"]           -- drop any matcher accepting "eq_def"

#eval isAuxiliaryName `Foo.myaux.go myPolicy  -- true
#eval isAuxiliaryName `Foo.recOn myPolicy     -- false
#eval shouldFilter `Foo.brecOn myPolicy       -- true (filterNames + isInternal also consulted)
```

`AuxiliaryNamePolicy` is `Inhabited`; helpers `addBases`,
`addMatcher`, `addSubHelper`, `removeBases`, `removeMatcher` return
new policies (no mutation). All policy fields and predicate
helpers are public — see
[`DependencyAnalysis/Inference.lean`](DocVerificationBridge/DependencyAnalysis/Inference.lean).

#### Default policy — Lean elaborator emission sites

The defaults track names Lean's elaborator emits as of `v4.30.0-rc2`.
Each entry below links to the upstream source so the list can be
audited against future Lean releases.

**Last-component bases** (`auxiliarySuffixComponents`, exact match):

| Last component | Lean source |
|---|---|
| `rec` (primitive recursor) | Kernel-emitted; numbered variants for nested inductives at [`Lean.Meta.IndPredBelow:225–226`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/IndPredBelow.lean#L225-L226) |
| `recOn` | [`Lean.AuxRecursor:17`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/AuxRecursor.lean#L17) (`recOnSuffix`); construction in `Lean.Meta.Constructions.RecOn` |
| `casesOn` | [`Lean.AuxRecursor:16`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/AuxRecursor.lean#L16) (`casesOnSuffix`); construction in `Lean.Meta.CasesOn` |
| `brecOn` | [`Lean.AuxRecursor:18`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/AuxRecursor.lean#L18) (`brecOnSuffix`); construction in [`Lean.Meta.Constructions.BRecOn`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/Constructions/BRecOn.lean); numbered nested variants at [`Lean.Meta.IndPredBelow:229–230`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/IndPredBelow.lean#L229-L230) |
| `below` | [`Lean.AuxRecursor:19`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/AuxRecursor.lean#L19) (`belowSuffix`); numbered nested variants at [`Lean.Meta.IndPredBelow:227–228`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/IndPredBelow.lean#L227-L228) |
| `ndrec` | [`Lean.AuxRecursor:36`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/AuxRecursor.lean#L36) (`Eq.ndrec` special case) |
| `ndrecOn` | [`Lean.AuxRecursor:37`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/AuxRecursor.lean#L37) (`Eq.ndrecOn` special case) |
| `noConfusion` | [`Lean.Meta.Constructions.NoConfusion:148`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/Constructions/NoConfusion.lean#L148) |
| `noConfusionType` | [`Lean.Meta.Constructions.NoConfusion:53`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/Constructions/NoConfusion.lean#L53) (`mkNoConfusionTypeName`) |
| `sizeOf` | [`Lean.Meta.SizeOf`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/SizeOf.lean) (instance `<T>.sizeOf`) |
| `sizeOf_spec` | [`Lean.Meta.SizeOf:194`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/SizeOf.lean#L194) |
| `injEq` | [`Lean.Meta.Injective:117`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/Injective.lean#L117) |
| `inj` | [`Lean.Meta.Injective:102`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/Injective.lean#L102) |
| `mk` | Anonymous structure constructor — emitted by [`Lean.Elab.Structure`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Elab/Structure.lean) |

**Numeric variants on the last component** (`isNumberedAuxComponentOver auxiliarySuffixComponents`):

Lean's own check is [`Lean.AuxRecursor:41`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/AuxRecursor.lean#L41) — `s.startsWith s!"{suffix}_"` combined with `isAuxRecursor`. We match `<base>_<digits>` for every entry in `auxiliarySuffixComponents`, plus the pattern-matcher family:

| Numeric pattern | Lean source |
|---|---|
| `match_<digits>` | [`Lean.Elab.Match:1136`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Elab/Match.lean#L1136) (`mkAuxName \`match`) → [`Lean.CoreM:147`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/CoreM.lean#L147) (`mkAuxDeclName`) |
| `<base>_<digits>` | Generated via `appendIndexAfter` at [`Lean.Meta.IndPredBelow:225–230`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/IndPredBelow.lean#L225-L230) for nested inductives |

**Standalone equation-lemma suffixes** (`isEqnLikeSuffix`, also on the last component):

| Last component | Lean source |
|---|---|
| `eq_def` | [`Lean.Meta.Eqns:72`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/Eqns.lean#L72) (`unfoldThmSuffix`) |
| `eq_unfold` | [`Lean.Meta.Eqns:73`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/Eqns.lean#L73) (`eqUnfoldThmSuffix`) |
| `eq_<digits>` | [`Lean.Meta.Eqns:65–69`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/Eqns.lean#L65-L69) (`eqn1ThmSuffix`, `isEqnReservedNameSuffix`) |

**Sub-helpers** (`subHelperMatchers`, traversed during walk):

| Sub-component | Lean source |
|---|---|
| `go` | [`Lean.Meta.Constructions.BRecOn:194`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/Constructions/BRecOn.lean#L194) (`brecOnName.str "go"`) |
| `eq` | [`Lean.Meta.Constructions.BRecOn:195`](https://github.com/leanprover/lean4/blob/v4.30.0-rc2/src/Lean/Meta/Constructions/BRecOn.lean#L195) (`brecOnName.str "eq"`) |
| `eq_def`, `eq_unfold`, `eq_<digits>` | Same `Lean.Meta.Eqns` sources as above — also walked through so `Foo.brecOn.eq_1` is reached |

#### Tests

```sh
cd DocVerificationBridge
lake build DependencyAnalysisTests
```

[`DocVerificationBridge/test/IsAuxiliaryNameTests.lean`](DocVerificationBridge/test/IsAuxiliaryNameTests.lean)
pins the three classification layers (exact-base, numeric, walk-up),
the standalone-equation-lemma case, the must-not-filter
discrimination (`Foo.go`, `Foo.bar.go`, `Foo.below.helper`,
`g_listrec`/`myrec`/`vec`), and the policy-customization helpers
(`addBases`, `addMatcher`, `removeBases`, `removeMatcher`).

### Supported Lean Versions

| Branch | Lean Versions | Notes |
|--------|--------------|-------|
| v4.27.0 | 4.24.0 – 4.27.0 | Legacy |
| v4.28.0 | 4.28.0 | |
| v4.29.0 / main | 4.29.0-rc1+ | Current development |

See the [Supported Version Range](#supported-version-range) table above for the authoritative min/max.

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
docvb_version = "v4.29.0"  # Which DocVerificationBridge branch to use (see Supported Version Range)
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

### Completed: Existential Binder Type Tracing (2026-03-20)

**Problem:** When a theorem concludes with `∃ vy : ValidYaml, vy.input = ... ∧ ...`, the `collectHeadConstants` function in [Inference.lean](DocVerificationBridge/DocVerificationBridge/Inference.lean) strips the existential binder and recurses into the body only. The field projections (`.input`, `.value`) are traced into `proves`, but the binder type (`ValidYaml`) is discarded. This means the parent structure never appears in any theorem's `proves` list, so its `verifiedBy` stays empty.

**Fix** — extract the binder type in the `Exists` branch of `collectHeadConstants`:
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

### Completed: Instance Classification Rules (2026-03-20)

**Problem:** Typeclass instances (e.g., `instance : Decidable (isFoldAppendChar c)`, `instance : BEq CollectionStyle`) are classified as plain definitions (`computationalOperation` or `mathematicalDefinition`). The bridge treats them identically to regular functions, missing important verification semantics:
- **Self-certifying** instances like `Decidable` are proofs by construction — their type *encodes* correctness
- **Requires-lawful** instances like `BEq` need companion proofs (`LawfulBEq`) for correctness
- **Infrastructure** instances like `ToString`, `Repr` are not verification-relevant

**Solution** — extensible typeclass rule system:

1. `InstanceDisposition` enum classifies how instances should be handled:
   ```lean
   inductive InstanceDisposition where
     | selfCertifying                       -- Instance IS the proof (Decidable, LawfulBEq)
     | requiresLawful (lawfulClass : Name)  -- Needs companion proof (BEq → LawfulBEq)
     | infrastructure                       -- Not verification-relevant (ToString, Repr)
   ```

2. `TypeclassRule` pairs a class name with its disposition:
   ```lean
   structure TypeclassRule where
     className : Name
     disposition : InstanceDisposition
     description : String := ""
   ```

3. `defaultTypeclassRules` provides rules for common typeclasses (extensible by adding entries):

   | Typeclass | Disposition | Rationale |
   |-----------|-------------|-----------|
   | `Decidable`, `DecidableEq`, `DecidablePred`, `DecidableRel` | `selfCertifying` | Decision procedure with proof |
   | `LawfulBEq`, `LawfulFunctor`, `LawfulMonad` | `selfCertifying` | Law-satisfaction proofs |
   | `BEq` | `requiresLawful LawfulBEq` | Needs correctness proof |
   | `Functor` | `requiresLawful LawfulFunctor` | Needs functor law proof |
   | `Monad` | `requiresLawful LawfulMonad` | Needs monad law proof |
   | `ToString`, `Repr`, `Inhabited`, `Nonempty`, `Hashable`, `Ord` | `infrastructure` | Not verification-relevant |

4. `extractInstanceInfo` (Classify.lean) detects instances via `Meta.isInstance`, extracts the class and argument types, and looks up the matching rule.

5. `computeVerifiedByMap` (TableData.lean) includes self-certifying instances in the reverse index — a `Decidable (isFoldAppendChar c)` instance now appears in `isFoldAppendChar.verifiedBy`.

6. JSON output adds `instanceOf`, `instanceDisposition`, `instanceLawfulClass`, `instanceArgTypes` fields to definition entries.

**Impact:**
- `instDecidableIsFoldAppendChar` → `instanceOf: Decidable, disposition: selfCertifying, argTypes: [isFoldAppendChar]` → fills `isFoldAppendChar.verifiedBy`
- `instBEqCollectionStyle` → `instanceOf: BEq, disposition: requiresLawful, lawfulClass: LawfulBEq, argTypes: [CollectionStyle]` → obligation tracked
- `instLawfulBEqCollectionStyle` → `instanceOf: LawfulBEq, disposition: selfCertifying, argTypes: [CollectionStyle]` → fills `CollectionStyle.verifiedBy`

| File | Change |
|------|--------|
| [Types.lean](DocVerificationBridge/DocVerificationBridge/Types.lean) | Add `InstanceDisposition`, `InstanceInfo`, `TypeclassRule`, `defaultTypeclassRules`; add `instanceInfo` to `DefData` |
| [Classify.lean](DocVerificationBridge/DocVerificationBridge/Classify.lean) | Add `extractInstanceInfo`, `collectInstanceArgTypes`, `findTypeclassRule`; call from `classifyConstantLight`/`classifyConstant` |
| [TableData.lean](DocVerificationBridge/DocVerificationBridge/TableData.lean) | Self-certifying instances in `computeVerifiedByMap`; add instance fields to `DefinitionTableEntry` |
| [Report.lean](DocVerificationBridge/DocVerificationBridge/Report.lean) | Update `DefData` pattern matches (4 fields) |
| [Unified.lean](DocVerificationBridge/DocVerificationBridge/Unified.lean) | Update `DefData` pattern matches (4 fields) |
| [UnifiedBasic.lean](DocVerificationBridge/DocVerificationBridge/UnifiedBasic.lean) | Update `DefData` pattern matches (4 fields) |

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
