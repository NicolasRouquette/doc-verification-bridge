-- DocVerificationBridge/Types.lean
-- Core types for theorem classification and verification tracking

import Lean
import DocVerificationBridge.Compatibility

/-!
# Core Types for Doc Verification Bridge

This module defines the fundamental types used for classifying theorems
and tracking verification coverage in specification-implementation refinement.

## Four-Category Ontology

Based on E.J. Lowe's *Four-Category Ontology* (Oxford, 2006), we distinguish
**mathematical** (universal, Prop-based) from **computational** (particular, decidable):

|                    | **Mathematical** (Universal) | **Computational** (Particular) |
|--------------------|------------------------------|--------------------------------|
| **Substantial**    | `mathematicalAbstraction`    | `computationalDatatype`        |
| **Non-substantial**| `mathematicalDefinition`     | `computationalOperation`       |

The key insight: **bridging theorems are the ontological glue** — they prove the
fundamental relations between abstraction levels.
-/

namespace DocVerificationBridge

open Lean

/-!
## Category Classifications
-/

/-- Categories for TYPES (structure/inductive/class) -/
inductive TypeCategory where
  /-- Abstract mathematical structures, Prop-based types (Lowe's "Kinds").
      Use for: type classes, abstract specifications, logical relations as types.
      Examples: `class MapLike`, `structure PackageRegistrySpec`, abstract map specs -/
  | mathematicalAbstraction
  /-- Concrete data structures with computational representation (Lowe's "Objects").
      Use for: data-carrying structures, enumeration types, concrete implementations.
      Examples: `DependencyKind`, `TypedDependency`, `SoftwarePackage`, `ExtTreeMap` -/
  | computationalDatatype
deriving Repr, BEq, Hashable, Inhabited

/-- Categories for DEFINITIONS (def) - auto-detected from return type -/
inductive DefCategory where
  /-- Prop-returning definitions: predicates, relations, properties (Lowe's "Attributes").
      Auto-detected for `def` that returns `Prop`.
      Examples: `depRel`, `depTransitive`, `IsAcyclic`, `wellFormed`, `contains` -/
  | mathematicalDefinition
  /-- Computable operations: Bool predicates, accessors, algorithms (Lowe's "Modes").
      Auto-detected for `def` that returns non-Prop.
      Examples: `dependsOn`, `isLeaf`, `lookup`, `insert`, `getDependencyNames`, `hasPath` -/
  | computationalOperation
deriving Repr, BEq, Hashable, Inhabited

/-!
## Verification Status
-/

/-- Verification coverage status -/
inductive CoverageStatus where
  | complete          -- Fully verified, no axioms
  | axiomDependent    -- Verified but uses axioms
  | partialCoverage   -- Some properties verified
  | unverified        -- No verification yet
  | specOnly          -- Specification only, no impl
  | notApplicable     -- No meaningful verification (e.g., ToString, Repr)
deriving Repr, BEq, Hashable, Inhabited

/-!
## Theorem Classification
-/

/-- Direction of a bridging property theorem (algorithmic soundness/completeness).
    This captures the direction of the implication between Bool computation and Prop specification. -/
inductive BridgingDirection where
  /-- Sound direction: comp = true → prop (or equivalently: ¬prop → comp = false)
      "If the algorithm says yes, the spec agrees." -/
  | sound
  /-- Complete direction: prop → comp = true (or equivalently: comp = false → ¬prop)
      "If the spec says yes, the algorithm finds it." -/
  | complete
  /-- Bidirectional: comp = true ↔ prop
      "The algorithm is both sound and complete." -/
  | iff
deriving Repr, BEq, Hashable, Inhabited

/-- Classification of theorem types based on what they prove -/
inductive TheoremKind where
  /-- Proves that a computational definition (BEq, Hashable, Ord, etc.) satisfies algebraic laws.
      Constraint: `proves ≠ ∅ ∧ validates = ∅` -/
  | computationalProperty
  /-- Proves abstract mathematical properties without validating computational instances.
      Constraint: `proves ≠ ∅ ∧ validates = ∅` -/
  | mathematicalProperty
  /-- Bridges logical properties (Prop-based relations) with computational decidability.
      Constraint: `proves ≠ ∅ ∧ validates ≠ ∅ ∧ proves ∩ validates = ∅` -/
  | bridgingProperty
  /-- Proves that a user-defined type/construction correctly embeds into an external
      mathematical specification (soundness/embedding lemma).
      Pattern: UserType args → ExternalType args
      Constraint: `proves` contains the user type being validated.
      Example: PathWithLength → TransGen (proves PathWithLength is sound) -/
  | soundnessProperty
  /-- Proves that an external mathematical specification can be represented by
      a user-defined type/construction (completeness/representation lemma).
      Pattern: ExternalType args → ∃ ..., UserType args
      Constraint: `proves` contains the user type being shown complete.
      Example: TransGen → ∃ n, PathWithLength (proves PathWithLength is complete) -/
  | completenessProperty
deriving Repr, BEq, Hashable, Inhabited

/-!
## Metadata Structures
-/

/-- Data carried by a definition declaration -/
structure DefData where
  /-- Auto-detected category -/
  category : DefCategory
  /-- Names of theorems that verify this definition -/
  verifiedBy : Array Name := #[]
  /-- Whether the definition body contains sorry -/
  hasSorry : Bool := false
deriving Repr, BEq, Inhabited

/-- Warning codes that can be suppressed -/
inductive SuppressableWarning where
  | ACW8   -- External types in assumes
  | ACW9   -- theoremKind but proves empty
  | ACW10  -- proves but no theoremKind
  | ACW11  -- minimal annotation
  | ACW15  -- assumes empty but inference suggests
  | ACW16  -- mis-categorization
  | ACW17  -- validates empty but inference suggests
  | ACW18  -- bridgingProperty but no Bool functions
  | ACW19  -- naming convention suggestion
  | ACW21  -- bridgingDirection suggestion
deriving Repr, BEq, Hashable, Inhabited, DecidableEq

/-- Data carried by a theorem declaration -/
structure TheoremData where
  /-- Classification of this theorem -/
  kind : Option TheoremKind := none
  /-- For bridgingProperty: the direction of the bridging (sound/complete/iff) -/
  bridgingDirection : Option BridgingDirection := none
  /-- Names of definitions this theorem assumes as hypotheses -/
  assumes : Array Name := #[]
  /-- Names of definitions this theorem proves properties about -/
  proves : Array Name := #[]
  /-- Names of computational instances this theorem validates -/
  validates : Array Name := #[]
  /-- Names of other theorems/lemmas this proof depends on (uses in its proof term) -/
  dependsOn : Array Name := #[]
  /-- Names of axioms this proof depends on -/
  axiomDeps : Array Name := #[]
  /-- Usage example (code snippet) -/
  usageExample : String := ""
  /-- Warnings to suppress for this declaration -/
  suppress : Array SuppressableWarning := #[]
  /-- Whether the proof contains sorry -/
  hasSorry : Bool := false
deriving Repr, BEq, Inhabited

/-- Kind of declaration being tracked -/
inductive DeclKind where
  | apiType (category : TypeCategory)
  | apiDef (data : DefData)
  | apiTheorem (data : TheoremData)
deriving Repr, BEq, Inhabited

/-- Convert DeclKind to a display string for the category/kind -/
def DeclKind.categoryString : DeclKind → String
  | .apiType .mathematicalAbstraction => "mathematicalAbstraction"
  | .apiType .computationalDatatype => "computationalDatatype"
  | .apiDef ⟨.mathematicalDefinition, _, _⟩ => "mathematicalDefinition"
  | .apiDef ⟨.computationalOperation, _, _⟩ => "computationalOperation"
  | .apiTheorem ⟨some .computationalProperty, _, _, _, _, _, _, _, _, _⟩ => "computationalProperty"
  | .apiTheorem ⟨some .mathematicalProperty, _, _, _, _, _, _, _, _, _⟩ => "mathematicalProperty"
  | .apiTheorem ⟨some .bridgingProperty, _, _, _, _, _, _, _, _, _⟩ => "bridgingProperty"
  | .apiTheorem ⟨some .soundnessProperty, _, _, _, _, _, _, _, _, _⟩ => "soundnessProperty"
  | .apiTheorem ⟨some .completenessProperty, _, _, _, _, _, _, _, _, _⟩ => "completenessProperty"
  | .apiTheorem ⟨none, _, _, _, _, _, _, _, _, _⟩ => "theorem"

/-- Get the theorem kind if this is a theorem -/
def DeclKind.theoremKind? : DeclKind → Option TheoremKind
  | .apiTheorem data => data.kind
  | _ => none

/-- Get theorem data if this is a theorem -/
def DeclKind.theoremData? : DeclKind → Option TheoremData
  | .apiTheorem data => some data
  | _ => none

/-- Get def data if this is a def -/
def DeclKind.defData? : DeclKind → Option DefData
  | .apiDef data => some data
  | _ => none

/-- Get verifiedBy if this is a def -/
def DeclKind.verifiedBy? : DeclKind → Option (Array Name)
  | .apiDef data => some data.verifiedBy
  | _ => none

/-- Get assumes if this is a theorem -/
def DeclKind.assumes? : DeclKind → Option (Array Name)
  | .apiTheorem data => some data.assumes
  | _ => none

/-- Get proves if this is a theorem -/
def DeclKind.proves? : DeclKind → Option (Array Name)
  | .apiTheorem data => some data.proves
  | _ => none

/-- Get validates if this is a theorem -/
def DeclKind.validates? : DeclKind → Option (Array Name)
  | .apiTheorem data => some data.validates
  | _ => none

/-- Get dependsOn if this is a theorem -/
def DeclKind.dependsOn? : DeclKind → Option (Array Name)
  | .apiTheorem data => some data.dependsOn
  | _ => none

/-- Get bridgingDirection if this is a theorem -/
def DeclKind.bridgingDirection? : DeclKind → Option BridgingDirection
  | .apiTheorem data => data.bridgingDirection
  | _ => none

/-!
## API Metadata
-/

/-- Unified API metadata for all tracked declarations -/
structure APIMeta where
  /-- What kind of declaration this is (with type-specific data) -/
  kind : DeclKind
  /-- Verification status -/
  coverage : CoverageStatus := .unverified
deriving Repr, Inhabited

/-- Check if this is a theorem/lemma -/
def APIMeta.isTheorem (m : APIMeta) : Bool :=
  match m.kind with
  | .apiTheorem .. => true
  | _ => false

/-- Get theorem kind if this is a theorem -/
def APIMeta.theoremKind? (m : APIMeta) : Option TheoremKind := m.kind.theoremKind?

/-- Get verifiedBy (for defs) -/
def APIMeta.verifiedBy (m : APIMeta) : Array Name :=
  m.kind.verifiedBy?.getD #[]

/-- Get assumes (for theorems) -/
def APIMeta.assumes (m : APIMeta) : Array Name :=
  m.kind.assumes?.getD #[]

/-- Get proves (for theorems) -/
def APIMeta.proves (m : APIMeta) : Array Name :=
  m.kind.proves?.getD #[]

/-- Get validates (for theorems) -/
def APIMeta.validates (m : APIMeta) : Array Name :=
  m.kind.validates?.getD #[]

/-- Get dependsOn (for theorems) -/
def APIMeta.dependsOn (m : APIMeta) : Array Name :=
  m.kind.dependsOn?.getD #[]

/-- Get bridgingDirection (for theorems) -/
def APIMeta.bridgingDirection? (m : APIMeta) : Option BridgingDirection :=
  m.kind.bridgingDirection?

/-- Check if the declaration contains sorry -/
def APIMeta.hasSorry (m : APIMeta) : Bool :=
  match m.kind with
  | .apiTheorem data => data.hasSorry
  | .apiDef data => data.hasSorry
  | .apiType _ => false

/-- Get the category string for display -/
def APIMeta.categoryString (m : APIMeta) : String := m.kind.categoryString

/-!
## Environment Extension
-/

/-- Environment extension to store API coverage data -/
initialize apiCoverageExt : SimplePersistentEnvExtension (Name × APIMeta) (NameMap APIMeta) ←
  registerSimplePersistentEnvExtension {
    name := `VerificationBridge.apiCoverageExt
    addEntryFn := fun map (name, apiMeta) => map.insert name apiMeta
    addImportedFn := fun arrays =>
      arrays.foldl (fun acc arr =>
        arr.foldl (fun acc (name, apiMeta) => acc.insert name apiMeta) acc
      ) {}
  }

/-- Get all API coverage entries from the environment -/
def getAPICoverage (env : Environment) : NameMap APIMeta :=
  apiCoverageExt.getState env

/-- Get API metadata for a specific declaration -/
def getAPIMeta? (env : Environment) (name : Name) : Option APIMeta :=
  (apiCoverageExt.getState env).find? name

/-- Add API metadata to the environment -/
def addAPIMeta (env : Environment) (name : Name) (apiMeta : APIMeta) : Environment :=
  apiCoverageExt.addEntry env (name, apiMeta)

end DocVerificationBridge
