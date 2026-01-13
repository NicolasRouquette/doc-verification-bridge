-- DocVerificationBridge/Attributes.lean
-- Attribute syntax for manual annotation (optional, for precise control)

import Lean
import DocVerificationBridge.Types

/-!
# Optional Annotation Attributes

These attributes provide manual control over classification when automatic
inference is insufficient or when precise annotations are desired.

## Usage

```lean
@[api_type { category := .mathematicalAbstraction }]
class MapLike (M : Type*) (K V : Type*) where ...

@[api_def]  -- category auto-detected from return type
def lookup (m : M) (k : K) : Option V := ...

@[api_theorem {
  theoremKind := .bridgingProperty,
  proves := #[`lookupProp],
  validates := #[`lookupBool]
}]
theorem lookup_sound : lookupBool m k = true → lookupProp m k := ...
```
-/

namespace DocVerificationBridge

open Lean
open Lean.Elab
open Lean.Elab.Command

/-!
## Attribute Syntax
-/

/-- Syntax for optional metadata block -/
syntax apiMetaBlock := "{" (ident ":=" term),* "}"

/-- Mark as a tracked API TYPE (for structure/inductive/class) with required category -/
syntax (name := apiTypeAttr) "api_type" (apiMetaBlock)? : attr

/-- Mark as a tracked API definition (for def) with optional metadata.
    Category is auto-detected from return type -/
syntax (name := apiDefAttr) "api_def" (apiMetaBlock)? : attr

/-- Mark as an API theorem with optional metadata -/
syntax (name := apiTheoremAttr) "api_theorem" (apiMetaBlock)? : attr

/-- Mark as an API lemma with optional metadata (synonym for api_theorem) -/
syntax (name := apiLemmaAttr) "api_lemma" (apiMetaBlock)? : attr

/-!
## Parsing Helpers
-/

/-- Parse a TypeCategory from syntax -/
def parseTypeCategory (stx : Syntax) : Option TypeCategory :=
  match stx with
  | `(term| .mathematicalAbstraction) => some .mathematicalAbstraction
  | `(term| .computationalDatatype) => some .computationalDatatype
  | `(term| TypeCategory.mathematicalAbstraction) => some .mathematicalAbstraction
  | `(term| TypeCategory.computationalDatatype) => some .computationalDatatype
  | _ => none

/-- Parse a CoverageStatus from syntax -/
def parseCoverage (stx : Syntax) : Option CoverageStatus :=
  match stx with
  | `(term| .complete) => some .complete
  | `(term| .axiomDependent) => some .axiomDependent
  | `(term| .partialCoverage) => some .partialCoverage
  | `(term| .unverified) => some .unverified
  | `(term| .specOnly) => some .specOnly
  | `(term| .notApplicable) => some .notApplicable
  | _ => none

/-- Parse a TheoremKind from syntax -/
def parseTheoremKind (stx : Syntax) : Option TheoremKind :=
  match stx with
  | `(term| .computationalProperty) => some .computationalProperty
  | `(term| .mathematicalProperty) => some .mathematicalProperty
  | `(term| .bridgingProperty) => some .bridgingProperty
  | `(term| .soundnessProperty) => some .soundnessProperty
  | `(term| .completenessProperty) => some .completenessProperty
  | _ => none

/-- Parse a BridgingDirection from syntax -/
def parseBridgingDirection (stx : Syntax) : Option BridgingDirection :=
  match stx with
  | `(term| .sound) => some .sound
  | `(term| .complete) => some .complete
  | `(term| .iff) => some .iff
  | _ => none

/-!
## Environment Extension for Attribute Storage
-/

/-- Environment extension to store api_type annotations -/
initialize apiTypeAttrExt : SimplePersistentEnvExtension (Name × APIMeta) (NameMap APIMeta) ←
  registerSimplePersistentEnvExtension {
    addEntryFn := fun map (name, apiMeta) => map.insert name apiMeta
    addImportedFn := fun arrays =>
      arrays.foldl (fun acc arr => arr.foldl (fun m (n, v) => m.insert n v) acc) {}
  }

/-- Environment extension to store api_def annotations -/
initialize apiDefAttrExt : SimplePersistentEnvExtension (Name × APIMeta) (NameMap APIMeta) ←
  registerSimplePersistentEnvExtension {
    addEntryFn := fun map (name, apiMeta) => map.insert name apiMeta
    addImportedFn := fun arrays =>
      arrays.foldl (fun acc arr => arr.foldl (fun m (n, v) => m.insert n v) acc) {}
  }

/-- Environment extension to store api_theorem annotations -/
initialize apiTheoremAttrExt : SimplePersistentEnvExtension (Name × APIMeta) (NameMap APIMeta) ←
  registerSimplePersistentEnvExtension {
    addEntryFn := fun map (name, apiMeta) => map.insert name apiMeta
    addImportedFn := fun arrays =>
      arrays.foldl (fun acc arr => arr.foldl (fun m (n, v) => m.insert n v) acc) {}
  }

/-!
## Attribute Lookup Functions
-/

/-- Look up api_type annotation for a declaration -/
def getApiTypeAttr (env : Environment) (declName : Name) : Option APIMeta :=
  apiTypeAttrExt.getState env |>.find? declName

/-- Look up api_def annotation for a declaration -/
def getApiDefAttr (env : Environment) (declName : Name) : Option APIMeta :=
  apiDefAttrExt.getState env |>.find? declName

/-- Look up api_theorem annotation for a declaration -/
def getApiTheoremAttr (env : Environment) (declName : Name) : Option APIMeta :=
  apiTheoremAttrExt.getState env |>.find? declName

/-- Look up any api_* annotation for a declaration -/
def getApiAttr (env : Environment) (declName : Name) : Option APIMeta :=
  getApiTypeAttr env declName <|>
  getApiDefAttr env declName <|>
  getApiTheoremAttr env declName

/-!
## Attribute Handlers
-/

/-- Parse metadata block into partial APIMeta updates -/
def parseMetaBlock (stx : Syntax) : CommandElabM (Option TypeCategory × Option TheoremKind × Option CoverageStatus) := do
  let mut cat : Option TypeCategory := none
  let mut kind : Option TheoremKind := none
  let mut cov : Option CoverageStatus := none

  match stx with
  | `(apiMetaBlock| { $[$ids := $vals],* }) =>
    for (id, val) in ids.zip vals do
      let idName := id.getId.toString
      match idName with
      | "category" =>
        if let some c := parseTypeCategory val then
          cat := some c
      | "theoremKind" =>
        if let some k := parseTheoremKind val then
          kind := some k
      | "coverage" =>
        if let some c := parseCoverage val then
          cov := some c
      | _ => pure ()
  | _ => pure ()

  return (cat, kind, cov)

/-- Attribute handler for @[api_type] -/
initialize registerBuiltinAttribute {
  name := `api_type
  descr := "Mark as a tracked API type with classification metadata"
  add := fun declName stx _attrKind => do
    let env ← getEnv
    let (cat, _, cov) := ← liftCommandElabM <| parseMetaBlock stx[1]!

    -- Default to mathematicalAbstraction if not specified
    let category := cat.getD .mathematicalAbstraction

    let apiMeta : APIMeta := {
      kind := .apiType category
      coverage := cov.getD .unverified
    }

    setEnv <| apiTypeAttrExt.addEntry env (declName, apiMeta)
}

/-- Attribute handler for @[api_def] -/
initialize registerBuiltinAttribute {
  name := `api_def
  descr := "Mark as a tracked API definition with classification metadata"
  add := fun declName stx _attrKind => do
    let env ← getEnv
    let (cat, _, cov) := ← liftCommandElabM <| parseMetaBlock stx[1]!

    -- Default to computationalOperation if not specified; map TypeCategory to DefCategory
    let defCat : DefCategory := match cat with
      | some .mathematicalAbstraction => .mathematicalDefinition
      | some .computationalDatatype => .computationalOperation
      | none => .computationalOperation

    let apiMeta : APIMeta := {
      kind := .apiDef { category := defCat }
      coverage := cov.getD .unverified
    }

    setEnv <| apiDefAttrExt.addEntry env (declName, apiMeta)
}

/-- Attribute handler for @[api_theorem] -/
initialize registerBuiltinAttribute {
  name := `api_theorem
  descr := "Mark as a tracked API theorem with classification metadata"
  add := fun declName stx _attrKind => do
    let env ← getEnv
    let (_, kind, cov) := ← liftCommandElabM <| parseMetaBlock stx[1]!

    -- Default to mathematicalProperty if not specified
    let theoremKind := kind.getD .mathematicalProperty

    let apiMeta : APIMeta := {
      kind := .apiTheorem { kind := some theoremKind }
      coverage := cov.getD .complete
    }

    setEnv <| apiTheoremAttrExt.addEntry env (declName, apiMeta)
}

/-- Attribute handler for @[api_lemma] (synonym for api_theorem) -/
initialize registerBuiltinAttribute {
  name := `api_lemma
  descr := "Mark as a tracked API lemma (synonym for api_theorem)"
  add := fun declName stx _attrKind => do
    let env ← getEnv
    let (_, kind, cov) := ← liftCommandElabM <| parseMetaBlock stx[1]!

    let theoremKind := kind.getD .mathematicalProperty

    let apiMeta : APIMeta := {
      kind := .apiTheorem { kind := some theoremKind }
      coverage := cov.getD .complete
    }

    setEnv <| apiTheoremAttrExt.addEntry env (declName, apiMeta)
}

end DocVerificationBridge
