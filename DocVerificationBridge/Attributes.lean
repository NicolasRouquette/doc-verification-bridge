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

/-- Syntax for optional metadata block (internal abbreviation, not exposed) -/
syntax apiMetaBlock := "{" (ident ":=" term),* "}"

/-- Mark as a tracked API TYPE (for structure/inductive/class) with required category -/
syntax (name := apiTypeAttr) &"api_type" (apiMetaBlock)? : attr

/-- Mark as a tracked API definition (for def) with optional metadata.
    Category is auto-detected from return type -/
syntax (name := apiDefAttr) &"api_def" (apiMetaBlock)? : attr

/-- Mark as an API theorem with optional metadata -/
syntax (name := apiTheoremAttr) &"api_theorem" (apiMetaBlock)? : attr

/-- Mark as an API lemma with optional metadata (synonym for api_theorem) -/
syntax (name := apiLemmaAttr) &"api_lemma" (apiMetaBlock)? : attr

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

/-- Parse a single SuppressableWarning from syntax -/
def parseSuppressableWarning (stx : Syntax) : Option SuppressableWarning :=
  match stx with
  | `(term| .ACW8) => some .ACW8
  | `(term| .ACW9) => some .ACW9
  | `(term| .ACW10) => some .ACW10
  | `(term| .ACW11) => some .ACW11
  | `(term| .ACW15) => some .ACW15
  | `(term| .ACW16) => some .ACW16
  | `(term| .ACW17) => some .ACW17
  | `(term| .ACW18) => some .ACW18
  | `(term| .ACW19) => some .ACW19
  | `(term| .ACW21) => some .ACW21
  | `(term| SuppressableWarning.ACW8) => some .ACW8
  | `(term| SuppressableWarning.ACW9) => some .ACW9
  | `(term| SuppressableWarning.ACW10) => some .ACW10
  | `(term| SuppressableWarning.ACW11) => some .ACW11
  | `(term| SuppressableWarning.ACW15) => some .ACW15
  | `(term| SuppressableWarning.ACW16) => some .ACW16
  | `(term| SuppressableWarning.ACW17) => some .ACW17
  | `(term| SuppressableWarning.ACW18) => some .ACW18
  | `(term| SuppressableWarning.ACW19) => some .ACW19
  | `(term| SuppressableWarning.ACW21) => some .ACW21
  | _ => none

/-- Parse an array of SuppressableWarning from #[...] syntax -/
def parseSuppressArray (stx : Syntax) : Array SuppressableWarning :=
  match stx with
  | `(term| #[$[$elems],*]) =>
    elems.filterMap parseSuppressableWarning
  | _ => #[]

/-- Parse a Name from backtick syntax -/
def parseNameLit (stx : Syntax) : Option Name :=
  match stx with
  | Syntax.ident _ _ val _ => some val
  | _ =>
    -- Try to extract from `name term
    if stx.isOfKind `Lean.Parser.Term.quotedName then
      match stx[0] with
      | Syntax.ident _ _ val _ => some val
      | _ => none
    else
      none

/-- Parse an array of Names from #[`name1, `name2, ...] syntax -/
def parseNameArray (stx : Syntax) : Array Name :=
  match stx with
  | `(term| #[$[$elems],*]) =>
    elems.filterMap parseNameLit
  | _ => #[]

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

/-- Parsed result for api_type metadata -/
structure ParsedTypeMetaBlock where
  category : Option TypeCategory := none
  coverage : Option CoverageStatus := none

/-- Parsed result for api_def metadata -/
structure ParsedDefMetaBlock where
  category : Option TypeCategory := none
  coverage : Option CoverageStatus := none

/-- Parsed result for api_theorem metadata -/
structure ParsedTheoremMetaBlock where
  theoremKind : Option TheoremKind := none
  bridgingDirection : Option BridgingDirection := none
  coverage : Option CoverageStatus := none
  assumes : Array Name := #[]
  proves : Array Name := #[]
  validates : Array Name := #[]
  suppress : Array SuppressableWarning := #[]

/-- Parse api_type metadata block -/
def parseTypeMetaBlock (stx : Syntax) : CommandElabM ParsedTypeMetaBlock := do
  let mut result : ParsedTypeMetaBlock := {}
  match stx with
  | `(apiMetaBlock| { $[$ids := $vals],* }) =>
    for (id, val) in ids.zip vals do
      let idName := id.getId.toString
      match idName with
      | "category" =>
        if let some c := parseTypeCategory val then
          result := { result with category := some c }
      | "coverage" =>
        if let some c := parseCoverage val then
          result := { result with coverage := some c }
      | _ => pure ()
  | _ => pure ()
  return result

/-- Parse api_def metadata block -/
def parseDefMetaBlock (stx : Syntax) : CommandElabM ParsedDefMetaBlock := do
  let mut result : ParsedDefMetaBlock := {}
  match stx with
  | `(apiMetaBlock| { $[$ids := $vals],* }) =>
    for (id, val) in ids.zip vals do
      let idName := id.getId.toString
      match idName with
      | "category" =>
        if let some c := parseTypeCategory val then
          result := { result with category := some c }
      | "coverage" =>
        if let some c := parseCoverage val then
          result := { result with coverage := some c }
      | _ => pure ()
  | _ => pure ()
  return result

/-- Parse api_theorem metadata block -/
def parseTheoremMetaBlock (stx : Syntax) : CommandElabM ParsedTheoremMetaBlock := do
  let mut result : ParsedTheoremMetaBlock := {}
  match stx with
  | `(apiMetaBlock| { $[$ids := $vals],* }) =>
    for (id, val) in ids.zip vals do
      let idName := id.getId.toString
      match idName with
      | "theoremKind" =>
        if let some k := parseTheoremKind val then
          result := { result with theoremKind := some k }
      | "bridgingDirection" =>
        if let some d := parseBridgingDirection val then
          result := { result with bridgingDirection := some d }
      | "coverage" =>
        if let some c := parseCoverage val then
          result := { result with coverage := some c }
      | "assumes" =>
        result := { result with assumes := parseNameArray val }
      | "proves" =>
        result := { result with proves := parseNameArray val }
      | "validates" =>
        result := { result with validates := parseNameArray val }
      | "suppress" =>
        result := { result with suppress := parseSuppressArray val }
      | _ => pure ()
  | _ => pure ()
  return result

/-- Attribute handler for @[api_type] -/
initialize registerBuiltinAttribute {
  name := `apiTypeAttr
  descr := "Mark as a tracked API type with classification metadata"
  add := fun declName stx _attrKind => do
    let env ← getEnv
    let parsed ← liftCommandElabM <| parseTypeMetaBlock stx[1]!

    -- Default to mathematicalAbstraction if not specified
    let category := parsed.category.getD .mathematicalAbstraction

    let apiMeta : APIMeta := {
      kind := .apiType category
      coverage := parsed.coverage.getD .unverified
    }

    setEnv <| apiTypeAttrExt.addEntry env (declName, apiMeta)
}

/-- Attribute handler for @[api_def] -/
initialize registerBuiltinAttribute {
  name := `apiDefAttr
  descr := "Mark as a tracked API definition with classification metadata"
  add := fun declName stx _attrKind => do
    let env ← getEnv
    let parsed ← liftCommandElabM <| parseDefMetaBlock stx[1]!

    -- Default to computationalOperation if not specified; map TypeCategory to DefCategory
    let defCat : DefCategory := match parsed.category with
      | some .mathematicalAbstraction => .mathematicalDefinition
      | some .computationalDatatype => .computationalOperation
      | none => .computationalOperation

    let apiMeta : APIMeta := {
      kind := .apiDef { category := defCat }
      coverage := parsed.coverage.getD .unverified
    }

    setEnv <| apiDefAttrExt.addEntry env (declName, apiMeta)
}

/-- Attribute handler for @[api_theorem] -/
initialize registerBuiltinAttribute {
  name := `apiTheoremAttr
  descr := "Mark as a tracked API theorem with classification metadata"
  add := fun declName stx _attrKind => do
    let env ← getEnv
    let parsed ← liftCommandElabM <| parseTheoremMetaBlock stx[1]!

    -- Default to mathematicalProperty if not specified
    let theoremKind := parsed.theoremKind.getD .mathematicalProperty

    let apiMeta : APIMeta := {
      kind := .apiTheorem {
        kind := some theoremKind
        bridgingDirection := parsed.bridgingDirection
        assumes := parsed.assumes
        proves := parsed.proves
        validates := parsed.validates
        suppress := parsed.suppress
      }
      coverage := parsed.coverage.getD .complete
    }

    setEnv <| apiTheoremAttrExt.addEntry env (declName, apiMeta)
}

/-- Attribute handler for @[api_lemma] (synonym for api_theorem) -/
initialize registerBuiltinAttribute {
  name := `apiLemmaAttr
  descr := "Mark as a tracked API lemma (synonym for api_theorem)"
  add := fun declName stx _attrKind => do
    let env ← getEnv
    let parsed ← liftCommandElabM <| parseTheoremMetaBlock stx[1]!

    let theoremKind := parsed.theoremKind.getD .mathematicalProperty

    let apiMeta : APIMeta := {
      kind := .apiTheorem {
        kind := some theoremKind
        bridgingDirection := parsed.bridgingDirection
        assumes := parsed.assumes
        proves := parsed.proves
        validates := parsed.validates
        suppress := parsed.suppress
      }
      coverage := parsed.coverage.getD .complete
    }

    setEnv <| apiTheoremAttrExt.addEntry env (declName, apiMeta)
}

end DocVerificationBridge
