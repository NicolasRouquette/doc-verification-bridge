-- DocVerificationBridge/Classify.lean
-- Automatic classification of declarations using doc-gen4's infrastructure

import Lean
import DocGen4.Process.Analyze
import DocGen4.Process.DocInfo
import DocVerificationBridge.Types
import DocVerificationBridge.Inference

/-!
# Automatic Declaration Classification

This module provides automatic classification of declarations without requiring
source code annotations. It uses doc-gen4's analysis infrastructure to enumerate
all declarations, then applies heuristics to classify them according to the
Four-Category Ontology.

## Classification Strategy

1. **Types** (from InductiveInfo, StructureInfo, ClassInfo):
   - Prop-based → `mathematicalAbstraction`
   - Data-carrying → `computationalDatatype`

2. **Definitions** (from DefinitionInfo):
   - Returns Prop → `mathematicalDefinition`
   - Returns non-Prop → `computationalOperation`

3. **Theorems** (from TheoremInfo):
   - Use `inferTheoremAnnotations` to determine assumes/proves/validates
   - Classify based on pattern (bridging, soundness, completeness, etc.)
-/

namespace DocVerificationBridge

open Lean Meta
open DocGen4.Process

/-!
## Classification Helpers
-/

/-- Check if a type is Prop-based (for classifying mathematical vs computational types) -/
def isPropBasedType (_env : Environment) (info : InductiveVal) : Bool :=
  -- A type is "Prop-based" if it lives in Prop
  info.type.isProp

/-- Classify an inductive type -/
def classifyInductiveType (env : Environment) (info : InductiveVal) : TypeCategory :=
  if isPropBasedType env info then
    .mathematicalAbstraction
  else
    .computationalDatatype

/-- Check if a definition returns Prop -/
def returnsProp (type : Expr) : MetaM Bool := do
  forallTelescope type fun _ body => do
    let body ← whnf body
    return body.isProp

/-- Classify a definition based on its return type -/
def classifyDefinition (type : Expr) : MetaM DefCategory := do
  if ← returnsProp type then
    return .mathematicalDefinition
  else
    return .computationalOperation

/-!
## Blacklist Filtering
-/

/-- Check if a declaration should be excluded from the report -/
def isBlackListed (env : Environment) (name : Name) : MetaM Bool := do
  -- Check for declaration ranges (no ranges = compiler-generated)
  match ← findDeclarationRanges? name with
  | some _ =>
    -- Has source location, check other criteria
    pure (name.isInternal)
    <||> (pure <| isAuxRecursor env name)
    <||> (pure <| isNoConfusion env name)
    <||> (pure <| name.isInternalDetail)
    <||> isRec name
    <||> isMatcher name
  | none => return true  -- No source location = exclude

/-- Additional filter for names we don't want in the report -/
def shouldExclude (name : Name) : Bool :=
  let str := name.toString
  -- Exclude private/internal definitions
  str.startsWith "_private" ||
  str.startsWith "_root_" ||
  -- Exclude instance auto-generated names
  (str.splitOn "inst").length > 1 && str.startsWith "inst"

/-!
## Main Classification
-/

/-- Result of classifying all declarations in a module -/
structure ClassificationResult where
  /-- Map from declaration name to its metadata -/
  entries : NameMap APIMeta
  /-- Warnings or notes from classification -/
  notes : Array String
deriving Inhabited

/-- Classify a single constant from the environment -/
def classifyConstant (env : Environment) (name : Name) (cinfo : ConstantInfo)
    (internalPrefixes : Array String) : MetaM (Option APIMeta) := do
  -- Skip blacklisted declarations
  if ← isBlackListed env name then return none
  if shouldExclude name then return none
  -- Skip if not from internal modules
  if !isInternalName env internalPrefixes name then return none

  match cinfo with
  | .thmInfo _info =>
    -- Theorem: infer annotations
    let inferred ← inferTheoremAnnotations name
    let theoremKind := suggestTheoremKind inferred
    let bridgingDir := if theoremKind == some .bridgingProperty
      then inferBridgingDirection inferred
      else none
    let thmData : TheoremData := {
      kind := theoremKind
      bridgingDirection := bridgingDir
      assumes := inferred.assumesCandidates.filter (·.isInternal) |>.map (·.name)
      proves := inferred.provesCandidates.filter (·.isInternal) |>.map (·.name)
      validates := inferred.validatesCandidates.filter (·.isInternal) |>.map (·.name)
    }
    return some { kind := .apiTheorem thmData, coverage := .unverified }

  | .defnInfo info =>
    -- Definition: classify by return type
    let category ← classifyDefinition info.type
    let defData : DefData := { category }
    return some { kind := .apiDef defData, coverage := .unverified }

  | .inductInfo info =>
    -- Inductive type: classify as mathematical or computational
    let category := classifyInductiveType env info
    return some { kind := .apiType category, coverage := .unverified }

  | .axiomInfo _ =>
    -- Axiom: treat as mathematical abstraction
    return some { kind := .apiType .mathematicalAbstraction, coverage := .axiomDependent }

  | _ => return none  -- Skip other kinds (constructors, recursors, etc.)

/-- Classify all declarations from relevant modules -/
def classifyAllDeclarations (env : Environment) (modulePrefix : Name) : MetaM ClassificationResult := do
  let internalPrefixes := #[modulePrefix.toString]
  let mut entries : NameMap APIMeta := {}
  let mut notes : Array String := #[]

  for (name, cinfo) in env.constants do
    -- Check if this constant is from a relevant module
    match env.getModuleIdxFor? name with
    | some modIdx =>
      let modName := env.header.moduleNames[modIdx.toNat]!
      if modulePrefix.isPrefixOf modName then
        if let some apiMeta ← classifyConstant env name cinfo internalPrefixes then
          entries := entries.insert name apiMeta
    | none => continue

  return { entries, notes }

/-- Compute the "provedBy" relationship: for each definition, which theorems prove things about it -/
def computeProvedByMap (entries : NameMap APIMeta) : NameMap (Array Name) := Id.run do
  let mut provedBy : NameMap (Array Name) := {}

  for (thmName, apiMeta) in entries do
    if apiMeta.isTheorem then
      -- This theorem proves things about the names in its `proves` field
      for defName in apiMeta.proves do
        let existing := provedBy.find? defName |>.getD #[]
        provedBy := provedBy.insert defName (existing.push thmName)

  return provedBy

/-- Update coverage status based on provedBy relationships -/
def updateCoverageStatus (entries : NameMap APIMeta) (provedBy : NameMap (Array Name)) : NameMap APIMeta := Id.run do
  let mut result := entries

  for (name, apiMeta) in entries do
    if !apiMeta.isTheorem then
      -- Check if any theorems prove things about this definition
      let proofs := provedBy.find? name |>.getD #[]
      if !proofs.isEmpty then
        let newMeta := { apiMeta with coverage := .complete }
        result := result.insert name newMeta

  return result

end DocVerificationBridge
