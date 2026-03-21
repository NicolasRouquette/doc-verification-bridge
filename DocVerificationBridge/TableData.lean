-- DocVerificationBridge/TableData.lean
-- JSON-serializable data structures for module table data

import Lean
import DocVerificationBridge.Types
import DocVerificationBridge.Compatibility

/-!
# Module Table Data

This module defines explicit JSON-serializable data structures to represent
the information rendered in module HTML pages. This allows programmatic access
to table data without needing to scrape HTML.

## Tables

1. **Definitions Table**: Shows all definitions with their categories and verification status
2. **Theorems Table**: Shows all theorems with their relationships and dependencies
-/

namespace DocVerificationBridge.TableData

open Lean

/-! ## Definitions Table -/

/-- A theorem reference with its classification -/
structure TheoremReference where
  /-- Theorem name -/
  name : Name
  /-- Theorem kind (computationalProperty, mathematicalProperty, etc.) -/
  kind : Option String
  /-- Bridging direction for bridging theorems -/
  bridgingDirection : Option String
  deriving Repr, Inhabited, ToJson, FromJson

/-- Entry in the definitions table -/
structure DefinitionTableEntry where
  /-- Definition name -/
  name : Name
  /-- Category (mathematicalAbstraction, computationalDatatype, etc.) -/
  category : String
  /-- Theorems that verify this definition -/
  verifiedBy : Array TheoremReference
  /-- Whether this definition contains sorry -/
  hasSorry : Bool
  /-- Whether this is a private declaration -/
  isPrivate : Bool
  /-- If this is a typeclass instance: the class name -/
  instanceOf : Option String := none
  /-- If this is a typeclass instance: the disposition (selfCertifying/requiresLawful/infrastructure) -/
  instanceDisposition : Option String := none
  /-- If requiresLawful: the needed lawful companion class -/
  instanceLawfulClass : Option String := none
  /-- If this is a typeclass instance: the internal argument types -/
  instanceArgTypes : Array Name := #[]
  deriving Repr, Inhabited, ToJson, FromJson

/-! ## Theorems Table -/

/-- Entry in the theorems table -/
structure TheoremTableEntry where
  /-- Theorem name -/
  name : Name
  /-- Theorem kind (computationalProperty, mathematicalProperty, bridgingProperty, etc.) -/
  kind : Option String
  /-- Bridging direction for bridging theorems (sound, complete, iff) -/
  bridgingDirection : Option String
  /-- Definitions assumed as hypotheses -/
  assumes : Array Name
  /-- Definitions that this theorem proves properties about -/
  proves : Array Name
  /-- Computational instances that this theorem validates -/
  validates : Array Name
  /-- Other theorems/lemmas this proof depends on -/
  dependsOn : Array Name
  /-- Whether the proof contains sorry -/
  hasSorry : Bool
  /-- Axioms this proof depends on -/
  axiomDeps : Array Name
  /-- Time in milliseconds spent extracting proof dependencies -/
  proofDepTimeMs : Option Nat
  /-- Whether this is a private declaration -/
  isPrivate : Bool
  deriving Repr, Inhabited, ToJson, FromJson

/-! ## Module Table Data -/

/-- Complete table data for a module -/
structure ModuleTableData where
  /-- Module name -/
  moduleName : Name
  /-- Definitions table entries -/
  definitions : Array DefinitionTableEntry
  /-- Theorems table entries -/
  theorems : Array TheoremTableEntry
  deriving Repr, Inhabited, ToJson, FromJson

/-! ## Conversion Functions -/

/-- Check if a name represents a private declaration -/
def isPrivateName (name : Name) : Bool :=
  name.toString.startsWith "_private"

/-- Convert TheoremKind to string -/
def theoremKindToString : TheoremKind → String
  | .computationalProperty => "computationalProperty"
  | .mathematicalProperty => "mathematicalProperty"
  | .bridgingProperty => "bridgingProperty"
  | .soundnessProperty => "soundnessProperty"
  | .completenessProperty => "completenessProperty"

/-- Convert BridgingDirection to string -/
def bridgingDirectionToString : BridgingDirection → String
  | .sound => "sound"
  | .complete => "complete"
  | .iff => "iff"

/-- Convert APIMeta for a theorem to a TheoremReference -/
def apiMetaToTheoremReference (name : Name) (apiMeta : APIMeta) : TheoremReference :=
  match apiMeta.kind with
  | .apiTheorem data => {
      name := name
      kind := data.kind.map theoremKindToString
      bridgingDirection := data.bridgingDirection.map bridgingDirectionToString
    }
  | _ => {
      name := name
      kind := none
      bridgingDirection := none
    }

/-- Pre-compute the reverse index: for each definition, which theorems verify it.
    Also includes self-certifying instances as verifiers of their argument types.
    This is O(n) over all entries, replacing the previous O(defs × total) scan. -/
def computeVerifiedByMap (allEntries : NameMap APIMeta) : Std.HashMap Name (Array TheoremReference) := Id.run do
  let mut result : Std.HashMap Name (Array TheoremReference) := {}
  let entriesArray := allEntries.foldl (init := #[]) fun acc name apiMeta => acc.push (name, apiMeta)
  for (declName, declMeta) in entriesArray do
    match declMeta.kind with
    | .apiTheorem thmData =>
      let ref := apiMetaToTheoremReference declName declMeta
      for defName in thmData.proves do
        let existing := result.getD defName #[]
        result := result.insert defName (existing.push ref)
      for defName in thmData.validates do
        let existing := result.getD defName #[]
        result := result.insert defName (existing.push ref)
    | .apiDef defData =>
      -- Self-certifying instances contribute to verifiedBy for their argument types
      if let some instInfo := defData.instanceInfo then
        if instInfo.disposition == .selfCertifying then
          let ref : TheoremReference := {
            name := declName
            kind := some "selfCertifyingInstance"
            bridgingDirection := none
          }
          for argType in instInfo.argTypes do
            let existing := result.getD argType #[]
            result := result.insert argType (existing.push ref)
    | _ => pure ()
  return result

/-- Convert APIMeta for a definition to a DefinitionTableEntry -/
def apiMetaToDefinitionEntry (name : Name) (apiMeta : APIMeta)
    (verifiedByMap : Std.HashMap Name (Array TheoremReference)) : DefinitionTableEntry :=
  let instFields := match apiMeta.kind with
    | .apiDef defData => defData.instanceInfo
    | _ => none
  {
    name := name
    category := apiMeta.categoryString
    verifiedBy := verifiedByMap.getD name #[]
    hasSorry := apiMeta.hasSorry
    isPrivate := isPrivateName name
    instanceOf := instFields.map (·.className.toString)
    instanceDisposition := instFields.map (·.disposition.toString)
    instanceLawfulClass := instFields.bind fun info =>
      match info.disposition with
      | .requiresLawful lawfulClass => some lawfulClass.toString
      | _ => none
    instanceArgTypes := instFields.map (·.argTypes) |>.getD #[]
  }

/-- Convert APIMeta for a theorem to a TheoremTableEntry -/
def apiMetaToTheoremEntry (name : Name) (apiMeta : APIMeta) : TheoremTableEntry :=
  match apiMeta.kind with
  | .apiTheorem data => {
      name := name
      kind := data.kind.map theoremKindToString
      bridgingDirection := data.bridgingDirection.map bridgingDirectionToString
      assumes := data.assumes
      proves := data.proves
      validates := data.validates
      dependsOn := data.dependsOn
      hasSorry := data.hasSorry
      axiomDeps := data.axiomDeps
      proofDepTimeMs := data.proofDepTimeMs
      isPrivate := isPrivateName name
    }
  | _ => {
      name := name
      kind := none
      bridgingDirection := none
      assumes := #[]
      proves := #[]
      validates := #[]
      dependsOn := #[]
      hasSorry := false
      axiomDeps := #[]
      proofDepTimeMs := none
      isPrivate := isPrivateName name
    }

/-- Extract table data for a pre-grouped module (entries already filtered) -/
def extractModuleTableDataFromGroup (moduleName : Name)
    (moduleDecls : Array (Name × APIMeta))
    (verifiedByMap : Std.HashMap Name (Array TheoremReference)) : ModuleTableData :=
  -- Separate into definitions and theorems
  let (defs, thms) := moduleDecls.foldl (init := (#[], #[])) fun (defAcc, thmAcc) (name, apiMeta) =>
    match apiMeta.kind with
    | .apiTheorem _ =>
      (defAcc, thmAcc.push (name, apiMeta))
    | .apiDef _ | .apiType _ =>
      (defAcc.push (name, apiMeta), thmAcc)

  -- Convert to table entries
  let defEntries := defs.map fun (name, apiMeta) =>
    apiMetaToDefinitionEntry name apiMeta verifiedByMap

  let thmEntries := thms.map fun (name, apiMeta) =>
    apiMetaToTheoremEntry name apiMeta

  {
    moduleName := moduleName
    definitions := defEntries
    theorems := thmEntries
  }

/-- Extract table data for all modules.
    Groups entries by module in a single pass using HashMap, and pre-computes
    the verifiedBy reverse index once. O(n) overall instead of O(modules × n). -/
def extractAllModuleTableData (allEntries : NameMap APIMeta) : Array ModuleTableData := Id.run do
  -- Pre-compute the verifiedBy reverse index once
  let verifiedByMap := computeVerifiedByMap allEntries

  -- Group entries by module in a single pass
  let mut moduleGroups : Std.HashMap Name (Array (Name × APIMeta)) := {}
  let entriesArray := allEntries.foldl (init := #[]) fun acc name apiMeta => acc.push (name, apiMeta)
  for (name, apiMeta) in entriesArray do
    let existing := moduleGroups.getD apiMeta.module #[]
    moduleGroups := moduleGroups.insert apiMeta.module (existing.push (name, apiMeta))

  -- Build ModuleTableData for each module from pre-grouped entries
  let mut result : Array ModuleTableData := #[]
  for (moduleName, moduleDecls) in moduleGroups.toArray do
    result := result.push (extractModuleTableDataFromGroup moduleName moduleDecls verifiedByMap)
  return result

/-! ## JSON File I/O -/

/-- Save module table data to a JSON file -/
def ModuleTableData.saveToFile (data : ModuleTableData) (path : System.FilePath) : IO Unit := do
  let json := toJson data
  IO.FS.writeFile path json.compress

/-- Load module table data from a JSON file -/
def ModuleTableData.loadFromFile (path : System.FilePath) : IO ModuleTableData := do
  let content ← IO.FS.readFile path
  match Json.parse content with
  | .error err => throw (IO.userError s!"Failed to parse JSON: {err}")
  | .ok json =>
    match fromJson? json with
    | .error err => throw (IO.userError s!"Failed to decode ModuleTableData: {err}")
    | .ok data => pure data

/-- Save all module table data to a directory (one JSON file per module) -/
def saveAllModuleTableData (allModuleData : Array ModuleTableData) (outputDir : System.FilePath) : IO Unit := do
  IO.FS.createDirAll outputDir
  for moduleData in allModuleData do
    let filename := moduleData.moduleName.toString.replace "." "_" ++ ".json"
    let filepath := outputDir / filename
    moduleData.saveToFile filepath

/-- Save a single combined JSON file with all module data -/
def saveAllModuleTableDataCombined (allModuleData : Array ModuleTableData) (outputFile : System.FilePath) : IO Unit := do
  let json := toJson allModuleData
  IO.FS.writeFile outputFile json.compress
  IO.println s!"Saved combined table data for {allModuleData.size} modules to {outputFile}"

end DocVerificationBridge.TableData
