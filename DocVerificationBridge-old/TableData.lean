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

/-- Convert APIMeta for a definition to a DefinitionTableEntry -/
def apiMetaToDefinitionEntry (name : Name) (apiMeta : APIMeta) (allEntries : NameMap APIMeta) : DefinitionTableEntry :=
  -- Compute verifiedBy dynamically by scanning all theorems
  -- A theorem verifies this definition if it appears in the theorem's proves or validates fields
  let verifiedBy := allEntries.toList.filterMap fun (thmName, thmMeta) =>
    match thmMeta.kind with
    | .apiTheorem thmData =>
      -- Check if this theorem proves or validates this definition
      if thmData.proves.contains name || thmData.validates.contains name then
        some (apiMetaToTheoremReference thmName thmMeta)
      else
        none
    | _ => none
  {
    name := name
    category := apiMeta.categoryString
    verifiedBy := verifiedBy.toArray
    hasSorry := apiMeta.hasSorry
    isPrivate := isPrivateName name
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

/-- Extract table data for a specific module from the complete metadata -/
def extractModuleTableData (moduleName : Name) (allEntries : NameMap APIMeta) : ModuleTableData :=
  -- Filter declarations that belong to this module
  let moduleDecls := allEntries.toList.foldl (init := #[]) fun acc (name, apiMeta) =>
    if apiMeta.module == moduleName then
      acc.push (name, apiMeta)
    else
      acc

  -- Separate into definitions and theorems
  let (defs, thms) := moduleDecls.foldl (init := (#[], #[])) fun (defAcc, thmAcc) (name, apiMeta) =>
    match apiMeta.kind with
    | .apiTheorem _ =>
      (defAcc, thmAcc.push (name, apiMeta))
    | .apiDef _ | .apiType _ =>
      (defAcc.push (name, apiMeta), thmAcc)

  -- Convert to table entries
  let defEntries := defs.map fun (name, apiMeta) =>
    apiMetaToDefinitionEntry name apiMeta allEntries

  let thmEntries := thms.map fun (name, apiMeta) =>
    apiMetaToTheoremEntry name apiMeta

  {
    moduleName := moduleName
    definitions := defEntries
    theorems := thmEntries
  }

/-- Extract table data for all modules -/
def extractAllModuleTableData (allEntries : NameMap APIMeta) : Array ModuleTableData :=
  -- Get unique module names
  let modules := allEntries.toList.foldl (init := (∅ : Std.HashSet Name)) fun acc (_, apiMeta) =>
    acc.insert apiMeta.module

  -- Extract table data for each module
  modules.fold (init := #[]) fun acc moduleName =>
    acc.push (extractModuleTableData moduleName allEntries)

/-! ## JSON File I/O -/

/-- Save module table data to a JSON file -/
def ModuleTableData.saveToFile (data : ModuleTableData) (path : System.FilePath) : IO Unit := do
  let json := toJson data
  let jsonStr := toString json
  IO.FS.writeFile path jsonStr

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
def saveAllModuleTableData (allEntries : NameMap APIMeta) (outputDir : System.FilePath) : IO Unit := do
  IO.FS.createDirAll outputDir
  let allModuleData := extractAllModuleTableData allEntries
  for moduleData in allModuleData do
    let filename := moduleData.moduleName.toString.replace "." "_" ++ ".json"
    let filepath := outputDir / filename
    moduleData.saveToFile filepath
    IO.println s!"Saved table data for {moduleData.moduleName} to {filepath}"

/-- Save a single combined JSON file with all module data -/
def saveAllModuleTableDataCombined (allEntries : NameMap APIMeta) (outputFile : System.FilePath) : IO Unit := do
  let allModuleData := extractAllModuleTableData allEntries
  let json := toJson allModuleData
  let jsonStr := toString json
  IO.FS.writeFile outputFile jsonStr
  IO.println s!"Saved combined table data for {allModuleData.size} modules to {outputFile}"

end DocVerificationBridge.TableData
