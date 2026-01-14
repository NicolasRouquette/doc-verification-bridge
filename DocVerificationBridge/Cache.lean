-- DocVerificationBridge/Cache.lean
-- JSON serialization for classification cache using Lean.Json

import Lean
import Lean.Data.Json
import DocVerificationBridge.Types

/-!
# Classification Cache

This module provides JSON serialization for classification results,
allowing large projects like Mathlib4 to skip the ~2 hour classification
step when only MkDocs generation needs to be re-run.

## File Format

The cache uses a **split format** with two files:

1. **Metadata file** (`<basename>.json`): Small JSON with version and entry count
2. **Entries file** (`<basename>.jsonl`): Pure JSONL with one entry per line

This design enables:
- Standard JSONL tooling support (`jq`, `wc -l`, `head`, `tail`, Python's `jsonlines`)
- Streaming writes and reads to avoid stack overflow with 280K+ entries
- Quick metadata inspection without loading all entries

### Metadata file (`classification-cache.json`):
```json
{
  "version": "1",
  "projectName": "mathlib4",
  "entryCount": 279337
}
```

### Entries file (`classification-cache.jsonl`):
```
{"name":"Nat.add_comm","kind":"theorem","category":"mathematicalProperty",...}
{"name":"Nat.mul_comm","kind":"theorem","category":"mathematicalProperty",...}
...
```

The CLI accepts a base path without extension (e.g., `--save-classification foo/cache`)
and creates both `foo/cache.json` and `foo/cache.jsonl`.
-/

namespace DocVerificationBridge.Cache

open Lean

/-!
## ToJson instances
-/

instance : ToJson TypeCategory where
  toJson
    | .mathematicalAbstraction => "mathematicalAbstraction"
    | .computationalDatatype => "computationalDatatype"

instance : ToJson DefCategory where
  toJson
    | .mathematicalDefinition => "mathematicalDefinition"
    | .computationalOperation => "computationalOperation"

instance : ToJson TheoremKind where
  toJson
    | .computationalProperty => "computationalProperty"
    | .mathematicalProperty => "mathematicalProperty"
    | .bridgingProperty => "bridgingProperty"
    | .soundnessProperty => "soundnessProperty"
    | .completenessProperty => "completenessProperty"

instance : ToJson BridgingDirection where
  toJson
    | .sound => "sound"
    | .complete => "complete"
    | .iff => "iff"

instance : ToJson CoverageStatus where
  toJson
    | .complete => "complete"
    | .axiomDependent => "axiomDependent"
    | .partialCoverage => "partialCoverage"
    | .unverified => "unverified"
    | .specOnly => "specOnly"
    | .notApplicable => "notApplicable"

instance : ToJson Name where
  toJson n := Json.str n.toString

/-- Convert APIMeta to JSON -/
def apiMetaToJson (name : Name) (m : APIMeta) : Json :=
  let kindStr := match m.kind with
    | .apiType _ => "type"
    | .apiDef _ => "def"
    | .apiTheorem _ => "theorem"

  let baseFields : List (String × Json) := [
    ("name", toJson name),
    ("kind", kindStr),
    ("category", m.categoryString),
    ("hasSorry", m.hasSorry),
    ("coverage", toJson m.coverage)
  ]

  let typeFields := match m.kind with
    | .apiType cat => [("typeCategory", toJson cat)]
    | .apiDef data =>
      let fields := [("defCategory", toJson data.category)]
      if data.verifiedBy.isEmpty then fields
      else fields ++ [("verifiedBy", toJson (data.verifiedBy.map toString))]
    | .apiTheorem data =>
      let fields : List (String × Json) := []
      let fields := match data.kind with
        | some k => fields ++ [("theoremKind", toJson k)]
        | none => fields
      let fields := match data.bridgingDirection with
        | some d => fields ++ [("bridgingDirection", toJson d)]
        | none => fields
      let fields := if !data.assumes.isEmpty then
        fields ++ [("assumes", toJson (data.assumes.map toString))] else fields
      let fields := if !data.proves.isEmpty then
        fields ++ [("proves", toJson (data.proves.map toString))] else fields
      let fields := if !data.validates.isEmpty then
        fields ++ [("validates", toJson (data.validates.map toString))] else fields
      let fields := if !data.dependsOn.isEmpty then
        fields ++ [("dependsOn", toJson (data.dependsOn.map toString))] else fields
      let fields := if !data.axiomDeps.isEmpty then
        fields ++ [("axiomDeps", toJson (data.axiomDeps.map toString))] else fields
      fields

  Json.mkObj (baseFields ++ typeFields)

/-- Convert all classification entries to JSON -/
def entriesToJson (entries : NameMap APIMeta) (projectName : String) : Json :=
  let entryArray := entries.foldl (init := #[]) fun acc name m =>
    acc.push (apiMetaToJson name m)

  Json.mkObj [
    ("version", "1"),
    ("projectName", projectName),
    ("entryCount", entries.size),
    ("entries", Json.arr entryArray)
  ]

/-!
## FromJson parsing
-/

instance : FromJson TypeCategory where
  fromJson?
    | Json.str "mathematicalAbstraction" => pure .mathematicalAbstraction
    | Json.str "computationalDatatype" => pure .computationalDatatype
    | _ => throw "invalid TypeCategory"

instance : FromJson DefCategory where
  fromJson?
    | Json.str "mathematicalDefinition" => pure .mathematicalDefinition
    | Json.str "computationalOperation" => pure .computationalOperation
    | _ => throw "invalid DefCategory"

instance : FromJson TheoremKind where
  fromJson?
    | Json.str "computationalProperty" => pure .computationalProperty
    | Json.str "mathematicalProperty" => pure .mathematicalProperty
    | Json.str "bridgingProperty" => pure .bridgingProperty
    | Json.str "soundnessProperty" => pure .soundnessProperty
    | Json.str "completenessProperty" => pure .completenessProperty
    | _ => throw "invalid TheoremKind"

instance : FromJson BridgingDirection where
  fromJson?
    | Json.str "sound" => pure .sound
    | Json.str "complete" => pure .complete
    | Json.str "iff" => pure .iff
    | _ => throw "invalid BridgingDirection"

instance : FromJson CoverageStatus where
  fromJson?
    | Json.str "complete" => pure .complete
    | Json.str "axiomDependent" => pure .axiomDependent
    | Json.str "partialCoverage" => pure .partialCoverage
    | Json.str "unverified" => pure .unverified
    | Json.str "specOnly" => pure .specOnly
    | Json.str "notApplicable" => pure .notApplicable
    | _ => pure .unverified  -- Default

/-- Parse array of Names from JSON -/
def parseNameArray (j : Json) : Except String (Array Name) := do
  let arr ← j.getArr?
  let names ← arr.mapM fun elem => do
    let s ← elem.getStr?
    return s.toName
  return names

/-- Parse a single entry from JSON -/
def parseEntry (j : Json) : Except String (Name × APIMeta) := do
  let nameStr ← j.getObjValAs? String "name"
  let name := nameStr.toName
  let kindStr ← j.getObjValAs? String "kind"
  let hasSorry := j.getObjValAs? Bool "hasSorry" |>.toOption |>.getD false
  let coverage ← j.getObjValAs? CoverageStatus "coverage" <|> pure .unverified

  let declKind ← match kindStr with
  | "type" =>
    let cat ← j.getObjValAs? TypeCategory "typeCategory"
    pure (DeclKind.apiType cat)
  | "def" =>
    let cat ← j.getObjValAs? DefCategory "defCategory"
    let verifiedBy := match j.getObjVal? "verifiedBy" with
      | Except.ok arr => parseNameArray arr |>.toOption |>.getD #[]
      | _ => #[]
    pure (DeclKind.apiDef { category := cat, verifiedBy, hasSorry })
  | "theorem" =>
    let kindOpt := j.getObjValAs? TheoremKind "theoremKind" |>.toOption
    let dirOpt := j.getObjValAs? BridgingDirection "bridgingDirection" |>.toOption
    let assumes := match j.getObjVal? "assumes" with
      | Except.ok arr => parseNameArray arr |>.toOption |>.getD #[]
      | _ => #[]
    let proves := match j.getObjVal? "proves" with
      | Except.ok arr => parseNameArray arr |>.toOption |>.getD #[]
      | _ => #[]
    let validates := match j.getObjVal? "validates" with
      | Except.ok arr => parseNameArray arr |>.toOption |>.getD #[]
      | _ => #[]
    let dependsOn := match j.getObjVal? "dependsOn" with
      | Except.ok arr => parseNameArray arr |>.toOption |>.getD #[]
      | _ => #[]
    let axiomDeps := match j.getObjVal? "axiomDeps" with
      | Except.ok arr => parseNameArray arr |>.toOption |>.getD #[]
      | _ => #[]
    pure (DeclKind.apiTheorem {
      kind := kindOpt
      bridgingDirection := dirOpt
      assumes, proves, validates, dependsOn, axiomDeps
      hasSorry
    })
  | _ => throw s!"unknown kind: {kindStr}"

  return (name, { kind := declKind, coverage })

/-- Parse all entries from JSON -/
def parseEntries (json : Json) : Except String (NameMap APIMeta) := do
  let entriesJson ← json.getObjVal? "entries"
  let entriesArr ← entriesJson.getArr?

  let mut entries : NameMap APIMeta := {}
  for entryJson in entriesArr do
    match parseEntry entryJson with
    | Except.ok (name, m) => entries := entries.insert name m
    | Except.error _ => pure ()  -- Skip invalid entries

  return entries

/-!
## Save/Load API
-/

/-- Get the metadata file path from a base path (adds .json extension) -/
def metadataPath (basePath : System.FilePath) : System.FilePath :=
  System.FilePath.mk (basePath.toString ++ ".json")

/-- Get the entries file path from a base path (adds .jsonl extension) -/
def entriesPath (basePath : System.FilePath) : System.FilePath :=
  System.FilePath.mk (basePath.toString ++ ".jsonl")

/-- Save classification results to split JSON + JSONL files.

    Creates two files from the base path:
    - `<basePath>.json`: Small metadata (version, projectName, entryCount)
    - `<basePath>.jsonl`: Pure JSONL with one entry per line

    This avoids stack overflow when serializing 280K+ entries by:
    1. Converting NameMap to Array first (avoids deep RBMap recursion)
    2. Writing one compressed JSON object per line (standard JSONL)
    3. Flushing periodically to avoid memory buildup -/
def saveClassification (entries : NameMap APIMeta) (projectName : String)
    (basePath : System.FilePath) : IO Unit := do
  let jsonPath := metadataPath basePath
  let jsonlPath := entriesPath basePath

  -- Ensure parent directory exists
  if let some parentDir := basePath.parent then
    IO.FS.createDirAll parentDir

  IO.println s!"  Saving {entries.size} entries to {basePath}.[json|jsonl]..."
  (← IO.getStdout).flush

  -- Write metadata file
  let metaJson := Json.mkObj [
    ("version", "1"),
    ("projectName", projectName),
    ("entryCount", entries.size)
  ]
  IO.FS.writeFile jsonPath (metaJson.pretty ++ "\n")

  -- Convert to array first to avoid deep recursion in RBMap traversal
  let entryArray := entries.foldl (init := #[]) fun acc name m => acc.push (name, m)

  -- Write JSONL entries file
  let handle ← IO.FS.Handle.mk jsonlPath .write
  let mut count := 0
  for (name, m) in entryArray do
    let json := apiMetaToJson name m
    handle.putStrLn json.compress
    count := count + 1
    -- Flush periodically to avoid memory buildup
    if count % 10000 == 0 then
      handle.flush
      IO.println s!"  Saved {count}/{entries.size} entries..."
      (← IO.getStdout).flush

  handle.flush
  IO.println s!"  Saved {entries.size} classification entries"
  IO.println s!"    Metadata: {jsonPath}"
  IO.println s!"    Entries:  {jsonlPath}"
  (← IO.getStdout).flush

/-- Load classification results from split JSON + JSONL files.

    Reads two files from the base path:
    - `<basePath>.json`: Metadata with version and expected entry count
    - `<basePath>.jsonl`: Pure JSONL entries, one per line

    This avoids stack overflow by parsing one line at a time. -/
def loadClassification (basePath : System.FilePath) : IO (NameMap APIMeta) := do
  let jsonPath := metadataPath basePath
  let jsonlPath := entriesPath basePath

  -- Check both files exist
  if !(← jsonPath.pathExists) then
    throw <| IO.userError s!"Classification cache metadata not found: {jsonPath}"
  if !(← jsonlPath.pathExists) then
    throw <| IO.userError s!"Classification cache entries not found: {jsonlPath}"

  -- Read and parse metadata
  let metaContent ← IO.FS.readFile jsonPath
  let metaJson ← match Json.parse metaContent with
    | Except.ok j => pure j
    | Except.error e => throw <| IO.userError s!"Failed to parse metadata: {e}"

  let expectedCount := metaJson.getObjValAs? Nat "entryCount" |>.toOption |>.getD 0
  IO.println s!"  Loading classification cache ({expectedCount} expected entries)..."
  IO.println s!"    Metadata: {jsonPath}"
  IO.println s!"    Entries:  {jsonlPath}"
  (← IO.getStdout).flush

  -- Read JSONL entries line by line
  let handle ← IO.FS.Handle.mk jsonlPath .read
  let mut entries : NameMap APIMeta := {}
  let mut count := 0

  while true do
    let line ← handle.getLine
    if line.isEmpty then break

    let trimmed := line.trimAscii.copy
    if trimmed.isEmpty then continue

    match Json.parse trimmed with
    | Except.ok json =>
      match parseEntry json with
      | Except.ok (name, m) =>
        entries := entries.insert name m
        count := count + 1
        if count % 50000 == 0 then
          IO.println s!"  Loaded {count} entries..."
          (← IO.getStdout).flush
      | Except.error _ => pure ()  -- Skip invalid entries
    | Except.error _ => pure ()  -- Skip unparseable lines

  IO.println s!"  Loaded {entries.size} entries"
  (← IO.getStdout).flush

  -- Warn if count doesn't match
  if entries.size != expectedCount then
    IO.println s!"  ⚠ Warning: Expected {expectedCount} entries, got {entries.size}"

  return entries

/-- Get the default cache base path for a project (without extension) -/
def defaultCachePath (buildDir : System.FilePath) : System.FilePath :=
  buildDir / "classification-cache"

end DocVerificationBridge.Cache
