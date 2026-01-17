-- DocVerificationBridge/Report.lean
-- Report generation for verification coverage

import Lean
import DocVerificationBridge.Types
import DocVerificationBridge.Classify
import DocVerificationBridge.Compatibility

/-!
# Report Generation

This module generates markdown reports showing:
- Classification of all tracked declarations
- Cross-references between theorems and definitions
- Coverage status
- Source code links
-/

namespace DocVerificationBridge

open Lean

/-!
## Git Platform Configuration
-/

/-- Supported Git hosting platforms -/
inductive GitPlatform where
  | github
  | gitlab
deriving Repr, BEq, Inhabited

/-- Cached git file listing for source URL lookup -/
structure GitFileCache where
  filesByName : Std.HashMap String (Array String) := {}
  allFiles : Array String := #[]

instance : Repr GitFileCache where
  reprPrec cache _ := s!"GitFileCache({cache.allFiles.size} files)"

instance : Inhabited GitFileCache where
  default := { filesByName := {}, allFiles := #[] }

/-!
## Git-based File Lookup
-/

/-- Build a git file cache by running `git ls-files` -/
def buildGitFileCache : IO GitFileCache := do
  let result ← IO.Process.output {
    cmd := "git"
    args := #["ls-files", "*.lean"]
  }
  if result.exitCode != 0 then
    return { filesByName := {}, allFiles := #[] }

  let files := result.stdout.splitOn "\n"
    |>.filter (·.endsWith ".lean")
    |>.toArray

  -- Build map from filename to paths
  let mut filesByName : Std.HashMap String (Array String) := {}
  for file in files do
    let filename := file.splitOn "/" |>.getLast!
    let existing := filesByName.getD filename #[]
    filesByName := filesByName.insert filename (existing.push file)

  return { filesByName, allFiles := files }

/-- Find a .lean file that matches a Lean name (e.g., `Batteries.AssocList` → `Batteries/Data/AssocList.lean`) -/
def findFileForName (cache : GitFileCache) (name : Name) : Option String :=
  -- Strategy: look for a file named after the last component of the name
  let rec getLastComponent : Name → Option String
    | .str _ s => some s
    | .num p _ => getLastComponent p
    | .anonymous => none

  match getLastComponent name with
  | none => none
  | some lastPart =>
    let targetFilename := lastPart ++ ".lean"
    match cache.filesByName.get? targetFilename with
    | none => none
    | some paths =>
      if paths.size == 1 then
        -- Unique match
        some paths[0]!
      else
        -- Multiple matches, try to find one that matches more of the namespace
        let nameComponents := name.components.map toString |>.reverse
        let scored := paths.map fun path =>
          -- Remove .lean extension and split by /
          -- Use dropRightCompat for cross-version compatibility
          let pathWithoutExt := if path.endsWith ".lean" then path.dropRightCompat 5 else path
          let pathParts := pathWithoutExt.splitOn "/"
          let score := nameComponents.foldl (fun acc comp =>
            if pathParts.contains comp then acc + 1 else acc) 0
          (path, score)
        -- Return the highest scoring path
        let sorted := scored.qsort (fun (_, s1) (_, s2) => s1 > s2)
        sorted[0]?.map (·.1)

/-- Find a folder that best matches a namespace -/
def findFolderForNamespace (cache : GitFileCache) (ns : Name) : Option String :=
  -- Look for any file that starts with the namespace path
  let nsPath := ns.components.map toString |> String.intercalate "/"
  -- Find files that are in or under this namespace
  let matching := cache.allFiles.filter fun f =>
    f.startsWith nsPath || f.startsWith (nsPath ++ "/")
  if matching.isEmpty then
    none
  else
    -- Return the common prefix folder
    some nsPath

/-!
## Report Configuration
-/

/-- Configuration for report generation -/
structure ReportConfig where
  outputDir : System.FilePath
  repoUrl : String
  platform : GitPlatform
  branch : String := "main"
  modules : List String := []
  /-- Cached git file listing -/
  gitCache : GitFileCache := default
  /-- Base URL for doc-gen4 API documentation (relative or absolute) -/
  docGenBaseUrl : Option String := none
deriving Repr, Inhabited

/-- Strip all trailing slashes from URL -/
def stripTrailingSlashes (s : String) : String :=
  let chars := s.toList
  let trimmed := chars.reverse.dropWhile (· == '/') |>.reverse
  -- Use foldl+push for cross-version compatibility (String.ofList doesn't exist in Lean < 4.26)
  trimmed.foldl (init := "") fun acc c => acc.push c

/-- Generate a source URL for a given file path and line number -/
def ReportConfig.sourceUrl (cfg : ReportConfig) (filePath : String) (line : Nat) : String :=
  let baseUrl := stripTrailingSlashes cfg.repoUrl
  match cfg.platform with
  | .github => s!"{baseUrl}/blob/{cfg.branch}/{filePath}#L{line}"
  | .gitlab => s!"{baseUrl}/-/blob/{cfg.branch}/{filePath}#L{line}"

/-- Generate a source URL without line number -/
def ReportConfig.sourceUrlNoLine (cfg : ReportConfig) (filePath : String) : String :=
  let baseUrl := stripTrailingSlashes cfg.repoUrl
  match cfg.platform with
  | .github => s!"{baseUrl}/blob/{cfg.branch}/{filePath}"
  | .gitlab => s!"{baseUrl}/-/blob/{cfg.branch}/{filePath}"

/-- Generate a folder URL (tree view) -/
def ReportConfig.folderUrl (cfg : ReportConfig) (folderPath : String) : String :=
  let baseUrl := stripTrailingSlashes cfg.repoUrl
  match cfg.platform with
  | .github => s!"{baseUrl}/tree/{cfg.branch}/{folderPath}"
  | .gitlab => s!"{baseUrl}/-/tree/{cfg.branch}/{folderPath}"

/-- Generate a doc-gen4 documentation URL for a declaration.
    The URL format is: <baseUrl>/<module-path>.html#<declaration-name>
    Example: ../api/Batteries/Data/List/Basic.html#List.Chain -/
def ReportConfig.docGenUrl (cfg : ReportConfig) (env : Environment) (name : Name) : Option String :=
  cfg.docGenBaseUrl.map fun baseUrl =>
    -- Get module name for this declaration
    let modulePath := match env.getModuleIdxFor? name with
      | some modIdx =>
        let modName := env.header.moduleNames[modIdx.toNat]!
        -- Convert module name to path: Batteries.Data.List.Basic → Batteries/Data/List/Basic
        modName.components.map toString |> String.intercalate "/"
      | none =>
        -- Fallback: use the name's namespace path
        let rec getModuleParts : Name → List String
          | .str p s => s :: getModuleParts p
          | .num p _ => getModuleParts p
          | .anonymous => []
        let parts := getModuleParts name |>.reverse
        if parts.length >= 2 then
          String.intercalate "/" (parts.dropLast)
        else
          "unknown"
    let base := stripTrailingSlashes baseUrl
    s!"{base}/{modulePath}.html#{name}"

/-!
## Markdown Generation Helpers
-/

/-- Convert coverage status to emoji -/
def coverageEmoji : CoverageStatus → String
  | .complete => "✅"
  | .axiomDependent => "⚠️"
  | .partialCoverage => "🔄"
  | .unverified => "❌"
  | .specOnly => "📝"
  | .notApplicable => "N/A"

/-- Convert a Lean name to a valid HTML anchor ID -/
def nameToAnchor (name : Name) : String :=
  name.toString.map fun c => if c == '.' then '-' else c

/-- Convert a Lean module name to a file path -/
def moduleToFilePath (moduleName : Name) : String :=
  let components := moduleName.components.map toString
  String.intercalate "/" components ++ ".lean"

/-- Convert a namespace to a folder path -/
def namespaceToFolderPath (ns : Name) : String :=
  let rec collectComponents (n : Name) (acc : List String) : List String :=
    match n with
    | .str p s => collectComponents p (s :: acc)
    | .num p _ => collectComponents p acc
    | .anonymous => acc
  String.intercalate "/" (collectComponents ns [])

/-- Infer source file path from declaration name when module lookup fails -/
def inferFilePathFromName (name : Name) : String :=
  -- Extract the module path from the name (all but the last component)
  let rec getModuleParts : Name → List String
    | .str p s => s :: getModuleParts p
    | .num p _ => getModuleParts p
    | .anonymous => []
  let parts := getModuleParts name |>.reverse
  -- The declaration name typically has the module path as prefix
  if parts.length >= 2 then
    String.intercalate "/" (parts.dropLast) ++ ".lean"
  else if parts.length == 1 then
    parts.head! ++ ".lean"
  else
    "unknown.lean"

/-- Get the starting line number for a declaration -/
def getDeclarationLine (env : Environment) (name : Name) : Option Nat := do
  let ranges ← Lean.declRangeExt.find? (level := .exported) env name <|>
                Lean.declRangeExt.find? (level := .server) env name
  return ranges.selectionRange.pos.line

/-- Helper function to display a name nicely -/
def displayName (name : Name) : String :=
  name.toString

/-- Format a symbol reference as a cross-reference link -/
def symbolLink (name : Name) (displayText : String) : String :=
  let anchor := nameToAnchor name
  s!"[{displayText}](#{anchor})"

/-- Format an array of names as cross-reference links with <br> separators -/
def formatSymbolLinks (names : Array Name) (shorten : Name → String) : String :=
  if names.isEmpty then "—"
  else
    let links := names.map fun n =>
      let display := shorten n
      let anchor := nameToAnchor n
      "[`" ++ display ++ "`](#" ++ anchor ++ ")"
    links.toList |> String.intercalate "<br>"

/-- Build a name cell with optional source link and anchor attribute -/
def buildNameCell (displayedName : String) (url : Option String) (anchorId : String) : String :=
  let attrPart := "{#" ++ anchorId ++ "}"
  match url with
  | some u => "[`" ++ displayedName ++ "`](" ++ u ++ ")" ++ attrPart
  | none => "`" ++ displayedName ++ "`" ++ attrPart

/-!
## Main Report Generation
-/

/-- Generate a flat markdown row for an API entry with cross-references -/
def flatEntryToMarkdownXRef (env : Environment) (cfg : Option ReportConfig) (name : Name)
    (m : APIMeta) (provedBy : Array Name) (rootNs : Name) (entries : NameMap APIMeta) : String :=
  -- Strip the root namespace from displayed name
  let shortName := name.replacePrefix rootNs .anonymous
  let displayedName := if shortName.isAnonymous then displayName name else toString shortName

  -- Generate anchor ID for this definition
  let anchorId := nameToAnchor name

  -- Shorten helper for names
  let shorten (n : Name) : String :=
    let short := n.replacePrefix rootNs .anonymous
    if short.isAnonymous then displayName n else toString short

  -- Build anchor attribute suffix
  let anchorAttr := "{#" ++ anchorId ++ "}"

  -- Get source location: prefer git-based lookup, fallback to module-based
  -- Also generate doc-gen4 documentation link if configured
  let (sourceLink, docLink) := match cfg with
    | some config =>
      -- Try git-based file lookup first (most accurate)
      let filePath := match findFileForName config.gitCache name with
        | some gitPath => gitPath
        | none =>
          -- Fallback: try module-based lookup
          match env.getModuleIdxFor? name with
          | some modIdx =>
            let modName := env.header.moduleNames[modIdx.toNat]!
            moduleToFilePath modName
          | none =>
            -- Last resort: infer from declaration name
            inferFilePathFromName name
      -- Try to get line number from declaration ranges
      let url := match getDeclarationLine env name with
        | some line => config.sourceUrl filePath line
        | none => config.sourceUrlNoLine filePath
      let srcLink := s!"[`{displayedName}`]({url})" ++ anchorAttr
      -- Generate doc-gen4 link if configured
      let docLnk := match config.docGenUrl env name with
        | some docUrl => s!" [:material-book-open-page-variant:]({docUrl} \"API Documentation\")"
        | none => ""
      (srcLink, docLnk)
    | none => (s!"`{displayedName}`" ++ anchorAttr, "")

  -- If this is a theorem, emit a row with TheoremKind, Assumes, Proves, Validates, and Depends On
  if m.isTheorem then
    let assumesCell := formatSymbolLinks m.assumes shorten
    let provesCell := formatSymbolLinks m.proves shorten
    let validatesCell := match m.theoremKind? with
      | some .bridgingProperty => formatSymbolLinks m.validates shorten
      | _ => "—"
    let dependsOnCell := formatSymbolLinks m.dependsOn shorten
    let tk := match m.theoremKind?, m.bridgingDirection? with
      | some .computationalProperty, _ => "computational"
      | some .mathematicalProperty, _ => "mathematical"
      | some .bridgingProperty, some .sound => "bridging (sound)"
      | some .bridgingProperty, some .complete => "bridging (complete)"
      | some .bridgingProperty, some .iff => "bridging (iff)"
      | some .bridgingProperty, none => "bridging"
      | some .soundnessProperty, _ => "soundness"
      | some .completenessProperty, _ => "completeness"
      | none, _ => "—"
    -- Add sorry indicator if proof contains sorry
    let sorryIndicator := if m.hasSorry then " ⚠️" else ""
    s!"| {sourceLink}{docLink}{sorryIndicator} | {tk} | {assumesCell} | {provesCell} | {validatesCell} | {dependsOnCell} |\n"
  else
    -- Definition/type: show theorem counts by kind with expandable list
    -- Group theorems by kind
    let init : Array Name × Array Name × Array Name × Array Name × Array Name × Array Name :=
      (#[], #[], #[], #[], #[], #[])
    let grouped := provedBy.foldl (fun (comp, math, bridge, sound, complete, other) thm =>
      match entries.find? thm with
      | some thmMeta =>
        match thmMeta.theoremKind? with
        | some .computationalProperty => (comp.push thm, math, bridge, sound, complete, other)
        | some .mathematicalProperty => (comp, math.push thm, bridge, sound, complete, other)
        | some .bridgingProperty => (comp, math, bridge.push thm, sound, complete, other)
        | some .soundnessProperty => (comp, math, bridge, sound.push thm, complete, other)
        | some .completenessProperty => (comp, math, bridge, sound, complete.push thm, other)
        | none => (comp, math, bridge, sound, complete, other.push thm)
      | none => (comp, math, bridge, sound, complete, other)
    ) init
    let (compThms, mathThms, bridgeThms, soundThms, completeThms, _otherThms) := grouped

    -- Format tally summary
    let tallyParts : List String := []
      |> (if compThms.size > 0 then (· ++ [s!"{compThms.size} comp"]) else id)
      |> (if mathThms.size > 0 then (· ++ [s!"{mathThms.size} math"]) else id)
      |> (if bridgeThms.size > 0 then (· ++ [s!"{bridgeThms.size} bridge"]) else id)
      |> (if soundThms.size > 0 then (· ++ [s!"{soundThms.size} sound"]) else id)
      |> (if completeThms.size > 0 then (· ++ [s!"{completeThms.size} complete"]) else id)

    -- Build expandable cell if there are theorems
    let tallyCell := if tallyParts.isEmpty then "—"
      else
        let summary := tallyParts |> String.intercalate ", "
        -- Build theorem list grouped by kind
        let formatGroup (label : String) (thms : Array Name) : String :=
          if thms.isEmpty then ""
          else
            let links := thms.map fun n =>
              let anchor := nameToAnchor n
              let display := shorten n
              s!"<a href=\"#{anchor}\"><code>{display}</code></a>"
            s!"<strong>{label}:</strong> " ++ (links.toList |> String.intercalate ", ") ++ "<br>"
        let details := formatGroup "comp" compThms ++
                       formatGroup "math" mathThms ++
                       formatGroup "bridge" bridgeThms ++
                       formatGroup "sound" soundThms ++
                       formatGroup "complete" completeThms
        s!"<details><summary>{summary}</summary>{details}</details>"

    -- Category display from DeclKind
    let catDisplay := match m.kind with
      | .apiType .mathematicalAbstraction => "MathAb"
      | .apiType .computationalDatatype => "CompData"
      | .apiDef ⟨.mathematicalDefinition, _, _⟩ => "MathDef"
      | .apiDef ⟨.computationalOperation, _, _⟩ => "CompOp"
      | .apiTheorem _ => "—"  -- shouldn't happen for defs
    -- Add sorry indicator if definition contains sorry
    let sorryIndicator := if m.hasSorry then " ⚠️" else ""
    s!"| {sourceLink}{docLink}{sorryIndicator} | {catDisplay} | {tallyCell} |\n"

/-- Generate the full markdown report with namespace hierarchy and source links -/
def generateReport (env : Environment) (entries : NameMap APIMeta)
    (cfg : Option ReportConfig := none) (title : String := "API Coverage Report")
    (modules : List String := []) : String := Id.run do
  let provedByMap := computeProvedByMap entries

  -- Header
  let mut output := s!"# {title}\n\n"

  -- Quick navigation for large reports
  output := output ++ "!!! tip \"Quick Navigation\"\n\n"
  output := output ++ "    📊 **[View Summary Page →](summary.html)** for a quick overview of statistics\n\n"
  output := output ++ "    ⬇️ [Jump to Summary](#summary) at the bottom of this page\n\n"

  -- Reproducibility section
  unless modules.isEmpty do
    let moduleList := modules |> String.intercalate " "
    output := output ++ "## Reproducibility\n\n"
    output := output ++ "To regenerate this report, run:\n\n"
    match cfg with
    | some config =>
      let platformStr := match config.platform with | .github => "github" | .gitlab => "gitlab"
      output := output ++ s!"```bash\nlake exe doc-verification-bridge --output {config.outputDir} --repo {config.repoUrl} --platform {platformStr} {moduleList}\n```\n\n"
    | none =>
      output := output ++ s!"```bash\nlake exe doc-verification-bridge {moduleList}\n```\n\n"

  -- Four-Category Ontology introduction
  output := output ++ "This report is organized according to a **Four-Category Ontology** inspired by E.J. Lowe:\n\n"
  output := output ++ "| Category | Abbrev | Lowe's Analog | Description |\n"
  output := output ++ "|----------|--------|---------------|-------------|\n"
  output := output ++ "| Mathematical Abstractions | MathAb | Kinds | Abstract types, Prop-based structures |\n"
  output := output ++ "| Computational Datatypes | CompData | Objects | Concrete data structures |\n"
  output := output ++ "| Mathematical Definitions | MathDef | Attributes | Prop-returning predicates, relations, properties |\n"
  output := output ++ "| Computational Operations | CompOp | Modes | Computable functions, Bool predicates, algorithms |\n\n"
  output := output ++ "**Bridging theorems** prove the fundamental ontological relations between levels.\n\n"

  -- Top-level section counter
  let mut sectionNum : Nat := 0

  -- Legend
  sectionNum := sectionNum + 1
  output := output ++ s!"## {sectionNum}. Legend\n\n"
  output := output ++ "### Theorem Kinds\n\n"
  output := output ++ "| Kind | Description |\n"
  output := output ++ "|------|-------------|\n"
  output := output ++ "| computational | Proves algebraic laws for computational definitions (BEq, Ord, etc.) |\n"
  output := output ++ "| mathematical | Proves abstract mathematical properties |\n"
  output := output ++ "| bridging | Bridges Prop specifications with Bool computations |\n"
  output := output ++ "| soundness | Proves user type correctly embeds into external spec |\n"
  output := output ++ "| completeness | Proves external spec can be represented by user type |\n\n"
  output := output ++ "### Theorem Columns\n\n"
  output := output ++ "| Column | Description |\n"
  output := output ++ "|--------|-------------|\n"
  output := output ++ "| Assumes | Definitions the theorem takes as preconditions/hypotheses |\n"
  output := output ++ "| Proves | Definitions the theorem establishes properties about |\n"
  output := output ++ "| Validates | Bool-returning functions validated by bridging theorems |\n"
  output := output ++ "| Depends On | Other theorems/lemmas used in the proof |\n\n"
  output := output ++ "### Definition Categories\n\n"
  output := output ++ "| Category | Description |\n"
  output := output ++ "|----------|-------------|\n"
  output := output ++ "| MathAb | Mathematical abstraction (Prop-based type class/structure) |\n"
  output := output ++ "| CompData | Computational datatype (data-carrying structure) |\n"
  output := output ++ "| MathDef | Mathematical definition (returns Prop) |\n"
  output := output ++ "| CompOp | Computational operation (returns non-Prop) |\n\n"

  -- Convert to array for sorting
  let entryList := entries.foldl (fun acc name m => acc.push (name, m)) #[]

  -- Helper to check if an entry is a theorem
  let isTheorem := fun (m : APIMeta) => m.isTheorem
  -- Helper to check if an entry is a definition
  let isDefinition := fun (m : APIMeta) => !isTheorem m

  -- Get the source file for each entry using git cache
  let getFilePath (config : ReportConfig) (name : Name) : String :=
    match findFileForName config.gitCache name with
    | some path => path
    | none =>
      -- Fallback to module-based lookup or inference
      match env.getModuleIdxFor? name with
      | some modIdx =>
        let modName := env.header.moduleNames[modIdx.toNat]!
        moduleToFilePath modName
      | none => inferFilePathFromName name

  -- Helper to get namespace from a name (everything except last component)
  let getNamespace (name : Name) : Name :=
    match name with
    | .str parent _ => parent
    | .num parent _ => parent
    | .anonymous => .anonymous

  -- Group entries by file
  let mut fileGroups : Std.HashMap String (Array (Name × APIMeta)) := {}
  match cfg with
  | some config =>
    for (name, apiMeta) in entryList do
      let filePath := getFilePath config name
      let existing := fileGroups.getD filePath #[]
      fileGroups := fileGroups.insert filePath (existing.push (name, apiMeta))
  | none =>
    -- Without config, group by inferred file path
    for (name, apiMeta) in entryList do
      let filePath := inferFilePathFromName name
      let existing := fileGroups.getD filePath #[]
      fileGroups := fileGroups.insert filePath (existing.push (name, apiMeta))

  -- Sort file paths
  let sortedFiles := fileGroups.toArray.map (·.1) |>.qsort (· < ·)

  let mut fileNum : Nat := sectionNum

  for filePath in sortedFiles do
    let fileEntries := fileGroups.getD filePath #[]
    if fileEntries.isEmpty then continue

    fileNum := fileNum + 1

    -- File section header - link to the file
    let fileHeader := match cfg with
      | some config =>
        let url := config.sourceUrlNoLine filePath
        -- Use just the filename for display, full path in link
        let displayName := filePath.splitOn "/" |>.getLast!.replace ".lean" ""
        s!"## {fileNum}. [{displayName}]({url})\n\n"
      | none =>
        let displayName := filePath.splitOn "/" |>.getLast!.replace ".lean" ""
        s!"## {fileNum}. {displayName}\n\n"
    output := output ++ fileHeader

    -- Group entries by namespace within this file
    let mut nsGroups : Std.HashMap Name (Array (Name × APIMeta)) := {}
    for (name, apiMeta) in fileEntries do
      let ns := getNamespace name
      let existing := nsGroups.getD ns #[]
      nsGroups := nsGroups.insert ns (existing.push (name, apiMeta))

    -- Sort namespaces
    let sortedNs := nsGroups.toArray.map (·.1) |>.qsort (fun a b => a.toString < b.toString)

    let mut nsNum : Nat := 0

    for ns in sortedNs do
      let nsEntries := nsGroups.getD ns #[]
      if nsEntries.isEmpty then continue

      nsNum := nsNum + 1

      -- Namespace subsection header
      -- Note: Namespaces are syntactic groupings, not declarations, so we can't
      -- reliably get a line number for them. We just link to the file.
      -- (A file can have multiple `namespace Foo ... end Foo` blocks, and
      -- namespaces don't have ConstantInfo entries in the environment.)
      let nsDisplay := if ns.isAnonymous then "(root)" else ns.toString
      let nsHeader := match cfg with
        | some config =>
          let url := config.sourceUrlNoLine filePath
          s!"### {fileNum}.{nsNum}. [{nsDisplay}]({url})\n\n"
        | none => s!"### {fileNum}.{nsNum}. {nsDisplay}\n\n"
      output := output ++ nsHeader

      -- Separate definitions and theorems
      let defs := nsEntries.filter (fun (_, m) => isDefinition m)
        |>.qsort (fun (n1, _) (n2, _) => n1.toString < n2.toString)
      let theorems := nsEntries.filter (fun (_, m) => isTheorem m)
        |>.qsort (fun (n1, _) (n2, _) => n1.toString < n2.toString)

      let mut subNum : Nat := 0

      -- Definitions table
      unless defs.isEmpty do
        subNum := subNum + 1
        output := output ++ s!"#### {fileNum}.{nsNum}.{subNum}. Definitions\n\n"
        output := output ++ "<div class=\"table-wrapper\" markdown=\"1\">\n\n"
        output := output ++ "| Name | Category | Theorems |\n"
        output := output ++ "|------|----------|----------|\n"
        for (name, m) in defs do
          let provedBy := provedByMap.find? name |>.getD #[]
          output := output ++ flatEntryToMarkdownXRef env cfg name m provedBy ns entries
        output := output ++ "\n</div>\n\n"

      -- Theorems table
      unless theorems.isEmpty do
        subNum := subNum + 1
        output := output ++ s!"#### {fileNum}.{nsNum}.{subNum}. Theorems/Lemmas\n\n"
        output := output ++ "<div class=\"table-wrapper\" markdown=\"1\">\n\n"
        output := output ++ "| Name | Kind | Assumes | Proves | Validates | Depends On |\n"
        output := output ++ "|------|------|---------|--------|-----------|------------|\n"
        for (name, m) in theorems do
          output := output ++ flatEntryToMarkdownXRef env cfg name m #[] ns entries
        output := output ++ "\n</div>\n\n"

  -- Summary stats: tally by theorem kind and definition category
  -- Also collect names for detailed lists
  let mut totalDefs := 0
  let mut totalTheorems := 0
  let mut mathAbstractions : Array Name := #[]
  let mut compDatatypes : Array Name := #[]
  let mut mathDefs : Array Name := #[]
  let mut compOps : Array Name := #[]
  let mut compTheorems : Array Name := #[]
  let mut mathTheorems : Array Name := #[]
  let mut bridgingTheorems : Array Name := #[]
  let mut soundnessTheorems : Array Name := #[]
  let mut completenessTheorems : Array Name := #[]
  let mut unclassifiedTheorems : Array Name := #[]

  for (name, m) in entryList do
    if m.isTheorem then
      totalTheorems := totalTheorems + 1
      match m.theoremKind? with
      | some .computationalProperty => compTheorems := compTheorems.push name
      | some .mathematicalProperty => mathTheorems := mathTheorems.push name
      | some .bridgingProperty => bridgingTheorems := bridgingTheorems.push name
      | some .soundnessProperty => soundnessTheorems := soundnessTheorems.push name
      | some .completenessProperty => completenessTheorems := completenessTheorems.push name
      | none => unclassifiedTheorems := unclassifiedTheorems.push name
    else
      totalDefs := totalDefs + 1
      match m.kind with
      | .apiType .mathematicalAbstraction => mathAbstractions := mathAbstractions.push name
      | .apiType .computationalDatatype => compDatatypes := compDatatypes.push name
      | .apiDef ⟨.mathematicalDefinition, _, _⟩ => mathDefs := mathDefs.push name
      | .apiDef ⟨.computationalOperation, _, _⟩ => compOps := compOps.push name
      | _ => pure ()

  -- Helper to format a list of names as links (HTML unordered list for details sections)
  let formatNameLinks (names : Array Name) : String :=
    if names.isEmpty then "<p><em>(none)</em></p>"
    else
      let sorted := names.qsort (fun a b => a.toString < b.toString)
      let links := sorted.map fun n =>
        let anchor := nameToAnchor n
        let display := displayName n
        s!"<li><a href=\"#{anchor}\"><code>{display}</code></a></li>"
      "<ul>\n" ++ (links.toList |> String.intercalate "\n") ++ "\n</ul>"

  let summaryNum := fileNum + 1
  output := output ++ s!"<a id=\"summary\"></a>\n\n"
  output := output ++ s!"## {summaryNum}. Summary\n\n"
  output := output ++ "📊 **[View full Summary Page →](summary.html)** for a standalone quick-loading summary\n\n"

  output := output ++ "### Definitions by Category\n\n"
  output := output ++ s!"| Category | Count |\n"
  output := output ++ s!"|----------|-------|\n"
  output := output ++ s!"| Mathematical Abstractions | {mathAbstractions.size} |\n"
  output := output ++ s!"| Computational Datatypes | {compDatatypes.size} |\n"
  output := output ++ s!"| Mathematical Definitions | {mathDefs.size} |\n"
  output := output ++ s!"| Computational Operations | {compOps.size} |\n"
  output := output ++ s!"| **Total Definitions** | **{totalDefs}** |\n\n"

  output := output ++ "<details>\n<summary><strong>Mathematical Abstractions</strong> (click to expand)</summary>\n"
  output := output ++ formatNameLinks mathAbstractions ++ "\n</details>\n\n"

  output := output ++ "<details>\n<summary><strong>Computational Datatypes</strong> (click to expand)</summary>\n"
  output := output ++ formatNameLinks compDatatypes ++ "\n</details>\n\n"

  output := output ++ "<details>\n<summary><strong>Mathematical Definitions</strong> (click to expand)</summary>\n"
  output := output ++ formatNameLinks mathDefs ++ "\n</details>\n\n"

  output := output ++ "<details>\n<summary><strong>Computational Operations</strong> (click to expand)</summary>\n"
  output := output ++ formatNameLinks compOps ++ "\n</details>\n\n"

  output := output ++ "### Theorems by Kind\n\n"
  output := output ++ s!"| Kind | Count |\n"
  output := output ++ s!"|------|-------|\n"
  output := output ++ s!"| Computational | {compTheorems.size} |\n"
  output := output ++ s!"| Mathematical | {mathTheorems.size} |\n"
  output := output ++ s!"| Bridging | {bridgingTheorems.size} |\n"
  output := output ++ s!"| Soundness | {soundnessTheorems.size} |\n"
  output := output ++ s!"| Completeness | {completenessTheorems.size} |\n"
  output := output ++ s!"| Unclassified | {unclassifiedTheorems.size} |\n"
  output := output ++ s!"| **Total Theorems** | **{totalTheorems}** |\n\n"

  output := output ++ "<details>\n<summary><strong>Computational Theorems</strong> (click to expand)</summary>\n"
  output := output ++ formatNameLinks compTheorems ++ "\n</details>\n\n"

  output := output ++ "<details>\n<summary><strong>Mathematical Theorems</strong> (click to expand)</summary>\n"
  output := output ++ formatNameLinks mathTheorems ++ "\n</details>\n\n"

  output := output ++ "<details>\n<summary><strong>Bridging Theorems</strong> (click to expand)</summary>\n"
  output := output ++ formatNameLinks bridgingTheorems ++ "\n</details>\n\n"

  output := output ++ "<details>\n<summary><strong>Soundness Theorems</strong> (click to expand)</summary>\n"
  output := output ++ formatNameLinks soundnessTheorems ++ "\n</details>\n\n"

  output := output ++ "<details>\n<summary><strong>Completeness Theorems</strong> (click to expand)</summary>\n"
  output := output ++ formatNameLinks completenessTheorems ++ "\n</details>\n\n"

  output := output ++ "<details>\n<summary><strong>Unclassified Theorems</strong> (click to expand)</summary>\n"
  output := output ++ formatNameLinks unclassifiedTheorems ++ "\n</details>\n\n"

  output

/-- Generate a standalone summary page (quick-loading, without full declaration tables) -/
def generateSummaryReport (entries : NameMap APIMeta) (projectName : String := "API Coverage") : String := Id.run do
  -- Tally definitions and theorems
  let entryList := entries.foldl (fun acc name m => acc.push (name, m)) #[]

  let mut totalDefs := 0
  let mut totalTheorems := 0
  let mut mathAbstractions := 0
  let mut compDatatypes := 0
  let mut mathDefs := 0
  let mut compOps := 0
  let mut compTheorems := 0
  let mut mathTheorems := 0
  let mut bridgingTheorems := 0
  let mut soundnessTheorems := 0
  let mut completenessTheorems := 0
  let mut unclassifiedTheorems := 0

  for (_, m) in entryList do
    if m.isTheorem then
      totalTheorems := totalTheorems + 1
      match m.theoremKind? with
      | some .computationalProperty => compTheorems := compTheorems + 1
      | some .mathematicalProperty => mathTheorems := mathTheorems + 1
      | some .bridgingProperty => bridgingTheorems := bridgingTheorems + 1
      | some .soundnessProperty => soundnessTheorems := soundnessTheorems + 1
      | some .completenessProperty => completenessTheorems := completenessTheorems + 1
      | none => unclassifiedTheorems := unclassifiedTheorems + 1
    else
      totalDefs := totalDefs + 1
      match m.kind with
      | .apiType .mathematicalAbstraction => mathAbstractions := mathAbstractions + 1
      | .apiType .computationalDatatype => compDatatypes := compDatatypes + 1
      | .apiDef ⟨.mathematicalDefinition, _, _⟩ => mathDefs := mathDefs + 1
      | .apiDef ⟨.computationalOperation, _, _⟩ => compOps := compOps + 1
      | _ => pure ()

  let mut output := s!"# {projectName} Summary\n\n"

  output := output ++ "This is a quick-loading summary page. For detailed per-module reports, "
  output := output ++ "see the [Module Index](index.html).\n\n"

  -- Overall stats cards (using admonitions for visual appeal)
  output := output ++ "## Overview\n\n"
  output := output ++ s!"!!! success \"Total Analyzed\"\n\n"
  output := output ++ s!"    **{totalDefs + totalTheorems}** declarations analyzed\n\n"
  output := output ++ s!"    - {totalDefs} definitions\n"
  output := output ++ s!"    - {totalTheorems} theorems\n\n"

  output := output ++ "## Definitions by Category\n\n"
  output := output ++ "| Category | Count | Percentage |\n"
  output := output ++ "|----------|------:|:----------:|\n"
  let defPct := fun n => if totalDefs > 0 then s!"{(n * 100) / totalDefs}%" else "—"
  output := output ++ s!"| Mathematical Abstractions | {mathAbstractions} | {defPct mathAbstractions} |\n"
  output := output ++ s!"| Computational Datatypes | {compDatatypes} | {defPct compDatatypes} |\n"
  output := output ++ s!"| Mathematical Definitions | {mathDefs} | {defPct mathDefs} |\n"
  output := output ++ s!"| Computational Operations | {compOps} | {defPct compOps} |\n"
  output := output ++ s!"| **Total Definitions** | **{totalDefs}** | **100%** |\n\n"

  output := output ++ "## Theorems by Kind\n\n"
  output := output ++ "| Kind | Count | Percentage |\n"
  output := output ++ "|------|------:|:----------:|\n"
  let thmPct := fun n => if totalTheorems > 0 then s!"{(n * 100) / totalTheorems}%" else "—"
  output := output ++ s!"| Computational | {compTheorems} | {thmPct compTheorems} |\n"
  output := output ++ s!"| Mathematical | {mathTheorems} | {thmPct mathTheorems} |\n"
  output := output ++ s!"| Bridging | {bridgingTheorems} | {thmPct bridgingTheorems} |\n"
  output := output ++ s!"| Soundness | {soundnessTheorems} | {thmPct soundnessTheorems} |\n"
  output := output ++ s!"| Completeness | {completenessTheorems} | {thmPct completenessTheorems} |\n"
  output := output ++ s!"| Unclassified | {unclassifiedTheorems} | {thmPct unclassifiedTheorems} |\n"
  output := output ++ s!"| **Total Theorems** | **{totalTheorems}** | **100%** |\n\n"

  -- Four-Category explanation (brief)
  output := output ++ "## Classification System\n\n"
  output := output ++ "This report uses a **Four-Category Ontology** inspired by E.J. Lowe:\n\n"
  output := output ++ "| Category | Description |\n"
  output := output ++ "|----------|-------------|\n"
  output := output ++ "| **MathAb** | Mathematical Abstractions — abstract types, Prop-based structures |\n"
  output := output ++ "| **CompData** | Computational Datatypes — concrete data structures |\n"
  output := output ++ "| **MathDef** | Mathematical Definitions — Prop-returning predicates |\n"
  output := output ++ "| **CompOp** | Computational Operations — computable functions |\n\n"

  output := output ++ "---\n\n"
  output := output ++ "[📋 Browse Modules →](index.html)\n"

  output

/-!
## Per-Module Report Generation (for fast loading)
-/

/-- Statistics for a single module/file -/
structure ModuleStats where
  filePath : String
  totalDefs : Nat := 0
  mathAbstractions : Nat := 0
  compDatatypes : Nat := 0
  mathDefs : Nat := 0
  compOps : Nat := 0
  totalTheorems : Nat := 0
  compTheorems : Nat := 0
  mathTheorems : Nat := 0
  bridgingTheorems : Nat := 0
  soundnessTheorems : Nat := 0
  completenessTheorems : Nat := 0
  unclassifiedTheorems : Nat := 0
  -- Sorry tracking
  defsWithSorry : Nat := 0
  theoremsWithSorry : Nat := 0
  deriving Repr, Inhabited

/-- Convert file path to a safe filename for markdown -/
def filePathToSafeFilename (filePath : String) : String :=
  filePath.replace "/" "_" |>.replace ".lean" ""

/-- Generate a single module's verification report -/
def generateModuleReport (env : Environment) (filePath : String)
    (entries : Array (Name × APIMeta)) (cfg : Option ReportConfig)
    (allEntries : NameMap APIMeta) (provedByMap : NameMap (Array Name)) : String × ModuleStats := Id.run do
  let isTheorem := fun (m : APIMeta) => m.isTheorem
  let isDefinition := fun (m : APIMeta) => !isTheorem m

  let getNamespace (name : Name) : Name :=
    match name with
    | .str parent _ => parent
    | .num parent _ => parent
    | .anonymous => .anonymous

  -- Calculate stats
  let mut stats : ModuleStats := { filePath }
  for (_, m) in entries do
    if m.isTheorem then
      stats := { stats with totalTheorems := stats.totalTheorems + 1 }
      if m.hasSorry then
        stats := { stats with theoremsWithSorry := stats.theoremsWithSorry + 1 }
      match m.theoremKind? with
      | some .computationalProperty => stats := { stats with compTheorems := stats.compTheorems + 1 }
      | some .mathematicalProperty => stats := { stats with mathTheorems := stats.mathTheorems + 1 }
      | some .bridgingProperty => stats := { stats with bridgingTheorems := stats.bridgingTheorems + 1 }
      | some .soundnessProperty => stats := { stats with soundnessTheorems := stats.soundnessTheorems + 1 }
      | some .completenessProperty => stats := { stats with completenessTheorems := stats.completenessTheorems + 1 }
      | none => stats := { stats with unclassifiedTheorems := stats.unclassifiedTheorems + 1 }
    else
      stats := { stats with totalDefs := stats.totalDefs + 1 }
      if m.hasSorry then
        stats := { stats with defsWithSorry := stats.defsWithSorry + 1 }
      match m.kind with
      | .apiType .mathematicalAbstraction => stats := { stats with mathAbstractions := stats.mathAbstractions + 1 }
      | .apiType .computationalDatatype => stats := { stats with compDatatypes := stats.compDatatypes + 1 }
      | .apiDef ⟨.mathematicalDefinition, _, _⟩ => stats := { stats with mathDefs := stats.mathDefs + 1 }
      | .apiDef ⟨.computationalOperation, _, _⟩ => stats := { stats with compOps := stats.compOps + 1 }
      | _ => pure ()

  -- Generate markdown
  let displayName := filePath.splitOn "/" |>.getLast!.replace ".lean" ""
  let mut output := s!"# {displayName}\n\n"

  -- Source link
  match cfg with
  | some config =>
    let url := config.sourceUrlNoLine filePath
    output := output ++ s!"📁 **Source:** [{filePath}]({url})\n\n"
  | none =>
    output := output ++ s!"📁 **Source:** `{filePath}`\n\n"

  -- Collect lists for expandable sections (sorted alphabetically)
  let defNames := entries.filter (fun (_, m) => !m.isTheorem) |>.map (·.1)
    |>.qsort (fun a b => a.toString < b.toString)
  let thmNames := entries.filter (fun (_, m) => m.isTheorem) |>.map (·.1)
    |>.qsort (fun a b => a.toString < b.toString)
  let defsSorry := entries.filter (fun (_, m) => !m.isTheorem && m.hasSorry) |>.map (·.1)
    |>.qsort (fun a b => a.toString < b.toString)
  let thmsSorry := entries.filter (fun (_, m) => m.isTheorem && m.hasSorry) |>.map (·.1)
    |>.qsort (fun a b => a.toString < b.toString)

  -- Helper to format name list as links
  let formatNameLinks := fun (names : Array Name) =>
    let links := names.map fun n =>
      let anchor := nameToAnchor n
      let display := n.toString.splitOn "." |>.getLast!
      s!"[`{display}`](#{anchor})"
    String.intercalate ", " links.toList

  -- Quick stats with expandable lists
  output := output ++ "## Statistics\n\n"
  output := output ++ s!"| Metric | Count |\n"
  output := output ++ s!"|--------|------:|\n"

  -- Definitions with expandable list
  if defNames.size > 0 then
    output := output ++ s!"| <details><summary>Definitions</summary>{formatNameLinks defNames}</details> | {stats.totalDefs} |\n"
  else
    output := output ++ s!"| Definitions | {stats.totalDefs} |\n"

  -- Theorems with expandable list
  if thmNames.size > 0 then
    output := output ++ s!"| <details><summary>Theorems</summary>{formatNameLinks thmNames}</details> | {stats.totalTheorems} |\n"
  else
    output := output ++ s!"| Theorems | {stats.totalTheorems} |\n"

  output := output ++ s!"| **Total** | **{stats.totalDefs + stats.totalTheorems}** |\n"

  -- Show sorry stats if any with expandable list
  let totalSorry := stats.defsWithSorry + stats.theoremsWithSorry
  if totalSorry > 0 then
    let allSorry := defsSorry ++ thmsSorry
    output := output ++ s!"| <details><summary>⚠️ With sorry</summary>{formatNameLinks allSorry}</details> | {totalSorry} |\n"
  output := output ++ "\n"

  -- Group by namespace
  let mut nsGroups : Std.HashMap Name (Array (Name × APIMeta)) := {}
  for (name, apiMeta) in entries do
    let ns := getNamespace name
    let existing := nsGroups.getD ns #[]
    nsGroups := nsGroups.insert ns (existing.push (name, apiMeta))

  let sortedNs := nsGroups.toArray.map (·.1) |>.qsort (fun a b => a.toString < b.toString)

  for ns in sortedNs do
    let nsEntries := nsGroups.getD ns #[]
    if nsEntries.isEmpty then continue

    let nsDisplay := if ns.isAnonymous then "(root)" else ns.toString
    output := output ++ s!"## {nsDisplay}\n\n"

    let defs := nsEntries.filter (fun (_, m) => isDefinition m)
      |>.qsort (fun (n1, _) (n2, _) => n1.toString < n2.toString)
    let theorems := nsEntries.filter (fun (_, m) => isTheorem m)
      |>.qsort (fun (n1, _) (n2, _) => n1.toString < n2.toString)

    unless defs.isEmpty do
      output := output ++ "### Definitions\n\n"
      output := output ++ "| Name | Category | Theorems |\n"
      output := output ++ "|------|----------|----------|\n"
      for (name, m) in defs do
        let provedBy := provedByMap.find? name |>.getD #[]
        output := output ++ flatEntryToMarkdownXRef env cfg name m provedBy ns allEntries
      output := output ++ "\n"

    unless theorems.isEmpty do
      output := output ++ "### Theorems\n\n"
      output := output ++ "| Name | Kind | Assumes | Proves | Validates | Depends On |\n"
      output := output ++ "|------|------|---------|--------|-----------|------------|\n"
      for (name, m) in theorems do
        output := output ++ flatEntryToMarkdownXRef env cfg name m #[] ns allEntries
      output := output ++ "\n"

  output := output ++ "---\n\n"
  output := output ++ "[← Back to Index](index.html)\n"

  (output, stats)

/-- Generate per-module verification reports (returns list of generated files and stats) -/
def generatePerModuleReports (env : Environment) (entries : NameMap APIMeta)
    (cfg : Option ReportConfig := none) : Array (String × String × ModuleStats) := Id.run do
  let entryList := entries.foldl (fun acc name m => acc.push (name, m)) #[]

  -- Compute provedByMap ONCE for all modules (was being recomputed per-module = O(n*m) bug!)
  let provedByMap := computeProvedByMap entries

  -- Get file path for each entry
  let getFilePath (config : ReportConfig) (name : Name) : String :=
    match findFileForName config.gitCache name with
    | some path => path
    | none =>
      match env.getModuleIdxFor? name with
      | some modIdx =>
        let modName := env.header.moduleNames[modIdx.toNat]!
        moduleToFilePath modName
      | none => inferFilePathFromName name

  -- Group by file
  let mut fileGroups : Std.HashMap String (Array (Name × APIMeta)) := {}
  match cfg with
  | some config =>
    for (name, apiMeta) in entryList do
      let filePath := getFilePath config name
      let existing := fileGroups.getD filePath #[]
      fileGroups := fileGroups.insert filePath (existing.push (name, apiMeta))
  | none =>
    for (name, apiMeta) in entryList do
      let filePath := inferFilePathFromName name
      let existing := fileGroups.getD filePath #[]
      fileGroups := fileGroups.insert filePath (existing.push (name, apiMeta))

  -- Generate report for each file
  let mut results : Array (String × String × ModuleStats) := #[]
  for (filePath, fileEntries) in fileGroups.toArray do
    if fileEntries.isEmpty then continue
    let (content, stats) := generateModuleReport env filePath fileEntries cfg entries provedByMap
    let safeFilename := filePathToSafeFilename filePath
    results := results.push (safeFilename, content, { stats with filePath })

  results

/-- Generate the module index page for per-module reports -/
def generateModuleIndex (allStats : Array ModuleStats) (projectName : String)
    (entries : NameMap APIMeta := {}) (_cfg : Option ReportConfig := none) : String := Id.run do
  -- Aggregate stats
  let mut totalDefs := 0
  let mut totalTheorems := 0
  let mut totalMathAb := 0
  let mut totalCompData := 0
  let mut totalMathDef := 0
  let mut totalCompOp := 0
  let mut totalCompThm := 0
  let mut totalMathThm := 0
  let mut totalBridging := 0
  let mut totalDefsWithSorry := 0
  let mut totalTheoremsWithSorry := 0

  -- Also collect sorry declarations for collapsible lists
  let mut defsWithSorry : Array (Name × APIMeta) := #[]
  let mut theoremsWithSorry : Array (Name × APIMeta) := #[]

  for s in allStats do
    totalDefs := totalDefs + s.totalDefs
    totalTheorems := totalTheorems + s.totalTheorems
    totalMathAb := totalMathAb + s.mathAbstractions
    totalCompData := totalCompData + s.compDatatypes
    totalMathDef := totalMathDef + s.mathDefs
    totalCompOp := totalCompOp + s.compOps
    totalCompThm := totalCompThm + s.compTheorems
    totalMathThm := totalMathThm + s.mathTheorems
    totalBridging := totalBridging + s.bridgingTheorems
    totalDefsWithSorry := totalDefsWithSorry + s.defsWithSorry
    totalTheoremsWithSorry := totalTheoremsWithSorry + s.theoremsWithSorry

  -- Collect sorry declarations from entries
  for (name, m) in entries do
    if m.hasSorry then
      if m.isTheorem then
        theoremsWithSorry := theoremsWithSorry.push (name, m)
      else
        defsWithSorry := defsWithSorry.push (name, m)

  let mut output := s!"# {projectName} Verification Coverage\n\n"

  -- Overall summary
  output := output ++ "## Overview\n\n"
  output := output ++ s!"| Metric | Count |\n"
  output := output ++ s!"|--------|------:|\n"
  output := output ++ s!"| **Total Declarations** | **{totalDefs + totalTheorems}** |\n"
  output := output ++ s!"| Definitions | {totalDefs} |\n"
  output := output ++ s!"| Theorems | {totalTheorems} |\n"
  output := output ++ s!"| Modules | {allStats.size} |\n\n"

  -- Sorry statistics (show only if there are any)
  let totalWithSorry := totalDefsWithSorry + totalTheoremsWithSorry
  if totalWithSorry > 0 then
    let provenDefs := totalDefs - totalDefsWithSorry
    let provenThms := totalTheorems - totalTheoremsWithSorry
    let pctProvenDefs := if totalDefs > 0 then (provenDefs * 100) / totalDefs else 100
    let pctProvenThms := if totalTheorems > 0 then (provenThms * 100) / totalTheorems else 100

    output := output ++ "### Proof Completeness\n\n"

    -- Summary table
    output := output ++ s!"| Category | Total | With Sorry | Complete | % Complete |\n"
    output := output ++ s!"|----------|------:|----------:|-------:|---------:|\n"
    output := output ++ s!"| Definitions | {totalDefs} | {totalDefsWithSorry} | {provenDefs} | {pctProvenDefs}% |\n"
    output := output ++ s!"| Theorems | {totalTheorems} | {totalTheoremsWithSorry} | {provenThms} | {pctProvenThms}% |\n\n"

    -- Collapsible list of definitions with sorry
    if totalDefsWithSorry > 0 then
      output := output ++ s!"??? warning \"Definitions with `sorry` ({totalDefsWithSorry})\"\n\n"
      -- Sort by name
      let sortedDefs := defsWithSorry.qsort (fun a b => a.1.toString < b.1.toString)
      for (name, _) in sortedDefs do
        let anchor := nameToAnchor name
        let filePath := inferFilePathFromName name
        let safeFilename := filePathToSafeFilename filePath
        output := output ++ s!"    - [`{name}`](modules/{safeFilename}.md#{anchor})\n"
      output := output ++ "\n"

    -- Collapsible list of theorems with sorry
    if totalTheoremsWithSorry > 0 then
      output := output ++ s!"??? warning \"Theorems with `sorry` ({totalTheoremsWithSorry})\"\n\n"
      -- Sort by name
      let sortedThms := theoremsWithSorry.qsort (fun a b => a.1.toString < b.1.toString)
      for (name, _) in sortedThms do
        let anchor := nameToAnchor name
        let filePath := inferFilePathFromName name
        let safeFilename := filePathToSafeFilename filePath
        output := output ++ s!"    - [`{name}`](modules/{safeFilename}.md#{anchor})\n"
      output := output ++ "\n"

  -- Definition breakdown
  output := output ++ "### Definitions by Category\n\n"
  output := output ++ "| Category | Count |\n"
  output := output ++ "|----------|------:|\n"
  output := output ++ s!"| Mathematical Abstractions | {totalMathAb} |\n"
  output := output ++ s!"| Computational Datatypes | {totalCompData} |\n"
  output := output ++ s!"| Mathematical Definitions | {totalMathDef} |\n"
  output := output ++ s!"| Computational Operations | {totalCompOp} |\n\n"

  -- Theorem breakdown
  output := output ++ "### Theorems by Kind\n\n"
  output := output ++ "| Kind | Count |\n"
  output := output ++ "|------|------:|\n"
  output := output ++ s!"| Computational | {totalCompThm} |\n"
  output := output ++ s!"| Mathematical | {totalMathThm} |\n"
  output := output ++ s!"| Bridging | {totalBridging} |\n\n"

  -- Module list with links
  output := output ++ "## Modules\n\n"
  output := output ++ "Click on a module to view its detailed verification report.\n\n"
  output := output ++ "| Module | Defs | Thms | Total |\n"
  output := output ++ "|--------|-----:|-----:|------:|\n"

  -- Sort alphabetically by file path (which gives alphabetical module order)
  let sorted := allStats.qsort (fun a b => a.filePath < b.filePath)
  for s in sorted do
    let safeFilename := filePathToSafeFilename s.filePath
    -- Show full module path (e.g., "Batteries.Classes.Cast" instead of just "Cast")
    let displayName := s.filePath.replace ".lean" "" |>.replace "/" "."
    let total := s.totalDefs + s.totalTheorems
    output := output ++ s!"| [{displayName}](modules/{safeFilename}.html) | {s.totalDefs} | {s.totalTheorems} | {total} |\n"

  output

/-!
## CSS and Index Generation
-/

/-- Generate extra.css for table styling -/
def generateExtraCss : String :=
  "/* Custom styles for API Coverage Report */
/* Generated by doc-verification-bridge */

/* Full-width content layout */
.md-main__inner {
  max-width: none;
}

.md-grid {
  max-width: none;
  margin-left: 1rem;
  margin-right: 1rem;
}

/* Full-width content area */
.md-content {
  max-width: none;
}

.md-content__inner {
  max-width: none;
  margin-left: 0;
  margin-right: 0;
}

/* Table wrapper for sticky header support */
.md-typeset .table-wrapper {
  max-height: 70vh;
  overflow-y: auto !important;
  overflow-x: auto !important;
  margin-bottom: 1rem;
  border: 1px solid var(--md-default-fg-color--lightest);
  border-radius: 4px;
  position: relative;
}

/* Override Material table scroll wrappers - make them transparent */
.md-typeset .table-wrapper .md-typeset__scrollwrap {
  overflow: visible !important;
  max-width: none !important;
}

.md-typeset .table-wrapper .md-typeset__table {
  display: block !important;
  overflow: visible !important;
}

/* Ensure table inside wrapper doesn't have extra margins */
.md-typeset .table-wrapper > table,
.md-typeset .table-wrapper table {
  margin: 0 !important;
  border: none;
}

/* Sticky thead - the entire header row stays fixed */
.md-typeset .table-wrapper thead {
  position: sticky;
  top: 0;
  z-index: 10;
}

.md-typeset .table-wrapper thead th {
  position: sticky !important;
  top: 0 !important;
  z-index: 10 !important;
  background: #ffffff !important;
  box-shadow: 0 2px 3px rgba(0, 0, 0, 0.15);
}

[data-md-color-scheme=\\\"slate\\\"] .md-typeset .table-wrapper thead th {
  background: #2e303e !important;
}

/* Tables use fixed layout for predictable column widths */
.md-typeset table:not([class]) {
  display: table;
  width: 100%;
  max-width: none;
  table-layout: fixed;
  border-collapse: separate;
  border-spacing: 0;
}

/* Base table cell styling */
.md-typeset table:not([class]) td,
.md-typeset table:not([class]) th {
  vertical-align: top;
  padding: 0.5rem 0.75rem;
}

/* Horizontal scroll for overflowing cell content */
.md-typeset table:not([class]) td > code,
.md-typeset table:not([class]) td > a {
  display: inline-block;
  max-width: 100%;
}

/* Cell content wrapper for horizontal scroll */
.md-typeset table:not([class]) td {
  overflow-x: auto;
  overflow-y: hidden;
  white-space: normal;
}

/* ===== Definition Tables (3 columns: Name, Category, Theorems) ===== */

/* Category column - fixed width for abbreviations (fits 'CompData') */
.md-typeset table:not([class]) td:nth-child(2),
.md-typeset table:not([class]) th:nth-child(2) {
  width: 120px;
  min-width: 120px;
  max-width: 120px;
  white-space: nowrap;
  text-align: center;
}

/* Theorems column (3rd in def tables) - flexible */
.md-typeset table:not([class]) td:nth-child(3),
.md-typeset table:not([class]) th:nth-child(3) {
  width: auto;
  min-width: 100px;
  white-space: normal;
  word-wrap: break-word;
}

/* ===== Theorem Tables (6 columns: Name, Kind, Assumes, Proves, Validates, Depends On) ===== */

/* Kind column (2nd in theorem tables) - narrow for abbreviations */
.md-typeset table:not([class]) th:nth-child(2):not(:last-child),
.md-typeset table:not([class]) td:nth-child(2):not(:last-child) {
  width: 100px;
  min-width: 100px;
  max-width: 100px;
  white-space: nowrap;
  text-align: center;
}

/* Name column (1st) - flexible, gets 20% of remaining space */
.md-typeset table:not([class]) td:first-child,
.md-typeset table:not([class]) th:first-child {
  width: 20%;
  min-width: 150px;
  white-space: normal;
  word-break: break-word;
}

/* Assumes/Proves/Validates/Depends On columns (3rd-6th in theorem tables) - equal flexible width */
.md-typeset table:not([class]) td:nth-child(3):not(:last-child),
.md-typeset table:not([class]) td:nth-child(4),
.md-typeset table:not([class]) td:nth-child(5),
.md-typeset table:not([class]) td:nth-child(6),
.md-typeset table:not([class]) th:nth-child(3):not(:last-child),
.md-typeset table:not([class]) th:nth-child(4),
.md-typeset table:not([class]) th:nth-child(5),
.md-typeset table:not([class]) th:nth-child(6) {
  width: 15%;
  min-width: 100px;
  white-space: normal;
  word-break: break-word;
}

/* Scrollable content within cells */
.md-typeset table:not([class]) td:nth-child(3),
.md-typeset table:not([class]) td:nth-child(4),
.md-typeset table:not([class]) td:nth-child(5),
.md-typeset table:not([class]) td:nth-child(6) {
  max-height: 200px;
  overflow-y: auto;
}

/* Responsive adjustments */
@media screen and (min-width: 76.25em) {
  .md-sidebar--primary {
    width: 12.1rem;
  }

  .md-sidebar--secondary {
    width: 12.1rem;
  }
}

/* Code styling in tables */
.md-typeset table:not([class]) code {
  font-size: 0.85em;
  word-break: break-word;
}

/* Links in tables - allow wrapping */
.md-typeset table:not([class]) a {
  word-break: break-word;
}

/* Alternate row colors for readability */
.md-typeset table:not([class]) tbody tr:nth-child(even) {
  background-color: var(--md-default-bg-color--light);
}

/* Scrollbar styling for cells */
.md-typeset table:not([class]) td::-webkit-scrollbar {
  height: 6px;
  width: 6px;
}

.md-typeset table:not([class]) td::-webkit-scrollbar-track {
  background: transparent;
}

.md-typeset table:not([class]) td::-webkit-scrollbar-thumb {
  background-color: var(--md-default-fg-color--lighter);
  border-radius: 3px;
}

.md-typeset table:not([class]) td::-webkit-scrollbar-thumb:hover {
  background-color: var(--md-default-fg-color--light);
}

/* Scrollbar styling for table wrapper */
.md-typeset .table-wrapper::-webkit-scrollbar {
  height: 8px;
  width: 8px;
}

.md-typeset .table-wrapper::-webkit-scrollbar-track {
  background: var(--md-default-bg-color--light);
  border-radius: 4px;
}

.md-typeset .table-wrapper::-webkit-scrollbar-thumb {
  background-color: var(--md-default-fg-color--lighter);
  border-radius: 4px;
}

.md-typeset .table-wrapper::-webkit-scrollbar-thumb:hover {
  background-color: var(--md-default-fg-color--light);
}

/* Firefox scrollbar styling */
.md-typeset .table-wrapper {
  scrollbar-width: thin;
  scrollbar-color: var(--md-default-fg-color--lighter) var(--md-default-bg-color--light);
}
"

/-- Generate index.md -/
def generateIndexMd (projectName : String := "API Coverage") : String :=
  s!"# {projectName}

Welcome to the API Coverage documentation.

## Quick Links

- [API Coverage Report](API_Coverage.html) - Full coverage report with cross-references

## About

This documentation is auto-generated by [doc-verification-bridge](https://github.com/YOUR_ORG/doc-verification-bridge),
a tool for classifying theorems by their role in specification-implementation refinement.

### Four-Category Ontology

The API is organized according to a **Four-Category Ontology** inspired by E.J. Lowe:

| Category | Lowe's Analog | Description |
|----------|---------------|-------------|
| Mathematical Abstractions | Kinds | Abstract types, Prop-based structures |
| Computational Datatypes | Objects | Concrete data structures |
| Mathematical Definitions | Attributes | Prop-returning predicates, relations, properties |
| Computational Operations | Modes | Computable functions, Bool predicates, algorithms |

**Bridging theorems** prove the fundamental ontological relations between levels.
"

end DocVerificationBridge
