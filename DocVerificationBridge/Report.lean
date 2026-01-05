-- DocVerificationBridge/Report.lean
-- Report generation for verification coverage

import Lean
import DocVerificationBridge.Types
import DocVerificationBridge.Classify

/-!
# Report Generation

This module generates MkDocs-compatible markdown reports showing:
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

/-- Configuration for report generation -/
structure ReportConfig where
  outputDir : System.FilePath
  repoUrl : String
  platform : GitPlatform
  branch : String := "main"
  modules : List String := []
deriving Repr, Inhabited

/-- Strip trailing slashes from URL -/
def stripTrailingSlash (s : String) : String :=
  -- Simple implementation without deprecated API
  if s.endsWith "/" then
    s.dropEnd 1 |>.toString
  else
    s

/-- Generate a source URL for a given file path and line number -/
def ReportConfig.sourceUrl (cfg : ReportConfig) (filePath : String) (line : Nat) : String :=
  let baseUrl := stripTrailingSlash cfg.repoUrl
  match cfg.platform with
  | .github => s!"{baseUrl}/blob/{cfg.branch}/{filePath}#L{line}"
  | .gitlab => s!"{baseUrl}/-/blob/{cfg.branch}/{filePath}#L{line}"

/-- Generate a source URL without line number -/
def ReportConfig.sourceUrlNoLine (cfg : ReportConfig) (filePath : String) : String :=
  let baseUrl := stripTrailingSlash cfg.repoUrl
  match cfg.platform with
  | .github => s!"{baseUrl}/blob/{cfg.branch}/{filePath}"
  | .gitlab => s!"{baseUrl}/-/blob/{cfg.branch}/{filePath}"

/-- Generate a folder URL (tree view) -/
def ReportConfig.folderUrl (cfg : ReportConfig) (folderPath : String) : String :=
  let baseUrl := stripTrailingSlash cfg.repoUrl
  match cfg.platform with
  | .github => s!"{baseUrl}/tree/{cfg.branch}/{folderPath}"
  | .gitlab => s!"{baseUrl}/-/tree/{cfg.branch}/{folderPath}"

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

/-- Generate the full markdown report -/
def generateReport (env : Environment) (entries : NameMap APIMeta)
    (cfg : Option ReportConfig := none) (title : String := "API Coverage Report")
    (modules : List String := []) : String := Id.run do
  let provedByMap := computeProvedByMap entries

  let mut output := s!"# {title}\n\n"

  -- Reproducibility section
  unless modules.isEmpty do
    let moduleList := modules |> String.intercalate " "
    output := output ++ "## Reproducibility\n\n"
    output := output ++ "To regenerate this report, run:\n\n"
    match cfg with
    | some config =>
      let platformStr := match config.platform with | .github => "github" | .gitlab => "gitlab"
      output := output ++ s!"```bash\nlake exe verification-bridge --output {config.outputDir} --repo {config.repoUrl} --platform {platformStr} {moduleList}\n```\n\n"
    | none =>
      output := output ++ s!"```bash\nlake exe verification-bridge {moduleList}\n```\n\n"

  -- Four-Category Ontology introduction
  output := output ++ "This report is organized according to a **Four-Category Ontology** inspired by E.J. Lowe:\n\n"
  output := output ++ "| Category | Lowe's Analog | Description |\n"
  output := output ++ "|----------|---------------|-------------|\n"
  output := output ++ "| Mathematical Abstractions | Kinds | Abstract types, Prop-based structures |\n"
  output := output ++ "| Computational Datatypes | Objects | Concrete data structures |\n"
  output := output ++ "| Mathematical Definitions | Attributes | Prop-returning predicates, relations, properties |\n"
  output := output ++ "| Computational Operations | Modes | Computable functions, Bool predicates, algorithms |\n\n"
  output := output ++ "**Bridging theorems** prove the fundamental ontological relations between levels.\n\n"

  -- Legend
  output := output ++ "## Legend\n\n"
  output := output ++ "| Symbol | Meaning |\n"
  output := output ++ "|--------|--------|\n"
  output := output ++ "| ✅ | Fully verified |\n"
  output := output ++ "| ⚠️ | Axiom-dependent |\n"
  output := output ++ "| 🔄 | Partially verified |\n"
  output := output ++ "| ❌ | Not verified |\n"
  output := output ++ "| 📝 | Spec only |\n"
  output := output ++ "| N/A | N/A (no verification needed) |\n\n"

  -- Convert to array and sort by name
  let entryList := entries.foldl (fun acc name m => acc.push (name, m)) #[]
    |>.qsort (fun (n1, _) (n2, _) => n1.toString < n2.toString)

  -- Separate definitions and theorems
  let definitions := entryList.filter (fun (_, m) => !m.isTheorem)
  let theorems := entryList.filter (fun (_, m) => m.isTheorem)

  -- Definitions section
  unless definitions.isEmpty do
    output := output ++ "## Definitions\n\n"
    output := output ++ "<div class=\"table-wrapper\" markdown=\"1\">\n\n"
    output := output ++ "| Name | Category | Status | Verified By |\n"
    output := output ++ "|------|----------|--------|-------------|\n"
    for (name, m) in definitions do
      let status := coverageEmoji m.coverage
      let category := m.categoryString
      let anchorId := nameToAnchor name
      let displayedName := displayName name

      let urlOpt := match cfg with
        | some config =>
          match env.getModuleIdxFor? name with
          | some modIdx =>
            let modName := env.header.moduleNames[modIdx.toNat]!
            let filePath := moduleToFilePath modName
            match getDeclarationLine env name with
            | some line => some (config.sourceUrl filePath line)
            | none => some (config.sourceUrlNoLine filePath)
          | none => none
        | none => none
      let sourceLink := buildNameCell displayedName urlOpt anchorId

      let provedBy := provedByMap.find? name |>.getD #[]
      let verifiedCell := if provedBy.isEmpty then ""
        else provedBy.map (fun n => "[" ++ displayName n ++ "](#" ++ nameToAnchor n ++ ")") |>.toList |> String.intercalate "<br>"

      output := output ++ s!"| {sourceLink} | {category} | {status} | {verifiedCell} |\n"
    output := output ++ "\n</div>\n\n"

  -- Theorems section
  unless theorems.isEmpty do
    output := output ++ "## Theorems\n\n"
    output := output ++ "<div class=\"table-wrapper\" markdown=\"1\">\n\n"
    output := output ++ "| Name | Status | Kind | Assumes | Proves | Validates |\n"
    output := output ++ "|------|--------|------|---------|--------|-----------|\n"
    for (name, m) in theorems do
      let status := coverageEmoji m.coverage
      let anchorId := nameToAnchor name
      let displayedName := displayName name

      let urlOpt := match cfg with
        | some config =>
          match env.getModuleIdxFor? name with
          | some modIdx =>
            let modName := env.header.moduleNames[modIdx.toNat]!
            let filePath := moduleToFilePath modName
            match getDeclarationLine env name with
            | some line => some (config.sourceUrl filePath line)
            | none => some (config.sourceUrlNoLine filePath)
          | none => none
        | none => none
      let sourceLink := buildNameCell displayedName urlOpt anchorId

      let kindStr := match m.theoremKind? with
        | some .computationalProperty => "computationalProperty"
        | some .mathematicalProperty => "mathematicalProperty"
        | some .bridgingProperty => "bridgingProperty"
        | some .soundnessProperty => "soundnessProperty"
        | some .completenessProperty => "completenessProperty"
        | none => "—"

      let shorten (n : Name) := displayName n
      let assumesCell := formatSymbolLinks m.assumes shorten
      let provesCell := formatSymbolLinks m.proves shorten
      let validatesCell := formatSymbolLinks m.validates shorten

      output := output ++ s!"| {sourceLink} | {status} | {kindStr} | {assumesCell} | {provesCell} | {validatesCell} |\n"
    output := output ++ "\n</div>\n\n"

  -- Summary stats
  let total := entries.size
  let verified := entries.foldl (fun acc _ m => if m.coverage == .complete then acc + 1 else acc) 0
  let axiomDep := entries.foldl (fun acc _ m => if m.coverage == .axiomDependent then acc + 1 else acc) 0
  let numDefs := definitions.size
  let numThms := theorems.size
  let pct := if total > 0 then verified * 100 / total else 0

  output := output ++ "## Summary\n\n"
  output := output ++ s!"- **Total APIs**: {total}\n"
  output := output ++ s!"- **Definitions**: {numDefs}\n"
  output := output ++ s!"- **Theorems**: {numThms}\n"
  output := output ++ s!"- **Fully Verified**: {verified}\n"
  output := output ++ s!"- **Axiom-Dependent**: {axiomDep}\n"
  output := output ++ s!"- **Coverage**: {pct}%\n"

  output

/-!
## MkDocs Configuration Generation
-/

/-- Generate mkdocs.yml configuration -/
def generateMkDocsConfig (projectName : String := "API Coverage") : String :=
  s!"# MkDocs configuration for {projectName}
site_name: {projectName}
theme:
  name: material
  palette:
    primary: indigo
  features:
    - content.code.copy
    - navigation.sections

markdown_extensions:
  - tables
  - attr_list
  - md_in_html
  - toc:
      permalink: true

plugins:
  - search
  - autorefs

extra_css:
  - stylesheets/extra.css

nav:
  - Home: index.md
  - API Coverage: API_Coverage.md
"

/-- Generate extra.css for table styling -/
def generateExtraCss : String :=
  "/* verification-bridge table styling */
.table-wrapper {
  width: 100%;
  overflow-x: auto;
}

.table-wrapper table {
  width: 100%;
  table-layout: fixed;
}

/* Sticky header support */
.md-typeset__scrollwrap {
  overflow: visible !important;
}

.md-typeset__table {
  overflow: visible !important;
}

.table-wrapper thead th {
  position: sticky;
  top: 0;
  background: var(--md-default-bg-color);
  z-index: 10;
}
"

/-- Generate index.md -/
def generateIndexMd (projectName : String := "API Coverage") : String :=
  s!"# {projectName}

Welcome to the verification coverage documentation.

## Contents

- [API Coverage Report](API_Coverage.md) - Complete coverage analysis

## About

This documentation is generated by [verification-bridge](https://github.com/YOUR_ORG/verification-bridge),
a tool for classifying theorems by their role in specification-implementation refinement.
"

end DocVerificationBridge
