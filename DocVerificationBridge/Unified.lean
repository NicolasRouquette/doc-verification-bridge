-- DocVerificationBridge/Unified.lean
-- Unified doc-gen4 + doc-verification-bridge pipeline with MkDocs-first architecture

import Lean
import DocGen4
import DocGen4.Load
import DocGen4.Output
import DocGen4.Process.Analyze
import DocGen4.Process.Hierarchy
import DocVerificationBridge.Classify
import DocVerificationBridge.Report

/-!
# Unified Documentation Pipeline (MkDocs-First)

This module provides a unified CLI that uses MkDocs as the primary documentation
system, with doc-gen4's API documentation embedded as static files.

Architecture:
1. doc-gen4 generates API HTML to a temp location
2. MkDocs builds the main site with verification reports
3. doc-gen4 output is copied into the MkDocs site under `/api/`

Result structure:
```
site/
  index.html          # MkDocs home
  verification/       # MkDocs verification report
  api/                # doc-gen4 API documentation (copied)
    index.html
    Batteries.html
    Batteries/
      Data/
        ...
```
-/

namespace DocVerificationBridge.Unified

open Lean System IO DocGen4 DocGen4.Output DocGen4.Process

/-- Configuration for the unified documentation pipeline -/
structure UnifiedConfig where
  /-- Build/output directory -/
  buildDir : System.FilePath := ".lake/build/doc"
  /-- Repository URL for source links -/
  repoUrl : String := ""
  /-- Git platform (github or gitlab) -/
  platform : GitPlatform := .github
  /-- Git branch name -/
  branch : String := "main"
  /-- Project name -/
  projectName : String := "Documentation"
  /-- Source directory (where the .lean files are, for git cache) -/
  sourceDir : System.FilePath := ".."
  /-- Whether to generate verification pages -/
  generateVerification : Bool := true
deriving Repr, Inhabited

/-- Result of the unified pipeline -/
structure UnifiedResult where
  /-- doc-gen4's analyzer result -/
  analyzerResult : AnalyzerResult
  /-- Module hierarchy -/
  hierarchy : Hierarchy
  /-- The loaded environment (needed for our classification) -/
  env : Environment
  /-- Verification entries (our classification) -/
  verificationEntries : NameMap APIMeta
  /-- Git file cache for accurate source links -/
  gitCache : GitFileCache

/-- Build a git file cache by running `git ls-files` in a specific directory -/
def buildGitFileCacheIn (dir : System.FilePath) : IO GitFileCache := do
  let result ← IO.Process.output {
    cmd := "git"
    args := #["ls-files", "*.lean"]
    cwd := some dir
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

/-- Load modules and run both doc-gen4's analysis and our classification -/
def loadAndAnalyze (cfg : UnifiedConfig) (modules : Array Name) : IO UnifiedResult := do
  -- Initialize search path
  Lean.initSearchPath (← Lean.findSysroot)

  IO.println s!"unified-doc: Loading {modules.size} module(s)..."

  -- Load environment using doc-gen4's helper
  let env ← DocGen4.envOfImports modules

  -- Run doc-gen4's analysis
  -- Use analyzePrefixModules for the first module to get all submodules
  let task := if modules.size == 1 then
    AnalyzeTask.analyzePrefixModules modules[0]!
  else
    AnalyzeTask.analyzeConcreteModules modules
  let config := {
    maxHeartbeats := 100000000,
    options := ⟨[
      (`pp.tagAppFns, true),
      (`pp.funBinderTypes, true),
      (`debug.skipKernelTC, true),
      (`Elab.async, false)
    ]⟩,
    fileName := default,
    fileMap := default,
  }
  let ((analyzerResult, hierarchy), _) ← Meta.MetaM.toIO (process task) config { env := env } {} {}

  IO.println s!"  Loaded {analyzerResult.moduleInfo.size} modules"

  -- Build git file cache from the source directory
  IO.println s!"unified-doc: Building git file cache from {cfg.sourceDir}..."
  let gitCache ← buildGitFileCacheIn cfg.sourceDir
  IO.println s!"  Found {gitCache.allFiles.size} .lean files"

  -- Run our classification
  IO.println s!"unified-doc: Classifying declarations..."

  let mut allEntries : NameMap APIMeta := {}

  -- Classify declarations from all analyzed modules
  for modName in modules do
    let coreCtx : Core.Context := {
      options := {},
      fileName := "<verification-bridge>",
      fileMap := default
    }
    let coreState : Core.State := { env }
    let (result, _) ← (classifyAllDeclarations env modName).run' {} |>.toIO coreCtx coreState
    allEntries := result.entries.foldl (fun acc name apiMeta => acc.insert name apiMeta) allEntries

  IO.println s!"  Classified {allEntries.size} declarations"

  return {
    analyzerResult
    hierarchy
    env
    verificationEntries := allEntries
    gitCache
  }

/-- Generate doc-gen4 documentation to a temporary directory -/
def generateDocGen4ToTemp (cfg : UnifiedConfig) (result : UnifiedResult) : IO System.FilePath := do
  IO.println s!"unified-doc: Generating doc-gen4 API documentation..."

  -- Generate to a temp directory that we'll copy later
  let apiTempDir := cfg.buildDir / "api-temp"
  IO.FS.createDirAll apiTempDir

  let baseConfig ← DocGen4.getSimpleBaseContext apiTempDir result.hierarchy

  -- Construct source URL for doc-gen4
  let sourceUrl := if cfg.repoUrl.isEmpty then none
    else
      let url := cfg.repoUrl
      let baseUrl := if url.endsWith "/" then url.take (url.length - 1) else url
      some s!"{baseUrl}/blob/{cfg.branch}/"

  discard <| DocGen4.htmlOutputResults baseConfig result.analyzerResult sourceUrl
  DocGen4.htmlOutputIndex baseConfig

  IO.println s!"  Generated API docs to {apiTempDir}/"

  return apiTempDir

/-- Generate unified MkDocs configuration (MkDocs-first architecture) -/
def generateUnifiedMkDocsConfig (cfg : UnifiedConfig) : String :=
  let repoSection := if cfg.repoUrl.isEmpty then ""
    else
      let repoName := cfg.repoUrl.splitOn "/" |>.getLast!
      s!"repo_url: {cfg.repoUrl}\nrepo_name: {repoName}\n"
  let socialSection := if cfg.repoUrl.isEmpty then ""
    else s!"extra:\n  social:\n    - icon: fontawesome/brands/github\n      link: {cfg.repoUrl}\n"
  s!"# Unified Documentation for {cfg.projectName}
# Generated by doc-verification-bridge (MkDocs-first architecture)

site_name: {cfg.projectName}
site_description: API Documentation and Verification Coverage
{repoSection}
theme:
  name: material
  features:
    - navigation.instant
    - navigation.tracking
    - navigation.tabs
    - navigation.tabs.sticky
    - navigation.sections
    - navigation.expand
    - navigation.top
    - search.highlight
    - search.share
    - content.code.copy
    - toc.follow
  palette:
    - scheme: default
      primary: indigo
      accent: indigo
      toggle:
        icon: material/brightness-7
        name: Switch to dark mode
    - scheme: slate
      primary: indigo
      accent: indigo
      toggle:
        icon: material/brightness-4
        name: Switch to light mode
  icon:
    repo: fontawesome/brands/github

plugins:
  - search
  - autorefs

markdown_extensions:
  - abbr
  - admonition
  - attr_list
  - def_list
  - footnotes
  - md_in_html
  - toc:
      permalink: true
      toc_depth: 4
  - pymdownx.arithmatex:
      generic: true
  - pymdownx.betterem:
      smart_enable: all
  - pymdownx.caret
  - pymdownx.details
  - pymdownx.emoji:
      emoji_index: !!python/name:material.extensions.emoji.twemoji
      emoji_generator: !!python/name:material.extensions.emoji.to_svg
  - pymdownx.highlight:
      anchor_linenums: true
      line_spans: __span
      pygments_lang_class: true
  - pymdownx.inlinehilite
  - pymdownx.keys
  - pymdownx.mark
  - pymdownx.smartsymbols
  - pymdownx.superfences:
      custom_fences:
        - name: mermaid
          class: mermaid
          format: !!python/name:pymdownx.superfences.fence_code_format
  - pymdownx.tabbed:
      alternate_style: true
  - pymdownx.tasklist:
      custom_checkbox: true
  - pymdownx.tilde

nav:
  - Home: index.md
  - API Reference: api.md
  - Verification:
    - Overview: verification/index.md
    - Coverage Report: verification/coverage.md

{socialSection}
extra_css:
  - stylesheets/extra.css
"

/-- Generate the home page for unified docs -/
def generateUnifiedHomeMd (projectName : String) : String :=
  "# " ++ projectName ++ "\n\n" ++
  "Welcome to the **" ++ projectName ++ "** documentation.\n\n" ++
  "## Documentation Sections\n\n" ++
  "### 📚 API Reference\n\n" ++
  "Browse the complete API documentation generated by doc-gen4.\n\n" ++
  "[→ API Documentation](api.md)\n\n" ++
  "### ✓ Verification Coverage\n\n" ++
  "View theorem classification and verification status across the codebase.\n\n" ++
  "[→ Coverage Report](verification/coverage.md)\n\n" ++
  "## Quick Links\n\n" ++
  "| Section | Description |\n" ++
  "|---------|-------------|\n" ++
  "| [API Reference](api/index.html) | Full API documentation (doc-gen4) |\n" ++
  "| [Verification Overview](verification/index.md) | Verification methodology |\n" ++
  "| [Coverage Report](verification/coverage.md) | Detailed theorem classification |\n\n" ++
  "---\n\n" ++
  "*Documentation generated by [doc-verification-bridge](https://github.com/leanprover-community/doc-verification-bridge).*\n"

/-- Generate API reference landing page (links to doc-gen4) -/
def generateApiLandingMd (_cfg : UnifiedConfig) : String :=
  "# API Reference\n\n" ++
  "The complete API documentation is available in the embedded doc-gen4 pages.\n\n" ++
  "!!! tip \"Browse API\"\n\n" ++
  "    [**Open API Documentation →**](api/index.html)\n\n" ++
  "## About the API Documentation\n\n" ++
  "The API reference is generated by [doc-gen4](https://github.com/leanprover-community/doc-gen4), " ++
  "the standard documentation generator for Lean 4 projects.\n\n" ++
  "### Features\n\n" ++
  "- **Complete type signatures** for all public definitions\n" ++
  "- **Source links** to the original implementation\n" ++
  "- **Module hierarchy** navigation\n" ++
  "- **Search** across all declarations\n\n" ++
  "### Navigation Tips\n\n" ++
  "- Use the **module tree** on the left to browse by namespace\n" ++
  "- Click on **type names** to navigate to their definitions\n" ++
  "- Use **Ctrl/Cmd + K** to search within the API docs\n\n" ++
  "---\n\n" ++
  "[Open Full API Reference](api/index.html)\n"

/-- Generate verification index page -/
def generateVerificationIndexMd (projectName : String) : String :=
  "# Verification Coverage\n\n" ++
  "This section documents the verification status of " ++ projectName ++
  " using a **Four-Category Ontology** inspired by E.J. Lowe's metaphysics.\n\n" ++
  "## Classification System\n\n" ++
  "| Category | Abbreviation | Description |\n" ++
  "|----------|--------------|-------------|\n" ++
  "| **Mathematical Abstraction** | MathAb | Pure mathematical structures (e.g., `Nat`, `List α`) |\n" ++
  "| **Computational Datatype** | CompData | Implementation-focused types (e.g., `HashMap`, `Array`) |\n" ++
  "| **Mathematical Definition** | MathDef | Specifications and properties (e.g., `IsEmpty`, `Sorted`) |\n" ++
  "| **Computational Operation** | CompOp | Algorithms and functions (e.g., `sort`, `filter`) |\n\n" ++
  "## Reports\n\n" ++
  "- **[Coverage Report](coverage.md)** - Detailed breakdown by module with theorem classification\n" ++
  "- **[API Reference](../api/index.html)** - Full documentation with source links\n\n" ++
  "## Methodology\n\n" ++
  "Declarations are classified based on:\n\n" ++
  "1. **Type analysis** - Structure of the declaration's type\n" ++
  "2. **Namespace heuristics** - Common naming patterns\n" ++
  "3. **Proof content** - Whether the declaration proves properties about other definitions\n\n" ++
  "See the [Coverage Report](coverage.md) for the complete classification of all " ++
  projectName ++ " declarations.\n"

/-- Generate extra CSS for unified docs -/
def generateUnifiedExtraCss : String :=
  "/* Unified documentation styles */\n\n" ++
  "/* Override MkDocs Material content width for wider tables */\n" ++
  ".md-content {\n" ++
  "  max-width: none;\n" ++
  "}\n\n" ++
  ".md-content__inner {\n" ++
  "  max-width: 100%;\n" ++
  "  margin: 0 auto;\n" ++
  "  padding: 0 1.5rem;\n" ++
  "}\n\n" ++
  "/* Make the main content area use full width */\n" ++
  ".md-main__inner {\n" ++
  "  max-width: none;\n" ++
  "}\n\n" ++
  "/* Verification report tables */\n" ++
  ".md-typeset table:not([class]) {\n" ++
  "  width: 100%;\n" ++
  "  display: table;\n" ++
  "  border-collapse: collapse;\n" ++
  "  font-size: 0.8rem;\n" ++
  "}\n\n" ++
  "/* Table column widths for coverage report */\n" ++
  ".md-typeset table:not([class]) th:nth-child(1),\n" ++
  ".md-typeset table:not([class]) td:nth-child(1) { width: 50%; min-width: 300px; }  /* Declaration */\n" ++
  ".md-typeset table:not([class]) th:nth-child(2),\n" ++
  ".md-typeset table:not([class]) td:nth-child(2) { width: 10%; text-align: center; }  /* Kind */\n" ++
  ".md-typeset table:not([class]) th:nth-child(3),\n" ++
  ".md-typeset table:not([class]) td:nth-child(3) { width: 15%; text-align: center; }  /* Assumes */\n" ++
  ".md-typeset table:not([class]) th:nth-child(4),\n" ++
  ".md-typeset table:not([class]) td:nth-child(4) { width: 15%; text-align: center; }  /* Proves */\n" ++
  ".md-typeset table:not([class]) th:nth-child(5),\n" ++
  ".md-typeset table:not([class]) td:nth-child(5) { width: 10%; text-align: center; }  /* Validates */\n\n" ++
  "/* Category badges */\n" ++
  ".category-badge {\n" ++
  "  display: inline-block;\n" ++
  "  padding: 0.125rem 0.5rem;\n" ++
  "  border-radius: 0.25rem;\n" ++
  "  font-size: 0.75rem;\n" ++
  "  font-weight: 600;\n" ++
  "}\n\n" ++
  ".category-mathab { background: #e3f2fd; color: #1565c0; }\n" ++
  ".category-compdata { background: #e8f5e9; color: #2e7d32; }\n" ++
  ".category-mathdef { background: #fff3e0; color: #ef6c00; }\n" ++
  ".category-compop { background: #fce4ec; color: #c2185b; }\n\n" ++
  "/* Expandable details sections */\n" ++
  "details {\n" ++
  "  margin: 0.5rem 0;\n" ++
  "  padding: 0.5rem;\n" ++
  "  background: var(--md-code-bg-color);\n" ++
  "  border-radius: 0.25rem;\n" ++
  "}\n\n" ++
  "details summary {\n" ++
  "  cursor: pointer;\n" ++
  "  font-weight: 500;\n" ++
  "}\n\n" ++
  "details summary:hover {\n" ++
  "  color: var(--md-accent-fg-color);\n" ++
  "}\n\n" ++
  "/* Hide TOC on coverage pages to save horizontal space */\n" ++
  ".md-sidebar--secondary {\n" ++
  "  /* Keep sidebar but can hide with: display: none; */\n" ++
  "}\n"

/-- Copy directory recursively -/
partial def copyDirRecursive (src dst : System.FilePath) : IO Unit := do
  IO.FS.createDirAll dst
  for entry in ← System.FilePath.readDir src do
    let srcPath := entry.path
    let dstPath := dst / entry.fileName
    if (← srcPath.isDir) then
      copyDirRecursive srcPath dstPath
    else
      let contents ← IO.FS.readBinFile srcPath
      IO.FS.writeBinFile dstPath contents

/-- Generate unified MkDocs site -/
def generateUnifiedMkDocsSite (cfg : UnifiedConfig) (result : UnifiedResult) (modules : List String) : IO System.FilePath := do
  IO.println s!"unified-doc: Generating MkDocs site..."

  -- Create MkDocs source directory structure
  let mkdocsSrcDir := cfg.buildDir / "mkdocs-src"
  let docsDir := mkdocsSrcDir / "docs"
  let verificationDir := docsDir / "verification"
  let stylesheetsDir := docsDir / "stylesheets"
  IO.FS.createDirAll verificationDir
  IO.FS.createDirAll stylesheetsDir

  -- Generate MkDocs configuration
  IO.FS.writeFile (mkdocsSrcDir / "mkdocs.yml") (generateUnifiedMkDocsConfig cfg)

  -- Generate home page
  IO.FS.writeFile (docsDir / "index.md") (generateUnifiedHomeMd cfg.projectName)

  -- Generate API landing page
  IO.FS.writeFile (docsDir / "api.md") (generateApiLandingMd cfg)

  -- Generate verification pages
  IO.FS.writeFile (verificationDir / "index.md") (generateVerificationIndexMd cfg.projectName)

  -- Generate coverage report (our main report)
  let reportCfg : ReportConfig := {
    outputDir := mkdocsSrcDir  -- Not used directly, but needed for structure
    repoUrl := cfg.repoUrl
    platform := cfg.platform
    branch := cfg.branch
    modules := modules
    gitCache := result.gitCache
  }
  let coverageReport := generateReport result.env result.verificationEntries (some reportCfg) cfg.projectName modules
  IO.FS.writeFile (verificationDir / "coverage.md") coverageReport

  -- Generate extra CSS
  IO.FS.writeFile (stylesheetsDir / "extra.css") generateUnifiedExtraCss

  IO.println s!"  Generated MkDocs source at {mkdocsSrcDir}/"

  -- Build MkDocs
  IO.FS.createDirAll (cfg.buildDir / "site")
  let siteDir ← IO.FS.realPath (cfg.buildDir / "site")
  IO.println s!"unified-doc: Running mkdocs build..."
  let mkdocsResult ← IO.Process.output {
    cmd := "mkdocs"
    args := #["build", "--site-dir", siteDir.toString]
    cwd := some mkdocsSrcDir
  }

  if mkdocsResult.exitCode != 0 then
    IO.eprintln s!"  Error: mkdocs build failed:"
    IO.eprintln mkdocsResult.stderr
    throw <| IO.userError "MkDocs build failed"
  else
    IO.println s!"  MkDocs site built at {siteDir}/"

  return siteDir

/-- Run the complete unified pipeline (MkDocs-first) -/
def runUnifiedPipeline (cfg : UnifiedConfig) (modules : Array Name) : IO UInt32 := do
  try
    -- Step 1: Load and analyze
    let result ← loadAndAnalyze cfg modules

    -- Step 2: Generate doc-gen4 to temp location
    let apiTempDir ← generateDocGen4ToTemp cfg result

    -- Step 3: Generate MkDocs site (includes verification)
    let siteDir ← generateUnifiedMkDocsSite cfg result (modules.toList.map toString)

    -- Step 4: Copy doc-gen4 output into MkDocs site
    IO.println s!"unified-doc: Copying API docs into site..."
    let apiDestDir := siteDir / "api"
    copyDirRecursive (apiTempDir / "doc") apiDestDir
    IO.println s!"  Copied API docs to {apiDestDir}/"

    IO.println ""
    IO.println "✅ Unified documentation generated successfully!"
    IO.println s!"   Site:          {siteDir}/"
    IO.println s!"   Home:          {siteDir}/index.html"
    IO.println s!"   API:           {siteDir}/api/index.html"
    IO.println s!"   Verification:  {siteDir}/verification/coverage/"
    IO.println ""
    IO.println "To serve locally:"
    IO.println s!"   python3 -m http.server -d {siteDir} 8000"

    return 0
  catch e =>
    IO.eprintln s!"Error: {e}"
    return 1

/-- Generate JSON mapping for external tools -/
def generateVerificationJson (buildDir : System.FilePath) (entries : NameMap APIMeta) : IO Unit := do
  let jsonPath := buildDir / "site" / "verification" / "verification-data.json"

  let jsonEntries := entries.foldl (fun acc name m =>
    let catStr := match m.kind with
      | .apiType .mathematicalAbstraction => "MathAbstract"
      | .apiType .computationalDatatype => "CompDatatype"
      | .apiDef ⟨.mathematicalDefinition, _⟩ => "MathDef"
      | .apiDef ⟨.computationalOperation, _⟩ => "CompOp"
      | .apiTheorem _ => "Theorem"
    let entry : Json := Json.mkObj [
      ("name", Json.str name.toString),
      ("category", Json.str catStr),
      ("isTheorem", Json.bool m.isTheorem),
      ("anchor", Json.str (nameToAnchor name)),
      ("proves", Json.arr (m.proves.map (Json.str ∘ toString))),
      ("assumes", Json.arr (m.assumes.map (Json.str ∘ toString)))
    ]
    acc.push entry
  ) #[]

  let json := Json.arr jsonEntries
  IO.FS.writeFile jsonPath json.compress
  IO.println s!"  Written {jsonPath}"

end DocVerificationBridge.Unified
