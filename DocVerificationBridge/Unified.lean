-- DocVerificationBridge/Unified.lean
-- Unified doc-gen4 + doc-verification-bridge pipeline with static HTML generation

import Lean
import DocGen4
import DocGen4.Load
import DocGen4.Output
import DocGen4.Process.Analyze
import DocGen4.Process.Hierarchy
import DocVerificationBridge.Classify
import DocVerificationBridge.Report
import DocVerificationBridge.SourceLinkerCompat
import DocVerificationBridge.StaticHtml
import DocVerificationBridge.VerificationDecorator

/-!
# Unified Documentation Pipeline

This module provides a unified CLI for documentation generation using static HTML.
The pipeline supports parallel generation for large projects.

Architecture:
1. doc-gen4 generates API HTML to a temp location
2. Static HTML generator creates verification reports
3. doc-gen4 output is copied into the site under `/api/`

Result structure:
```
site/
  index.html          # Home page with verification stats
  modules/            # Per-module verification reports
  api/                # doc-gen4 API documentation (copied)
```
-/

namespace DocVerificationBridge.Unified

open Lean
open DocGen4

/-- Format milliseconds as human-readable duration -/
def formatDurationMs (ms : Nat) : String :=
  let secs := ms / 1000
  let mins := secs / 60
  let hours := mins / 60
  if hours > 0 then
    s!"{hours}h {mins % 60}m {secs % 60}s"
  else if mins > 0 then
    s!"{mins}m {secs % 60}s"
  else
    s!"{secs}.{(ms % 1000) / 100}s"

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
  /-- Whether to skip doc-gen4 generation (use existing api-temp output) -/
  skipDocGen : Bool := false
  /-- Number of workers for parallel proof dependency extraction (0 = sequential) -/
  proofDepWorkers : Nat := 0
  /-- Skip proof dependency extraction entirely -/
  skipProofDeps : Bool := false
  /-- Theorem names to skip during proof dependency extraction (for slow theorems) -/
  proofDepBlacklist : Array String := #[]
  /-- Path to load classification cache from (skips classification step) -/
  loadClassificationCache : Option System.FilePath := none
  /-- Path to save classification cache to (for future runs) -/
  saveClassificationCache : Option System.FilePath := none
  /-- Number of parallel workers for HTML file writing (0 = sequential) -/
  htmlWorkers : Nat := 0
  /-- Threshold in seconds before warning about slow theorems during proof dep extraction -/
  slowThresholdSecs : Nat := 30
  /-- Optional project description (shown on index page) -/
  projectDescription : Option String := none
  /-- Top-level modules being analyzed (shown on index page) -/
  projectModules : Array String := #[]
  /-- Additional config settings to display (key-value pairs) -/
  projectSettings : Array (String × String) := #[]
  /-- External documentation URLs for transitive dependencies (module prefix → base URL).
      Configured via [external_docs] section in config.toml. -/
  externalDocs : List (String × String) := []
deriving Repr, Inhabited

/-- Result of the unified pipeline -/
structure UnifiedResult : Type where
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
  /-- Processing warnings from doc-gen4 (declarations that couldn't be analyzed) -/
  processingWarnings : Array StaticHtml.ProcessingWarning := #[]

/-- Get the current git commit hash in a directory -/
def getGitCommitHash (dir : System.FilePath) : IO (Option String) := do
  let result ← IO.Process.output {
    cmd := "git"
    args := #["rev-parse", "HEAD"]
    cwd := some dir
  }
  if result.exitCode != 0 then
    return none
  return some result.stdout.trimCompat

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
  (← IO.getStdout).flush

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
  (← IO.getStdout).flush

  -- Build git file cache from the source directory
  IO.println s!"unified-doc: Building git file cache from {cfg.sourceDir}..."
  (← IO.getStdout).flush
  let gitCache ← buildGitFileCacheIn cfg.sourceDir
  IO.println s!"  Found {gitCache.allFiles.size} .lean files"
  (← IO.getStdout).flush

  -- Run our classification
  IO.println s!"unified-doc: Classifying declarations..."
  (← IO.getStdout).flush

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
  (← IO.getStdout).flush

  return {
    analyzerResult
    hierarchy
    env
    verificationEntries := allEntries
    gitCache
    processingWarnings := #[]  -- Not available in upstream doc-gen4
  }

/-- Construct file path from module name (e.g., `Batteries.Data.List` → `Batteries/Data/List.lean`) -/
def moduleToPath (module : Name) : String :=
  let parts := module.components.map (Name.toString (escape := false))
  "/".intercalate parts ++ ".lean"

/-- GitHub/GitLab source linker with line range support -/
def mkGitSourceLinker (baseUrl : String) (range : Option DeclarationRange) : String :=
  match range with
  | some range => s!"{baseUrl}#L{range.pos.line}-L{range.endPos.line}"
  | none => baseUrl

/-- Create a custom source linker for GitHub/GitLab that appends module paths.
    The `repoBaseUrl` should be the base URL including `/blob/{ref}/`.
    Uses explicit function type for cross-version compatibility (v4.24.0+). -/
def makeSourceLinker (repoBaseUrl : String) : Name → Option DeclarationRange → String := fun module range =>
  let leanHash := Lean.githash
  let root := module.getRoot
  -- Core modules link to lean4 repo
  if root == `Lean ∨ root == `Init ∨ root == `Std then
    let parts := module.components.map (Name.toString (escape := false))
    let path := "/".intercalate parts
    mkGitSourceLinker s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/{path}.lean" range
  else if root == `Lake then
    let parts := module.components.map (Name.toString (escape := false))
    let path := "/".intercalate parts
    mkGitSourceLinker s!"https://github.com/leanprover/lean4/blob/{leanHash}/src/lake/{path}.lean" range
  else
    -- Project modules: append module path to base URL
    let baseUrl := if repoBaseUrl.endsWith "/" then repoBaseUrl else repoBaseUrl ++ "/"
    let path := moduleToPath module
    mkGitSourceLinker s!"{baseUrl}{path}" range

/-- Generate doc-gen4 documentation to a temporary directory -/
def generateDocGen4ToTemp (cfg : UnifiedConfig) (result : UnifiedResult) : IO System.FilePath := do
  IO.println s!"unified-doc [5/7]: Generating doc-gen4 API documentation..."
  (← IO.getStdout).flush

  -- Generate to a temp directory that we'll copy later
  let apiTempDir := cfg.buildDir / "api-temp"
  IO.FS.createDirAll apiTempDir

  let baseConfig ← DocGen4.getSimpleBaseContext apiTempDir result.hierarchy

  -- Compute source URL and optional custom linker
  let (sourceUrl?, customLinker?) ← if cfg.repoUrl.isEmpty then pure (none, none)
    else
      let url := cfg.repoUrl
      -- Use dropLast for cross-version compatibility
      let baseUrl := if url.endsWith "/" then String.ofList (url.toList.take (url.length - 1)) else url
      -- Try to get git commit hash, fall back to branch name
      let gitRef ← match ← getGitCommitHash cfg.sourceDir with
        | some hash => pure hash
        | none => pure cfg.branch
      let srcUrl := s!"{baseUrl}/blob/{gitRef}/"
      -- Create custom linker for better source link handling
      let linker := makeSourceLinker srcUrl
      pure (some srcUrl, some linker)

  -- Create verification decorator for bidirectional navigation
  -- Links from API pages to verification coverage pages
  let verificationDecorator := makeVerificationDecorator result.verificationEntries

  -- Use compatibility shim with custom linker and decorator
  discard <| htmlOutputResultsCompat baseConfig result.analyzerResult sourceUrl? customLinker? (some verificationDecorator)
  DocGen4.htmlOutputIndex baseConfig

  -- Inject verification badge CSS into doc-gen4's stylesheet
  let stylePath := apiTempDir / "doc" / "style.css"
  if ← stylePath.pathExists then
    let existingCss ← IO.FS.readFile stylePath
    IO.FS.writeFile stylePath (existingCss ++ "\n" ++ verificationBadgesCss)
    IO.println s!"  Injected verification badge styles"

  IO.println s!"  Generated API docs to {apiTempDir}/"
  (← IO.getStdout).flush

  return apiTempDir

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

/-- Generate static HTML site -/
def generateStaticHtmlSite (cfg : UnifiedConfig) (result : UnifiedResult) (modules : List String) : IO System.FilePath := do
  IO.println s!"unified-doc [6/7]: Generating static HTML site..."
  (← IO.getStdout).flush

  -- Create output directory
  let siteDir := cfg.buildDir / "site"
  IO.FS.createDirAll siteDir
  IO.FS.createDirAll (siteDir / "modules")

  -- Generate per-module reports
  let reportCfg : ReportConfig := {
    outputDir := siteDir
    repoUrl := cfg.repoUrl
    platform := cfg.platform
    branch := cfg.branch
    modules := modules
    gitCache := result.gitCache
    docGenBaseUrl := some "../api"  -- Link to doc-gen4 docs from modules/X.html
  }

  IO.println s!"unified-doc: Generating per-module verification reports..."
  IO.println s!"  DEBUG: entries.size = {result.verificationEntries.size}"
  (← IO.getStdout).flush

  let reportsStartTime ← IO.monoMsNow
  let moduleReports := generatePerModuleReports result.env result.verificationEntries (some reportCfg)
  let reportsEndTime ← IO.monoMsNow
  let reportsDuration := formatDurationMs (reportsEndTime - reportsStartTime)
  IO.println s!"  DEBUG: generatePerModuleReports returned {moduleReports.size} reports ({reportsDuration})"
  (← IO.getStdout).flush

  -- Use StaticHtml generator
  -- dataFilesDir is the buildDir (parent of siteDir) where classification-cache.json etc. are stored
  let staticCfg : StaticHtml.StaticHtmlConfig := {
    outputDir := siteDir
    projectName := cfg.projectName
    repoUrl := cfg.repoUrl
    branch := cfg.branch
    apiBaseUrl := "api"
    workers := cfg.htmlWorkers
    dataFilesDir := some cfg.buildDir
    projectDescription := cfg.projectDescription
    projectModules := cfg.projectModules
    projectSettings := cfg.projectSettings
  }

  StaticHtml.generateStaticSite staticCfg result.verificationEntries moduleReports result.processingWarnings

  return siteDir

/-- Run the complete unified pipeline -/
def runUnifiedPipeline (cfg : UnifiedConfig) (modules : Array Name) : IO UInt32 := do
  try
    -- Step 1: Load and analyze
    let result ← loadAndAnalyze cfg modules

    -- Step 2: Generate doc-gen4 to temp location
    let apiTempDir ← generateDocGen4ToTemp cfg result

    -- Step 3: Generate static HTML site
    let siteDir ← generateStaticHtmlSite cfg result (modules.toList.map toString)

    -- Step 4: Copy doc-gen4 output into site
    IO.println s!"unified-doc: Copying API docs into site..."
    let apiDestDir := siteDir / "api"
    copyDirRecursive (apiTempDir / "doc") apiDestDir
    IO.println s!"  Copied API docs to {apiDestDir}/"

    -- Step 5: Create stub pages for missing dependency modules
    -- doc-gen4 generates links to dependencies (Batteries, Init, Std, etc.) that aren't included
    StaticHtml.createMissingDependencyStubs apiDestDir cfg.projectName cfg.externalDocs

    IO.println ""
    IO.println "✅ Unified documentation generated successfully!"
    IO.println s!"   Site:          {siteDir}/"
    IO.println s!"   Home:          {siteDir}/index.html"
    IO.println s!"   API:           {siteDir}/api/index.html"
    IO.println s!"   Verification:  {siteDir}/modules/"
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
      | .apiDef ⟨.mathematicalDefinition, _, _⟩ => "MathDef"
      | .apiDef ⟨.computationalOperation, _, _⟩ => "CompOp"
      | .apiTheorem _ => "Theorem"
    let entry : Json := Json.mkObj [
      ("name", Json.str name.toString),
      ("category", Json.str catStr),
      ("isTheorem", Json.bool m.isTheorem),
      ("anchor", Json.str (nameToAnchor name)),
      ("proves", Json.arr (m.proves.map (Json.str ∘ toString))),
      ("assumes", Json.arr (m.assumes.map (Json.str ∘ toString))),
      ("validates", Json.arr (m.validates.map (Json.str ∘ toString))),
      ("dependsOn", Json.arr (m.dependsOn.map (Json.str ∘ toString)))
    ]
    acc.push entry
  ) #[]

  let json := Json.arr jsonEntries
  IO.FS.writeFile jsonPath json.compress
  IO.println s!"  Written {jsonPath}"

end DocVerificationBridge.Unified
