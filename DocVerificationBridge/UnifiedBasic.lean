-- DocVerificationBridge/UnifiedBasic.lean
-- Basic unified pipeline for toolchains < 4.27.0 (without decorator support)
-- This version generates documentation without bidirectional navigation badges

import Lean
import DocGen4
import DocGen4.Load
import DocGen4.Output
import DocGen4.Process.Analyze
import DocGen4.Process.Hierarchy
import DocVerificationBridge.Classify
import DocVerificationBridge.Report
import DocVerificationBridge.StaticHtml

/-!
# Basic Unified Documentation Pipeline (No Decorator Support)

This module provides a unified CLI for documentation generation using static HTML.
This is the compatibility version for Lean < 4.27.0 toolchains that don't have
the doc-gen4 decorator support from PR #344.

The generated API documentation will NOT have verification badges or bidirectional
navigation links. Verification coverage pages are still generated separately.
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
  /-- Path to load classification cache from (skips classification step) -/
  loadClassificationCache : Option System.FilePath := none
  /-- Path to save classification cache to (for future runs) -/
  saveClassificationCache : Option System.FilePath := none
  /-- Number of parallel workers for HTML file writing (0 = sequential) -/
  htmlWorkers : Nat := 0
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
  }

/-- Generate doc-gen4 documentation to a temporary directory.
    This is the basic version without decorator support (for Lean < 4.27.0). -/
def generateDocGen4ToTemp (cfg : UnifiedConfig) (result : UnifiedResult) : IO System.FilePath := do
  IO.println s!"unified-doc [5/7]: Generating doc-gen4 API documentation..."
  IO.println s!"  Note: Verification badges not available (requires Lean >= 4.27.0)"
  (← IO.getStdout).flush

  -- Generate to a temp directory that we'll copy later
  let apiTempDir := cfg.buildDir / "api-temp"
  IO.FS.createDirAll apiTempDir

  let baseConfig ← DocGen4.getSimpleBaseContext apiTempDir result.hierarchy

  -- Call doc-gen4's htmlOutputResults WITHOUT decorator support (standard API)
  -- For Lean < 4.27.0, we use the basic API without custom linker or decorator
  let sourceUrl? := if cfg.repoUrl.isEmpty then none else some cfg.repoUrl
  discard <| DocGen4.htmlOutputResults baseConfig result.analyzerResult sourceUrl? none
  DocGen4.htmlOutputIndex baseConfig

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
  let staticCfg : StaticHtml.StaticHtmlConfig := {
    outputDir := siteDir
    projectName := cfg.projectName
    repoUrl := cfg.repoUrl
    branch := cfg.branch
    apiBaseUrl := "api"
    workers := cfg.htmlWorkers
  }

  StaticHtml.generateStaticSite staticCfg result.verificationEntries moduleReports

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

/-- Generate JSON mapping for external tools (stub for compatibility) -/
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
