-- UnifiedMain.lean
-- Unified CLI that combines doc-gen4 and doc-verification-bridge

import Cli
import Lean
import DocGen4
import DocGen4.Load
import DocVerificationBridge
import DocVerificationBridge.Unified
import DocVerificationBridge.Cache

/-!
# Unified Documentation CLI

This CLI provides commands to:
1. Generate combined doc-gen4 + verification documentation
2. Run standalone doc-gen4
3. Run standalone verification analysis

## Classification Modes

- `--auto` (default): Automatic classification using heuristics based on type analysis
- `--annotated`: Only classify declarations with explicit `@[api_type]`, `@[api_def]`,
  `@[api_theorem]` annotations

The unified mode shares module loading for efficiency.
-/

open Lean System Cli DocVerificationBridge DocVerificationBridge.Unified DocVerificationBridge.StaticHtml

/-!
## Timing Helpers
-/

/-- Format milliseconds as human-readable duration -/
def formatDuration (ms : Nat) : String :=
  let secs := ms / 1000
  let mins := secs / 60
  let hours := mins / 60
  if hours > 0 then
    s!"{hours}h {mins % 60}m {secs % 60}s"
  else if mins > 0 then
    s!"{mins}m {secs % 60}s"
  else
    s!"{secs}.{(ms % 1000) / 100}s"

/-- Print a timed step message with delta and cumulative time -/
def printTimedStep (step : String) (startTime : Nat) (lastStepTime : Nat) : IO Nat := do
  let now ← IO.monoMsNow
  let delta := now - lastStepTime
  let total := now - startTime
  IO.println s!"{step} (Δ{formatDuration delta}, total: {formatDuration total})"
  (← IO.getStdout).flush
  return now

/-- Classification mode for the verification analysis -/
inductive ClassificationMode
  | auto      -- Automatic heuristic-based classification
  | annotated -- Only use explicit annotations
deriving Repr, BEq

/-- Get api_type annotation metadata from the environment extension -/
def getApiTypeAnnotation (env : Environment) (declName : Name) : MetaM (Option APIMeta) := do
  return DocVerificationBridge.getApiTypeAttr env declName

/-- Get api_def annotation metadata from the environment extension -/
def getApiDefAnnotation (env : Environment) (declName : Name) : MetaM (Option APIMeta) := do
  return DocVerificationBridge.getApiDefAttr env declName

/-- Get api_theorem annotation metadata from the environment extension -/
def getApiTheoremAnnotation (env : Environment) (declName : Name) : MetaM (Option APIMeta) := do
  return DocVerificationBridge.getApiTheoremAttr env declName

/-- Try to get annotation metadata for a declaration -/
def tryGetAnnotation (env : Environment) (declName : Name) : MetaM (Option APIMeta) := do
  let typeOpt ← getApiTypeAnnotation env declName
  if typeOpt.isSome then return typeOpt
  let defOpt ← getApiDefAnnotation env declName
  if defOpt.isSome then return defOpt
  getApiTheoremAnnotation env declName

/-- Collect only declarations with explicit api_* annotations.
    Returns empty result if no annotations found (annotations not yet implemented). -/
def classifyAnnotatedDeclarations (env : Environment) (modName : Name) : MetaM ClassificationResult := do
  -- Get module's declarations
  let some modIdx := env.getModuleIdx? modName
    | return { entries := {}, notes := #[] }

  let moduleDecls := env.header.moduleData[modIdx.toNat]!.constNames

  let entries ← moduleDecls.foldlM (init := ({} : NameMap APIMeta)) fun acc declName => do
    let annot ← tryGetAnnotation env declName
    return annot.map (acc.insert declName ·) |>.getD acc

  return { entries, notes := #[] }

/-- Classify declarations based on the selected mode -/
def classifyWithMode (env : Environment) (modName : Name) (mode : ClassificationMode)
    (skipProofDeps : Bool := false) (proofDepWorkers : Nat := 0)
    (overallStartTime : Nat := 0) (proofDepBlacklist : Array String := #[])
    (slowThresholdSecs : Nat := 30) : MetaM ClassificationResult := do
  match mode with
  | .auto =>
    -- Use automatic heuristic-based classification
    classifyAllDeclarations env modName skipProofDeps proofDepWorkers overallStartTime proofDepBlacklist slowThresholdSecs
  | .annotated =>
    -- Only collect declarations with explicit annotations
    classifyAnnotatedDeclarations env modName

/-- Load modules and analyze with the specified classification mode -/
def loadAndAnalyzeWithMode (cfg : UnifiedConfig) (modules : Array Name)
    (mode : ClassificationMode) : IO (UnifiedResult × Nat × Nat) := do
  -- Initialize search path and timing
  let pipelineStart ← IO.monoMsNow
  let mut lastStep := pipelineStart

  Lean.initSearchPath (← Lean.findSysroot)

  IO.println s!"unified-doc [1/7]: Loading {modules.size} module(s)..."
  (← IO.getStdout).flush

  -- Load environment using doc-gen4's helper
  let env ← DocGen4.envOfImports modules

  -- Run doc-gen4's analysis
  let task := if modules.size == 1 then
    DocGen4.Process.AnalyzeTask.analyzePrefixModules modules[0]!
  else
    DocGen4.Process.AnalyzeTask.analyzeConcreteModules modules
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
  let ((analyzerResult, hierarchy), _) ← Meta.MetaM.toIO (DocGen4.Process.process task) config { env := env } {} {}

  -- Use first module name as project identifier for logging
  let projectName := if modules.isEmpty then "unknown" else modules[0]!.toString
  lastStep ← printTimedStep s!"  [{projectName}] Loaded {analyzerResult.moduleInfo.size} modules" pipelineStart lastStep

  -- Build git file cache from the source directory
  IO.println s!"unified-doc [2/7]: [{projectName}] Building git file cache from {cfg.sourceDir}..."
  (← IO.getStdout).flush
  let gitCache ← buildGitFileCacheIn cfg.sourceDir
  lastStep ← printTimedStep s!"  [{projectName}] Found {gitCache.allFiles.size} .lean files" pipelineStart lastStep

  -- Check if we should load classification from cache
  let allEntries ← if let some cachePath := cfg.loadClassificationCache then
    IO.println s!"unified-doc [3/7]: [{projectName}] Loading classification from cache..."
    (← IO.getStdout).flush
    let entries ← Cache.loadClassification cachePath
    lastStep ← printTimedStep s!"  [{projectName}] Loaded {entries.size} declarations from cache" pipelineStart lastStep
    pure entries
  else do
    -- Run classification with the specified mode
    let proofDepsInfo := if cfg.skipProofDeps then " (no proof deps)"
                         else if cfg.proofDepWorkers > 0 then s!" ({cfg.proofDepWorkers} workers)"
                         else " (sequential)"
    IO.println s!"unified-doc [3/7]: [{projectName}] Classifying declarations (mode: {repr mode}){proofDepsInfo}..."
    (← IO.getStdout).flush

    let mut allEntries : NameMap APIMeta := {}

    for modName in modules do
      let coreCtx : Core.Context := {
        options := {},
        fileName := "<verification-bridge>",
        fileMap := default,
        maxHeartbeats := 0  -- Unlimited heartbeats for classification
      }
      let coreState : Core.State := { env }
      let (result, _) ← (classifyWithMode env modName mode cfg.skipProofDeps cfg.proofDepWorkers pipelineStart cfg.proofDepBlacklist cfg.slowThresholdSecs).run' {} |>.toIO coreCtx coreState
      -- Convert to array first, then use foldl (tail-recursive) to avoid stack overflow
      let entriesArray := result.entries.foldl (init := #[]) fun acc name apiMeta => acc.push (name, apiMeta)
      allEntries := entriesArray.foldl (init := allEntries) fun acc (name, apiMeta) =>
        acc.insert name apiMeta

    lastStep ← printTimedStep s!"  [{projectName}] Classified {allEntries.size} declarations" pipelineStart lastStep

    -- Save to cache if requested
    if let some cachePath := cfg.saveClassificationCache then
      if let some parentDir := cachePath.parent then
        IO.FS.createDirAll parentDir
      Cache.saveClassification allEntries cfg.projectName cachePath

    if mode == .annotated && allEntries.isEmpty then
      IO.println "  ⚠ Warning: No annotated declarations found."
      IO.println "    Use @[api_type], @[api_def], or @[api_theorem] to annotate declarations,"
      IO.println "    or use --auto mode for automatic classification."

    pure allEntries

  return ({
    analyzerResult
    hierarchy
    env
    verificationEntries := allEntries
    gitCache
  }, pipelineStart, lastStep)

/-- Run unified pipeline with specified classification mode -/
def runUnifiedPipelineWithMode (cfg : UnifiedConfig) (modules : Array Name)
    (mode : ClassificationMode) : IO UInt32 := do
  try
    -- Step 1: Load and analyze with the specified mode
    let (result, pipelineStart, lastStepInit) ← loadAndAnalyzeWithMode cfg modules mode
    let mut lastStep := lastStepInit

    -- Step 2: Generate doc-gen4 to temp location (or use existing)
    let apiTempDir := cfg.buildDir / "api-temp"
    if cfg.skipDocGen then
      IO.println s!"unified-doc [5/7]: Skipping doc-gen4 (using existing output)..."
      -- Verify existing output exists
      let expectedDoc := apiTempDir / "doc"
      if !(← expectedDoc.pathExists) then
        throw <| IO.userError s!"--skip-docgen requires existing doc-gen4 output at {expectedDoc}"
      lastStep ← printTimedStep s!"  Found existing API docs at {apiTempDir}/" pipelineStart lastStep
    else
      let _ ← generateDocGen4ToTemp cfg result
      lastStep ← printTimedStep s!"  Generated API docs to {apiTempDir}/" pipelineStart lastStep

    -- Step 3: Generate static HTML site (includes verification)
    let siteDir ← generateStaticHtmlSite cfg result (modules.toList.map toString)
    lastStep ← printTimedStep s!"  Generated static HTML site at {siteDir}/" pipelineStart lastStep

    -- Step 4: Copy doc-gen4 output into site
    IO.println s!"unified-doc [7/7]: Copying API docs into site..."
    let apiDestDir := siteDir / "api"
    copyDirRecursive (apiTempDir / "doc") apiDestDir
    let _ ← printTimedStep s!"  Copied API docs to {apiDestDir}/" pipelineStart lastStep

    -- Step 5: Create stub pages for missing dependency modules
    -- doc-gen4 generates links to dependencies (Batteries, Init, Std, etc.) that aren't included
    -- Create in both locations for consistency
    StaticHtml.createMissingDependencyStubs apiDestDir cfg.projectName
    StaticHtml.createMissingDependencyStubs (apiTempDir / "doc") cfg.projectName

    -- Final timing
    let finalTime ← IO.monoMsNow
    let totalDuration := finalTime - pipelineStart

    IO.println ""
    IO.println s!"✅ Unified documentation generated successfully! (total: {formatDuration totalDuration})"
    IO.println s!"   Site:          {siteDir}/"
    IO.println s!"   Home:          {siteDir}/index.html"
    IO.println s!"   API:           {siteDir}/api/index.html"
    IO.println s!"   Verification:  {siteDir}/modules/"
    IO.println ""
    IO.println "To serve locally:"
    IO.println s!"   python3 -m http.server -d {siteDir} 8000"

    return (0 : UInt32)
  catch e =>
    IO.eprintln s!"Error: {e}"
    return (1 : UInt32)

/-- Run the unified doc-gen4 + verification pipeline -/
def runUnifiedCmd (args : Cli.Parsed) : IO UInt32 := do
  let modules : Array String := args.variableArgsAs! String

  if modules.isEmpty then
    IO.eprintln "Error: At least one module name required"
    return 1

  -- Validate module names - empty strings would become Name.anonymous which matches everything!
  for m in modules do
    if m.isEmpty then
      IO.eprintln s!"Error: Empty module name detected. Module names must be non-empty."
      IO.eprintln s!"  Received modules: {modules}"
      return 1

  -- Determine classification mode
  let mode : ClassificationMode :=
    if args.hasFlag "annotated" then .annotated
    else .auto  -- default

  let cfg : UnifiedConfig := {
    buildDir := args.flag? "output" |>.map (·.as! String) |>.getD ".lake/build/doc"
    repoUrl := args.flag? "repo" |>.map (·.as! String) |>.getD ""
    platform := if (args.flag? "platform" |>.map (·.as! String) |>.getD "github") == "gitlab"
                then .gitlab else .github
    branch := args.flag? "branch" |>.map (·.as! String) |>.getD "main"
    projectName := args.flag? "project" |>.map (·.as! String) |>.getD "Documentation"
    sourceDir := args.flag? "source-dir" |>.map (·.as! String) |>.map (·) |>.getD ".."
    generateVerification := !(args.hasFlag "no-verification")
    skipDocGen := args.hasFlag "skip-docgen"
    proofDepWorkers := args.flag? "proof-dep-workers" |>.map (·.as! Nat) |>.getD 0
    skipProofDeps := args.hasFlag "skip-proof-deps"
    proofDepBlacklist := args.flag? "proof-dep-blacklist" |>.map (·.as! String)
      |>.map (fun s => s.splitOn "," |>.map (·.trimCompat) |>.filter (·.length > 0) |>.toArray)
      |>.getD #[]
    loadClassificationCache := args.flag? "load-classification" |>.map (·.as! String)
    saveClassificationCache := args.flag? "save-classification" |>.map (·.as! String)
    htmlWorkers := args.flag? "html-workers" |>.map (·.as! Nat) |>.getD 0
    slowThresholdSecs := args.flag? "slow-threshold" |>.map (·.as! Nat) |>.getD 30
  }

  IO.println s!"Classification mode: {repr mode}"
  IO.println s!"Site generation: Static HTML (Python-free)"
  if cfg.skipProofDeps then
    IO.println s!"Proof deps: skipped"
  else if cfg.proofDepWorkers > 0 then
    IO.println s!"Proof dep workers: {cfg.proofDepWorkers} (dedicated threads)"
  (← IO.getStdout).flush
  runUnifiedPipelineWithMode cfg (modules.map String.toName) mode

/-- Run standalone doc-gen4 (delegates to doc-gen4's infrastructure) -/
def runDocGen4Cmd (args : Cli.Parsed) : IO UInt32 := do
  let modules : Array String := args.variableArgsAs! String

  if modules.isEmpty then
    IO.eprintln "Error: At least one module name required"
    return 1

  let buildDir := args.flag? "output" |>.map (·.as! String) |>.getD ".lake/build/doc"
  let sourceUrl := args.flag? "source-url" |>.map (·.as! String)

  IO.println s!"doc-gen4: Loading {modules.size} module(s)..."
  (← IO.getStdout).flush

  Lean.initSearchPath (← Lean.findSysroot)

  let moduleNames := modules.map String.toName
  let task := DocGen4.Process.AnalyzeTask.analyzeConcreteModules moduleNames
  let (result, hierarchy) ← DocGen4.load task

  IO.println s!"doc-gen4: Generating HTML..."
  (← IO.getStdout).flush
  DocGen4.htmlOutput buildDir result hierarchy sourceUrl

  IO.println s!"✅ Documentation generated at {buildDir}/doc/"
  (← IO.getStdout).flush
  return 0

/-- Run standalone verification analysis -/
def runVerifyCmd (args : Cli.Parsed) : IO UInt32 := do
  let modules : List String := (args.variableArgsAs! String).toList

  if modules.isEmpty then
    IO.eprintln "Error: At least one module name required"
    return 1

  -- Determine classification mode
  let mode : ClassificationMode :=
    if args.hasFlag "annotated" then .annotated
    else .auto  -- default

  let outputDir := args.flag? "output" |>.map (·.as! String) |>.getD "docs"
  let repoUrl := args.flag? "repo" |>.map (·.as! String) |>.getD ""
  let platformStr := args.flag? "platform" |>.map (·.as! String) |>.getD "github"
  let branch := args.flag? "branch" |>.map (·.as! String) |>.getD "main"
  let projectName := args.flag? "project" |>.map (·.as! String) |>.getD "API Coverage"

  let platform := if platformStr == "gitlab" then GitPlatform.gitlab else GitPlatform.github

  IO.println s!"verification-bridge: Building git file cache..."
  (← IO.getStdout).flush
  let gitCache ← buildGitFileCache
  IO.println s!"  Found {gitCache.allFiles.size} .lean files in repository"
  (← IO.getStdout).flush

  let cfg : ReportConfig := {
    outputDir := outputDir
    repoUrl := repoUrl
    platform := platform
    branch := branch
    modules := modules
    gitCache := gitCache
  }

  IO.println s!"verification-bridge: Analyzing {modules.length} module(s) (mode: {repr mode})..."
  (← IO.getStdout).flush

  Lean.initSearchPath (← Lean.findSysroot)

  let moduleNames := modules.map String.toName
  let options := {}
  let env ← importModules (moduleNames.toArray.map (fun n => { module := n })) options

  IO.println s!"  Loaded environment with {env.header.moduleNames.size} modules"
  (← IO.getStdout).flush

  let mut allEntries : NameMap APIMeta := {}
  for modName in moduleNames do
    let coreCtx : Core.Context := {
      options := {},
      fileName := "<verification-bridge>",
      fileMap := default,
      maxHeartbeats := 0  -- Unlimited heartbeats for classification
    }
    let coreState : Core.State := { env }
    let (result, _) ← (classifyWithMode env modName mode).run' {} |>.toIO coreCtx coreState
    -- Convert to array first, then use foldl (tail-recursive) to avoid stack overflow
    let entriesArray := result.entries.foldl (init := #[]) fun acc name apiMeta => acc.push (name, apiMeta)
    allEntries := entriesArray.foldl (init := allEntries) fun acc (name, apiMeta) =>
      acc.insert name apiMeta

  IO.println s!"  Classified {allEntries.size} declarations"
  (← IO.getStdout).flush

  if mode == .annotated && allEntries.isEmpty then
    IO.println "  ⚠ Warning: No annotated declarations found."
    IO.println "    Use @[api_type], @[api_def], or @[api_theorem] to annotate declarations,"
    IO.println "    or use --auto mode for automatic classification."

  let reportCfg := if repoUrl.isEmpty then none else some cfg
  let report := generateReport env allEntries reportCfg projectName modules

  let outputPath : System.FilePath := outputDir
  let docsPath := outputPath / "docs"
  let stylesheetsPath := docsPath / "stylesheets"

  IO.FS.createDirAll stylesheetsPath

  IO.FS.writeFile (docsPath / "API_Coverage.md") report
  IO.FS.writeFile (docsPath / "index.md") (generateIndexMd projectName)
  IO.FS.writeFile (stylesheetsPath / "extra.css") generateExtraCss

  IO.println s!"  Written to {outputPath}/"
  IO.println ""
  IO.println "To serve the documentation locally:"
  IO.println s!"  python3 -m http.server -d {outputPath} 8000"

  return 0

/-- Unified subcommand -/
def unifiedCmd := `[Cli|
  unified VIA runUnifiedCmd;
  "Generate combined doc-gen4 + verification documentation (recommended)"

  FLAGS:
    o, output : String;      "Output directory (default: .lake/build/doc)"
    r, repo : String;        "Repository URL for source links (e.g., https://github.com/owner/repo)"
    p, platform : String;    "Git platform: github or gitlab (default: github)"
    b, branch : String;      "Git branch name (default: main)"
    project : String;        "Project name (default: Documentation)"
    "source-dir" : String;   "Source directory for git file lookup (default: ..)"
    "no-verification";       "Skip verification report generation"
    "skip-docgen";           "Skip doc-gen4 generation (use existing api-temp output)"
    "skip-proof-deps";       "Skip proof dependency extraction (faster, but no dependsOn data)"
    "proof-dep-workers" : Nat; "Number of parallel workers for proof dep extraction (default: 0 = sequential)"
    "proof-dep-blacklist" : String; "Comma-separated list of theorem names to skip during proof dep extraction"
    "load-classification" : String; "Load classification from cache file (skip classification step)"
    "save-classification" : String; "Save classification to cache file (for future runs)"
    "html-workers" : Nat;    "Number of parallel workers for HTML file writing (default: 0 = sequential)"
    "slow-threshold" : Nat;  "Seconds before warning about slow theorem proof dep extraction (default: 30)"
    auto;                    "Use automatic heuristic-based classification (default)"
    annotated;               "Only classify declarations with explicit @[api_*] annotations"

  ARGS:
    ...modules : String;     "Module names to analyze"
]

/-- Doc-gen4 only subcommand -/
def docgen4Cmd := `[Cli|
  docgen4 VIA runDocGen4Cmd;
  "Generate doc-gen4 HTML documentation only"

  FLAGS:
    o, output : String;      "Output directory (default: .lake/build/doc)"
    "source-url" : String;   "Source URL template"

  ARGS:
    ...modules : String;     "Module names to document"
]

/-- Verification only subcommand -/
def verifyCmd := `[Cli|
  verify VIA runVerifyCmd;
  "Generate verification coverage report only (static HTML format)"

  FLAGS:
    o, output : String;    "Output directory for generated docs (default: docs)"
    r, repo : String;      "Repository URL for source links"
    p, platform : String;  "Git platform: github or gitlab (default: github)"
    b, branch : String;    "Git branch name (default: main)"
    project : String;      "Project name for documentation (default: API Coverage)"
    auto;                  "Use automatic heuristic-based classification (default)"
    annotated;             "Only classify declarations with explicit @[api_*] annotations"

  ARGS:
    ...modules : String;   "Module names to analyze"
]

/-- Default command shows help -/
def runDefaultCmd (_args : Cli.Parsed) : IO UInt32 := do
  IO.println "unified-doc - Unified Lean 4 Documentation"
  IO.println ""
  IO.println "Usage:"
  IO.println "  lake exe unified-doc unified [OPTIONS] <modules...>"
  IO.println "  lake exe unified-doc docgen4 [OPTIONS] <modules...>"
  IO.println "  lake exe unified-doc verify [OPTIONS] <modules...>"
  IO.println ""
  IO.println "Classification Modes:"
  IO.println "  --auto       Automatic heuristic-based classification (default)"
  IO.println "  --annotated  Only classify declarations with @[api_*] annotations"
  IO.println ""
  IO.println "Run with --help for more information."
  return 0

/-- Main CLI command with subcommands -/
def mainCmd : Cmd := `[Cli|
  "unified-doc" VIA runDefaultCmd; ["1.0.0"]
  "Unified Lean 4 documentation with theorem classification"

  SUBCOMMANDS:
    unifiedCmd;
    docgen4Cmd;
    verifyCmd

  EXTENSIONS:
    author "doc-verification-bridge contributors";
    longDescription "
unified-doc - Unified Lean 4 Documentation

Combines doc-gen4's HTML documentation with theorem classification
for specification-implementation refinement.

COMMANDS:

  unified   Generate combined doc-gen4 + verification (recommended)
  docgen4   Generate doc-gen4 HTML only
  verify    Generate verification report only (static HTML)

CLASSIFICATION MODES:

  --auto       Automatic heuristic-based classification (default)
               Uses type analysis to infer categories for all declarations.

  --annotated  Annotation-based classification
               Only classifies declarations with explicit attributes:
               @[api_type], @[api_def], @[api_theorem], @[api_lemma]

FOUR-CATEGORY ONTOLOGY:

  • Mathematical Abstractions: Abstract types, Prop-based structures
  • Computational Datatypes: Concrete data structures
  • Mathematical Definitions: Prop-returning predicates/relations
  • Computational Operations: Computable functions, Bool predicates

Bridging theorems prove the relations between these levels.

EXAMPLES:

  # Generate unified documentation with auto classification
  lake exe unified-doc unified --auto \\
    --repo https://github.com/leanprover-community/batteries \\
    --project 'Batteries' \\
    Batteries

  # Generate docs using only annotated declarations
  lake exe unified-doc unified --annotated \\
    --repo https://github.com/example/project \\
    MyProject

  # Just run verification analysis
  lake exe unified-doc verify \\
    --output docs \\
    --repo https://github.com/example/project \\
    MyProject
"
]

def main (args : List String) : IO UInt32 :=
  mainCmd.validate args
