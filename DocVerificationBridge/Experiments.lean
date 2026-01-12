/-
  Experiment runner for doc-verification-bridge evaluation.

  This module provides a parallel pipeline for:
  1. Cloning Lean 4 repositories
  2. Setting up docvb directories
  3. Building projects and running unified-doc
  4. Generating a meta-summary page with sortable statistics
-/
import Lean

open Lean System IO

namespace Experiments

/-! ## Configuration Types -/

/-- State of a project's experiment run -/
inductive ProjectState where
  | notStarted
  | inProgress
  | completed
  | failed
  deriving Repr, Inhabited, BEq

/-- Run mode for experiments -/
inductive RunMode where
  | fresh    -- Process all projects from scratch
  | resume   -- Skip completed, restart incomplete
  | update   -- Update repos and re-run for all
  deriving Repr, Inhabited, BEq

/-- Classification mode for documentation generation -/
inductive ClassificationMode
  | auto      -- Automatic heuristic-based classification (default)
  | annotated -- Only use explicit @[api_*] annotations
deriving Repr, Inhabited, BEq

instance : ToString ClassificationMode where
  toString
    | .auto => "auto"
    | .annotated => "annotated"

/-- A project to analyze -/
structure Project where
  name : String
  repo : String
  modules : Array String
  description : String := ""
  classificationMode : ClassificationMode := .auto
  /-- Subdirectory within repo containing the Lean project (for monorepos) -/
  subdirectory : Option String := none
  /-- Whether to run `lake exe cache get` before building (for mathlib4 and dependents) -/
  lakeExeCacheGet : Bool := false
  /-- Whether to disable equation generation (avoids timeouts for complex projects) -/
  disableEquations : Bool := false
  /-- Git branch name (auto-detected from repo if not specified) -/
  branch : Option String := none
  /-- Whether to skip proof dependency extraction (significantly speeds up large projects) -/
  skipProofDeps : Bool := false
  deriving Repr, Inhabited

/-- Experiment configuration -/
structure Config where
  docVerificationBridgePath : FilePath
  reposDir : FilePath
  sitesDir : FilePath
  basePort : Nat
  maxParallelJobs : Nat
  projects : Array Project
  /-- Use custom source linker from NicolasRouquette/doc-gen4 fork (requires feat/source-linker branch) -/
  useCustomSourceLinker : Bool := false
  deriving Repr, Inhabited

/-- Result of processing a project -/
structure ProjectResult where
  name : String
  repo : String
  success : Bool
  errorMessage : Option String := none
  buildLog : String := ""
  -- Statistics
  totalDefinitions : Nat := 0
  mathAbstractions : Nat := 0
  compDatatypes : Nat := 0
  mathDefinitions : Nat := 0
  compOperations : Nat := 0
  totalTheorems : Nat := 0
  computationalTheorems : Nat := 0
  mathematicalTheorems : Nat := 0
  bridgingTheorems : Nat := 0
  soundnessTheorems : Nat := 0
  completenessTheorems : Nat := 0
  unclassifiedTheorems : Nat := 0
  -- Sorry tracking
  defsWithSorry : Nat := 0
  theoremsWithSorry : Nat := 0
  siteDir : Option FilePath := none
  deriving Repr, Inhabited

/-! ## TOML Parsing (Simple) -/

/-- Parse a simple TOML config file -/
def parseConfig (content : String) (baseDir : FilePath) : IO Config := do
  -- Simple line-by-line parsing for our specific format
  let lines := content.splitOn "\n"

  let mut dvbPath : FilePath := ".."
  let mut reposDir : FilePath := "repos"
  let mut sitesDir : FilePath := "sites"
  let mut basePort : Nat := 9000
  let mut maxJobs : Nat := 8
  let mut useCustomSourceLinker : Bool := false
  let mut projects : Array Project := #[]
  let mut currentProject : Option Project := none

  for line in lines do
    let line := line.trimAscii.copy
    if line.startsWith "#" || line.isEmpty then continue

    if line.startsWith "[[projects]]" then
      -- Save previous project if any
      if let some proj := currentProject then
        projects := projects.push proj
      currentProject := some { name := "", repo := "", modules := #[] }
    else if line.startsWith "[settings]" then
      continue
    else if line.contains "=" then
      let parts := line.splitOn "="
      if parts.length >= 2 then
        let key := parts[0]!.trimAscii.copy
        -- Strip inline comments (everything after #) and quotes
        let rawValue := (parts.drop 1 |> String.intercalate "=").trimAscii.copy
        let valueNoComment := match rawValue.splitOn "#" with
          | v :: _ => v.trimAscii.copy
          | [] => rawValue
        let value := valueNoComment.replace "\"" ""

        match currentProject with
        | some proj =>
          let updatedProj := match key with
            | "name" => { proj with name := value }
            | "repo" => { proj with repo := value }
            | "description" => { proj with description := value }
            | "subdirectory" => { proj with subdirectory := some value }
            | "lake_exe_cache_get" => { proj with lakeExeCacheGet := value == "true" }
            | "disable_equations" => { proj with disableEquations := value == "true" }
            | "skip_proof_deps" => { proj with skipProofDeps := value == "true" }
            | "modules" =>
              -- Parse array like ["Batteries"]
              let mods := value.replace "[" "" |>.replace "]" ""
                |>.splitOn "," |>.map (·.trimAscii.copy.replace "\"" "")
                |>.filter (·.length > 0) |>.toArray
              { proj with modules := mods }
            | "classification_mode" =>
              let mode := if value == "annotated" then ClassificationMode.annotated
                          else ClassificationMode.auto
              { proj with classificationMode := mode }
            | "branch" => { proj with branch := some value }
            | _ => proj
          currentProject := some updatedProj
        | none =>
          match key with
          | "doc_verification_bridge_path" => dvbPath := baseDir / value
          | "repos_dir" => reposDir := baseDir / value
          | "sites_dir" => sitesDir := baseDir / value
          | "base_port" => basePort := value.toNat? |>.getD 9000
          | "max_parallel_jobs" => maxJobs := value.toNat? |>.getD 8
          | "use_custom_source_linker" => useCustomSourceLinker := value == "true"
          | _ => pure ()

  -- Don't forget the last project
  if let some proj := currentProject then
    projects := projects.push proj

  return {
    docVerificationBridgePath := dvbPath
    reposDir := reposDir
    sitesDir := sitesDir
    basePort := basePort
    maxParallelJobs := maxJobs
    projects := projects
    useCustomSourceLinker := useCustomSourceLinker
  }

/-! ## Utilities -/

/-- Split an array into chunks of at most `n` elements -/
def chunks (arr : Array α) (n : Nat) : Array (Array α) :=
  if n == 0 then #[arr]
  else Id.run do
    let mut result : Array (Array α) := #[]
    let mut i := 0
    while i < arr.size do
      let chunk := arr.extract i (min (i + n) arr.size)
      result := result.push chunk
      i := i + n
    return result

/-- Get the state file path for a project -/
def stateFilePath (sitesDir : FilePath) (projectName : String) : FilePath :=
  sitesDir / projectName / ".state"

/-- Read the current state of a project -/
def readProjectState (sitesDir : FilePath) (projectName : String) : IO ProjectState := do
  let path := stateFilePath sitesDir projectName
  if ← path.pathExists then
    let content ← IO.FS.readFile path
    match content.trimAscii.copy with
    | "in-progress" => return .inProgress
    | "completed" => return .completed
    | "failed" => return .failed
    | _ => return .notStarted
  else
    return .notStarted

/-- Write the state of a project -/
def writeProjectState (sitesDir : FilePath) (projectName : String) (state : ProjectState) : IO Unit := do
  let path := stateFilePath sitesDir projectName
  IO.FS.createDirAll (sitesDir / projectName)
  let stateStr := match state with
    | .notStarted => "not-started"
    | .inProgress => "in-progress"
    | .completed => "completed"
    | .failed => "failed"
  IO.FS.writeFile path stateStr

/-- Remove a directory recursively -/
def removeDir (path : FilePath) : IO Unit := do
  if ← path.pathExists then
    discard <| IO.Process.output { cmd := "rm", args := #["-rf", path.toString] }

/-! ## Command Logging -/

/-- Status of a command execution -/
inductive CommandStatus where
  | running
  | success
  | failed
  deriving Repr, Inhabited, BEq

instance : ToString CommandStatus where
  toString
    | .running => "running"
    | .success => "success"
    | .failed => "failed"

/-- A logged command entry -/
structure CommandLogEntry where
  step : String
  command : String
  args : Array String
  cwd : Option String
  status : CommandStatus := .running
  exitCode : Option UInt32 := none
  duration_ms : Option Nat := none
  deriving Repr, Inhabited

/-- Mutable command log for a project -/
abbrev CommandLog := Array CommandLogEntry

/-- Context needed for incremental log saving -/
structure LogContext where
  projectName : String
  repo : String
  outputDir : FilePath
  deriving Repr, Inhabited

/-- Format command log as YAML -/
def commandLogToYaml (log : CommandLog) (projectName : String) (repo : String) : String :=
  let entries := log.toList.map fun e =>
    let cwdLine := match e.cwd with
      | some cwd => s!"    cwd: \"{cwd}\"\n"
      | none => ""
    let argsFormatted := e.args.toList.map (s!"\"{·}\"") |> String.intercalate ", "
    let exitCodeLine := match e.exitCode with
      | some code => s!"    exit_code: {code}\n"
      | none => ""
    s!"  - step: \"{e.step}\"
    command: \"{e.command}\"
    args: [{argsFormatted}]
{cwdLine}    status: \"{e.status}\"
{exitCodeLine}"
  let entriesStr := String.intercalate "\n" entries
  s!"# Command log for {projectName}
# Repository: {repo}
# Generated by doc-verification-bridge experiments

project: \"{projectName}\"
repository: \"{repo}\"
commands:
{entriesStr}
"

/-- Save command log to a YAML file -/
def saveCommandLog (log : CommandLog) (projectName : String) (repo : String)
    (outputDir : FilePath) : IO Unit := do
  IO.FS.createDirAll outputDir
  let yaml := commandLogToYaml log projectName repo
  IO.FS.writeFile (outputDir / "commands.yaml") yaml

/-- Save command log using context -/
def saveCommandLogCtx (log : CommandLog) (ctx : LogContext) : IO Unit :=
  saveCommandLog log ctx.projectName ctx.repo ctx.outputDir

/-! ## Shell Commands -/

/-- Run a shell command with incremental logging (writes before and after execution) -/
def runCmdLogged (step : String) (cmd : String) (args : Array String)
    (cwd : Option FilePath := none) (log : CommandLog := #[])
    (ctx : Option LogContext := none)
    (_timeout : Nat := 3600) (env : Array (String × Option String) := #[])
    : IO (Bool × String × CommandLog) := do
  -- Create entry with "running" status
  let runningEntry : CommandLogEntry := {
    step := step
    command := cmd
    args := args
    cwd := cwd.map toString
    status := .running
    exitCode := none
  }

  -- Add to log and save BEFORE execution
  let logWithRunning := log.push runningEntry

  if let some c := ctx then
    saveCommandLogCtx logWithRunning c

  -- Execute the command
  let result ← IO.Process.output {
    cmd := cmd
    args := args
    cwd := cwd
    env := env
  }

  -- Update entry with result
  let completedEntry : CommandLogEntry := {
    runningEntry with
    status := if result.exitCode == 0 then .success else .failed
    exitCode := some result.exitCode
  }

  -- Replace running entry with completed entry
  let finalLog := log.push completedEntry

  -- Save AFTER execution
  if let some c := ctx then
    saveCommandLogCtx finalLog c

  return (result.exitCode == 0, result.stdout ++ result.stderr, finalLog)

/-- Run a shell command and return (success, output) - legacy interface -/
def runCmd (cmd : String) (args : Array String) (cwd : Option FilePath := none)
    (_timeout : Nat := 3600) (env : Array (String × Option String) := #[]) : IO (Bool × String) := do
  let result ← IO.Process.output {
    cmd := cmd
    args := args
    cwd := cwd
    env := env
  }
  return (result.exitCode == 0, result.stdout ++ result.stderr)

/-- Detect the current branch of a git repository -/
def detectGitBranch (repoDir : FilePath) : IO String := do
  let result ← IO.Process.output {
    cmd := "git"
    args := #["rev-parse", "--abbrev-ref", "HEAD"]
    cwd := some repoDir
  }
  if result.exitCode == 0 then
    return result.stdout.trimAscii.copy
  else
    -- Fallback to main if detection fails
    return "main"

/-- Clone or update a git repository -/
def cloneRepository (repoUrl : String) (targetDir : FilePath) : IO (Bool × String) := do
  if ← targetDir.pathExists then
    -- Pull latest
    runCmd "git" #["pull"] (some targetDir)
  else
    -- Clone
    runCmd "git" #["clone", "--depth", "1", repoUrl, targetDir.toString] none

/-- Detect the main package name from a lakefile -/
def detectPackageName (projectDir : FilePath) (fallback : String) : IO String := do
  let lakefileToml := projectDir / "lakefile.toml"
  let lakefileLean := projectDir / "lakefile.lean"

  if ← lakefileToml.pathExists then
    let content ← IO.FS.readFile lakefileToml
    for line in content.splitOn "\n" do
      let line := line.trimAscii.copy
      if line.startsWith "name" && line.contains "=" then
        let parts := line.splitOn "="
        if parts.length >= 2 then
          return parts[1]!.trimAscii.copy.replace "\"" ""
    return fallback
  else if ← lakefileLean.pathExists then
    let content ← IO.FS.readFile lakefileLean
    -- Look for: package «name» or package name
    for line in content.splitOn "\n" do
      if line.contains "package" then
        -- Extract name after "package"
        let afterPackage := (line.splitOn "package").getD 1 ""
        let name := afterPackage.trimAscii.copy
          |>.replace "«" "" |>.replace "»" ""
          |>.splitOn " " |>.getD 0 ""
          |>.replace "where" "" |>.trimAscii.copy
        if name.length > 0 then
          return name
    return fallback
  else
    return fallback

/-- Read a lean-toolchain file and return the version string -/
def readToolchain (path : FilePath) : IO (Option String) := do
  if ← path.pathExists then
    let content ← IO.FS.readFile path
    return some content.trimAscii.copy
  else
    return none

/-- Parse a Lean version string like "leanprover/lean4:v4.7.0" into major.minor.patch -/
def parseVersion (toolchain : String) : Option (Nat × Nat × Nat) := do
  -- Extract version after :v
  let parts := toolchain.splitOn ":v"
  if parts.length < 2 then failure
  let versionPart := parts[1]!
  -- Take characters while they are digits or dots
  let versionChars := versionPart.toList.takeWhile (fun c => c.isDigit || c == '.')
  let versionStr := String.mk versionChars
  let nums := versionStr.splitOn "."
  if nums.length < 3 then failure
  let major ← nums[0]!.toNat?
  let minor ← nums[1]!.toNat?
  let patch ← nums[2]!.toNat?
  return (major, minor, patch)

/-- Check if version a >= version b -/
def versionGe (a b : Nat × Nat × Nat) : Bool :=
  let (aMaj, aMin, aPat) := a
  let (bMaj, bMin, bPat) := b
  if aMaj > bMaj then true
  else if aMaj < bMaj then false
  else if aMin > bMin then true
  else if aMin < bMin then false
  else aPat >= bPat

/-- Result of checking toolchain compatibility -/
structure ToolchainCheck where
  projectToolchain : String
  dvbToolchain : String
  compatible : Bool
  message : String
  /-- The doc-gen4 version tag to use (matches project toolchain) -/
  docgen4Tag : String := ""
  deriving Repr, Inhabited

/-- Minimum supported Lean version for doc-verification-bridge -/
def minSupportedVersion : (Nat × Nat × Nat) := (4, 24, 0)

/-- Maximum supported Lean version for doc-verification-bridge -/
def maxSupportedVersion : (Nat × Nat × Nat) := (4, 27, 0)

/-- Extract version tag from toolchain string (e.g., "leanprover/lean4:v4.26.0" → "v4.26.0") -/
def extractVersionTag (toolchain : String) : String :=
  match toolchain.splitOn ":v" with
  | [_, ver] => s!"v{ver.trimAscii.toString}"
  | _ => match toolchain.splitOn ":" with
    | [_, ver] => ver.trimAscii.toString
    | _ => "main"

/-- Check if a project's toolchain is compatible with doc-verification-bridge -/
def checkToolchainCompatibility (projectDir : FilePath) (dvbPath : FilePath)
    : IO ToolchainCheck := do
  let projectTc ← readToolchain (projectDir / "lean-toolchain")
  let dvbTc ← readToolchain (dvbPath / "lean-toolchain")

  let projectStr := projectTc.getD "unknown"
  let dvbStr := dvbTc.getD "unknown"

  match projectTc with
  | some pTc =>
    match parseVersion pTc with
    | some pVer =>
      let versionTag := extractVersionTag pTc
      -- Check: minVersion <= projectVersion <= maxVersion
      if versionGe pVer minSupportedVersion && versionGe maxSupportedVersion pVer then
        return { projectToolchain := projectStr, dvbToolchain := dvbStr,
                 compatible := true, docgen4Tag := versionTag,
                 message := s!"✓ Project uses {projectStr} (supported range: v{minSupportedVersion.1}.{minSupportedVersion.2.1}.{minSupportedVersion.2.2} - v{maxSupportedVersion.1}.{maxSupportedVersion.2.1}.{maxSupportedVersion.2.2})" }
      else if !versionGe pVer minSupportedVersion then
        return { projectToolchain := projectStr, dvbToolchain := dvbStr,
                 compatible := false, docgen4Tag := versionTag,
                 message := s!"✗ Toolchain too old: project uses {projectStr} but minimum supported is v{minSupportedVersion.1}.{minSupportedVersion.2.1}.{minSupportedVersion.2.2}" }
      else
        return { projectToolchain := projectStr, dvbToolchain := dvbStr,
                 compatible := false, docgen4Tag := versionTag,
                 message := s!"✗ Toolchain too new: project uses {projectStr} but maximum supported is v{maxSupportedVersion.1}.{maxSupportedVersion.2.1}.{maxSupportedVersion.2.2}" }
    | none =>
      -- Can't parse version, try to use anyway
      let versionTag := extractVersionTag pTc
      return { projectToolchain := projectStr, dvbToolchain := dvbStr,
               compatible := true, docgen4Tag := versionTag,
               message := s!"? Could not parse version from {projectStr}, attempting anyway" }
  | none =>
    return { projectToolchain := "missing", dvbToolchain := dvbStr,
             compatible := false,
             message := "✗ Project has no lean-toolchain file" }

/-- Copy a file if source exists -/
def copyFileIfExists (src dst : FilePath) : IO Bool := do
  if ← src.pathExists then
    let content ← IO.FS.readFile src
    IO.FS.writeFile dst content
    return true
  return false

/-- Setup the docvb directory for a project with dynamic toolchain support -/
def setupDocvbDirectory (projectDir : FilePath) (projectName : String)
    (dvbPath : FilePath) (_modules : Array String) (useCustomSourceLinker : Bool := false)
    : IO (Bool × ToolchainCheck) := do
  let docvbDir := projectDir / "docvb"
  IO.FS.createDirAll docvbDir

  -- Check toolchain compatibility FIRST
  let tcCheck ← checkToolchainCompatibility projectDir dvbPath

  -- Use the PROJECT's toolchain for docvb (enables cross-version compatibility)
  let toolchainSrc := projectDir / "lean-toolchain"
  let toolchainDst := docvbDir / "lean-toolchain"
  if ← toolchainSrc.pathExists then
    let content ← IO.FS.readFile toolchainSrc
    IO.FS.writeFile toolchainDst content
  else
    IO.FS.writeFile toolchainDst s!"leanprover/lean4:v{minSupportedVersion.1}.{minSupportedVersion.2.1}.{minSupportedVersion.2.2}\n"

  -- Detect main package name
  let mainPackage ← detectPackageName projectDir projectName

  -- Copy doc-verification-bridge source files to docvb (avoids cross-toolchain dependency)
  let dvbSrcDir := docvbDir / "DocVerificationBridge"
  IO.FS.createDirAll dvbSrcDir

  -- Copy all necessary source files
  let sourceFiles := #["Types.lean", "Attributes.lean", "Inference.lean",
                       "Classify.lean", "Report.lean", "Unified.lean"]
  for file in sourceFiles do
    discard <| copyFileIfExists (dvbPath / "DocVerificationBridge" / file) (dvbSrcDir / file)

  -- Copy the appropriate SourceLinkerCompat version based on config
  -- Standard version: uses official doc-gen4 3-arg API
  -- Custom version: uses NicolasRouquette/doc-gen4 fork with 4-arg API
  let compatSrcFile := if useCustomSourceLinker
    then dvbPath / "DocVerificationBridge" / "SourceLinkerCompatCustom.lean"
    else dvbPath / "DocVerificationBridge" / "SourceLinkerCompat.lean"
  discard <| copyFileIfExists compatSrcFile (dvbSrcDir / "SourceLinkerCompat.lean")

  -- Copy UnifiedMain.lean
  discard <| copyFileIfExists (dvbPath / "UnifiedMain.lean") (docvbDir / "UnifiedMain.lean")

  -- Copy DocVerificationBridge.lean (the root module file)
  discard <| copyFileIfExists (dvbPath / "DocVerificationBridge.lean") (docvbDir / "DocVerificationBridge.lean")

  -- Determine which doc-gen4 version/repo to use
  -- If custom source linker is enabled, use the fork; otherwise use official release tags
  let (docgen4Repo, docgen4Ref) := if useCustomSourceLinker then
    ("https://github.com/NicolasRouquette/doc-gen4", "feat/source-linker")
  else
    let tag := if tcCheck.docgen4Tag.isEmpty then "main" else tcCheck.docgen4Tag
    ("https://github.com/leanprover/doc-gen4", tag)

  -- Create lakefile.lean (using .lean format for more flexibility)
  -- Note: We do NOT share packagesDir with the main project because toolchains may differ
  let lakefileContent := s!"import Lake
open Lake DSL

package docvb where
  -- Share parent project's packagesDir so we can find transitive dependencies
  packagesDir := \"../.lake/packages\"

-- Require the main project
require «{mainPackage}» from \"../\"

-- Require doc-gen4
require «doc-gen4» from git
  \"{docgen4Repo}\" @ \"{docgen4Ref}\"

-- Local library with doc-verification-bridge sources (copied for toolchain compatibility)
lean_lib DocVerificationBridge where

-- Unified CLI executable
@[default_target]
lean_exe «unified-doc» where
  root := `UnifiedMain
  supportInterpreter := true
"
  IO.FS.writeFile (docvbDir / "lakefile.lean") lakefileContent

  -- Download lake-manifest.json from doc-gen4's repo/ref to lock transitive dependencies
  -- Only download for official doc-gen4 releases (the fork may not have matching manifest)
  if !useCustomSourceLinker then
    let manifestUrl := s!"https://raw.githubusercontent.com/leanprover/doc-gen4/{docgen4Ref}/lake-manifest.json"
    let (manifestOk, manifestContent) ← runCmd "curl" #["-fsSL", manifestUrl] none 30
    if manifestOk && !manifestContent.isEmpty then
      IO.FS.writeFile (docvbDir / "lake-manifest.json") manifestContent
      IO.println s!"[{projectName}] Downloaded lake-manifest.json from doc-gen4@{docgen4Ref}"
    else
      IO.println s!"[{projectName}] Warning: Could not download lake-manifest.json from doc-gen4@{docgen4Ref}"
  else
    IO.println s!"[{projectName}] Using custom source linker (fork: {docgen4Repo}@{docgen4Ref})"

  -- Remove old lakefile.toml if it exists (we're now using lakefile.lean)
  let oldLakefile := docvbDir / "lakefile.toml"
  if ← oldLakefile.pathExists then
    IO.FS.removeFile oldLakefile

  return (!tcCheck.compatible, tcCheck)

/-- Build a project with lake -/
def buildProject (projectDir : FilePath) (lakeExeCacheGet : Bool := false) : IO (Bool × String) := do
  -- For projects like mathlib4, fetch the cloud cache first
  if lakeExeCacheGet then
    let (cacheOk, cacheLog) ← runCmd "lake" #["exe", "cache", "get"] (some projectDir) 3600
    if !cacheOk then
      return (false, s!"lake exe cache get failed:\n{cacheLog}")
  runCmd "lake" #["build"] (some projectDir) 3600

/-- Run unified-doc to generate documentation -/
def runUnifiedDoc (docvbDir : FilePath) (modules : Array String)
    (outputDir : FilePath) (classMode : ClassificationMode := .auto)
    (disableEquations : Bool := false) (projectToolchain : String := "") : IO (Bool × String) := do
  -- Update lake dependencies with retry logic to handle lock contention
  -- Note: Post-update hook failures (e.g., mathlib cache) are treated as warnings, not errors
  let mut updateOk := false
  let mut updateLog := ""
  let maxRetries := 5
  for attempt in [:maxRetries] do
    -- Update all dependencies (doc-gen4, Cli, main project)
    let (ok, log) ← runCmd "lake" #["update"] (some docvbDir) 600
    if ok then
      updateOk := true
      updateLog := log
      break
    else if log.contains "could not acquire" && attempt + 1 < maxRetries then
      -- Lock contention - wait a bit and retry
      let delayMs := (attempt + 1) * 2000  -- 2s, 4s, 6s, 8s
      IO.println s!"  Lock contention detected, retrying in {delayMs}ms..."
      IO.sleep (delayMs.toUInt32)
      updateLog := log
    else if log.contains "post-update hooks" || log.contains "failed to fetch cache" then
      -- Post-update hook failure (e.g., mathlib cache pruning) - treat as warning, not error
      IO.println s!"  Warning: post-update hooks failed, continuing anyway..."
      updateOk := true  -- Treat as success - the actual update likely worked
      updateLog := log
      break
    else
      updateLog := log
      break

  if !updateOk then
    return (false, s!"lake update failed:\n{updateLog}")

  -- CRITICAL: Restore the project's toolchain after lake update
  -- Lake/Elan may have "updated" it to a different version during post-update hooks
  if !projectToolchain.isEmpty then
    let toolchainPath := docvbDir / "lean-toolchain"
    IO.FS.writeFile toolchainPath s!"{projectToolchain}\n"

  -- Build docvb
  let (buildOk, buildLog) ← runCmd "lake" #["build"] (some docvbDir) 1800
  if !buildOk then
    return (false, s!"lake build failed:\n{buildLog}")

  -- Run unified-doc (the executable is "unified-doc", subcommand is "unified")
  -- Add --auto or --annotated based on classification mode
  let modeFlag := if classMode == .annotated then "--annotated" else "--auto"
  let args := #["exe", "unified-doc", "unified", modeFlag,
                "--output", outputDir.toString] ++ modules

  -- Set DISABLE_EQUATIONS=1 if requested (avoids timeouts for complex projects like mathlib4)
  let env : Array (String × Option String) := if disableEquations then
    #[("DISABLE_EQUATIONS", some "1")]
  else
    #[]

  if disableEquations then
    IO.println s!"  (DISABLE_EQUATIONS=1 for faster processing)"

  let (docOk, docLog) ← runCmd "lake" args (some docvbDir) 3600 env

  return (docOk, updateLog ++ "\n" ++ buildLog ++ "\n" ++ docLog)

/-! ## Statistics Parsing -/

/-- Parse coverage statistics from a coverage.md file -/
def parseCoverageStats (coveragePath : FilePath) : IO ProjectResult := do
  let emptyResult : ProjectResult := {
    name := "", repo := "", success := true
  }

  -- Try the new index.md format first, then fall back to old coverage.md
  let indexPath := coveragePath.parent.get! / "index.md"
  let actualPath := if (← indexPath.pathExists) then indexPath else coveragePath

  if !(← actualPath.pathExists) then
    return emptyResult

  let content ← IO.FS.readFile actualPath

  let parseCount (pattern : String) : Nat := Id.run do
    -- Look for "| Pattern | count |" in markdown tables
    let lines := content.splitOn "\n"
    for line in lines do
      if line.contains pattern then
        -- Extract number after the pattern
        let parts := line.splitOn "|"
        for part in parts do
          -- Strip markdown bold markers (**) before parsing
          let cleaned := part.trimAscii.replace "**" ""
          if let some n := cleaned.toNat? then
            return n
    return 0

  -- New index.md format uses "| Definitions |" and "| Theorems |" in Overview section
  -- Old coverage.md uses "**Total Definitions**" and "**Total Theorems**"
  let totalDefs := parseCount "| Definitions |"
  let totalDefs := if totalDefs > 0 then totalDefs else parseCount "**Total Definitions**"

  let totalThms := parseCount "| Theorems |"
  let totalThms := if totalThms > 0 then totalThms else parseCount "**Total Theorems**"

  -- Parse sorry stats from the new "Proof Completeness" section
  -- The sorry line has format "| Definitions | total | withSorry | proven | pct |"
  -- We need to parse specifically from the Proof Completeness table
  let parseFromSorryTable (category : String) : Nat := Id.run do
    let lines := content.splitOn "\n"
    let mut inSorrySection := false
    for line in lines do
      if line.contains "Proof Completeness" then
        inSorrySection := true
      else if inSorrySection && line.startsWith "###" then
        inSorrySection := false  -- Hit next section
      else if inSorrySection && line.contains category then
        let parts := line.splitOn "|"
        -- Format: | Category | Total | With Sorry | Proven | % Proven |
        if parts.length >= 4 then
          let withSorryPart := parts[3]!.trimAscii
          if let some n := withSorryPart.toNat? then
            return n
    return 0

  return {
    emptyResult with
    totalDefinitions := totalDefs
    mathAbstractions := parseCount "Mathematical Abstractions"
    compDatatypes := parseCount "Computational Datatypes"
    mathDefinitions := parseCount "Mathematical Definitions"
    compOperations := parseCount "Computational Operations"
    totalTheorems := totalThms
    computationalTheorems := parseCount "| Computational |"
    mathematicalTheorems := parseCount "| Mathematical |"
    bridgingTheorems := parseCount "| Bridging |"
    soundnessTheorems := parseCount "| Soundness |"
    completenessTheorems := parseCount "| Completeness |"
    unclassifiedTheorems := parseCount "| Unclassified |"
    defsWithSorry := parseFromSorryTable "Definitions"
    theoremsWithSorry := parseFromSorryTable "Theorems"
  }

/-! ## HTML Generation -/

/-- Generate an error page for a failed build with detailed diagnostics -/
def generateErrorPage (projectName : String) (errorMsg : String)
    (buildLog : String) (outputDir : FilePath)
    (toolchainInfo : Option ToolchainCheck := none) : IO Unit := do
  IO.FS.createDirAll outputDir

  let toolchainSection := match toolchainInfo with
    | some tc =>
      let statusColor := if tc.compatible then "#4ecdc4" else "#ff6b6b"
      let statusIcon := if tc.compatible then "✓" else "✗"
      s!"<div class='toolchain-info'>
<h2>Toolchain Information</h2>
<table class='info-table'>
<tr><td>Project Toolchain</td><td>{tc.projectToolchain}</td></tr>
<tr><td>doc-verification-bridge Toolchain</td><td>{tc.dvbToolchain}</td></tr>
<tr><td>Status</td><td style='color: {statusColor}'>{statusIcon} {if tc.compatible then "Compatible" else "Incompatible"}</td></tr>
</table>
<p class='message'>{tc.message}</p>
</div>"
    | none => ""

  let css := "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 40px; background: #1a1a2e; color: #eee; } h1 { color: #ff6b6b; } h2 { color: #fff; margin-top: 30px; } .error { background: #2d2d44; padding: 20px; border-radius: 8px; border-left: 4px solid #ff6b6b; margin-bottom: 20px; } .toolchain-info { background: #2d2d44; padding: 20px; border-radius: 8px; border-left: 4px solid #ffa500; margin-bottom: 20px; } .info-table { width: 100%; border-collapse: collapse; margin: 10px 0; } .info-table td { padding: 8px; border-bottom: 1px solid rgba(255,255,255,0.1); } .info-table td:first-child { color: #888; width: 250px; } .message { margin-top: 15px; padding: 10px; background: rgba(0,0,0,0.2); border-radius: 4px; } pre { background: #16213e; padding: 15px; border-radius: 4px; overflow-x: auto; font-size: 12px; white-space: pre-wrap; max-height: 500px; overflow-y: auto; } a { color: #4ecdc4; } .commands-link { margin-top: 10px; display: block; }"

  let commandsLink := "<p class='commands-link'>📋 <a href='commands.yaml'>View command log (YAML)</a></p>"

  let html := s!"<!DOCTYPE html>
<html>
<head>
<meta charset=\"UTF-8\">
<title>Build Failed: {projectName}</title>
<style>{css}</style>
</head>
<body>
<h1>❌ Build Failed: {projectName}</h1>
<div class='error'>
<h2>Error Summary</h2>
<pre>{errorMsg}</pre>
</div>
{toolchainSection}
{commandsLink}
<h2>Full Build Log</h2>
<pre>{buildLog}</pre>
<p><a href='/'>← Back to Summary</a></p>
</body>
</html>"

  IO.FS.writeFile (outputDir / "index.html") html

/-- Generate a status page for in-progress or incomplete projects showing commands.yaml -/
def generateStatusPage (projectName : String) (repo : String) (outputDir : FilePath)
    (state : ProjectState) : IO Unit := do
  -- Read commands.yaml if it exists
  let commandsPath := outputDir / "commands.yaml"
  let commandsContent ← if ← commandsPath.pathExists then
    IO.FS.readFile commandsPath
  else
    pure "# No commands recorded yet"

  let stateLabel := match state with
    | .notStarted => ("⏳", "Not Started", "#888")
    | .inProgress => ("🔄", "In Progress", "#ffa500")
    | .completed => ("✅", "Completed", "#4ecdc4")
    | .failed => ("❌", "Failed", "#ff6b6b")

  let css := "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 40px; background: #1a1a2e; color: #eee; } h1 { color: #4ecdc4; } h2 { color: #fff; margin-top: 30px; } .status-box { background: #2d2d44; padding: 20px; border-radius: 8px; margin-bottom: 20px; } .status-label { font-size: 1.2em; margin-bottom: 10px; } .repo-link { color: #4ecdc4; } pre { background: #16213e; padding: 15px; border-radius: 4px; overflow-x: auto; font-size: 12px; white-space: pre-wrap; max-height: 600px; overflow-y: auto; } a { color: #4ecdc4; } .site-link { margin-top: 20px; padding: 15px; background: rgba(78, 205, 196, 0.1); border-radius: 8px; border-left: 4px solid #4ecdc4; }"

  let siteLink := if ← (outputDir / "site" / "index.html").pathExists then
    s!"<div class='site-link'><strong>📚 Site Ready:</strong> <a href='site/'>View Documentation Site</a></div>"
  else ""

  let html := s!"<!DOCTYPE html>
<html>
<head>
<meta charset=\"UTF-8\">
<title>{projectName} - Build Status</title>
<style>{css}</style>
</head>
<body>
<h1>{stateLabel.1} {projectName}</h1>
<div class='status-box'>
<p class='status-label'>Status: <span style='color: {stateLabel.2.2}'>{stateLabel.2.1}</span></p>
<p>Repository: <a class='repo-link' href='{repo}' target='_blank'>{repo}</a></p>
</div>
{siteLink}
<h2>📋 Command Log</h2>
<pre>{commandsContent}</pre>
<p><a href='/'>← Back to Summary</a></p>
</body>
</html>"

  IO.FS.createDirAll outputDir
  IO.FS.writeFile (outputDir / "index.html") html

/-- Generate the meta-summary page -/
def generateSummaryPage (results : Array ProjectResult) (outputPath : FilePath) : IO Unit := do
  let successful := results.filter (·.success)
  let unsuccessful := results.filter (!·.success)
  -- Separate incomplete (no error message or "Incomplete") from actual failures
  let incomplete := unsuccessful.filter fun r =>
    r.errorMessage.isNone || r.errorMessage == some "Incomplete"
  let failed := unsuccessful.filter fun r =>
    r.errorMessage.isSome && r.errorMessage != some "Incomplete"

  let totalDefs := successful.foldl (· + ·.totalDefinitions) 0
  let totalThms := successful.foldl (· + ·.totalTheorems) 0
  let totalBridging := successful.foldl (· + ·.bridgingTheorems) 0
  let totalDefsSorry := successful.foldl (· + ·.defsWithSorry) 0
  let totalThmsSorry := successful.foldl (· + ·.theoremsWithSorry) 0
  let totalSorry := totalDefsSorry + totalThmsSorry

  let now ← IO.Process.output { cmd := "date", args := #["+%Y-%m-%d %H:%M:%S"] }
  let timestamp := now.stdout.trimAscii.copy

  let mut tableRows := ""
  for r in successful do
    let sorryIndicator := if r.defsWithSorry + r.theoremsWithSorry > 0 then "⚠️ " else ""
    tableRows := tableRows ++ s!"<tr><td><a href='{r.name}/site/' target='_blank'>{sorryIndicator}{r.name}</a></td><td>{r.totalDefinitions}</td><td>{r.defsWithSorry}</td><td>{r.totalTheorems}</td><td>{r.theoremsWithSorry}</td><td>{r.bridgingTheorems}</td><td>{r.unclassifiedTheorems}</td></tr>\n"

  -- Build incomplete projects list (not yet started or in progress)
  let mut incompleteList := ""
  for r in incomplete do
    let statusLink := s!"<a href='{r.name}/index.html' target='_blank'>View Status</a>"
    incompleteList := incompleteList ++ s!"<li><strong>{r.name}</strong><br><small><a href='{r.repo}' target='_blank'>{r.repo}</a> | {statusLink}</small></li>\n"

  let incompleteSection := if incomplete.isEmpty then "" else
    s!"<div class='section'><h2>⏳ Incomplete Projects ({incomplete.size})</h2><ul class='incomplete-list'>{incompleteList}</ul></div>"

  -- Build failed projects list (actual errors)
  let mut failedList := ""
  for r in failed do
    let errMsg := r.errorMessage.getD "Unknown error"
    let statusLink := s!"<a href='{r.name}/index.html' target='_blank'>View Details</a>"
    failedList := failedList ++ s!"<li><strong>{r.name}</strong> — {errMsg}<br><small><a href='{r.repo}' target='_blank'>{r.repo}</a> | {statusLink}</small></li>\n"

  let failedSection := if failed.isEmpty then "" else
    s!"<div class='section'><h2>❌ Failed Projects ({failed.size})</h2><ul class='failed-list'>{failedList}</ul></div>"

  -- CSS without interpolation (no curly braces to escape)
  let css := "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 0; padding: 40px; background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%); color: #eee; min-height: 100vh; } h1 { color: #4ecdc4; margin-bottom: 10px; } .subtitle { color: #888; margin-bottom: 30px; } .summary-cards { display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 20px; margin-bottom: 40px; } .card { background: rgba(255,255,255,0.05); padding: 20px; border-radius: 12px; text-align: center; border: 1px solid rgba(255,255,255,0.1); } .card-value { font-size: 2.5em; font-weight: bold; color: #4ecdc4; } .card-label { color: #888; margin-top: 5px; } table { width: 100%; border-collapse: collapse; background: rgba(255,255,255,0.02); border-radius: 12px; overflow: hidden; } th { background: rgba(78, 205, 196, 0.2); padding: 15px; text-align: left; cursor: pointer; user-select: none; } th:hover { background: rgba(78, 205, 196, 0.3); } td { padding: 12px 15px; border-bottom: 1px solid rgba(255,255,255,0.05); } tr:hover { background: rgba(255,255,255,0.05); } .success { color: #4ecdc4; } .failure { color: #ff6b6b; } a { color: #4ecdc4; text-decoration: none; } a:hover { text-decoration: underline; } .section { margin-bottom: 40px; } .section h2 { color: #fff; border-bottom: 2px solid #4ecdc4; padding-bottom: 10px; margin-bottom: 20px; } .incomplete-list { list-style: none; padding: 0; } .incomplete-list li { background: rgba(136, 136, 136, 0.1); padding: 15px; margin-bottom: 10px; border-radius: 8px; border-left: 4px solid #888; } .failed-list { list-style: none; padding: 0; } .failed-list li { background: rgba(255, 107, 107, 0.1); padding: 15px; margin-bottom: 10px; border-radius: 8px; border-left: 4px solid #ff6b6b; } .timestamp { color: #666; font-size: 0.9em; }"

  -- JavaScript for table sorting (kept simple without interpolation)
  let js := "document.querySelectorAll('#results-table th').forEach(function(th, index) { th.addEventListener('click', function() { var table = th.closest('table'); var tbody = table.querySelector('tbody'); var rows = Array.from(tbody.querySelectorAll('tr')); var sortType = th.dataset.sort; var isAsc = th.classList.contains('sorted-asc'); table.querySelectorAll('th').forEach(function(h) { h.classList.remove('sorted-asc', 'sorted-desc'); }); rows.sort(function(a, b) { var aVal = a.cells[index].textContent; var bVal = b.cells[index].textContent; if (sortType === 'number') { aVal = parseFloat(aVal) || 0; bVal = parseFloat(bVal) || 0; return isAsc ? bVal - aVal : aVal - bVal; } else { return isAsc ? bVal.localeCompare(aVal) : aVal.localeCompare(bVal); } }); rows.forEach(function(row) { tbody.appendChild(row); }); th.classList.add(isAsc ? 'sorted-desc' : 'sorted-asc'); }); });"

  let html := s!"<!DOCTYPE html>
<html>
<head>
<meta charset=\"UTF-8\">
<title>doc-verification-bridge Experiment Results</title>
<style>{css}</style>
</head>
<body>
<h1>🔬 doc-verification-bridge Experiment Results</h1>
<p class='subtitle'>Automatic theorem classification across Lean 4 projects</p>
<p class='timestamp'>Generated: {timestamp}</p>
<div class='summary-cards'>
<div class='card'><div class='card-value'>{successful.size}/{results.size}</div><div class='card-label'>Projects Analyzed</div></div>
<div class='card'><div class='card-value'>{totalDefs}</div><div class='card-label'>Total Definitions</div></div>
<div class='card'><div class='card-value'>{totalThms}</div><div class='card-label'>Total Theorems</div></div>
<div class='card'><div class='card-value'>{totalBridging}</div><div class='card-label'>Bridging Theorems</div></div>
<div class='card'><div class='card-value' style='color: #ff9f43'>{totalSorry}</div><div class='card-label'>⚠️ With Sorry</div></div>
</div>
<div class='section'>
<h2>✅ Successful Projects ({successful.size})</h2>
<table id='results-table'>
<thead><tr>
<th data-sort='string'>Project</th><th data-sort='number'>Defs</th>
<th data-sort='number'>Defs⚠️</th><th data-sort='number'>Thms</th>
<th data-sort='number'>Thms⚠️</th><th data-sort='number'>Bridge</th><th data-sort='number'>Unclass</th>
</tr></thead>
<tbody>{tableRows}</tbody>
</table>
</div>
{incompleteSection}
{failedSection}
<script>{js}</script>
</body>
</html>"

  IO.FS.createDirAll outputPath.parent.get!
  IO.FS.writeFile outputPath html

/-! ## Main Processing -/

/-- Process a single project with state management and command logging -/
def processProject (project : Project) (config : Config) (mode : RunMode) : IO ProjectResult := do
  let name := project.name
  let repo := project.repo
  let modules := project.modules

  let repoDir := config.reposDir / name  -- Where the repo is cloned
  -- For monorepos, the Lean project may be in a subdirectory
  let projectDir := match project.subdirectory with
    | some subdir => repoDir / subdir
    | none => repoDir
  let outputDir := config.sitesDir / name

  -- Initialize command log and context for incremental saving
  let mut cmdLog : CommandLog := #[]
  let logCtx : LogContext := { projectName := name, repo := repo, outputDir := outputDir }

  -- Check current state and decide what to do
  let currentState ← readProjectState config.sitesDir name

  match mode with
  | .fresh =>
    -- Always process from scratch - clean up first
    IO.println s!"[{name}] Fresh run - cleaning up..."
    removeDir repoDir
    removeDir outputDir
  | .resume =>
    -- Skip if completed, restart if in-progress or failed
    match currentState with
    | .completed =>
      IO.println s!"[{name}] Already completed, skipping (use --update to re-run)"
      -- Read existing stats if available (from the output directory)
      let coverageMd := outputDir / "mkdocs-src" / "docs" / "verification" / "coverage.md"
      let stats ← parseCoverageStats coverageMd
      return { stats with name, repo, success := true, siteDir := some (outputDir / "site") }
    | .inProgress | .failed =>
      IO.println s!"[{name}] Incomplete/failed - restarting..."
      removeDir repoDir
      removeDir outputDir
    | .notStarted =>
      IO.println s!"[{name}] Not started yet"
  | .update =>
    -- Update git repo (don't delete), regenerate docs
    IO.println s!"[{name}] Update mode - will update repo and regenerate docs"
    removeDir outputDir  -- Only remove the generated site

  -- Mark as in-progress
  writeProjectState config.sitesDir name .inProgress

  IO.println s!"[{name}] Starting processing..."

  -- Clone or update repository
  if mode == .update && (← repoDir.pathExists) then
    IO.println s!"[{name}] Updating repository..."
    let (pullOk, pullLog, newLog) ← runCmdLogged "git-pull" "git" #["pull"] (some repoDir) cmdLog (some logCtx)
    cmdLog := newLog
    if !pullOk then
      writeProjectState config.sitesDir name .failed
      return { name, repo, success := false,
               errorMessage := some "Git pull failed", buildLog := pullLog }
  else
    IO.println s!"[{name}] Cloning repository..."
    let (cloneOk, cloneLog, newLog) ← runCmdLogged "git-clone" "git"
      #["clone", "--depth", "1", repo, repoDir.toString] none cmdLog (some logCtx)
    cmdLog := newLog
    if !cloneOk then
      writeProjectState config.sitesDir name .failed
      return { name, repo, success := false,
               errorMessage := some "Clone failed", buildLog := cloneLog }

  -- Detect git branch (use configured value or auto-detect from repo)
  let branch ← match project.branch with
    | some b => pure b
    | none => detectGitBranch repoDir
  IO.println s!"[{name}] Using branch: {branch}"

  -- Verify the project directory exists (relevant for subdirectory projects)
  if !(← projectDir.pathExists) then
    let errMsg := s!"Project directory not found: {projectDir}" ++
      (if project.subdirectory.isSome then s!" (subdirectory: {project.subdirectory.get!})" else "")
    saveCommandLogCtx cmdLog logCtx
    generateErrorPage name errMsg "" outputDir
    writeProjectState config.sitesDir name .failed
    return { name, repo, success := false,
             errorMessage := some errMsg, siteDir := some outputDir }

  -- Setup docvb directory and check toolchain compatibility
  IO.println s!"[{name}] Setting up docvb directory..."
  let (hasToolchainIssue, tcCheck) ← setupDocvbDirectory projectDir name config.docVerificationBridgePath modules config.useCustomSourceLinker

  -- Log the toolchain check result
  IO.println s!"[{name}] {tcCheck.message}"

  -- If there's a toolchain incompatibility, fail early with a clear message
  if hasToolchainIssue then
    let errMsg := s!"Toolchain incompatibility: {tcCheck.message}\n\nProject uses {tcCheck.projectToolchain} but doc-verification-bridge requires {tcCheck.dvbToolchain}.\n\nTo analyze this project, it must be updated to use Lean {tcCheck.dvbToolchain} or newer."
    saveCommandLogCtx cmdLog logCtx
    generateErrorPage name errMsg "" outputDir (some tcCheck)
    writeProjectState config.sitesDir name .failed
    return { name, repo, success := false,
             errorMessage := some s!"Toolchain incompatibility: project uses {tcCheck.projectToolchain}, requires {tcCheck.dvbToolchain}",
             siteDir := some outputDir }

  -- Build main project
  IO.println s!"[{name}] Building project{if project.lakeExeCacheGet then " (with cache)" else ""}..."

  -- For projects like mathlib4, fetch the cloud cache first
  if project.lakeExeCacheGet then
    let (cacheOk, cacheLog, newLog) ← runCmdLogged "lake-exe-cache-get" "lake"
      #["exe", "cache", "get"] (some projectDir) cmdLog (some logCtx)
    cmdLog := newLog
    if !cacheOk then
      generateErrorPage name "lake exe cache get failed" cacheLog outputDir (some tcCheck)
      writeProjectState config.sitesDir name .failed
      return { name, repo, success := false,
               errorMessage := some "cache get failed", buildLog := cacheLog,
               siteDir := some outputDir }

  let (buildOk, buildLog, newLog) ← runCmdLogged "lake-build" "lake" #["build"] (some projectDir) cmdLog (some logCtx)
  cmdLog := newLog
  if !buildOk then
    generateErrorPage name "Project build failed" buildLog outputDir (some tcCheck)
    writeProjectState config.sitesDir name .failed
    return { name, repo, success := false,
             errorMessage := some "Build failed", buildLog,
             siteDir := some outputDir }

  -- Run unified-doc
  IO.println s!"[{name}] Generating documentation (mode: {project.classificationMode})..."
  let docvbDir := projectDir / "docvb"

  -- Update lake dependencies
  let (updateOk, updateLog, newLog) ← runCmdLogged "lake-update" "lake"
    #["update"] (some docvbDir) cmdLog (some logCtx)
  cmdLog := newLog

  if !updateOk then
    -- Check if it's a recoverable error
    if updateLog.contains "post-update hooks" || updateLog.contains "failed to fetch cache" then
      IO.println s!"[{name}] Warning: post-update hooks failed, continuing anyway..."
    else
      generateErrorPage name "lake update failed" updateLog outputDir (some tcCheck)
      writeProjectState config.sitesDir name .failed
      return { name, repo, success := false,
               errorMessage := some "lake update failed", buildLog := updateLog,
               siteDir := some outputDir }

  -- CRITICAL: Restore toolchain after lake update
  -- Mathlib's post-update hooks can cause elan to switch to a different toolchain.
  -- We need to restore the project's original toolchain to ensure we compile with
  -- the version the project was designed for.
  let docvbToolchainPath := docvbDir / "lean-toolchain"
  IO.FS.writeFile docvbToolchainPath tcCheck.projectToolchain
  IO.println s!"[{name}] Restored toolchain to {tcCheck.projectToolchain}"

  -- Build docvb
  let (docvbBuildOk, docvbBuildLog, newLog) ← runCmdLogged "docvb-build" "lake"
    #["build"] (some docvbDir) cmdLog (some logCtx)
  cmdLog := newLog
  if !docvbBuildOk then
    generateErrorPage name "docvb build failed" docvbBuildLog outputDir (some tcCheck)
    writeProjectState config.sitesDir name .failed
    return { name, repo, success := false,
             errorMessage := some "docvb build failed", buildLog := docvbBuildLog,
             siteDir := some outputDir }

  -- Run unified-doc
  let modeFlag := if project.classificationMode == .annotated then "--annotated" else "--auto"
  -- Convert outputDir to absolute path since unified-doc runs from docvbDir
  let outputDirAbs ← IO.FS.realPath outputDir
  let unifiedArgs := #["exe", "unified-doc", "unified", modeFlag,
                       "--output", outputDirAbs.toString,
                       "--repo", repo,
                       "--branch", branch] ++ modules
  let mut env : Array (String × Option String) := #[]
  if project.disableEquations then
    env := env.push ("DISABLE_EQUATIONS", some "1")
  if project.skipProofDeps then
    env := env.push ("SKIP_PROOF_DEPS", some "1")

  let (docOk, docLog, newLog) ← runCmdLogged "unified-doc" "lake" unifiedArgs (some docvbDir) cmdLog (some logCtx) 3600 env
  cmdLog := newLog

  -- Log is already saved incrementally by runCmdLogged

  if !docOk then
    generateErrorPage name "Documentation generation failed" (buildLog ++ "\n\n" ++ docLog) outputDir (some tcCheck)
    writeProjectState config.sitesDir name .failed
    return { name, repo, success := false,
             errorMessage := some "unified-doc failed", buildLog := docLog,
             siteDir := some outputDir }

  -- Parse statistics from the generated coverage.md
  -- unified-doc outputs to: outputDir/mkdocs-src/docs/verification/coverage.md
  let coverageMd := outputDir / "mkdocs-src" / "docs" / "verification" / "coverage.md"
  let stats ← parseCoverageStats coverageMd

  -- Mark as completed
  writeProjectState config.sitesDir name .completed
  IO.println s!"[{name}] ✓ Complete!"

  return { stats with
    name, repo, success := true,
    buildLog := buildLog ++ "\n\n" ++ docLog,
    siteDir := some (outputDir / "site")
  }

/-- Save results to JSON -/
def saveResultsJson (results : Array ProjectResult) (outputPath : FilePath) : IO Unit := do
  let lb := "{"  -- left brace
  let rb := "}"  -- right brace
  let mut json := "[\n"
  for h : i in [:results.size] do
    let r := results[i]
    let comma := if i < results.size - 1 then "," else ""
    let siteDir := r.siteDir.map (s!"\"{·}\"") |>.getD "null"
    let errMsg := r.errorMessage.map (s!"\"{·}\"") |>.getD "null"
    json := json ++ s!"  {lb}
    \"name\": \"{r.name}\",
    \"repo\": \"{r.repo}\",
    \"success\": {r.success},
    \"error_message\": {errMsg},
    \"total_definitions\": {r.totalDefinitions},
    \"math_abstractions\": {r.mathAbstractions},
    \"comp_datatypes\": {r.compDatatypes},
    \"math_definitions\": {r.mathDefinitions},
    \"comp_operations\": {r.compOperations},
    \"total_theorems\": {r.totalTheorems},
    \"computational_theorems\": {r.computationalTheorems},
    \"mathematical_theorems\": {r.mathematicalTheorems},
    \"bridging_theorems\": {r.bridgingTheorems},
    \"soundness_theorems\": {r.soundnessTheorems},
    \"completeness_theorems\": {r.completenessTheorems},
    \"unclassified_theorems\": {r.unclassifiedTheorems},
    \"site_dir\": {siteDir}
  {rb}{comma}
"
  json := json ++ "]"
  IO.FS.writeFile outputPath json

/-! ## CLI -/

def runExperiments (configPath : FilePath) (mode : RunMode := .fresh)
    (projectFilter : Option (Array String) := none) : IO UInt32 := do
  let baseDir := configPath.parent.get!

  -- Load config
  let configContent ← IO.FS.readFile configPath
  let config ← parseConfig configContent baseDir

  -- Filter projects if specified
  let projects ← match projectFilter with
    | some names =>
      let filtered := config.projects.filter (names.contains ·.name)
      if filtered.isEmpty then
        IO.println s!"Error: No matching projects found for: {names}"
        IO.println s!"Available projects: {config.projects.map (·.name)}"
        return 1
      else
        pure filtered
    | none => pure config.projects

  -- Create directories
  IO.FS.createDirAll config.reposDir
  IO.FS.createDirAll config.sitesDir

  let modeStr := match mode with
    | .fresh => "fresh"
    | .resume => "resume"
    | .update => "update"
  let filterStr := match projectFilter with
    | some names => s!" (filtered: {names})"
    | none => ""
  IO.println s!"Processing {projects.size} projects (max {config.maxParallelJobs} parallel, mode: {modeStr}){filterStr}"
  IO.println s!"Base port: {config.basePort}"
  IO.println ""

  -- Process projects in parallel batches
  let mut results : Array ProjectResult := #[]

  -- Worker pool: keep up to maxParallelJobs running at all times
  -- As soon as one completes, start another
  let mut pending := projects.toList  -- Projects not yet started
  let mut running : Array (Task (Except IO.Error ProjectResult)) := #[]

  while !pending.isEmpty || !running.isEmpty do
    -- Start new tasks if we have capacity and pending work
    while running.size < config.maxParallelJobs && !pending.isEmpty do
      match pending with
      | project :: rest =>
        pending := rest
        let task ← IO.asTask (prio := .dedicated) (processProject project config mode)
        running := running.push task
        IO.println s!"[{project.name}] Started ({running.size} running, {pending.length} pending)"
      | [] => break

    -- Wait a bit then check for completed tasks
    -- (Lean doesn't have a "wait for any" primitive, so we poll)
    IO.sleep 100  -- 100ms polling interval

    -- Check each task for completion using IO.hasFinished
    let mut newRunning : Array (Task (Except IO.Error ProjectResult)) := #[]
    for task in running do
      let finished ← IO.hasFinished task
      if finished then
        match task.get with
        | .ok result =>
          results := results.push result
          let status := if result.success then "✓" else "✗"
          IO.println s!"[{result.name}] {status} Complete ({results.size}/{projects.size} done)"
        | .error e =>
          IO.println s!"[task] Error: {e}"
      else
        newRunning := newRunning.push task
    running := newRunning

  -- Generate summary page
  let summaryPath := config.sitesDir / "index.html"
  generateSummaryPage results summaryPath
  IO.println s!"\nSummary page generated: {summaryPath}"

  -- Save results as JSON
  let resultsJson := baseDir / "results.json"
  saveResultsJson results resultsJson
  IO.println s!"Results saved to: {resultsJson}"

  -- Print summary
  let sep := "".pushn '=' 60
  IO.println s!"\n{sep}"
  IO.println "SUMMARY"
  IO.println sep

  let successful := results.filter (·.success)
  let failed := results.filter (!·.success)
  IO.println s!"Successful: {successful.size}/{results.size}"
  IO.println s!"Failed: {failed.size}/{results.size}"

  if !successful.isEmpty then
    IO.println s!"\nTotal definitions: {successful.foldl (· + ·.totalDefinitions) 0}"
    IO.println s!"Total theorems: {successful.foldl (· + ·.totalTheorems) 0}"
    IO.println s!"Bridging theorems: {successful.foldl (· + ·.bridgingTheorems) 0}"

  return 0

def serveResults (configPath : FilePath) : IO UInt32 := do
  let baseDir := configPath.parent.get!
  let configContent ← IO.FS.readFile configPath
  let config ← parseConfig configContent baseDir

  IO.println "Starting HTTP server..."
  IO.println s!"Summary available at: http://localhost:{config.basePort}/"
  IO.println ""
  IO.println "Tip: Run 'experiments refresh' first to update the summary page."
  IO.println ""
  IO.println "Press Ctrl+C to stop."
  IO.println ""

  -- Start Python's http.server for simplicity (Lean doesn't have a built-in HTTP server)
  let sitesDir ← IO.FS.realPath config.sitesDir
  let _ ← IO.Process.spawn {
    cmd := "python3"
    args := #["-m", "http.server", config.basePort.repr, "--directory", sitesDir.toString]
  }

  -- Wait forever (until Ctrl+C) - just wait for process
  let _ ← IO.Process.output { cmd := "sleep", args := #["infinity"] }
  return 0

/-- Refresh the summary page by re-reading all existing coverage stats -/
def refreshSummary (configPath : FilePath) : IO UInt32 := do
  let baseDir := configPath.parent.get!
  let configContent ← IO.FS.readFile configPath
  let config ← parseConfig configContent baseDir

  IO.println s!"Refreshing summary from {config.projects.size} projects..."
  IO.println ""

  let mut results : Array ProjectResult := #[]

  for project in config.projects do
    let name := project.name
    let repo := project.repo
    let outputDir := config.sitesDir / name

    -- Check if project was completed
    let state ← readProjectState config.sitesDir name

    -- Read coverage stats if available
    let coverageMd := outputDir / "mkdocs-src" / "docs" / "verification" / "coverage.md"
    let stats ← parseCoverageStats coverageMd

    let (success, errorMsg) := match state with
      | .completed => (true, none)
      | .failed => (false, some "Previously failed")
      | .inProgress => (false, some "Incomplete")
      | .notStarted => (false, some "Not started")

    -- Generate status page for incomplete projects (shows commands.yaml)
    -- Don't overwrite error pages for failed projects
    if !success && state != .failed then
      generateStatusPage name repo outputDir state

    let result : ProjectResult := { stats with
      name, repo, success,
      errorMessage := errorMsg,
      siteDir := some (outputDir / "site")
    }

    let statusIcon := if success then "✓" else "✗"
    let statsStr := if stats.totalDefinitions > 0 || stats.totalTheorems > 0
      then s!" ({stats.totalDefinitions} defs, {stats.totalTheorems} thms)"
      else ""
    IO.println s!"[{name}] {statusIcon}{statsStr}"

    results := results.push result

  -- Generate summary page
  let summaryPath := config.sitesDir / "index.html"
  generateSummaryPage results summaryPath
  IO.println s!"\nSummary page generated: {summaryPath}"

  -- Save results as JSON
  let resultsJson := baseDir / "results.json"
  saveResultsJson results resultsJson
  IO.println s!"Results saved to: {resultsJson}"

  -- Print summary
  let sep := "".pushn '=' 60
  IO.println s!"\n{sep}"
  IO.println "SUMMARY"
  IO.println sep

  let successful := results.filter (·.success)
  let failed := results.filter (!·.success)
  IO.println s!"Successful: {successful.size}/{results.size}"
  IO.println s!"Failed: {failed.size}/{results.size}"

  if !successful.isEmpty then
    IO.println s!"\nTotal definitions: {successful.foldl (· + ·.totalDefinitions) 0}"
    IO.println s!"Total theorems: {successful.foldl (· + ·.totalTheorems) 0}"
    IO.println s!"Bridging theorems: {successful.foldl (· + ·.bridgingTheorems) 0}"

  return 0

end Experiments

/-- Parse --projects argument from args list -/
def parseProjectsArg (args : List String) : Option (Array String) × List String :=
  match args with
  | "--projects" :: rest =>
    -- Collect project names until next flag or end
    let (names, remaining) := rest.span (fun s => !s.startsWith "--")
    if names.isEmpty then (none, args)
    else (some names.toArray, remaining)
  | x :: rest =>
    let (filter, remaining) := parseProjectsArg rest
    (filter, x :: remaining)
  | [] => (none, [])

/-- Entry point for standalone experiments executable -/
def experimentsMain (args : List String) : IO UInt32 := do
  -- Parse --projects first, then handle the rest
  let (projectFilter, remainingArgs) := parseProjectsArg args

  -- Simple argument parsing
  match remainingArgs with
  -- Run commands with modes
  | ["run"] => Experiments.runExperiments "config.toml" .fresh projectFilter
  | ["run", "--resume"] => Experiments.runExperiments "config.toml" .resume projectFilter
  | ["run", "--update"] => Experiments.runExperiments "config.toml" .update projectFilter
  | ["run", "--config", path] => Experiments.runExperiments path .fresh projectFilter
  | ["run", "--resume", "--config", path] => Experiments.runExperiments path .resume projectFilter
  | ["run", "--config", path, "--resume"] => Experiments.runExperiments path .resume projectFilter
  | ["run", "--update", "--config", path] => Experiments.runExperiments path .update projectFilter
  | ["run", "--config", path, "--update"] => Experiments.runExperiments path .update projectFilter
  -- Serve commands
  | ["serve"] => Experiments.serveResults "config.toml"
  | ["serve", "--config", path] => Experiments.serveResults path
  -- Refresh commands
  | ["refresh"] => Experiments.refreshSummary "config.toml"
  | ["refresh", "--config", path] => Experiments.refreshSummary path
  | _ =>
    IO.println "Usage: experiments <command> [options]"
    IO.println ""
    IO.println "Commands:"
    IO.println "  run              Clone, build, and analyze all configured projects"
    IO.println "  refresh          Regenerate summary page from existing coverage data"
    IO.println "  serve            Start HTTP server to view results"
    IO.println ""
    IO.println "Run Options:"
    IO.println "  --resume             Skip completed projects, restart incomplete/failed ones"
    IO.println "  --update             Update git repos and regenerate docs for all projects"
    IO.println "  --config <path>      Path to config.toml (default: ./config.toml)"
    IO.println "  --projects <names>   Only run specified projects (space-separated)"
    IO.println ""
    IO.println "Common Options (all commands):"
    IO.println "  --config <path>      Path to config.toml (default: ./config.toml)"
    IO.println ""
    IO.println "Examples:"
    IO.println "  experiments run                          # Fresh run of all projects"
    IO.println "  experiments run --resume                 # Continue interrupted run"
    IO.println "  experiments run --update                 # Update repos and regenerate"
    IO.println "  experiments run --projects mathlib4      # Run only mathlib4"
    IO.println "  experiments run --projects batteries mm0 # Run batteries and mm0"
    IO.println "  experiments run --update --projects mathlib4  # Update only mathlib4"
    IO.println "  experiments refresh                      # Regenerate summary only"
    IO.println "  experiments serve                        # Start HTTP server"
    return 1
