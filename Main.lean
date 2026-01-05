-- Main.lean
-- CLI entry point for doc-verification-bridge

import Cli
import Lean
import DocGen4.Load
import DocVerificationBridge

/-!
# Doc Verification Bridge CLI

Command-line interface for running doc-verification-bridge analysis
and generating coverage reports.
-/

open Lean System Cli DocVerificationBridge

/-- Run a MetaM computation in IO -/
def runMetaM (env : Environment) (x : MetaM α) : IO α := do
  let coreCtx : Core.Context := {
    options := {},
    fileName := "<verification-bridge>",
    fileMap := default
  }
  let coreState : Core.State := { env }
  let (result, _) ← x.run' {} |>.toIO coreCtx coreState
  return result

/-- Run automatic classification and generate report -/
def runAutoClassification (args : Cli.Parsed) : IO UInt32 := do
  -- Get modules from variadic arguments
  let modules : List String := (args.variableArgsAs! String).toList

  if modules.isEmpty then
    IO.eprintln "Error: At least one module name required"
    return 1

  -- Parse configuration
  let outputDir := args.flag? "output" |>.map (·.as! String) |>.getD "docs"
  let repoUrl := args.flag? "repo" |>.map (·.as! String) |>.getD ""
  let platformStr := args.flag? "platform" |>.map (·.as! String) |>.getD "github"
  let branch := args.flag? "branch" |>.map (·.as! String) |>.getD "main"
  let projectName := args.flag? "project" |>.map (·.as! String) |>.getD "API Coverage"

  let platform := if platformStr == "gitlab" then GitPlatform.gitlab else GitPlatform.github

  let cfg : ReportConfig := {
    outputDir := outputDir
    repoUrl := repoUrl
    platform := platform
    branch := branch
    modules := modules
  }

  IO.println s!"verification-bridge: Analyzing {modules.length} module(s)..."

  -- Initialize search path
  Lean.initSearchPath (← Lean.findSysroot)

  -- Load modules using doc-gen4's infrastructure
  let moduleNames := modules.map String.toName

  -- Import modules
  let options := {}
  let env ← importModules (moduleNames.toArray.map (fun n => { module := n })) options

  IO.println s!"  Loaded environment with {env.header.moduleNames.size} modules"

  -- Run automatic classification for each module
  let mut allEntries : NameMap APIMeta := {}
  for modName in moduleNames do
    let result ← runMetaM env <| classifyAllDeclarations env modName
    allEntries := result.entries.foldl (fun acc name apiMeta => acc.insert name apiMeta) allEntries

  IO.println s!"  Classified {allEntries.size} declarations"

  -- Generate report
  let reportCfg := if repoUrl.isEmpty then none else some cfg
  let report := generateReport env allEntries reportCfg projectName modules

  -- Write output files
  let outputPath : System.FilePath := outputDir
  let docsPath := outputPath / "docs"
  let stylesheetsPath := docsPath / "stylesheets"

  -- Create directories
  IO.FS.createDirAll stylesheetsPath

  -- Write files
  IO.FS.writeFile (docsPath / "API_Coverage.md") report
  IO.FS.writeFile (docsPath / "index.md") (generateIndexMd projectName)
  IO.FS.writeFile (stylesheetsPath / "extra.css") generateExtraCss
  IO.FS.writeFile (outputPath / "mkdocs.yml") (generateMkDocsConfig projectName)

  IO.println s!"  Written to {outputPath}/"
  IO.println ""
  IO.println "To serve the documentation locally:"
  IO.println s!"  cd {outputPath}"
  IO.println "  mkdocs serve"

  return 0

/-- The main CLI command -/
def docVerificationBridgeCmd : Cmd := `[Cli|
  "doc-verification-bridge" VIA runAutoClassification;
  "Theorem classification for specification-implementation refinement."

  FLAGS:
    o, output : String;    "Output directory for generated docs (default: docs)"
    r, repo : String;      "Repository URL for source links"
    p, platform : String;  "Git platform: github or gitlab (default: github)"
    b, branch : String;    "Git branch name (default: main)"
    project : String;      "Project name for documentation (default: API Coverage)"

  ARGS:
    ...modules : String;   "Module names to analyze"

  EXTENSIONS:
    author "doc-verification-bridge contributors";
    longDescription "
doc-verification-bridge - Theorem classification for specification-implementation refinement

Automatically classifies Lean 4 declarations according to a Four-Category Ontology
inspired by E.J. Lowe:

  • Mathematical Abstractions (Kinds): Abstract types, Prop-based structures
  • Computational Datatypes (Objects): Concrete data structures
  • Mathematical Definitions (Attributes): Prop-returning predicates/relations
  • Computational Operations (Modes): Computable functions, Bool predicates

Bridging theorems prove the fundamental ontological relations between these levels.

SETUP (for analyzing external projects):

  To analyze a project like 'batteries' or 'mathlib4', create a nested docbuild
  directory with a lakefile.toml that depends on both the target project and
  doc-verification-bridge:

    cd /path/to/target-project
    mkdir docbuild && cd docbuild
    # Create lakefile.toml (see README for template)
    cp ../lean-toolchain .
    lake update doc-verification-bridge

  Then run commands from inside the docbuild directory.

EXAMPLES:
  # Analyze a single module (run from docbuild directory)
  lake exe doc-verification-bridge MyProject.Core

  # Analyze multiple modules with source links
  lake exe doc-verification-bridge --repo https://github.com/org/repo --output docs \\
    MyProject.Core MyProject.Data MyProject.Theorems

  # Analyze batteries library
  lake exe doc-verification-bridge --output docs --project Batteries Batteries

  # Use GitLab for source links
  lake exe doc-verification-bridge --platform gitlab --repo https://gitlab.com/org/repo \\
    MyProject

See README.md for detailed setup instructions.
"
]

/-- Main entry point -/
def main (args : List String) : IO UInt32 :=
  docVerificationBridgeCmd.validate args
