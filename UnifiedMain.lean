-- UnifiedMain.lean
-- Unified CLI that combines doc-gen4 and doc-verification-bridge

import Cli
import Lean
import DocGen4
import DocGen4.Load
import DocVerificationBridge
import DocVerificationBridge.Unified

/-!
# Unified Documentation CLI

This CLI provides commands to:
1. Generate combined doc-gen4 + verification documentation
2. Run standalone doc-gen4
3. Run standalone verification analysis

The unified mode shares module loading for efficiency.
-/

open Lean System Cli DocVerificationBridge DocVerificationBridge.Unified

/-- Run the unified doc-gen4 + verification pipeline -/
def runUnifiedCmd (args : Cli.Parsed) : IO UInt32 := do
  let modules : Array String := args.variableArgsAs! String

  if modules.isEmpty then
    IO.eprintln "Error: At least one module name required"
    return 1

  let cfg : UnifiedConfig := {
    buildDir := args.flag? "output" |>.map (·.as! String) |>.getD ".lake/build/doc"
    repoUrl := args.flag? "repo" |>.map (·.as! String) |>.getD ""
    platform := if (args.flag? "platform" |>.map (·.as! String) |>.getD "github") == "gitlab"
                then .gitlab else .github
    branch := args.flag? "branch" |>.map (·.as! String) |>.getD "main"
    projectName := args.flag? "project" |>.map (·.as! String) |>.getD "Documentation"
    sourceDir := args.flag? "source-dir" |>.map (·.as! String) |>.map (·) |>.getD ".."
    generateVerification := !(args.hasFlag "no-verification")
  }

  runUnifiedPipeline cfg (modules.map String.toName)

/-- Run standalone doc-gen4 (delegates to doc-gen4's infrastructure) -/
def runDocGen4Cmd (args : Cli.Parsed) : IO UInt32 := do
  let modules : Array String := args.variableArgsAs! String

  if modules.isEmpty then
    IO.eprintln "Error: At least one module name required"
    return 1

  let buildDir := args.flag? "output" |>.map (·.as! String) |>.getD ".lake/build/doc"
  let sourceUrl := args.flag? "source-url" |>.map (·.as! String)

  IO.println s!"doc-gen4: Loading {modules.size} module(s)..."

  Lean.initSearchPath (← Lean.findSysroot)

  let moduleNames := modules.map String.toName
  let task := DocGen4.Process.AnalyzeTask.analyzeConcreteModules moduleNames
  let (result, hierarchy) ← DocGen4.load task

  IO.println s!"doc-gen4: Generating HTML..."
  DocGen4.htmlOutput buildDir result hierarchy sourceUrl

  IO.println s!"✅ Documentation generated at {buildDir}/doc/"
  return 0

/-- Run standalone verification analysis (existing behavior) -/
def runVerifyCmd (args : Cli.Parsed) : IO UInt32 := do
  let modules : List String := (args.variableArgsAs! String).toList

  if modules.isEmpty then
    IO.eprintln "Error: At least one module name required"
    return 1

  let outputDir := args.flag? "output" |>.map (·.as! String) |>.getD "docs"
  let repoUrl := args.flag? "repo" |>.map (·.as! String) |>.getD ""
  let platformStr := args.flag? "platform" |>.map (·.as! String) |>.getD "github"
  let branch := args.flag? "branch" |>.map (·.as! String) |>.getD "main"
  let projectName := args.flag? "project" |>.map (·.as! String) |>.getD "API Coverage"

  let platform := if platformStr == "gitlab" then GitPlatform.gitlab else GitPlatform.github

  IO.println s!"verification-bridge: Building git file cache..."
  let gitCache ← buildGitFileCache
  IO.println s!"  Found {gitCache.allFiles.size} .lean files in repository"

  let cfg : ReportConfig := {
    outputDir := outputDir
    repoUrl := repoUrl
    platform := platform
    branch := branch
    modules := modules
    gitCache := gitCache
  }

  IO.println s!"verification-bridge: Analyzing {modules.length} module(s)..."

  Lean.initSearchPath (← Lean.findSysroot)

  let moduleNames := modules.map String.toName
  let options := {}
  let env ← importModules (moduleNames.toArray.map (fun n => { module := n })) options

  IO.println s!"  Loaded environment with {env.header.moduleNames.size} modules"

  let mut allEntries : NameMap APIMeta := {}
  for modName in moduleNames do
    let coreCtx : Core.Context := {
      options := {},
      fileName := "<verification-bridge>",
      fileMap := default
    }
    let coreState : Core.State := { env }
    let (result, _) ← (classifyAllDeclarations env modName).run' {} |>.toIO coreCtx coreState
    allEntries := result.entries.foldl (fun acc name apiMeta => acc.insert name apiMeta) allEntries

  IO.println s!"  Classified {allEntries.size} declarations"

  let reportCfg := if repoUrl.isEmpty then none else some cfg
  let report := generateReport env allEntries reportCfg projectName modules

  let outputPath : System.FilePath := outputDir
  let docsPath := outputPath / "docs"
  let stylesheetsPath := docsPath / "stylesheets"

  IO.FS.createDirAll stylesheetsPath

  let repoUrlOpt := if repoUrl.isEmpty then none else some repoUrl
  IO.FS.writeFile (docsPath / "API_Coverage.md") report
  IO.FS.writeFile (docsPath / "index.md") (generateIndexMd projectName)
  IO.FS.writeFile (stylesheetsPath / "extra.css") generateExtraCss
  IO.FS.writeFile (outputPath / "mkdocs.yml") (generateMkDocsConfig projectName repoUrlOpt)

  IO.println s!"  Written to {outputPath}/"
  IO.println ""
  IO.println "To serve the documentation locally:"
  IO.println s!"  cd {outputPath}"
  IO.println "  mkdocs serve"

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
  "Generate verification coverage report only (MkDocs format)"

  FLAGS:
    o, output : String;    "Output directory for generated docs (default: docs)"
    r, repo : String;      "Repository URL for source links"
    p, platform : String;  "Git platform: github or gitlab (default: github)"
    b, branch : String;    "Git branch name (default: main)"
    project : String;      "Project name for documentation (default: API Coverage)"

  ARGS:
    ...modules : String;   "Module names to analyze"
]

/-- Default command runs unified -/
def runDefaultCmd (_args : Cli.Parsed) : IO UInt32 := do
  -- If no subcommand, show help
  IO.println "doc-verification-bridge - Unified Lean 4 Documentation"
  IO.println ""
  IO.println "Usage:"
  IO.println "  lake exe doc-verification-bridge unified [OPTIONS] <modules...>"
  IO.println "  lake exe doc-verification-bridge docgen4 [OPTIONS] <modules...>"
  IO.println "  lake exe doc-verification-bridge verify [OPTIONS] <modules...>"
  IO.println ""
  IO.println "Run with --help for more information."
  return 0

/-- Main CLI command with subcommands -/
def mainCmd : Cmd := `[Cli|
  "doc-verification-bridge" VIA runDefaultCmd; ["1.0.0"]
  "Unified Lean 4 documentation with theorem classification"

  SUBCOMMANDS:
    unifiedCmd;
    docgen4Cmd;
    verifyCmd

  EXTENSIONS:
    author "doc-verification-bridge contributors";
    longDescription "
doc-verification-bridge - Unified Lean 4 Documentation

Combines doc-gen4's HTML documentation with theorem classification
for specification-implementation refinement.

COMMANDS:

  unified   Generate combined doc-gen4 + verification (recommended)
  docgen4   Generate doc-gen4 HTML only
  verify    Generate verification report only (MkDocs)

FOUR-CATEGORY ONTOLOGY:

  • Mathematical Abstractions: Abstract types, Prop-based structures
  • Computational Datatypes: Concrete data structures
  • Mathematical Definitions: Prop-returning predicates/relations
  • Computational Operations: Computable functions, Bool predicates

Bridging theorems prove the relations between these levels.

EXAMPLES:

  # Generate unified documentation for batteries
  lake exe doc-verification-bridge unified \\
    --repo https://github.com/leanprover-community/batteries \\
    --project 'Batteries' \\
    Batteries

  # Just run verification analysis
  lake exe doc-verification-bridge verify \\
    --output docs \\
    --repo https://github.com/example/project \\
    MyProject
"
]

def main (args : List String) : IO UInt32 :=
  mainCmd.validate args
