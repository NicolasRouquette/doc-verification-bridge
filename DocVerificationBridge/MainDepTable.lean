-- MainDepTable.lean — entry point for the `unified-dep-table` exe.
--
-- Produces a per-declaration dependency table for a given namespace
-- prefix (typically a Lean module path).  See DependencyTable.lean for
-- the analysis library.
--
-- Two modes:
--   * fresh:  load the named modules, run classification, build table.
--             Slow on first call; populates dependsOn from proof terms.
--   * cached: load a pre-saved classification (from `unified-doc unified
--             --save-classification ...`) and just build the table.
--             Cheap; recommended for iteration.
--
-- Examples (run from the project being analyzed, after sourcing its
-- toolchain so DVB sees the same .olean files):
--
--   # Fresh classification + table for L4YAML.Proofs.ParserWellBehaved
--   lake env -- unified-dep-table fresh \
--     --namespace L4YAML.Proofs.ParserWellBehaved \
--     --output dep-parser-wellbehaved.md \
--     L4YAML
--
--   # From an existing classification cache
--   lake env -- unified-dep-table cached \
--     --namespace L4YAML.Proofs.ParserWellBehaved \
--     --classification cache.json \
--     --output dep-parser-wellbehaved.md

import Cli
import Lean
import DocVerificationBridge
import DocVerificationBridge.DependencyTable
import DependencyAnalysis.Cache

open Lean System Cli DependencyAnalysis DocVerificationBridge

/-- Run classification for one or more modules and return a unified
    `NameMap APIMeta`.  This is the *fresh* path; it walks each module's
    declarations and extracts proof-term dependencies, so it can take
    minutes on a large library. -/
def classifyModules (moduleNames : Array Name) (skipProofDeps : Bool)
    (proofDepWorkers : Nat) : IO (NameMap APIMeta) := do
  Lean.initSearchPath (← Lean.findSysroot)
  let env ← importModules
    (moduleNames.map (fun n => { module := n })) {}
  IO.println s!"  Loaded {env.header.moduleNames.size} modules"
  (← IO.getStdout).flush

  let pipelineStart ← IO.monoMsNow
  let mut allEntries : NameMap APIMeta := {}
  for modName in moduleNames do
    let coreCtx : Core.Context := {
      options := {},
      fileName := "<unified-dep-table>",
      fileMap := default,
      maxHeartbeats := 0  -- unlimited; classification is the bottleneck
    }
    let coreState : Core.State := { env }
    let (result, _) ←
      (classifyAllDeclarations env modName skipProofDeps proofDepWorkers
                                pipelineStart #[] 30).run' {}
        |>.toIO coreCtx coreState
    -- Tail-recursive merge to avoid stack overflow on very large maps.
    let entriesArray := result.entries.foldl (init := (#[] : Array (Name × APIMeta)))
      fun acc name apiMeta => acc.push (name, apiMeta)
    allEntries := entriesArray.foldl (init := allEntries) fun acc (name, apiMeta) =>
      acc.insert name apiMeta
  IO.println s!"  Classified {allEntries.size} declarations"
  (← IO.getStdout).flush
  return allEntries

/-- Write the rendered table to `output`, or stdout if `output` is none. -/
def writeOutput (output : Option String) (markdown : String) : IO Unit := do
  match output with
  | some path => do
    if let some parent := (System.FilePath.mk path).parent then
      IO.FS.createDirAll parent
    IO.FS.writeFile path markdown
    IO.println s!"  Wrote {path}"
  | none =>
    IO.println markdown

/-- Common back-end: given a populated `allEntries`, build and emit the
    table according to args. -/
def emitTable (allEntries : NameMap APIMeta) (args : Cli.Parsed) : IO UInt32 := do
  let nsStr := args.flag? "namespace" |>.map (·.as! String) |>.getD ""
  if nsStr.isEmpty then
    IO.eprintln "Error: --namespace is required."
    return 1
  let namespacePrefix := nsStr.toName
  let externalOnly := args.hasFlag "external-only"
  let table := DependencyTable.build allEntries namespacePrefix externalOnly
  let modeStr := if externalOnly then " (external-only)" else ""
  IO.println s!"  Built table: {table.entries.size} entries in `{namespacePrefix}`{modeStr}"
  IO.println s!"  Deletion candidates: {table.deletionCandidates.size}"
  let outFlag := args.flag? "output" |>.map (·.as! String)
  let format := args.flag? "format" |>.map (·.as! String) |>.getD "markdown"
  let rendered := match format with
    | "json" => (table.toJson).pretty
    | _      => table.toMarkdown
  writeOutput outFlag rendered
  return 0

/-- `fresh` subcommand: classify modules from scratch, then build table. -/
def runFresh (args : Cli.Parsed) : IO UInt32 := do
  let modules : Array String := args.variableArgsAs! String
  if modules.isEmpty then
    IO.eprintln "Error: At least one module name required (positional arg)."
    return 1
  let skipProofDeps := args.hasFlag "skip-proof-deps"
  if skipProofDeps then
    IO.eprintln "Warning: --skip-proof-deps disables proof-term scanning."
    IO.eprintln "         The dependsOn column will be empty and deletion"
    IO.eprintln "         candidates will not be meaningful."
  let proofDepWorkers := args.flag? "proof-dep-workers" |>.map (·.as! Nat) |>.getD 0
  IO.println s!"unified-dep-table [1/3]: Loading {modules.size} module(s)..."
  (← IO.getStdout).flush
  let allEntries ← classifyModules (modules.map String.toName) skipProofDeps proofDepWorkers
  IO.println "unified-dep-table [2/3]: Building table..."
  emitTable allEntries args

/-- `cached` subcommand: load a pre-saved classification, then build table. -/
def runCached (args : Cli.Parsed) : IO UInt32 := do
  let cachePath := args.flag? "classification" |>.map (·.as! String) |>.getD ""
  if cachePath.isEmpty then
    IO.eprintln "Error: --classification is required (path to a saved classification)."
    IO.eprintln "       Generate one with `unified-doc unified --save-classification PATH ...`"
    return 1
  IO.println s!"unified-dep-table [1/2]: Loading classification from {cachePath}..."
  (← IO.getStdout).flush
  let allEntries ← Cache.loadClassification cachePath
  IO.println s!"  Loaded {allEntries.size} declarations"
  IO.println "unified-dep-table [2/2]: Building table..."
  emitTable allEntries args

/-- Default: print help. -/
def runDefault (_args : Cli.Parsed) : IO UInt32 := do
  IO.println "unified-dep-table — per-declaration dependency table"
  IO.println ""
  IO.println "Usage:"
  IO.println "  unified-dep-table fresh  --namespace <ns> [OPTIONS] <modules...>"
  IO.println "  unified-dep-table cached --namespace <ns> --classification <path> [OPTIONS]"
  IO.println ""
  IO.println "Run with --help for full options."
  return 0

def freshCmd := `[Cli|
  fresh VIA runFresh;
  "Classify modules from scratch then build the table"

  FLAGS:
    n, "namespace" : String;       "Namespace prefix to filter on (REQUIRED)"
    o, output     : String;       "Output path (default: stdout)"
    f, format     : String;       "Output format: markdown (default) or json"
    "external-only";               "Suppress in-namespace callers; surfaces transitively-dead clusters"
    "skip-proof-deps";             "Skip proof-term dep extraction (faster, but no calledBy data)"
    "proof-dep-workers" : Nat;     "Parallel workers for proof dep extraction (default: 0 = sequential)"

  ARGS:
    ...modules : String;          "Module names to classify"
]

def cachedCmd := `[Cli|
  cached VIA runCached;
  "Load a pre-saved classification then build the table"

  FLAGS:
    n, "namespace"     : String;  "Namespace prefix to filter on (REQUIRED)"
    c, "classification" : String;  "Path to saved classification (from `unified-doc --save-classification`) (REQUIRED)"
    o, output          : String;  "Output path (default: stdout)"
    f, format          : String;  "Output format: markdown (default) or json"
    "external-only";               "Suppress in-namespace callers; surfaces transitively-dead clusters"
]

def mainCmd : Cmd := `[Cli|
  "unified-dep-table" VIA runDefault; ["1.0.0"]
  "Per-declaration dependency table over a Lean library"

  SUBCOMMANDS:
    freshCmd;
    cachedCmd

  EXTENSIONS:
    author "doc-verification-bridge contributors";
    longDescription "
unified-dep-table - Per-declaration dependency table

Produces a markdown (or JSON) table covering every classified
declaration in a chosen namespace, with:

  * dependsOn — proof-term references to other classified declarations
  * calledBy  — inverse map (transposed from the global graph)
  * hasSorry  — sorry usage in proof or body

The 'Deletion candidates' section flags declarations that carry sorry
AND have no in-scope callers — strong signal that the declaration is
dead weight within the analyzed scope.

EXAMPLES:

  # First time — classify L4YAML and emit a table for ParserWellBehaved
  unified-dep-table fresh \\
    --namespace L4YAML.Proofs.ParserWellBehaved \\
    --output dep-pwb.md \\
    L4YAML

  # Subsequent runs — reuse the classification cache
  unified-dep-table cached \\
    --namespace L4YAML.Proofs.ParserWellBehaved \\
    --classification .lake/build/doc/classification.json \\
    --output dep-pwb.md

  # Pipe JSON to jq for custom filtering
  unified-dep-table cached --format json --classification ... \\
    --namespace L4YAML.Proofs.Output | jq '.entries[] | select(.hasSorry)'
"
]

def main (args : List String) : IO UInt32 :=
  mainCmd.validate args
