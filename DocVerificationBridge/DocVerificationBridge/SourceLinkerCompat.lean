-- DocVerificationBridge/SourceLinkerCompat.lean
-- Compatibility shim for doc-gen4 v4.29.0+ database-based API

import Lean
import DocGen4
import DocGen4.Output
import DocGen4.DB

open Lean DocGen4 DocGen4.Output DocGen4.Process DocGen4.DB

namespace DocVerificationBridge

/-- Compatibility shim for doc-gen4 HTML output.
    This version bridges the old in-memory AnalyzerResult API to the new database-based API
    used in doc-gen4 v4.29.0+. -/
def htmlOutputResultsCompat
    (baseConfig : SiteBaseContext)
    (result : AnalyzerResult)
    (sourceUrl? : Option String)
    (customLinker? : Option (Name → Option DeclarationRange → String) := none)
    (declarationDecorator? : Option DeclarationDecoratorFn := none)
    : IO (Array System.FilePath × Array JsonModule) := do
  -- Create a temporary database to bridge to the new API
  let dbPath := baseConfig.buildDir / "temp-docgen.db"

  -- Write AnalyzerResult to database
  updateModuleDb builtinDocstringValues result baseConfig.buildDir "temp-docgen.db" sourceUrl?

  -- Open database for reading and load linking context
  let db ← openForReading dbPath builtinDocstringValues
  let linkCtx ← db.loadLinkingContext

  -- Convert custom linker to SourceLinkerFn format
  let sourceLinkerFn? : Option SourceLinkerFn := customLinker?.map fun linker =>
    fun _sourceUrl? modName range => linker modName range

  -- Generate HTML using new parallel API
  let (outputs, jsonModules) ← htmlOutputResultsParallel
    baseConfig dbPath linkCtx linkCtx.moduleNames sourceLinkerFn? declarationDecorator?

  return (outputs, jsonModules)

/-- Collect all tactics from an AnalyzerResult and convert them to HTML.
    This is needed for htmlOutputIndex in v4.29.0+. -/
def collectTactics (result : AnalyzerResult) (baseConfig : SiteBaseContext) : IO (Array (Process.TacticInfo Html)) := do
  -- Collect all tactics from all modules
  let mut allTacticsRaw : Array (Process.TacticInfo Process.MarkdownDocstring) := #[]
  for (_, modInfo) in result.moduleInfo.toArray do
    allTacticsRaw := allTacticsRaw ++ modInfo.tactics

  -- Build minimal SiteContext for docstring conversion
  let refsMap : Std.HashMap String BibItem :=
    Std.HashMap.emptyWithCapacity baseConfig.refs.size |>.insertMany
      (baseConfig.refs.iter.map fun x => (x.citekey, x))
  let minimalSiteCtx : SiteContext := {
    result := result
    sourceLinker := fun _ _ => "#"
    refsMap := refsMap
  }

  -- Convert markdown docstrings to HTML
  let (allTactics, _) := allTacticsRaw.mapM Process.TacticInfo.docStringToHtml
    |>.run {} minimalSiteCtx baseConfig

  return allTactics

end DocVerificationBridge
