-- DocVerificationBridge/SourceLinkerCompat.lean
-- Compatibility shim for doc-gen4 HTML output.
-- Adapts to the DB-based pipeline in doc-gen4 ≥ v4.29.0-rc1.

import Lean
import DocGen4
import DocGen4.Output
import DocGen4.DB

open Lean DocGen4 DocGen4.Output DocGen4.Process DocGen4.DB

namespace DocVerificationBridge

/-- Write an `AnalyzerResult` to a temporary SQLite database, then render HTML
    using doc-gen4's parallel pipeline.  The optional `sourceLinker?` and
    `declarationDecorator?` are forwarded to `htmlOutputResultsParallel`. -/
def htmlOutputResultsCompat
    (baseConfig : SiteBaseContext)
    (result : AnalyzerResult)
    (sourceUrl? : Option String)
    (customLinker? : Option (Name → Option DeclarationRange → String) := none)
    (declarationDecorator? : Option DeclarationDecoratorFn := none)
    : IO (Array System.FilePath) := do
  -- 1. Persist the AnalyzerResult into a temporary SQLite database
  let dbPath := baseConfig.buildDir / "docgen4-temp.sqlite"
  updateModuleDb builtinDocstringValues result (toString baseConfig.buildDir) "docgen4-temp.sqlite" sourceUrl?

  -- 2. Open the DB for reading and build the linking context
  let db ← openForReading dbPath builtinDocstringValues
  let linkCtx ← db.loadLinkingContext

  -- 3. Wrap the custom linker in SourceLinkerFn form
  let sourceLinkerFn? : Option SourceLinkerFn := customLinker?.map fun linker =>
    fun _sourceUrl? modName range => linker modName range

  -- 4. Run the parallel HTML generator
  let (outputs, jsonModules) ← htmlOutputResultsParallel baseConfig dbPath linkCtx
    linkCtx.moduleNames sourceLinkerFn? declarationDecorator?

  -- 5. Build search index + static pages (tactics are loaded from DB)
  let allTacticsRaw ← db.loadAllTactics
  let refsMap : Std.HashMap String BibItem :=
    Std.HashMap.emptyWithCapacity baseConfig.refs.size |>.insertMany
      (baseConfig.refs.iter.map fun x => (x.citekey, x))
  let minimalSiteCtx : SiteContext := {
    result := { name2ModIdx := linkCtx.name2ModIdx, moduleNames := linkCtx.moduleNames, moduleInfo := {} }
    sourceLinker := fun _ _ => "#"
    refsMap := refsMap
  }
  let (allTactics, _) := allTacticsRaw.mapM TacticInfo.docStringToHtml |>.run {} minimalSiteCtx baseConfig
  htmlOutputIndex baseConfig jsonModules allTactics

  return outputs

end DocVerificationBridge
