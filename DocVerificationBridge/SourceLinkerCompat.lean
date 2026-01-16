-- DocVerificationBridge/SourceLinkerCompat.lean
-- Uses NicolasRouquette/doc-gen4 fork with custom source linker and declaration decorator support

import Lean
import DocGen4
import DocGen4.Output

open Lean DocGen4 DocGen4.Output DocGen4.Process

namespace DocVerificationBridge

/-- Compatibility shim for doc-gen4 HTML output.
    This version uses the extended API with custom source linker and declaration decorator. -/
def htmlOutputResultsCompat
    (baseConfig : SiteBaseContext)
    (result : AnalyzerResult)
    (sourceUrl? : Option String)
    (customLinker? : Option (Name → Option DeclarationRange → String) := none)
    (declarationDecorator? : Option DeclarationDecoratorFn := none)
    : IO (Array System.FilePath) := do
  -- Use extended API with custom linker wrapped appropriately
  let sourceLinkerFn? : Option SourceLinkerFn := customLinker?.map fun linker =>
    fun _sourceUrl? modName range => linker modName range
  DocGen4.htmlOutputResults baseConfig result sourceUrl? sourceLinkerFn? declarationDecorator?

end DocVerificationBridge
