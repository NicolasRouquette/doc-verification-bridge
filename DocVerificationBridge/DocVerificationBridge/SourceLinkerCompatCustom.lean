-- DocVerificationBridge/SourceLinkerCompatCustom.lean
-- Custom version: Uses NicolasRouquette/doc-gen4 fork with 4-argument API
-- This file is copied to docvb/ (as SourceLinkerCompat.lean) when useCustomSourceLinker = true
-- Requires: doc-gen4 from https://github.com/NicolasRouquette/doc-gen4 @ feat/source-linker

import Lean
import DocGen4
import DocGen4.Output

open Lean DocGen4 DocGen4.Output DocGen4.Process

namespace DocVerificationBridge

/-- Compatibility shim for doc-gen4 HTML output.
    This version uses the custom 4-argument API from NicolasRouquette/doc-gen4 fork
    which supports custom source linker functions. -/
def htmlOutputResultsCompat
    (baseConfig : SiteBaseContext)
    (result : AnalyzerResult)
    (sourceUrl? : Option String)
    (customLinker? : Option (Name → Option DeclarationRange → String) := none)
    : IO (Array System.FilePath) := do
  -- Convert our linker type to doc-gen4's SourceLinkerFn type
  -- SourceLinkerFn = Option String → Name → Option DeclarationRange → String
  let sourceLinkerFn? : Option SourceLinkerFn := customLinker?.map fun linker =>
    fun _srcUrl? modName range => linker modName range
  DocGen4.htmlOutputResults baseConfig result sourceUrl? sourceLinkerFn?

end DocVerificationBridge
